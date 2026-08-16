box::use(
  shiny[
    reactiveVal,
    reactive,
    observeEvent,
    observe,
    invalidateLater,
    req,
    isolate,
    renderUI,
    HTML,
    downloadHandler,
    updateNumericInput,
    updateRadioButtons,
    updateDateInput,
    updateDateRangeInput,
    updateActionButton,
    showModal,
    removeModal,
    showNotification,
    modalDialog,
    modalButton,
    tagList,
    outputOptions,
    div,
    p,
    span,
    tags,
    actionButton
  ],
  shinyjs[runjs],
  bslib[nav_select],
  bsicons[bs_icon],
  DBI[dbGetQuery],
)

MN_PRESET <- list(
  total_hours_goal = 4000,
  therapy_hours_goal = 1000,
  relational_hours_goal = 500,
  supervision_individual_goal = 100,
  supervision_group_goal = 100
)

# admin_hours_goal is excluded -- it's always computed as the remainder
# (total minus therapy minus both supervision goals), never set directly
GOAL_INPUT_IDS <- c(
  "total_hours_goal",
  "therapy_hours_goal",
  "relational_hours_goal",
  "supervision_individual_goal",
  "supervision_group_goal"
)

sanitize_filename <- function(name) {
  if (is.null(name) || name == "") {
    name <- "Hours"
  }
  gsub("[^A-Za-z0-9]+", "_", name)
}

#' Friendly placeholder shown wherever a data-dependent card would otherwise
#' render blank/NA for a brand-new account with zero logged hours.
#' Only one card on the dashboard should show the "Log your first hours" CTA
#' (show_cta = TRUE, the default) -- the rest just state there's no data yet.
empty_state <- function(message, show_cta = TRUE) {
  # min-height, not h-100 -- these render inside a plain uiOutput div with no
  # defined height of its own, so h-100 (height: 100%) has nothing to resolve
  # against and only the horizontal centering would take effect
  if (!show_cta) {
    return(div(
      class = "d-flex align-items-center justify-content-center text-center p-3",
      style = "min-height: 220px;",
      p(class = "text-muted mb-0", message)
    ))
  }

  div(
    class = "d-flex flex-column align-items-center justify-content-center text-center p-3",
    style = "min-height: 220px;",
    div(
      class = "mb-2",
      style = "font-size: 1.5rem; color: #C11C84;",
      bs_icon("clock-history")
    ),
    p(class = "text-muted mb-3", message),
    actionButton(
      "goto_track_hours",
      "Log your first hours",
      class = "btn btn-primary btn-sm"
    )
  )
}

#' @param config config module (see R/config.R)
#' @param db db module (see R/db.R)
#' @param auth auth module (see R/auth.R)
#' @param users users module (see R/users.R)
#' @param licensure licensure module (see R/licensure.R)
#' @param ui_components ui_components module (see R/ui_components.R)
#' @param utils utils module (see R/utils.R)
#' @param plots plots module (see R/plots.R)
#' @param report report module (see R/report.R)
#' @export
server <- function(
  input,
  output,
  session,
  config,
  db,
  auth,
  users,
  licensure,
  ui_components,
  utils,
  plots,
  report
) {
  cfg <- config$get_config()
  tbl <- db$get_table_name(config)
  google_client_id <- cfg$google_client_id

  pool <- db$connect(config)
  session$onSessionEnded(function() {
    db$disconnect(pool)
  })

  authenticated <- reactiveVal(FALSE)
  needs_signup <- reactiveVal(FALSE)
  pending_identity <- reactiveVal(NULL)
  user_id <- reactiveVal(NULL)
  user_name <- reactiveVal(NULL)
  user_email <- reactiveVal(NULL)
  user_cfg <- reactiveVal(NULL)

  output$is_authenticated <- reactive({
    authenticated()
  })
  outputOptions(output, "is_authenticated", suspendWhenHidden = FALSE)

  output$needs_signup <- reactive({
    needs_signup()
  })
  outputOptions(output, "needs_signup", suspendWhenHidden = FALSE)

  observeEvent(input$google_credential, {
    claims <- auth$verify_google_id_token(
      input$google_credential,
      google_client_id
    )

    if (is.null(claims) || !claims$email_verified) {
      shinyjs::show("login_error")
      return()
    }
    shinyjs::hideElement("login_error")

    # Scopes RLS on `users` for the lookup/insert below, which necessarily
    # run before set_session_user() -- discovering/creating the user row is
    # how the app learns what app.user_id even is. Persists for the rest of
    # this connection, so it also covers create_user() in the signup handler.
    db$set_session_pending_sub(pool, claims$sub)

    existing <- users$get_user_by_google_sub(pool, claims$sub)

    if (nrow(existing) > 0) {
      uid <- existing$id[1]
      db$set_session_user(pool, uid)
      user_id(uid)
      user_name(existing$name[1])
      user_email(existing$email[1])
      user_cfg(users$get_user_config(pool, uid))
      authenticated(TRUE)
    } else {
      pending_identity(claims)
      needs_signup(TRUE)
    }
  })

  observeEvent(
    input$state_preset,
    {
      if (input$state_preset == "mn") {
        updateNumericInput(
          session,
          "total_hours_goal",
          value = MN_PRESET$total_hours_goal
        )
        updateNumericInput(
          session,
          "therapy_hours_goal",
          value = MN_PRESET$therapy_hours_goal
        )
        updateNumericInput(
          session,
          "relational_hours_goal",
          value = MN_PRESET$relational_hours_goal
        )
        updateNumericInput(
          session,
          "supervision_individual_goal",
          value = MN_PRESET$supervision_individual_goal
        )
        updateNumericInput(
          session,
          "supervision_group_goal",
          value = MN_PRESET$supervision_group_goal
        )
        lapply(GOAL_INPUT_IDS, shinyjs::disable)
      } else {
        lapply(GOAL_INPUT_IDS, shinyjs::enable)
      }
      shinyjs::disable("admin_hours_goal") # always computed, never directly editable
    },
    ignoreInit = FALSE
  )

  observe({
    req(
      input$total_hours_goal,
      input$therapy_hours_goal,
      input$supervision_individual_goal,
      input$supervision_group_goal
    )
    admin <- input$total_hours_goal -
      input$therapy_hours_goal -
      input$supervision_individual_goal -
      input$supervision_group_goal
    updateNumericInput(session, "admin_hours_goal", value = max(0, admin))
  })

  observeEvent(user_cfg(), {
    cfg_now <- user_cfg()
    updateNumericInput(session, "acct_total_hours_goal", value = cfg_now$total_hours_goal)
    updateNumericInput(session, "acct_therapy_hours_goal", value = cfg_now$therapy_hours_goal)
    updateNumericInput(session, "acct_relational_hours_goal", value = cfg_now$relational_hours_goal)
    updateNumericInput(
      session,
      "acct_supervision_individual_goal",
      value = cfg_now$supervision_individual_goal
    )
    updateNumericInput(
      session,
      "acct_supervision_group_goal",
      value = cfg_now$supervision_group_goal
    )
    updateNumericInput(session, "acct_admin_hours_goal", value = cfg_now$admin_hours_goal)

    if (cfg_now$relational_hours_goal > 0) {
      shinyjs::show("hrs_wrapper_relational_couple")
      shinyjs::show("hrs_wrapper_relational_family")
    } else {
      shinyjs::hide("hrs_wrapper_relational_couple")
      shinyjs::hide("hrs_wrapper_relational_family")
    }
  })

  observe({
    req(
      input$acct_total_hours_goal,
      input$acct_therapy_hours_goal,
      input$acct_supervision_individual_goal,
      input$acct_supervision_group_goal
    )
    admin <- input$acct_total_hours_goal -
      input$acct_therapy_hours_goal -
      input$acct_supervision_individual_goal -
      input$acct_supervision_group_goal
    updateNumericInput(session, "acct_admin_hours_goal", value = max(0, admin))
  })

  observeEvent(input$save_goals, {
    req(authenticated())

    result <- tryCatch(
      {
        users$update_user_config(
          pool,
          user_id(),
          total_hours_goal = input$acct_total_hours_goal,
          therapy_hours_goal = input$acct_therapy_hours_goal,
          relational_hours_goal = input$acct_relational_hours_goal,
          supervision_individual_goal = input$acct_supervision_individual_goal,
          supervision_group_goal = input$acct_supervision_group_goal,
          admin_hours_goal = input$acct_admin_hours_goal
        )
        TRUE
      },
      error = function(e) e
    )

    if (isTRUE(result)) {
      user_cfg(users$get_user_config(pool, user_id()))
      showNotification("Goals saved.", type = "message")
    } else {
      showNotification(paste("Couldn't save goals:", conditionMessage(result)), type = "error")
    }
  })

  signup_waiter <- waiter::Waiter$new(
    id = "signup_wizard_container",
    html = waiter::spin_heart(),
    color = "rgba(248, 200, 220, 0.7)"
  )

  observeEvent(input$complete_signup, {
    req(pending_identity())

    if (!isTRUE(input$policy_accepted)) {
      shinyjs::show("signup_error")
      return()
    }
    shinyjs::hideElement("signup_error")

    signup_waiter$show()
    on.exit(signup_waiter$hide(), add = TRUE)

    claims <- pending_identity()
    uid <- users$create_user(
      pool,
      sub = claims$sub,
      email = claims$email,
      name = claims$name,
      total_hours_goal = input$total_hours_goal,
      therapy_hours_goal = input$therapy_hours_goal,
      relational_hours_goal = input$relational_hours_goal,
      supervision_individual_goal = input$supervision_individual_goal,
      supervision_group_goal = input$supervision_group_goal,
      admin_hours_goal = input$admin_hours_goal
    )

    db$set_session_user(pool, uid)
    user_id(uid)
    user_name(claims$name)
    user_email(claims$email)
    user_cfg(users$get_user_config(pool, uid))
    pending_identity(NULL)
    needs_signup(FALSE)
    authenticated(TRUE)
  })

  observeEvent(input$goto_track_hours, {
    nav_select("main_navbar", selected = "Track Hours", session = session)
  })

  observeEvent(input$logout, {
    req(authenticated())
    session$reload()
  })

  observeEvent(input$nuke_account, {
    req(authenticated())
    showModal(modalDialog(
      title = "Delete your account?",
      "This permanently deletes your account and every hours entry you've logged. This cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_nuke_account", "Delete My Account & All Data", class = "btn btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_nuke_account, {
    req(authenticated())
    removeModal()
    users$delete_user(pool, user_id())
    session$reload()
  })

  quotes <- cfg$quotes

  available_quotes <- reactiveVal(quotes[-1])
  shown_quotes <- reactiveVal(list(quotes[[1]]))
  current_quote <- reactiveVal(quotes[[1]])

  therapy_metrics <- reactiveVal(NULL)
  work_metrics <- reactiveVal(NULL)
  monthly_breakdown <- reactiveVal(NULL)
  licensure_data <- reactiveVal(NULL)
  hours_view <- reactiveVal("all")

  observeEvent(input$hours_view_all, hours_view("all"))
  observeEvent(input$hours_view_therapy, hours_view("therapy"))
  observeEvent(input$hours_view_other, hours_view("other"))

  local({
    first_pick <- sample(quotes, 1)[[1]]
    available_quotes(setdiff(quotes, list(first_pick)))
    shown_quotes(list(first_pick))
    current_quote(first_pick)
  })

  observe({
    req(authenticated(), user_cfg(), input$main_navbar == "Dashboard")

    therapy_metrics(list(
      week = db$get_therapy_hours(pool, tbl, "week", user_cfg = user_cfg()),
      month = db$get_therapy_hours(pool, tbl, "month", user_cfg = user_cfg()),
      year = db$get_therapy_hours(pool, tbl, "year", user_cfg = user_cfg()),
      last_year = db$get_therapy_hours(
        pool,
        tbl,
        "year",
        1,
        user_cfg = user_cfg()
      )
    ))

    work_metrics(list(
      week = db$get_work_hours(pool, tbl, "week", user_cfg = user_cfg()),
      month = db$get_work_hours(pool, tbl, "month", user_cfg = user_cfg()),
      year = db$get_work_hours(pool, tbl, "year", user_cfg = user_cfg()),
      last_year = db$get_work_hours(pool, tbl, "year", 1, user_cfg = user_cfg())
    ))

    licensure_data(licensure$get_licensure_progress(pool, tbl, user_cfg()))
  })

  observe({
    req(authenticated(), user_cfg(), input$main_navbar == "Dashboard")

    months <- isolate(input$months_slider)
    if (is.null(months)) {
      months <- 6
    }

    monthly_breakdown(db$get_monthly_hours_breakdown(
      pool,
      tbl,
      as.numeric(months),
      user_cfg()
    ))
  })

  observeEvent(
    input$months_slider,
    {
      req(authenticated(), user_cfg(), input$months_slider)
      monthly_breakdown(db$get_monthly_hours_breakdown(
        pool,
        tbl,
        as.numeric(input$months_slider),
        user_cfg()
      ))
    },
    ignoreInit = FALSE
  )

  output$current_quote <- renderUI({
    req(authenticated())
    HTML(current_quote())
  })

  observeEvent(input$generate_btn, {
    req(authenticated())
    runjs("document.getElementById('quote-text').classList.add('fade-out');")
    invalidateLater(300)

    if (length(available_quotes()) == 0) {
      available_quotes(unlist(shown_quotes(), recursive = FALSE))
      shown_quotes(list())
    }

    new_quote <- sample(available_quotes(), 1)[[1]]
    available_quotes(setdiff(available_quotes(), list(new_quote)))
    shown_quotes(c(shown_quotes(), list(new_quote)))
    current_quote(new_quote)

    runjs(
      "
      setTimeout(function() {
        document.getElementById('quote-text').classList.remove('fade-out');
      }, 50);
    "
    )
  })

  output$licensure_progress <- renderUI({
    req(authenticated())
    progress_data <- licensure_data()

    if (is.null(progress_data)) {
      return(div(
        class = "d-flex justify-content-center align-items-center h-100",
        "Loading licensure progress data..."
      ))
    }

    if (progress_data$licensure_total == 0) {
      return(empty_state(
        "No hours logged yet — track your first entry to start seeing your licensure progress here."
      ))
    }

    recent_hours_query <- sprintf(
      "
      WITH monthly_data AS (
        SELECT
          date_trunc('month', start_date)::date as month,
          SUM(individual + relational_couple + relational_family +
              supervision_individual + supervision_group + consultation +
              case_notes + session_plan + emails + letters +
              staff_meetings + cont_ed + exam_prep) as monthly_total
        FROM %s
        WHERE start_date >= current_date - interval '3 months'
        GROUP BY date_trunc('month', start_date)
        ORDER BY date_trunc('month', start_date) DESC
      )
      SELECT AVG(monthly_total) as avg_monthly_hours
      FROM monthly_data",
      tbl
    )

    recent_avg <- dbGetQuery(pool, recent_hours_query)
    avg_monthly_hours <- as.numeric(recent_avg$avg_monthly_hours)

    estimated_completion_date <- if (
      is.na(avg_monthly_hours) || avg_monthly_hours <= 0
    ) {
      "..."
    } else {
      months_remaining <- ceiling(
        progress_data$licensure_remaining / avg_monthly_hours
      )
      format(Sys.Date() + months(months_remaining), "%B %Y")
    }

    licensure$render_licensure_progress(
      progress_data,
      estimated_completion_date
    )
  })

  output$hours_view_toggle <- renderUI({
    view <- hours_view()

    toggle_btn <- function(id, label, key) {
      actionButton(
        id,
        label,
        class = paste(
          "btn btn-sm",
          if (view == key) "btn-primary" else "btn-outline-primary"
        )
      )
    }

    div(
      class = "btn-group btn-group-sm",
      role = "group",
      toggle_btn("hours_view_all", "All", "all"),
      toggle_btn("hours_view_therapy", "Therapy", "therapy"),
      toggle_btn("hours_view_other", "Other", "other")
    )
  })

  output$hours_summary <- renderUI({
    req(authenticated())
    metrics <- work_metrics()
    therapy <- therapy_metrics()
    view <- hours_view()

    if (is.null(metrics) || is.null(therapy)) {
      return(div(
        class = "d-flex justify-content-center align-items-center h-100",
        "Loading metrics..."
      ))
    }

    if (metrics$week$grand_total == 0) {
      return(empty_state("No hours logged yet.", show_cta = FALSE))
    }

    if (view == "therapy") {
      grand_total <- therapy$week$grand_total
      row_data <- therapy
      track_relational <- user_cfg()$relational_hours_goal > 0
      relational_expr <- if (track_relational) "relational_couple + relational_family" else "0"
      couple_expr <- if (track_relational) "relational_couple" else "0"
      family_expr <- if (track_relational) "relational_family" else "0"

      grand_total_query <- sprintf(
        "
      SELECT
        SUM(individual) as total_individual,
        SUM(%s) as total_relational,
        SUM(%s) as total_couple,
        SUM(%s) as total_family,
        SUM(individual + %s) as total_all
      FROM %s",
        relational_expr,
        couple_expr,
        family_expr,
        relational_expr,
        tbl
      )

      totals <- dbGetQuery(pool, grand_total_query)

      individual_ratio <- round(
        totals$total_individual / totals$total_all * 100,
        0
      )

      breakdown <- div(
        class = "mb-2",
        style = "border-left: 3px solid #C11C84; padding-left: 10px;",
        div(
          sprintf(
            "Individual: %s hrs (%s%%)",
            utils$format_number(totals$total_individual),
            utils$format_number(individual_ratio)
          )
        ),
        if (track_relational) {
          relational_ratio <- round(
            totals$total_relational / totals$total_all * 100,
            0
          )
          couple_ratio <- round(
            totals$total_couple / totals$total_relational * 100,
            0
          )
          family_ratio <- round(
            totals$total_family / totals$total_relational * 100,
            0
          )
          tagList(
            div(
              sprintf(
                "Relational: %s hrs (%s%%)",
                utils$format_number(totals$total_relational),
                utils$format_number(relational_ratio)
              )
            ),
            div(
              class = "ms-3 mt-1",
              style = "font-size: 0.9em;",
              sprintf(
                "Couple: %s hrs (%s%%)",
                utils$format_number(totals$total_couple),
                utils$format_number(couple_ratio)
              )
            ),
            div(
              class = "ms-3",
              style = "font-size: 0.9em;",
              sprintf(
                "Family: %s hrs (%s%%)",
                utils$format_number(totals$total_family),
                utils$format_number(family_ratio)
              )
            )
          )
        }
      )
    } else if (view == "other") {
      grand_total <- metrics$week$grand_total - therapy$week$grand_total

      grand_total_query <- sprintf(
        "
      SELECT
        SUM(supervision_individual) as total_supervision_individual,
        SUM(supervision_group) as total_supervision_group,
        SUM(supervision_individual + supervision_group) as total_supervision,
        SUM(consultation + case_notes + session_plan + emails + letters +
            staff_meetings + cont_ed + exam_prep) as total_admin,
        SUM(supervision_individual + supervision_group + consultation + case_notes +
            session_plan + emails + letters + staff_meetings + cont_ed + exam_prep) as total_all
      FROM %s",
        tbl
      )

      totals <- dbGetQuery(pool, grand_total_query)

      supervision_ratio <- round(
        totals$total_supervision / totals$total_all * 100,
        0
      )
      admin_ratio <- round(totals$total_admin / totals$total_all * 100, 0)
      supervision_individual_ratio <- round(
        totals$total_supervision_individual / totals$total_supervision * 100,
        0
      )
      supervision_group_ratio <- round(
        totals$total_supervision_group / totals$total_supervision * 100,
        0
      )

      breakdown <- div(
        class = "mb-2",
        style = "border-left: 3px solid #C11C84; padding-left: 10px;",
        div(
          sprintf(
            "Supervision: %s hrs (%s%%)",
            utils$format_number(totals$total_supervision),
            utils$format_number(supervision_ratio)
          )
        ),
        div(
          class = "ms-3 mt-1",
          style = "font-size: 0.9em;",
          sprintf(
            "Individual: %s hrs (%s%%)",
            utils$format_number(totals$total_supervision_individual),
            utils$format_number(supervision_individual_ratio)
          )
        ),
        div(
          class = "ms-3",
          style = "font-size: 0.9em;",
          sprintf(
            "Group: %s hrs (%s%%)",
            utils$format_number(totals$total_supervision_group),
            utils$format_number(supervision_group_ratio)
          )
        ),
        div(
          class = "mt-1",
          sprintf(
            "Admin/Other: %s hrs (%s%%)",
            utils$format_number(totals$total_admin),
            utils$format_number(admin_ratio)
          )
        )
      )
      row_data <- list(
        week = list(
          total = round(metrics$week$total - therapy$week$total, 1),
          average = round(metrics$week$average - therapy$week$average, 1)
        ),
        month = list(
          total = round(metrics$month$total - therapy$month$total, 1),
          average = round(metrics$month$average - therapy$month$average, 1)
        ),
        year = list(total = round(metrics$year$total - therapy$year$total, 1)),
        last_year = list(
          total = round(metrics$last_year$total - therapy$last_year$total, 1)
        )
      )
    } else {
      grand_total <- metrics$week$grand_total
      row_data <- metrics

      therapy_total <- therapy$week$grand_total
      non_therapy_total <- metrics$week$grand_total - therapy_total
      therapy_ratio <- round(therapy_total / metrics$week$grand_total * 100, 0)
      non_therapy_ratio <- round(
        non_therapy_total / metrics$week$grand_total * 100,
        0
      )

      breakdown <- div(
        class = "mb-2",
        style = "border-left: 3px solid #C11C84; padding-left: 10px;",
        div(
          sprintf(
            "Therapy: %s hrs (%s%%)",
            utils$format_number(therapy_total),
            utils$format_number(therapy_ratio)
          )
        ),
        div(
          sprintf(
            "Non-therapy: %s hrs (%s%%)",
            utils$format_number(non_therapy_total),
            utils$format_number(non_therapy_ratio)
          )
        )
      )
    }

    div(
      div(
        class = "h3 mb-2",
        utils$format_number(grand_total),
        " hours"
      ),
      breakdown,
      ui_components$create_metric_row(
        "This week",
        utils$format_number(row_data$week$total),
        utils$format_number(row_data$week$average)
      ),
      ui_components$create_metric_row(
        "This month",
        utils$format_number(row_data$month$total),
        utils$format_number(row_data$month$average)
      ),
      ui_components$create_metric_row(
        "This year",
        utils$format_number(row_data$year$total)
      ),
      ui_components$create_metric_row(
        "Last year",
        utils$format_number(row_data$last_year$total)
      )
    )
  })

  export_cols <- c("start_date", db$HOURS_CATEGORY_COLS)
  export_col_labels <- c(
    start_date = "Date",
    ui_components$HOURS_CATEGORY_LABELS
  )

  hours_entries <- reactiveVal(NULL)
  editing_id <- reactiveVal(NULL)
  pending_delete_id <- reactiveVal(NULL)

  refresh_hours_entries <- function() {
    hours_entries(db$get_hours_entries(pool, tbl))
  }

  reset_entry_form <- function() {
    editing_id(NULL)
    updateRadioButtons(session, "entry_mode", selected = "exact")
    updateDateInput(session, "entry_date", value = Sys.Date())
    updateDateRangeInput(
      session,
      "entry_range",
      start = Sys.Date(),
      end = Sys.Date()
    )
    for (col in db$HOURS_CATEGORY_COLS) {
      updateNumericInput(session, paste0("hrs_", col), value = 0)
    }
    updateActionButton(session, "save_entry", label = "Save")
    output$entry_form_error <- renderUI(NULL)
  }

  hours_group_subtotal <- function(cols) {
    renderUI({
      total <- sum(vapply(cols, function(col) {
        v <- input[[paste0("hrs_", col)]]
        if (is.null(v) || is.na(v)) 0 else v
      }, numeric(1)))
      span(class = "text-muted small fw-normal ms-2", paste0("(", round(total, 2), " hrs)"))
    })
  }

  output$clinical_subtotal <- hours_group_subtotal(ui_components$CLINICAL_COLS)
  output$supervision_subtotal <- hours_group_subtotal(ui_components$SUPERVISION_COLS)
  output$other_subtotal <- hours_group_subtotal(ui_components$OTHER_COLS)

  observeEvent(authenticated(), {
    req(authenticated())
    refresh_hours_entries()
  })

  output$hours_list <- renderUI({
    req(authenticated())
    entries <- hours_entries()

    if (is.null(entries) || nrow(entries) == 0) {
      return(empty_state("No hours logged yet.", show_cta = FALSE))
    }

    rows <- lapply(seq_len(nrow(entries)), function(i) {
      row <- entries[i, ]
      total <- round(sum(unlist(row[db$HOURS_CATEGORY_COLS]), na.rm = TRUE), 2)
      date_label <- if (is.na(row$end_date)) {
        format(as.Date(row$start_date), "%b %d, %Y")
      } else {
        sprintf(
          "%s – %s",
          format(as.Date(row$start_date), "%b %d"),
          format(as.Date(row$end_date), "%b %d, %Y")
        )
      }

      div(
        class = "d-flex justify-content-between align-items-center py-2 border-bottom",
        div(
          span(date_label, class = "d-block"),
          span(paste(total, "hrs"), class = "text-muted small d-block")
        ),
        div(
          class = "d-flex gap-1",
          tags$button(
            type = "button",
            class = "btn btn-sm btn-outline-secondary",
            onclick = sprintf(
              "Shiny.setInputValue('edit_click', %s, {priority: 'event'})",
              row$id
            ),
            bs_icon("pencil-square")
          ),
          tags$button(
            type = "button",
            class = "btn btn-sm btn-outline-danger",
            onclick = sprintf(
              "Shiny.setInputValue('delete_click', %s, {priority: 'event'})",
              row$id
            ),
            bs_icon("trash")
          )
        )
      )
    })

    div(rows)
  })

  observeEvent(input$add_entry, {
    req(authenticated())
    reset_entry_form()
  })

  observeEvent(input$cancel_entry, {
    req(authenticated())
    reset_entry_form()
  })

  observeEvent(input$edit_click, {
    req(authenticated())
    entries <- hours_entries()
    row <- entries[entries$id == input$edit_click, ]
    req(nrow(row) == 1)

    editing_id(row$id)

    if (is.na(row$end_date)) {
      updateRadioButtons(session, "entry_mode", selected = "exact")
      updateDateInput(session, "entry_date", value = row$start_date)
    } else {
      updateRadioButtons(session, "entry_mode", selected = "range")
      updateDateRangeInput(
        session,
        "entry_range",
        start = row$start_date,
        end = row$end_date
      )
    }

    for (col in db$HOURS_CATEGORY_COLS) {
      updateNumericInput(session, paste0("hrs_", col), value = row[[col]])
    }

    updateActionButton(session, "save_entry", label = "Update")
    output$entry_form_error <- renderUI(NULL)
  })

  observeEvent(input$delete_click, {
    req(authenticated())
    pending_delete_id(input$delete_click)
    showModal(modalDialog(
      title = "Delete this entry?",
      "This can't be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Delete", class = "btn btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_delete, {
    req(authenticated(), pending_delete_id())
    id <- pending_delete_id()

    db$delete_hours_entry(pool, tbl, id)
    removeModal()
    pending_delete_id(NULL)
    refresh_hours_entries()
    showNotification("Entry deleted", type = "message", duration = 3)

    if (identical(editing_id(), id)) reset_entry_form()
  })

  output$entry_form_error <- renderUI(NULL)

  observeEvent(input$save_entry, {
    req(authenticated())

    values <- stats::setNames(
      vapply(
        db$HOURS_CATEGORY_COLS,
        function(col) {
          v <- input[[paste0("hrs_", col)]]
          if (is.null(v) || is.na(v)) 0 else v
        },
        numeric(1)
      ),
      db$HOURS_CATEGORY_COLS
    )

    if (any(values < 0)) {
      output$entry_form_error <- renderUI(
        div(class = "alert alert-danger mt-2", "Hours can't be negative.")
      )
      return()
    }

    if (identical(input$entry_mode, "range")) {
      req(input$entry_range)
      start_date <- input$entry_range[1]
      end_date <- input$entry_range[2]

      if (end_date < start_date) {
        output$entry_form_error <- renderUI(
          div(
            class = "alert alert-danger mt-2",
            "End date can't be before start date."
          )
        )
        return()
      }
    } else {
      req(input$entry_date)
      start_date <- input$entry_date
      end_date <- NULL
    }

    was_editing <- !is.null(editing_id())

    if (!was_editing) {
      db$insert_hours_entry(pool, tbl, start_date, end_date, values)
    } else {
      db$update_hours_entry(
        pool,
        tbl,
        editing_id(),
        start_date,
        end_date,
        values
      )
    }

    refresh_hours_entries()
    reset_entry_form()
    showNotification(
      if (was_editing) "Entry updated" else "Entry saved",
      type = "message",
      duration = 3
    )
  })

  export_data <- reactive({
    req(input$export_date_range)
    dbGetQuery(
      pool,
      sprintf(
        "WITH daily AS (%s) SELECT day as start_date, %s FROM daily WHERE day BETWEEN $1 AND $2 ORDER BY day",
        db$daily_hours_cte(tbl),
        paste(db$HOURS_CATEGORY_COLS, collapse = ", ")
      ),
      params = list(input$export_date_range[1], input$export_date_range[2])
    )
  })

  export_selected_cols <- reactive({
    c(
      input$export_clinical_cols,
      input$export_supervision_cols,
      input$export_other_cols
    )
  })

  export_waiter <- waiter::Waiter$new(
    id = "export_report_wrapper",
    html = waiter::spin_heart(),
    color = "rgba(248, 200, 220, 0.7)"
  )

  output$download_report <- downloadHandler(
    filename = function() {
      sprintf(
        "%s_Hours_%s_to_%s.pdf",
        sanitize_filename(user_name()),
        input$export_date_range[1],
        input$export_date_range[2]
      )
    },
    content = function(file) {
      export_waiter$show()
      on.exit(export_waiter$hide(), add = TRUE)

      df <- export_data()
      req(nrow(df) > 0)

      cols <- c("start_date", export_selected_cols())
      report$render_hours_report(
        data = df[, intersect(cols, names(df)), drop = FALSE],
        start_date = input$export_date_range[1],
        end_date = input$export_date_range[2],
        clinician_name = user_name(),
        output_file = file
      )
    }
  )

  output$download_excel <- downloadHandler(
    filename = function() {
      sprintf(
        "%s_Hours_%s_to_%s.xlsx",
        sanitize_filename(user_name()),
        input$export_date_range[1],
        input$export_date_range[2]
      )
    },
    content = function(file) {
      export_waiter$show()
      on.exit(export_waiter$hide(), add = TRUE)

      df <- export_data()
      req(nrow(df) > 0)

      cols <- c("start_date", export_selected_cols())
      excel_df <- df[, intersect(cols, names(df)), drop = FALSE]
      names(excel_df) <- export_col_labels[names(excel_df)]

      writexl::write_xlsx(excel_df, file)
    }
  )

  output$download_backup <- downloadHandler(
    filename = function() {
      sprintf("gemnotes_hours_backup_%s.csv", Sys.Date())
    },
    content = function(file) {
      backup_cols <- c("start_date", "end_date", db$HOURS_CATEGORY_COLS)
      df <- dbGetQuery(
        pool,
        sprintf(
          "SELECT %s FROM %s ORDER BY start_date",
          paste(backup_cols, collapse = ", "),
          tbl
        )
      )
      utils::write.csv(df, file, row.names = FALSE)
    }
  )

  plots$server_plots(
    input,
    output,
    session,
    authenticated,
    monthly_breakdown,
    pool,
    tbl
  )
}
