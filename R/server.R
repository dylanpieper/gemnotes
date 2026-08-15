box::use(
  shiny[
    reactiveVal, reactive, observeEvent, observe, invalidateLater, req, isolate,
    renderUI, HTML, downloadHandler, updateTextInput,
    outputOptions, div, actionButton
  ],
  shinyjs[runjs],
  DBI[dbGetQuery],
  editbl[eDT],
  dbplyr,
)

#' @param config config module (see R/config.R)
#' @param db db module (see R/db.R)
#' @param licensure licensure module (see R/licensure.R)
#' @param ui_components ui_components module (see R/ui_components.R)
#' @param utils utils module (see R/utils.R)
#' @param plots plots module (see R/plots.R)
#' @param report report module (see R/report.R)
#' @export
server <- function(input, output, session, pool, config, db, licensure, ui_components, utils, plots, report) {
  tbl <- db$get_table_name(config)

  CORRECT_PASSWORD <- Sys.getenv("GEM_PASS")
  authenticated <- reactiveVal(FALSE)
  password_modified <- reactiveVal(FALSE)

  output$is_authenticated <- reactive({
    authenticated()
  })
  outputOptions(output, "is_authenticated", suspendWhenHidden = FALSE)

  observeEvent(input$password, {
    if (input$password != "") {
      password_modified(TRUE)
    }
  })

  observeEvent(input$validate_password | input$login, {
    shinyjs::disable("login")
    shinyjs::html("login", "Love is patient...")

    if (input$password == CORRECT_PASSWORD) {
      authenticated(TRUE)
      shinyjs::hideElement("login_error")
    } else {
      shinyjs::enable("login")
      shinyjs::html("login", "Login")
      updateTextInput(session, "password", value = "")

      if (password_modified() && input$login != "") {
        shinyjs::show("login_error")
      }
    }
  })

  quotes <- config$get_config()$quotes

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
    req(authenticated())
    invalidateLater(5000 * 6)

    therapy_metrics(list(
      week = db$get_therapy_hours(pool, "week", config = config),
      month = db$get_therapy_hours(pool, "month", config = config),
      year = db$get_therapy_hours(pool, "year", config = config),
      last_year = db$get_therapy_hours(pool, "year", 1, config = config)
    ))

    work_metrics(list(
      week = db$get_work_hours(pool, "week", config = config),
      month = db$get_work_hours(pool, "month", config = config),
      year = db$get_work_hours(pool, "year", config = config),
      last_year = db$get_work_hours(pool, "year", 1, config = config)
    ))

    licensure_data(licensure$get_licensure_progress(pool, config))
  })

  observe({
    req(authenticated())
    isolate({
      if (is.null(monthly_breakdown())) {
        monthly_breakdown(db$get_monthly_hours_breakdown(pool, 6, config = config))
      }
    })
  })

  observeEvent(input$months_slider,
    {
      req(authenticated(), input$months_slider)
      monthly_breakdown(db$get_monthly_hours_breakdown(pool, as.numeric(input$months_slider), config = config))
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

    runjs("
      setTimeout(function() {
        document.getElementById('quote-text').classList.remove('fade-out');
      }, 50);
    ")
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

    recent_hours_query <- sprintf("
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
      FROM monthly_data", tbl)

    recent_avg <- dbGetQuery(pool, recent_hours_query)
    avg_monthly_hours <- as.numeric(recent_avg$avg_monthly_hours)

    months_remaining <- ceiling(progress_data$licensure_remaining / avg_monthly_hours)
    estimated_completion_date <- format(Sys.Date() + months(months_remaining), "%B %Y")

    licensure$render_licensure_progress(progress_data, estimated_completion_date)
  })

  output$hours_view_toggle <- renderUI({
    view <- hours_view()

    toggle_btn <- function(id, label, key) {
      actionButton(
        id,
        label,
        class = paste("btn btn-sm", if (view == key) "btn-primary" else "btn-outline-primary")
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

    if (view == "therapy") {
      grand_total <- therapy$week$grand_total
      row_data <- therapy

      grand_total_query <- sprintf("
      SELECT
        SUM(individual) as total_individual,
        SUM(relational_couple + relational_family) as total_relational,
        SUM(relational_couple) as total_couple,
        SUM(relational_family) as total_family,
        SUM(individual + relational_couple + relational_family) as total_all
      FROM %s", tbl)

      totals <- dbGetQuery(pool, grand_total_query)

      individual_ratio <- round(totals$total_individual / totals$total_all * 100, 0)
      relational_ratio <- round(totals$total_relational / totals$total_all * 100, 0)
      couple_ratio <- round(totals$total_couple / totals$total_relational * 100, 0)
      family_ratio <- round(totals$total_family / totals$total_relational * 100, 0)

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
    } else if (view == "other") {
      grand_total <- metrics$week$grand_total - therapy$week$grand_total

      grand_total_query <- sprintf("
      SELECT
        SUM(supervision_individual) as total_supervision_individual,
        SUM(supervision_group) as total_supervision_group,
        SUM(supervision_individual + supervision_group) as total_supervision,
        SUM(consultation + case_notes + session_plan + emails + letters +
            staff_meetings + cont_ed + exam_prep) as total_admin,
        SUM(supervision_individual + supervision_group + consultation + case_notes +
            session_plan + emails + letters + staff_meetings + cont_ed + exam_prep) as total_all
      FROM %s", tbl)

      totals <- dbGetQuery(pool, grand_total_query)

      supervision_ratio <- round(totals$total_supervision / totals$total_all * 100, 0)
      admin_ratio <- round(totals$total_admin / totals$total_all * 100, 0)
      supervision_individual_ratio <- round(totals$total_supervision_individual / totals$total_supervision * 100, 0)
      supervision_group_ratio <- round(totals$total_supervision_group / totals$total_supervision * 100, 0)

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
        last_year = list(total = round(metrics$last_year$total - therapy$last_year$total, 1))
      )
    } else {
      grand_total <- metrics$week$grand_total
      row_data <- metrics

      therapy_total <- therapy$week$grand_total
      non_therapy_total <- metrics$week$grand_total - therapy_total
      therapy_ratio <- round(therapy_total / metrics$week$grand_total * 100, 0)
      non_therapy_ratio <- round(non_therapy_total / metrics$week$grand_total * 100, 0)

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

  server_tables <- list(
    hours = eDT(
      id = "hours",
      data = dplyr::tbl(pool, tbl),
      in_place = TRUE,
      escape = FALSE,
      filter = "top",
      options = list(
        pageLength = 50,
        orderClasses = TRUE,
        scrollX = TRUE,
        scrollY = "60vh",
        columnDefs = list(
          list(
            targets = "_all",
            className = "dt-center"
          )
        )
      )
    )
  )

  export_data <- reactive({
    req(input$export_date_range)
    dbGetQuery(
      pool,
      sprintf("SELECT * FROM %s WHERE start_date BETWEEN $1 AND $2 ORDER BY start_date", tbl),
      params = list(input$export_date_range[1], input$export_date_range[2])
    )
  })

  export_col_labels <- c(
    start_date = "Date",
    individual = "Individual Therapy",
    relational_couple = "Couple Therapy",
    relational_family = "Family Therapy",
    supervision_individual = "Individual Supervision",
    supervision_group = "Group Supervision",
    consultation = "Consultation",
    case_notes = "Case Notes",
    session_plan = "Session Planning",
    emails = "Emails",
    letters = "Letters",
    staff_meetings = "Staff Meetings",
    cont_ed = "Continuing Education",
    exam_prep = "Exam Preparation"
  )

  export_selected_cols <- reactive({
    c(input$export_clinical_cols, input$export_supervision_cols, input$export_other_cols)
  })

  export_waiter <- waiter::Waiter$new(
    id = "export_report_wrapper",
    html = waiter::spin_heart(),
    color = "rgba(248, 200, 220, 0.7)"
  )

  output$download_report <- downloadHandler(
    filename = function() {
      sprintf(
        "Julia_Jorgensen_Hours_%s_to_%s.pdf",
        input$export_date_range[1], input$export_date_range[2]
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
        output_file = file
      )
    }
  )

  output$download_excel <- downloadHandler(
    filename = function() {
      sprintf(
        "Julia_Jorgensen_Hours_%s_to_%s.xlsx",
        input$export_date_range[1], input$export_date_range[2]
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
      df <- dbGetQuery(pool, sprintf("SELECT * FROM %s ORDER BY start_date", tbl))
      utils::write.csv(df, file, row.names = FALSE)
    }
  )

  plots$server_plots(input, output, session, authenticated, monthly_breakdown, pool, tbl)
}
