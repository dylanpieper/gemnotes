box::use(
  shiny[
    div,
    span,
    p,
    h5,
    h6,
    hr,
    strong,
    dateInput,
    dateRangeInput,
    downloadButton,
    checkboxGroupInput,
    checkboxInput,
    conditionalPanel,
    numericInput,
    radioButtons,
    selectInput,
    actionButton,
    uiOutput,
    tagList
  ],
  bslib[card, card_header, card_body, popover, accordion, accordion_panel],
  bsicons[bs_icon],
  lubridate[floor_date],
)

#' @export
HOURS_CATEGORY_LABELS <- c(
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

#' @export
CLINICAL_COLS <- c("individual", "relational_couple", "relational_family")
#' @export
SUPERVISION_COLS <- c("supervision_individual", "supervision_group")
#' @export
OTHER_COLS <- c(
  "consultation",
  "case_notes",
  "session_plan",
  "emails",
  "letters",
  "staff_meetings",
  "cont_ed",
  "exam_prep"
)

#' A group of numericInputs, one per hours category column, 3 per row
hours_input_group <- function(cols) {
  div(
    class = "row gx-2",
    lapply(cols, function(col) {
      div(
        class = "col-6 col-lg-4 mb-1",
        numericInput(
          paste0("hrs_", col),
          HOURS_CATEGORY_LABELS[[col]],
          value = 0,
          min = 0,
          step = 0.25
        )
      )
    })
  )
}

#' @export
DATA_POLICY_TEXT <- paste(
  "Use at your own risk. We do our best to keep this app online and your",
  "data backed up, but we make no guarantees about uptime or data retention.",
  "Always keep an updated copy of your own data. You can permanently delete",
  "your account and all of your data from the app."
)

#' @export
create_metric_row <- function(label, value, avg = NULL) {
  div(
    class = "d-flex justify-content-between align-items-center mb-1",
    span(label, style = "color: #666;"),
    div(
      class = "d-flex align-items-center gap-2",
      span(value),
      if (!is.null(avg)) {
        span(paste0("(avg: ", avg, ")"), style = "color: #666;")
      }
    )
  )
}

#' @export
track_hours_ui <- div(
  class = "container-fluid",
  div(
    class = "row mt-4",
    div(
      class = "col-12 col-lg-4 mb-4",
      card(
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
            span("Entries"),
            actionButton("add_entry", "+ Add", class = "btn-sm btn-primary")
          )
        ),
        card_body(
          style = "max-height: 65vh; overflow-y: auto;",
          uiOutput("hours_list")
        )
      )
    ),
    div(
      class = "col-12 col-lg-8 mb-4",
      card(
        card_header("Log Hours"),
        card_body(
          radioButtons(
            "entry_mode",
            tagList(
              "When",
              popover(
                bs_icon("question-circle", class = "text-muted ms-1"),
                "Use \"Exact date\" for a single session or once a week with that week's start date. \"Custom range\" is for catching up on past logs all at once: hours are split evenly across the days in the range.",
                title = "Choosing a date mode"
              )
            ),
            choices = c("Exact date" = "exact", "Custom range" = "range"),
            selected = "exact",
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.entry_mode == 'exact'",
            dateInput("entry_date", NULL, value = Sys.Date())
          ),
          conditionalPanel(
            condition = "input.entry_mode == 'range'",
            dateRangeInput(
              "entry_range",
              NULL,
              start = Sys.Date(),
              end = Sys.Date()
            )
          ),
          accordion(
            id = "hours_accordion",
            class = "compact-hours-form mb-3",
            open = "Clinical Hours",
            accordion_panel(
              tagList(
                "Clinical Hours",
                uiOutput("clinical_subtotal", inline = TRUE)
              ),
              value = "Clinical Hours",
              hours_input_group(CLINICAL_COLS)
            ),
            accordion_panel(
              tagList(
                "Supervision Hours",
                uiOutput("supervision_subtotal", inline = TRUE)
              ),
              value = "Supervision Hours",
              hours_input_group(SUPERVISION_COLS)
            ),
            accordion_panel(
              tagList(
                "Admin/Other Hours",
                uiOutput("other_subtotal", inline = TRUE)
              ),
              value = "Admin/Other Hours",
              hours_input_group(OTHER_COLS)
            )
          ),
          uiOutput("entry_form_error"),
          div(
            class = "d-flex gap-2",
            actionButton("save_entry", "Save", class = "btn btn-primary"),
            actionButton(
              "cancel_entry",
              "Cancel",
              class = "btn btn-outline-secondary"
            )
          )
        )
      )
    )
  )
)

#' @export
export_report_ui <- div(
  class = "container-fluid",
  div(
    id = "export_report_wrapper",
    class = "row mt-4",
    div(
      class = "col-12",
      card(
        card_header("Export Hours Report"),
        card_body(
          p(
            class = "text-muted mb-3",
            "Generate a report of your hours for your supervisor to sign off on, or to keep for your own records."
          ),
          div(
            class = "d-flex align-items-end gap-3 mb-3 flex-wrap",
            dateRangeInput(
              "export_date_range",
              "Report date range",
              start = floor_date(Sys.Date(), "month"),
              end = Sys.Date()
            ),
            downloadButton(
              "download_report",
              "Generate PDF",
              class = "btn-primary mb-3"
            ),
            downloadButton(
              "download_excel",
              "Export Excel",
              class = "btn-outline-primary mb-3"
            )
          ),
          div(
            class = "row",
            div(
              class = "col-md-4",
              h6("Clinical Hours"),
              checkboxGroupInput(
                "export_clinical_cols",
                label = NULL,
                choices = stats::setNames(
                  CLINICAL_COLS,
                  HOURS_CATEGORY_LABELS[CLINICAL_COLS]
                ),
                selected = CLINICAL_COLS
              )
            ),
            div(
              class = "col-md-4",
              h6("Supervision Hours"),
              checkboxGroupInput(
                "export_supervision_cols",
                label = NULL,
                choices = stats::setNames(
                  SUPERVISION_COLS,
                  HOURS_CATEGORY_LABELS[SUPERVISION_COLS]
                ),
                selected = SUPERVISION_COLS
              )
            ),
            div(
              class = "col-md-4",
              h6("Admin/Other Hours"),
              checkboxGroupInput(
                "export_other_cols",
                label = NULL,
                choices = stats::setNames(
                  OTHER_COLS,
                  HOURS_CATEGORY_LABELS[OTHER_COLS]
                ),
                selected = OTHER_COLS
              )
            )
          )
        )
      )
    )
  )
)

#' @export
signup_wizard_ui <- div(
  id = "signup_wizard_container",
  class = "container-fluid vh-100 d-flex align-items-start justify-content-center",
  style = "position: fixed; top: 0; left: 0; right: 0; bottom: 0; z-index: 1000; overflow-y: auto; padding: 2rem 1rem;",
  div(
    class = "card shadow-sm",
    style = "max-width: 640px; width: 100%; max-height: calc(100vh - 4rem); overflow-y: auto;",
    card_header(h5("Welcome!", class = "mb-0")),
    card_body(
      p(
        class = "text-muted small",
        "Leave a category at 0 if you don't want to track it."
      ),
      selectInput(
        "state_preset",
        "State preset",
        choices = c("Minnesota (LMFT)" = "mn", "Custom" = "custom"),
        selected = "mn"
      ),
      div(
        class = "row",
        div(
          class = "col-12 col-sm-6 col-md-4",
          numericInput("total_hours_goal", "Total hours", value = 4000, min = 0)
        ),
        div(
          class = "col-12 col-sm-6 col-md-4",
          numericInput(
            "therapy_hours_goal",
            "Therapy hours",
            value = 1000,
            min = 0
          )
        ),
        div(
          class = "col-12 col-sm-6 col-md-4",
          numericInput(
            "relational_hours_goal",
            "Relational hours",
            value = 500,
            min = 0
          )
        ),
        div(
          class = "col-12 col-sm-6 col-md-4",
          numericInput(
            "supervision_individual_goal",
            "Individual supervision",
            value = 100,
            min = 0
          )
        ),
        div(
          class = "col-12 col-sm-6 col-md-4",
          numericInput(
            "supervision_group_goal",
            "Group supervision",
            value = 100,
            min = 0
          )
        ),
        div(
          class = "col-12 col-sm-6 col-md-4",
          numericInput(
            "admin_hours_goal",
            "Admin/other hours",
            value = 2800,
            min = 0
          )
        )
      ),

      hr(),

      checkboxInput(
        "policy_accepted",
        list(strong("I agree to this data policy:"), " ", DATA_POLICY_TEXT),
        value = FALSE,
        width = "100%"
      ),

      div(
        id = "signup_error",
        class = "alert alert-danger",
        style = "display: none;",
        "Please accept the data policy to continue."
      ),

      div(
        class = "d-grid",
        actionButton(
          "complete_signup",
          "Get Started",
          class = "btn btn-primary btn-lg"
        )
      )
    )
  )
)

#' @export
account_ui <- div(
  class = "container-fluid",
  div(
    class = "row mt-4 justify-content-center",
    div(
      class = "col-12 col-md-6",
      card(
        card_header("Data Policy"),
        card_body(p(DATA_POLICY_TEXT))
      ),
      card(
        class = "mt-4",
        card_header("Export Data"),
        card_body(
          p(
            class = "text-muted mb-3",
            "Download all of your data. Use this for a complete backup."
          ),
          downloadButton(
            "download_backup",
            "Export Data (CSV)",
            class = "btn-outline-secondary"
          )
        )
      ),
      card(
        class = "mt-4 border-danger",
        card_header(strong("Danger Zone")),
        card_body(
          p(
            class = "text-muted",
            "This permanently deletes your account and every hours entry you've logged. This cannot be undone."
          ),
          actionButton(
            "nuke_account",
            "Delete My Account & All Data",
            class = "btn btn-danger"
          )
        )
      )
    )
  )
)
