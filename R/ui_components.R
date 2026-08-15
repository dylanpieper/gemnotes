box::use(
  shiny[div, span, p, h6, dateRangeInput, downloadButton, checkboxGroupInput],
  bslib[card, card_header, card_body],
  editbl[eDTOutput],
  lubridate[floor_date],
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
      class = "col-12",
      card(
        card_header("Therapy Hours"),
        card_body(
          eDTOutput("hours")
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
                choices = c(
                  "Individual Therapy" = "individual",
                  "Couple Therapy" = "relational_couple",
                  "Family Therapy" = "relational_family"
                ),
                selected = c(
                  "individual",
                  "relational_couple",
                  "relational_family"
                )
              )
            ),
            div(
              class = "col-md-4",
              h6("Supervision Hours"),
              checkboxGroupInput(
                "export_supervision_cols",
                label = NULL,
                choices = c(
                  "Individual Supervision" = "supervision_individual",
                  "Group Supervision" = "supervision_group"
                ),
                selected = c("supervision_individual", "supervision_group")
              )
            ),
            div(
              class = "col-md-4",
              h6("Admin/Other Hours"),
              checkboxGroupInput(
                "export_other_cols",
                label = NULL,
                choices = c(
                  "Consultation" = "consultation",
                  "Case Notes" = "case_notes",
                  "Session Planning" = "session_plan",
                  "Emails" = "emails",
                  "Letters" = "letters",
                  "Staff Meetings" = "staff_meetings",
                  "Continuing Education" = "cont_ed",
                  "Exam Preparation" = "exam_prep"
                ),
                selected = c(
                  "consultation",
                  "case_notes",
                  "session_plan",
                  "emails",
                  "letters",
                  "staff_meetings",
                  "cont_ed",
                  "exam_prep"
                )
              )
            )
          )
        )
      ),
      div(
        class = "col-12 mt-4",
        card(
          card_header("Full Table Backup"),
          card_body(
            p(
              class = "text-muted mb-3",
              "Download every row of the hours table, all columns, no date or category filter. Use this for a complete backup."
            ),
            downloadButton(
              "download_backup",
              "Backup All Data (CSV)",
              class = "btn-outline-secondary"
            )
          )
        )
      )
    )
  )
)
