box::use(
  DBI[dbGetQuery],
  shiny[div, span, h5, p],
)

#' Get licensure progress metrics
#' @param pool Database connection pool
#' @param tbl Hours table name (see db$get_table_name)
#' @param user_cfg Named list of the signed-in user's tracking goals (see R/users.R get_user_config)
#' @return Named list with licensure progress metrics
#' @export
get_licensure_progress <- function(pool, tbl, user_cfg) {
  TOTAL_REQUIRED <- user_cfg$total_hours_goal
  THERAPY_REQUIRED <- user_cfg$therapy_hours_goal
  RELATIONAL_REQUIRED <- user_cfg$relational_hours_goal
  INDIVIDUAL_REQUIRED <- THERAPY_REQUIRED - RELATIONAL_REQUIRED
  SUPERVISION_INDIVIDUAL_REQUIRED <- user_cfg$supervision_individual_goal
  SUPERVISION_GROUP_REQUIRED <- user_cfg$supervision_group_goal
  ADMIN_REQUIRED <- user_cfg$admin_hours_goal

  track_relational <- RELATIONAL_REQUIRED > 0
  track_supervision_individual <- SUPERVISION_INDIVIDUAL_REQUIRED > 0
  track_supervision_group <- SUPERVISION_GROUP_REQUIRED > 0

  relational_expr <- if (track_relational) "relational_couple + relational_family" else "0"
  supervision_individual_expr <- if (track_supervision_individual) "supervision_individual" else "0"
  supervision_group_expr <- if (track_supervision_group) "supervision_group" else "0"

  query <- sprintf("
    SELECT
      -- Core therapy hours
      SUM(individual + %s) as therapy_total,
      SUM(individual) as total_individual,
      SUM(%s) as total_relational,

      -- Supervision hours
      SUM(%s) as total_supervision_individual,
      SUM(%s) as total_supervision_group,

      -- Admin/other hours (everything that's not therapy or supervision)
      SUM(consultation + case_notes + session_plan + emails + letters +
          staff_meetings + cont_ed + exam_prep) as total_admin,

      -- Grand total of all hours
      SUM(individual + %s +
          %s + %s + consultation +
          case_notes + session_plan + emails + letters +
          staff_meetings + cont_ed + exam_prep) as grand_total
    FROM %s",
    relational_expr,
    relational_expr,
    supervision_individual_expr,
    supervision_group_expr,
    relational_expr,
    supervision_individual_expr, supervision_group_expr,
    tbl
  )

  result <- dbGetQuery(pool, query)
  result[is.na(result)] <- 0 # brand-new accounts have zero rows, so every SUM() comes back NULL/NA

  safe_percent <- function(completed, required) {
    if (required <= 0) return(0)
    min(100, round(completed / required * 100, 1))
  }

  grand_total_percent <- safe_percent(as.numeric(result$grand_total), TOTAL_REQUIRED)
  therapy_percent <- safe_percent(as.numeric(result$therapy_total), THERAPY_REQUIRED)
  individual_percent <- safe_percent(as.numeric(result$total_individual), INDIVIDUAL_REQUIRED)
  relational_percent <- safe_percent(as.numeric(result$total_relational), RELATIONAL_REQUIRED)
  supervision_individual_percent <- safe_percent(as.numeric(result$total_supervision_individual), SUPERVISION_INDIVIDUAL_REQUIRED)
  supervision_group_percent <- safe_percent(as.numeric(result$total_supervision_group), SUPERVISION_GROUP_REQUIRED)
  admin_percent <- safe_percent(as.numeric(result$total_admin), ADMIN_REQUIRED)

  total_supervision_required <- SUPERVISION_INDIVIDUAL_REQUIRED + SUPERVISION_GROUP_REQUIRED
  total_supervision_completed <- as.numeric(result$total_supervision_individual) + as.numeric(result$total_supervision_group)
  supervision_percent <- safe_percent(total_supervision_completed, total_supervision_required)

  list(
    licensure_total = round(as.numeric(result$grand_total), 1),
    licensure_remaining = max(0, TOTAL_REQUIRED - as.numeric(result$grand_total)),
    licensure_percent = grand_total_percent,
    licensure_required = TOTAL_REQUIRED,

    therapy_total = round(as.numeric(result$therapy_total), 1),
    therapy_required = THERAPY_REQUIRED,
    therapy_remaining = max(0, THERAPY_REQUIRED - as.numeric(result$therapy_total)),
    therapy_percent = therapy_percent,

    individual_total = round(as.numeric(result$total_individual), 1),
    individual_required = INDIVIDUAL_REQUIRED,
    individual_remaining = max(0, INDIVIDUAL_REQUIRED - as.numeric(result$total_individual)),
    individual_percent = individual_percent,

    relational_total = round(as.numeric(result$total_relational), 1),
    relational_required = RELATIONAL_REQUIRED,
    relational_remaining = max(0, RELATIONAL_REQUIRED - as.numeric(result$total_relational)),
    relational_percent = relational_percent,

    supervision_total = round(total_supervision_completed, 1),
    supervision_required = total_supervision_required,
    supervision_remaining = max(0, total_supervision_required - total_supervision_completed),
    supervision_percent = supervision_percent,
    supervision_individual_total = round(as.numeric(result$total_supervision_individual), 1),
    supervision_individual_required = SUPERVISION_INDIVIDUAL_REQUIRED,
    supervision_individual_remaining = max(0, SUPERVISION_INDIVIDUAL_REQUIRED - as.numeric(result$total_supervision_individual)),
    supervision_individual_percent = supervision_individual_percent,
    supervision_group_total = round(as.numeric(result$total_supervision_group), 1),
    supervision_group_required = SUPERVISION_GROUP_REQUIRED,
    supervision_group_remaining = max(0, SUPERVISION_GROUP_REQUIRED - as.numeric(result$total_supervision_group)),
    supervision_group_percent = supervision_group_percent,

    admin_total = round(as.numeric(result$total_admin), 1),
    admin_required = ADMIN_REQUIRED,
    admin_remaining = max(0, ADMIN_REQUIRED - as.numeric(result$total_admin)),
    admin_percent = admin_percent,

    track_relational = track_relational,
    track_supervision_individual = track_supervision_individual,
    track_supervision_group = track_supervision_group
  )
}

#' Generate a progress bar UI component
#' @param value Current value
#' @param max Maximum value
#' @param percent_complete Percentage complete (0-100)
#' @param label Label for the progress bar
#' @param color Color for the progress bar
#' @return HTML for the progress bar component
#' @export
create_progress_bar <- function(value, max, percent_complete, label, color = "primary") {
  div(
    class = "mb-3",
    div(
      class = "d-flex justify-content-between align-items-center mb-1",
      span(label, style = "font-weight: 500;"),
      span(paste0(value, " / ", max, " hrs (", percent_complete, "%)"), class = "small")
    ),
    div(
      class = "progress",
      style = "height: 12px;",
      div(
        class = paste0("progress-bar bg-", color),
        role = "progressbar",
        style = paste0("width: ", percent_complete, "%;"),
        `aria-valuenow` = percent_complete,
        `aria-valuemin` = "0",
        `aria-valuemax` = "100"
      )
    )
  )
}

#' Generate licensure progress UI
#' @param licensure_data Licensure progress data
#' @param estimated_completion_date Estimated completion date (optional)
#' @return UI elements for licensure progress display
#' @export
render_licensure_progress <- function(licensure_data, estimated_completion_date = "...") {
  div(
    class = "licensure-progress",
    div(
      class = "row mb-3",
      div(
        class = "col-6 mb-2",
        div(
          class = "card h-100",
          div(
            class = "card-body p-3",
            h5("Hours Completed", class = "card-title mb-1"),
            p(class = "card-text display-6 mb-0", licensure_data$licensure_total),
            p(class = "card-text text-muted small", paste0(licensure_data$licensure_percent, "% of required hours"))
          )
        )
      ),
      div(
        class = "col-6 mb-2",
        div(
          class = "card h-100",
          div(
            class = "card-body p-3",
            h5("Hours Remaining", class = "card-title mb-1"),
            p(class = "card-text display-6 mb-0", licensure_data$licensure_remaining),
            p(
              class = "card-text text-muted small",
              span(id = "estimated_completion", estimated_completion_date)
            )
          )
        )
      )
    ),

    create_progress_bar(
      licensure_data$licensure_total,
      licensure_data$licensure_required,
      licensure_data$licensure_percent,
      "Overall Progress",
      "custom-pink"
    ),

    h5("Requirements", class = "mt-3 mb-3"),

    create_progress_bar(
      licensure_data$therapy_total,
      licensure_data$therapy_required,
      licensure_data$therapy_percent,
      "Therapy Hours",
      "custom-pink"
    ),

    div(
      class = "ms-4 mb-2",
      create_progress_bar(
        licensure_data$individual_total,
        licensure_data$individual_required,
        licensure_data$individual_percent,
        "Individual Therapy",
        "custom-pink"
      )
    ),

    if (licensure_data$track_relational) {
      div(
        class = "ms-4 mb-2",
        create_progress_bar(
          licensure_data$relational_total,
          licensure_data$relational_required,
          licensure_data$relational_percent,
          "Relational Therapy",
          "custom-pink"
        )
      )
    },

    create_progress_bar(
      licensure_data$supervision_total,
      licensure_data$supervision_required,
      licensure_data$supervision_percent,
      "Supervision Hours",
      "custom-pink"
    ),

    if (licensure_data$track_supervision_individual) {
      div(
        class = "ms-4 mb-2",
        create_progress_bar(
          licensure_data$supervision_individual_total,
          licensure_data$supervision_individual_required,
          licensure_data$supervision_individual_percent,
          "Individual Supervision",
          "custom-pink"
        )
      )
    },

    if (licensure_data$track_supervision_group) {
      div(
        class = "ms-4 mb-2",
        create_progress_bar(
          licensure_data$supervision_group_total,
          licensure_data$supervision_group_required,
          licensure_data$supervision_group_percent,
          "Group Supervision",
          "custom-pink"
        )
      )
    },

    create_progress_bar(
      licensure_data$admin_total,
      licensure_data$admin_required,
      licensure_data$admin_percent,
      "Admin/Other Hours",
      "custom-pink"
    )
  )
}
