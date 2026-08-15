box::use(
  DBI[dbConnect, dbGetQuery, dbDisconnect],
  RPostgres[Postgres],
  lubridate[floor_date, ceiling_date, weeks, days, years],
)

#' Connects using host/port/dbname/user from config.yml (or config_prod.yml)
#' and the password from GEM_SUPA_PASS, so nothing project-specific is
#' hardcoded here
#' @param config config module (see R/config.R)
#' @export
connect <- function(config) {
  cfg <- config$get_config()
  dbConnect(
    drv = Postgres(),
    dbname = cfg$db_name,
    host = cfg$db_host,
    port = cfg$db_port,
    user = cfg$db_user,
    password = Sys.getenv("GEM_SUPA_PASS")
  )
}

#' @export
disconnect <- function(pool) {
  dbDisconnect(pool)
}

#' Replace a bare column reference with "0" when its goal is 0 in config.yml
#' Uses word boundaries so compound aliases (e.g. "avg_supervision_individual") are untouched
mask_untracked <- function(query, column, tracked) {
  if (tracked) return(query)
  gsub(paste0("\\b", column, "\\b"), "0", query, perl = TRUE)
}

#' Get the configured hours table name, validated as a safe SQL identifier
#' @param config config module (see R/config.R)
#' @export
get_table_name <- function(config) {
  tbl <- config$get_config()$table_name
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", tbl)) {
    stop("Invalid table_name in config.yml: must be a valid SQL identifier")
  }
  tbl
}

#' Get therapy hours summary for different time periods
#' @param pool Database connection pool
#' @param period "week", "month", or "year"
#' @param offset Number of periods to look back (0 for current period)
#' @param config config module (see R/config.R)
#' @return Named list with total hours and average
#' @export
get_therapy_hours <- function(pool, period = "week", offset = 0, config) {
  current_date <- Sys.Date()
  cfg <- config$get_config()
  tbl <- get_table_name(config)
  track_relational <- cfg$relational_hours_goal > 0

  grand_total_query <- sprintf("
    SELECT
      SUM(individual + relational_couple + relational_family) as grand_total,
      SUM(individual) as total_individual,
      SUM(relational_couple + relational_family) as total_relational,
      SUM(relational_couple) as total_couple,
      SUM(relational_family) as total_family
    FROM %s", tbl)
  grand_total_query <- mask_untracked(grand_total_query, "relational_couple", track_relational)
  grand_total_query <- mask_untracked(grand_total_query, "relational_family", track_relational)

  grand_total_result <- dbGetQuery(pool, grand_total_query)

  if (period == "week") {
    start_date <- floor_date(current_date - weeks(offset), "week", week_start = 1)
    end_date <- start_date + days(6)

    query <- sprintf("
      WITH current_period AS (
        SELECT
          SUM(individual + relational_couple + relational_family) as total_hours,
          SUM(individual) as individual_hours,
          SUM(relational_couple + relational_family) as relational_hours,
          SUM(relational_couple) as couple_hours,
          SUM(relational_family) as family_hours
        FROM %s
        WHERE start_date BETWEEN '%s' AND '%s'
      ),
      all_time_average AS (
        SELECT
          AVG(weekly_total) as avg_hours,
          AVG(weekly_individual) as avg_individual,
          AVG(weekly_relational) as avg_relational,
          AVG(weekly_couple) as avg_couple,
          AVG(weekly_family) as avg_family
        FROM (
          SELECT
            start_date,
            SUM(individual + relational_couple + relational_family) as weekly_total,
            SUM(individual) as weekly_individual,
            SUM(relational_couple + relational_family) as weekly_relational,
            SUM(relational_couple) as weekly_couple,
            SUM(relational_family) as weekly_family
          FROM %s
          GROUP BY start_date
        ) weekly_totals
      )
      SELECT *
      FROM current_period, all_time_average
    ", tbl, start_date, end_date, tbl)

  } else if (period == "month") {
    start_date <- floor_date(current_date - months(offset), "month")
    end_date <- ceiling_date(start_date, "month") - days(1)

    query <- sprintf("
      WITH current_period AS (
        SELECT
          SUM(individual + relational_couple + relational_family) as total_hours,
          SUM(individual) as individual_hours,
          SUM(relational_couple + relational_family) as relational_hours,
          SUM(relational_couple) as couple_hours,
          SUM(relational_family) as family_hours
        FROM %s
        WHERE start_date BETWEEN '%s' AND '%s'
      ),
      all_time_average AS (
        SELECT
          AVG(monthly_total) as avg_hours,
          AVG(monthly_individual) as avg_individual,
          AVG(monthly_relational) as avg_relational,
          AVG(monthly_couple) as avg_couple,
          AVG(monthly_family) as avg_family
        FROM (
          SELECT
            date_trunc('month', start_date) as month,
            SUM(individual + relational_couple + relational_family) as monthly_total,
            SUM(individual) as monthly_individual,
            SUM(relational_couple + relational_family) as monthly_relational,
            SUM(relational_couple) as monthly_couple,
            SUM(relational_family) as monthly_family
          FROM %s
          GROUP BY date_trunc('month', start_date)
        ) monthly_totals
      )
      SELECT *
      FROM current_period, all_time_average
    ", tbl, start_date, end_date, tbl)

  } else if (period == "year") {
    start_date <- floor_date(current_date - years(offset), "year")
    end_date <- ceiling_date(start_date, "year") - days(1)

    query <- sprintf("
      SELECT
        SUM(individual + relational_couple + relational_family) as total_hours,
        SUM(individual) as individual_hours,
        SUM(relational_couple + relational_family) as relational_hours,
        SUM(relational_couple) as couple_hours,
        SUM(relational_family) as family_hours
      FROM %s
      WHERE start_date BETWEEN '%s' AND '%s'
    ", tbl, start_date, end_date)
  }

  query <- mask_untracked(query, "relational_couple", track_relational)
  query <- mask_untracked(query, "relational_family", track_relational)

  result <- dbGetQuery(pool, query)

  if (period == "year") {
    list(
      grand_total = round(as.numeric(grand_total_result$grand_total), 1),
      total = round(as.numeric(result$total_hours), 1),
      individual = round(as.numeric(result$individual_hours), 1),
      relational = round(as.numeric(result$relational_hours), 1),
      couple = round(as.numeric(result$couple_hours), 1),
      family = round(as.numeric(result$family_hours), 1),
      average = NULL
    )
  } else {
    list(
      grand_total = round(as.numeric(grand_total_result$grand_total), 1),
      total = round(as.numeric(result$total_hours), 1),
      individual = round(as.numeric(result$individual_hours), 1),
      relational = round(as.numeric(result$relational_hours), 1),
      couple = round(as.numeric(result$couple_hours), 1),
      family = round(as.numeric(result$family_hours), 1),
      average = round(as.numeric(result$avg_hours), 1),
      avg_individual = round(as.numeric(result$avg_individual), 1),
      avg_relational = round(as.numeric(result$avg_relational), 1),
      avg_couple = round(as.numeric(result$avg_couple), 1),
      avg_family = round(as.numeric(result$avg_family), 1)
    )
  }
}

#' Get work hours summary for different time periods
#' @param pool Database connection pool
#' @param period "week", "month", or "year"
#' @param offset Number of periods to look back (0 for current period)
#' @param config config module (see R/config.R)
#' @return Named list with total hours and average
#' @export
get_work_hours <- function(pool, period = "week", offset = 0, config) {
  current_date <- Sys.Date()
  cfg <- config$get_config()
  tbl <- get_table_name(config)
  track_relational <- cfg$relational_hours_goal > 0
  track_supervision_individual <- cfg$supervision_individual_goal > 0
  track_supervision_group <- cfg$supervision_group_goal > 0

  grand_total_query <- sprintf("
    SELECT SUM(individual + relational_couple + relational_family +
               supervision_individual + supervision_group + consultation +
               case_notes + session_plan + emails + letters +
               staff_meetings + cont_ed + exam_prep) as grand_total
    FROM %s", tbl)
  grand_total_query <- mask_untracked(grand_total_query, "relational_couple", track_relational)
  grand_total_query <- mask_untracked(grand_total_query, "relational_family", track_relational)
  grand_total_query <- mask_untracked(grand_total_query, "supervision_individual", track_supervision_individual)
  grand_total_query <- mask_untracked(grand_total_query, "supervision_group", track_supervision_group)

  grand_total_result <- dbGetQuery(pool, grand_total_query)

  if (period == "week") {
    start_date <- floor_date(current_date, "week", week_start = 1)
    end_date <- ceiling_date(current_date, "week", week_start = 1) - days(1)

    if (offset > 0) {
      start_date <- start_date - weeks(offset)
      end_date <- end_date - weeks(offset)
    }

    query <- sprintf("
      WITH current_period AS (
        SELECT
          COALESCE(SUM(individual + relational_couple + relational_family +
              supervision_individual + supervision_group + consultation +
              case_notes + session_plan + emails + letters +
              staff_meetings + cont_ed + exam_prep), 0) as total_hours,
          COALESCE(SUM(supervision_individual), 0) as supervision_individual_hours,
          COALESCE(SUM(supervision_group), 0) as supervision_group_hours
        FROM %s
        WHERE start_date BETWEEN '%s' AND '%s'
      ),
      all_time_average AS (
        SELECT
          AVG(CASE WHEN weekly_total > 0 THEN weekly_total END) as avg_hours,
          AVG(CASE WHEN weekly_supervision_individual > 0 THEN weekly_supervision_individual END) as avg_supervision_individual,
          AVG(CASE WHEN weekly_supervision_group > 0 THEN weekly_supervision_group END) as avg_supervision_group
        FROM (
          SELECT
            start_date,
            COALESCE(SUM(individual + relational_couple + relational_family +
                supervision_individual + supervision_group + consultation +
                case_notes + session_plan + emails + letters +
                staff_meetings + cont_ed + exam_prep), 0) as weekly_total,
            COALESCE(SUM(supervision_individual), 0) as weekly_supervision_individual,
            COALESCE(SUM(supervision_group), 0) as weekly_supervision_group
          FROM %s
          GROUP BY start_date
        ) weekly_totals
      )
      SELECT *
      FROM current_period, all_time_average
    ", tbl, start_date, end_date, tbl)

  } else if (period == "month") {
    start_date <- floor_date(current_date - months(offset), "month")
    end_date <- ceiling_date(start_date, "month") - days(1)

    query <- sprintf("
      WITH current_period AS (
        SELECT
          COALESCE(SUM(individual + relational_couple + relational_family +
              supervision_individual + supervision_group + consultation +
              case_notes + session_plan + emails + letters +
              staff_meetings + cont_ed + exam_prep), 0) as total_hours,
          COALESCE(SUM(supervision_individual), 0) as supervision_individual_hours,
          COALESCE(SUM(supervision_group), 0) as supervision_group_hours
        FROM %s
        WHERE start_date BETWEEN '%s' AND '%s'
      ),
      all_time_average AS (
        SELECT
          AVG(CASE WHEN monthly_total > 0 THEN monthly_total END) as avg_hours,
          AVG(CASE WHEN monthly_supervision_individual > 0 THEN monthly_supervision_individual END) as avg_supervision_individual,
          AVG(CASE WHEN monthly_supervision_group > 0 THEN monthly_supervision_group END) as avg_supervision_group
        FROM (
          SELECT
            date_trunc('month', start_date) as month,
            COALESCE(SUM(individual + relational_couple + relational_family +
                supervision_individual + supervision_group + consultation +
                case_notes + session_plan + emails + letters +
                staff_meetings + cont_ed + exam_prep), 0) as monthly_total,
            COALESCE(SUM(supervision_individual), 0) as monthly_supervision_individual,
            COALESCE(SUM(supervision_group), 0) as monthly_supervision_group
          FROM %s
          GROUP BY date_trunc('month', start_date)
        ) monthly_totals
      )
      SELECT *
      FROM current_period, all_time_average
    ", tbl, start_date, end_date, tbl)

  } else if (period == "year") {
    start_date <- floor_date(current_date - years(offset), "year")
    end_date <- ceiling_date(start_date, "year") - days(1)

    query <- sprintf("
      SELECT
        COALESCE(SUM(individual + relational_couple + relational_family +
            supervision_individual + supervision_group + consultation +
            case_notes + session_plan + emails + letters +
            staff_meetings + cont_ed + exam_prep), 0) as total_hours,
        COALESCE(SUM(supervision_individual), 0) as supervision_individual_hours,
        COALESCE(SUM(supervision_group), 0) as supervision_group_hours,
        0 as avg_hours,
        0 as avg_supervision_individual,
        0 as avg_supervision_group
      FROM %s
      WHERE start_date BETWEEN '%s' AND '%s'
    ", tbl, start_date, end_date)
  }

  query <- mask_untracked(query, "relational_couple", track_relational)
  query <- mask_untracked(query, "relational_family", track_relational)
  query <- mask_untracked(query, "supervision_individual", track_supervision_individual)
  query <- mask_untracked(query, "supervision_group", track_supervision_group)

  result <- dbGetQuery(pool, query)

  list(
    grand_total = round(as.numeric(grand_total_result$grand_total), 1),
    total = round(as.numeric(result$total_hours), 1),
    supervision_individual = round(as.numeric(result$supervision_individual_hours), 1),
    supervision_group = round(as.numeric(result$supervision_group_hours), 1),
    average = round(as.numeric(result$avg_hours), 1),
    avg_supervision_individual = round(as.numeric(result$avg_supervision_individual), 1),
    avg_supervision_group = round(as.numeric(result$avg_supervision_group), 1)
  )
}

#' Zero out SUM(column) when its goal is 0 in config.yml, leaving the "as column" alias intact
#' (word-boundary masking would corrupt aliases that equal the column name itself, as they do here)
mask_untracked_sum <- function(query, column, tracked) {
  if (tracked) return(query)
  gsub(sprintf("SUM(%s)", column), "SUM(0)", query, fixed = TRUE)
}

#' Get monthly work hours breakdown
#' @param pool Database connection pool
#' @param months Number of months to look back (0 = all time)
#' @param config config module (see R/config.R)
#' @return Dataframe with monthly hours by category
#' @export
get_monthly_hours_breakdown <- function(pool, months = 0, config) {
  current_date <- Sys.Date()
  cfg <- config$get_config()
  tbl <- get_table_name(config)
  track_relational <- cfg$relational_hours_goal > 0
  track_supervision_individual <- cfg$supervision_individual_goal > 0
  track_supervision_group <- cfg$supervision_group_goal > 0

  if (months > 0) {
    start_date <- floor_date(current_date - months(months), "month")

    query <- sprintf("
    WITH monthly_data AS (
      SELECT
        date_trunc('month', start_date)::date as month,
        SUM(individual) as individual,
        SUM(relational_couple) as relational_couple,
        SUM(relational_family) as relational_family,
        SUM(supervision_individual) as supervision_individual,
        SUM(supervision_group) as supervision_group,
        SUM(consultation) as consultation,
        SUM(case_notes) as case_notes,
        SUM(session_plan) as session_plan,
        SUM(emails) as emails,
        SUM(letters) as letters,
        SUM(staff_meetings) as staff_meetings,
        SUM(cont_ed) as cont_ed,
        SUM(exam_prep) as exam_prep
      FROM %s
      WHERE start_date >= '%s'
      GROUP BY date_trunc('month', start_date)
      ORDER BY date_trunc('month', start_date) ASC
    )
    SELECT
      *,
      individual + relational_couple + relational_family +
      supervision_individual + supervision_group + consultation +
      case_notes + session_plan + emails + letters +
      staff_meetings + cont_ed + exam_prep as total_hours
    FROM monthly_data",
                     tbl, start_date)
    query <- mask_untracked_sum(query, "relational_couple", track_relational)
    query <- mask_untracked_sum(query, "relational_family", track_relational)
    query <- mask_untracked_sum(query, "supervision_individual", track_supervision_individual)
    query <- mask_untracked_sum(query, "supervision_group", track_supervision_group)
  } else {
    query <- sprintf("
    WITH monthly_data AS (
      SELECT
        date_trunc('month', start_date)::date as month,
        SUM(individual) as individual,
        SUM(relational_couple) as relational_couple,
        SUM(relational_family) as relational_family,
        SUM(supervision_individual) as supervision_individual,
        SUM(supervision_group) as supervision_group,
        SUM(consultation) as consultation,
        SUM(case_notes) as case_notes,
        SUM(session_plan) as session_plan,
        SUM(emails) as emails,
        SUM(letters) as letters,
        SUM(staff_meetings) as staff_meetings,
        SUM(cont_ed) as cont_ed,
        SUM(exam_prep) as exam_prep
      FROM %s
      GROUP BY date_trunc('month', start_date)
      ORDER BY date_trunc('month', start_date) ASC
    )
    SELECT
      *,
      individual + relational_couple + relational_family +
      supervision_individual + supervision_group + consultation +
      case_notes + session_plan + emails + letters +
      staff_meetings + cont_ed + exam_prep as total_hours
    FROM monthly_data", tbl)
    query <- mask_untracked_sum(query, "relational_couple", track_relational)
    query <- mask_untracked_sum(query, "relational_family", track_relational)
    query <- mask_untracked_sum(query, "supervision_individual", track_supervision_individual)
    query <- mask_untracked_sum(query, "supervision_group", track_supervision_group)
  }

  result <- dbGetQuery(pool, query)

  result$month <- as.Date(result$month)

  result <- result[order(result$month, decreasing = TRUE), ]

  return(result)
}
