box::use(
  DBI[dbConnect, dbGetQuery, dbExecute, dbQuoteLiteral, dbDisconnect],
  RPostgres[Postgres],
  lubridate[floor_date, ceiling_date, weeks, days, years],
)

#' Every category column on the hours table, in the order the table stores them
#' @export
HOURS_CATEGORY_COLS <- c(
  "individual", "relational_couple", "relational_family",
  "supervision_individual", "supervision_group", "consultation",
  "case_notes", "session_plan", "emails", "letters",
  "staff_meetings", "cont_ed", "exam_prep", "travel", "shopping", "other"
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

#' Scope this session's connection to a single user for Postgres row-level
#' security. Persists for the life of the connection (session-level, not
#' transaction-level), so it only needs to be called once after auth.
#' @param pool Database connection (per-session, not the old global pool)
#' @param user_id UUID of the authenticated user (from the users table)
#' @export
set_session_user <- function(pool, user_id) {
  dbExecute(pool, paste("SET app.user_id =", dbQuoteLiteral(pool, user_id)))
}

#' Scope this session's connection to a verified-but-not-yet-resolved Google
#' identity, for RLS on the `users` table's pre-login lookup and signup
#' insert -- both necessarily run before set_session_user() exists, since
#' discovering/creating the user row is how the app learns the user_id at
#' all. Call once right after the ID token verifies; persists for the life
#' of the connection like set_session_user().
#' @param pool Database connection (per-session, not the old global pool)
#' @param sub Google 'sub' claim from the verified ID token
#' @export
set_session_pending_sub <- function(pool, sub) {
  dbExecute(pool, paste("SET app.pending_sub =", dbQuoteLiteral(pool, sub)))
}

#' Replace a bare column reference with "0" when its goal is 0 in config.yml
#' Uses word boundaries so compound aliases (e.g. "avg_supervision_individual") are untouched
mask_untracked <- function(query, column, tracked) {
  if (tracked) return(query)
  gsub(paste0("\\b", column, "\\b"), "0", query, perl = TRUE)
}

#' Zero out SUM(column) when its goal is 0 in config.yml, leaving the "as column" alias intact
#' (word-boundary masking would corrupt aliases that equal the column name itself, as they do here)
mask_untracked_sum <- function(query, column, tracked) {
  if (tracked) return(query)
  gsub(sprintf("SUM(%s)", column), "SUM(0)", query, fixed = TRUE)
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

#' Expand every row of the hours table into one row per calendar day it
#' covers, dividing each category's hours evenly across that span. A row
#' logged for a single date (end_date IS NULL) has a span of 1 day, so it
#' passes through unchanged -- only ranged entries actually get split. This
#' is what lets a "catch-up" range entry contribute the right amount to
#' whichever week/month bucket each of its days falls into, and what lets a
#' reporting window that only partially overlaps a range prorate correctly.
#' Callers wrap this in `WITH daily AS (%s) ...` and query FROM daily.
#' @param tbl Hours table name (see get_table_name)
#' @export
daily_hours_cte <- function(tbl) {
  select_cols <- paste(
    sprintf("%s / s.span as %s", HOURS_CATEGORY_COLS, HOURS_CATEGORY_COLS),
    collapse = ",\n        "
  )

  sprintf("
    SELECT
      d::date as day,
      %s
    FROM %s h
    CROSS JOIN LATERAL generate_series(h.start_date, COALESCE(h.end_date, h.start_date), interval '1 day') as d
    CROSS JOIN LATERAL (SELECT (COALESCE(h.end_date, h.start_date) - h.start_date + 1)::numeric as span) s",
    select_cols, tbl
  )
}

#' Get therapy hours summary for different time periods
#' @param pool Database connection pool
#' @param tbl Hours table name (see get_table_name)
#' @param period "week", "month", or "year"
#' @param offset Number of periods to look back (0 for current period)
#' @param user_cfg Named list of the signed-in user's tracking goals (see R/users.R get_user_config)
#' @return Named list with total hours and average
#' @export
get_therapy_hours <- function(pool, tbl, period = "week", offset = 0, user_cfg) {
  current_date <- Sys.Date()
  track_relational <- user_cfg$relational_hours_goal > 0

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
  grand_total_result[is.na(grand_total_result)] <- 0

  # The CTE's own column list must never pass through mask_untracked below --
  # it word-boundary-replaces a bare column name with "0" anywhere in the
  # string, which would turn its "x AS relational_couple" alias into the
  # invalid "x AS 0". So it's kept out of `query` (as a token) until after
  # masking runs, then spliced in unmasked -- the outer SUM()s are all that
  # need masking, since a masked SUM(relational_couple) is 0 regardless of
  # what the untouched CTE computed for that column.
  daily <- daily_hours_cte(tbl)
  daily_token <- "@@DAILY_CTE@@"

  if (period == "week") {
    start_date <- floor_date(current_date - weeks(offset), "week", week_start = 1)
    end_date <- start_date + days(6)

    query <- sprintf("
      WITH daily AS (%s),
      current_period AS (
        SELECT
          SUM(individual + relational_couple + relational_family) as total_hours,
          SUM(individual) as individual_hours,
          SUM(relational_couple + relational_family) as relational_hours,
          SUM(relational_couple) as couple_hours,
          SUM(relational_family) as family_hours
        FROM daily
        WHERE day BETWEEN '%s' AND '%s'
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
            day,
            SUM(individual + relational_couple + relational_family) as weekly_total,
            SUM(individual) as weekly_individual,
            SUM(relational_couple + relational_family) as weekly_relational,
            SUM(relational_couple) as weekly_couple,
            SUM(relational_family) as weekly_family
          FROM daily
          GROUP BY day
        ) weekly_totals
      )
      SELECT *
      FROM current_period, all_time_average
    ", daily_token, start_date, end_date)

  } else if (period == "month") {
    start_date <- floor_date(current_date - months(offset), "month")
    end_date <- ceiling_date(start_date, "month") - days(1)

    query <- sprintf("
      WITH daily AS (%s),
      current_period AS (
        SELECT
          SUM(individual + relational_couple + relational_family) as total_hours,
          SUM(individual) as individual_hours,
          SUM(relational_couple + relational_family) as relational_hours,
          SUM(relational_couple) as couple_hours,
          SUM(relational_family) as family_hours
        FROM daily
        WHERE day BETWEEN '%s' AND '%s'
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
            date_trunc('month', day) as month,
            SUM(individual + relational_couple + relational_family) as monthly_total,
            SUM(individual) as monthly_individual,
            SUM(relational_couple + relational_family) as monthly_relational,
            SUM(relational_couple) as monthly_couple,
            SUM(relational_family) as monthly_family
          FROM daily
          GROUP BY date_trunc('month', day)
        ) monthly_totals
      )
      SELECT *
      FROM current_period, all_time_average
    ", daily_token, start_date, end_date)

  } else if (period == "year") {
    start_date <- floor_date(current_date - years(offset), "year")
    end_date <- ceiling_date(start_date, "year") - days(1)

    query <- sprintf("
      WITH daily AS (%s)
      SELECT
        SUM(individual + relational_couple + relational_family) as total_hours,
        SUM(individual) as individual_hours,
        SUM(relational_couple + relational_family) as relational_hours,
        SUM(relational_couple) as couple_hours,
        SUM(relational_family) as family_hours
      FROM daily
      WHERE day BETWEEN '%s' AND '%s'
    ", daily_token, start_date, end_date)
  }

  query <- mask_untracked(query, "relational_couple", track_relational)
  query <- mask_untracked(query, "relational_family", track_relational)
  query <- sub(daily_token, daily, query, fixed = TRUE)

  result <- dbGetQuery(pool, query)
  result[is.na(result)] <- 0

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
#' @param tbl Hours table name (see get_table_name)
#' @param period "week", "month", or "year"
#' @param offset Number of periods to look back (0 for current period)
#' @param user_cfg Named list of the signed-in user's tracking goals (see R/users.R get_user_config)
#' @return Named list with total hours and average
#' @export
get_work_hours <- function(pool, tbl, period = "week", offset = 0, user_cfg) {
  current_date <- Sys.Date()
  track_relational <- user_cfg$relational_hours_goal > 0
  track_supervision_individual <- user_cfg$supervision_individual_goal > 0
  track_supervision_group <- user_cfg$supervision_group_goal > 0

  grand_total_query <- sprintf("
    SELECT SUM(individual + relational_couple + relational_family +
               supervision_individual + supervision_group + consultation +
               case_notes + session_plan + emails + letters +
               staff_meetings + cont_ed + exam_prep + travel + shopping + other) as grand_total
    FROM %s", tbl)
  grand_total_query <- mask_untracked(grand_total_query, "relational_couple", track_relational)
  grand_total_query <- mask_untracked(grand_total_query, "relational_family", track_relational)
  grand_total_query <- mask_untracked(grand_total_query, "supervision_individual", track_supervision_individual)
  grand_total_query <- mask_untracked(grand_total_query, "supervision_group", track_supervision_group)

  grand_total_result <- dbGetQuery(pool, grand_total_query)
  grand_total_result[is.na(grand_total_result)] <- 0

  daily <- daily_hours_cte(tbl)
  daily_token <- "@@DAILY_CTE@@"

  if (period == "week") {
    start_date <- floor_date(current_date, "week", week_start = 1)
    end_date <- ceiling_date(current_date, "week", week_start = 1) - days(1)

    if (offset > 0) {
      start_date <- start_date - weeks(offset)
      end_date <- end_date - weeks(offset)
    }

    query <- sprintf("
      WITH daily AS (%s),
      current_period AS (
        SELECT
          COALESCE(SUM(individual + relational_couple + relational_family +
              supervision_individual + supervision_group + consultation +
              case_notes + session_plan + emails + letters +
              staff_meetings + cont_ed + exam_prep + travel + shopping + other), 0) as total_hours,
          COALESCE(SUM(supervision_individual), 0) as supervision_individual_hours,
          COALESCE(SUM(supervision_group), 0) as supervision_group_hours
        FROM daily
        WHERE day BETWEEN '%s' AND '%s'
      ),
      all_time_average AS (
        SELECT
          AVG(CASE WHEN weekly_total > 0 THEN weekly_total END) as avg_hours,
          AVG(CASE WHEN weekly_supervision_individual > 0 THEN weekly_supervision_individual END) as avg_supervision_individual,
          AVG(CASE WHEN weekly_supervision_group > 0 THEN weekly_supervision_group END) as avg_supervision_group
        FROM (
          SELECT
            day,
            COALESCE(SUM(individual + relational_couple + relational_family +
                supervision_individual + supervision_group + consultation +
                case_notes + session_plan + emails + letters +
                staff_meetings + cont_ed + exam_prep + travel + shopping + other), 0) as weekly_total,
            COALESCE(SUM(supervision_individual), 0) as weekly_supervision_individual,
            COALESCE(SUM(supervision_group), 0) as weekly_supervision_group
          FROM daily
          GROUP BY day
        ) weekly_totals
      )
      SELECT *
      FROM current_period, all_time_average
    ", daily_token, start_date, end_date)

  } else if (period == "month") {
    start_date <- floor_date(current_date - months(offset), "month")
    end_date <- ceiling_date(start_date, "month") - days(1)

    query <- sprintf("
      WITH daily AS (%s),
      current_period AS (
        SELECT
          COALESCE(SUM(individual + relational_couple + relational_family +
              supervision_individual + supervision_group + consultation +
              case_notes + session_plan + emails + letters +
              staff_meetings + cont_ed + exam_prep + travel + shopping + other), 0) as total_hours,
          COALESCE(SUM(supervision_individual), 0) as supervision_individual_hours,
          COALESCE(SUM(supervision_group), 0) as supervision_group_hours
        FROM daily
        WHERE day BETWEEN '%s' AND '%s'
      ),
      all_time_average AS (
        SELECT
          AVG(CASE WHEN monthly_total > 0 THEN monthly_total END) as avg_hours,
          AVG(CASE WHEN monthly_supervision_individual > 0 THEN monthly_supervision_individual END) as avg_supervision_individual,
          AVG(CASE WHEN monthly_supervision_group > 0 THEN monthly_supervision_group END) as avg_supervision_group
        FROM (
          SELECT
            date_trunc('month', day) as month,
            COALESCE(SUM(individual + relational_couple + relational_family +
                supervision_individual + supervision_group + consultation +
                case_notes + session_plan + emails + letters +
                staff_meetings + cont_ed + exam_prep + travel + shopping + other), 0) as monthly_total,
            COALESCE(SUM(supervision_individual), 0) as monthly_supervision_individual,
            COALESCE(SUM(supervision_group), 0) as monthly_supervision_group
          FROM daily
          GROUP BY date_trunc('month', day)
        ) monthly_totals
      )
      SELECT *
      FROM current_period, all_time_average
    ", daily_token, start_date, end_date)

  } else if (period == "year") {
    start_date <- floor_date(current_date - years(offset), "year")
    end_date <- ceiling_date(start_date, "year") - days(1)

    query <- sprintf("
      WITH daily AS (%s)
      SELECT
        COALESCE(SUM(individual + relational_couple + relational_family +
            supervision_individual + supervision_group + consultation +
            case_notes + session_plan + emails + letters +
            staff_meetings + cont_ed + exam_prep + travel + shopping + other), 0) as total_hours,
        COALESCE(SUM(supervision_individual), 0) as supervision_individual_hours,
        COALESCE(SUM(supervision_group), 0) as supervision_group_hours,
        0 as avg_hours,
        0 as avg_supervision_individual,
        0 as avg_supervision_group
      FROM daily
      WHERE day BETWEEN '%s' AND '%s'
    ", daily_token, start_date, end_date)
  }

  query <- mask_untracked(query, "relational_couple", track_relational)
  query <- mask_untracked(query, "relational_family", track_relational)
  query <- mask_untracked(query, "supervision_individual", track_supervision_individual)
  query <- mask_untracked(query, "supervision_group", track_supervision_group)
  query <- sub(daily_token, daily, query, fixed = TRUE)

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

#' Get monthly work hours breakdown
#' @param pool Database connection pool
#' @param tbl Hours table name (see get_table_name)
#' @param months Number of months to look back (0 = all time)
#' @param user_cfg Named list of the signed-in user's tracking goals (see R/users.R get_user_config)
#' @return Dataframe with monthly hours by category
#' @export
get_monthly_hours_breakdown <- function(pool, tbl, months = 0, user_cfg) {
  current_date <- Sys.Date()
  track_relational <- user_cfg$relational_hours_goal > 0
  track_supervision_individual <- user_cfg$supervision_individual_goal > 0
  track_supervision_group <- user_cfg$supervision_group_goal > 0

  daily <- daily_hours_cte(tbl)

  if (months > 0) {
    start_date <- floor_date(current_date - months(months), "month")

    query <- sprintf("
    WITH daily AS (%s),
    monthly_data AS (
      SELECT
        date_trunc('month', day)::date as month,
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
        SUM(exam_prep) as exam_prep,
        SUM(travel) as travel,
        SUM(shopping) as shopping,
        SUM(other) as other
      FROM daily
      WHERE day >= '%s'
      GROUP BY date_trunc('month', day)
      ORDER BY date_trunc('month', day) ASC
    )
    SELECT
      *,
      individual + relational_couple + relational_family +
      supervision_individual + supervision_group + consultation +
      case_notes + session_plan + emails + letters +
      staff_meetings + cont_ed + exam_prep + travel + shopping + other as total_hours
    FROM monthly_data",
                     daily, start_date)
  } else {
    query <- sprintf("
    WITH daily AS (%s),
    monthly_data AS (
      SELECT
        date_trunc('month', day)::date as month,
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
        SUM(exam_prep) as exam_prep,
        SUM(travel) as travel,
        SUM(shopping) as shopping,
        SUM(other) as other
      FROM daily
      GROUP BY date_trunc('month', day)
      ORDER BY date_trunc('month', day) ASC
    )
    SELECT
      *,
      individual + relational_couple + relational_family +
      supervision_individual + supervision_group + consultation +
      case_notes + session_plan + emails + letters +
      staff_meetings + cont_ed + exam_prep + travel + shopping + other as total_hours
    FROM monthly_data", daily)
  }

  query <- mask_untracked_sum(query, "relational_couple", track_relational)
  query <- mask_untracked_sum(query, "relational_family", track_relational)
  query <- mask_untracked_sum(query, "supervision_individual", track_supervision_individual)
  query <- mask_untracked_sum(query, "supervision_group", track_supervision_group)

  result <- dbGetQuery(pool, query)

  result$month <- as.Date(result$month)

  result <- result[order(result$month, decreasing = TRUE), ]

  return(result)
}

#' Get every hours entry, newest first, for the Track Hours list panel
#' @param pool Database connection pool
#' @param tbl Hours table name (see get_table_name)
#' @return Dataframe with id, start_date, end_date, and every category column
#' @export
get_hours_entries <- function(pool, tbl) {
  cols <- paste(c("id", "start_date", "end_date", HOURS_CATEGORY_COLS), collapse = ", ")
  dbGetQuery(pool, sprintf(
    "SELECT %s FROM %s ORDER BY start_date DESC, id DESC", cols, tbl
  ))
}

#' Insert a new hours entry
#' @param pool Database connection pool
#' @param tbl Hours table name (see get_table_name)
#' @param start_date Date
#' @param end_date Date or NULL (NULL for an exact-date/week-of entry)
#' @param values Named list/vector of category hours, names matching HOURS_CATEGORY_COLS
#' @return The new row's id
#' @export
insert_hours_entry <- function(pool, tbl, start_date, end_date, values) {
  cols <- c("start_date", "end_date", HOURS_CATEGORY_COLS)
  placeholders <- paste0("$", seq_along(cols))
  params <- unname(c(
    list(as.character(start_date), if (is.null(end_date)) NA else as.character(end_date)),
    as.list(values[HOURS_CATEGORY_COLS])
  ))

  result <- dbGetQuery(pool, sprintf(
    "INSERT INTO %s (%s) VALUES (%s) RETURNING id",
    tbl, paste(cols, collapse = ", "), paste(placeholders, collapse = ", ")
  ), params = params)

  result$id[1]
}

#' Update an existing hours entry
#' @param pool Database connection pool
#' @param tbl Hours table name (see get_table_name)
#' @param id Row id to update
#' @param start_date Date
#' @param end_date Date or NULL (NULL for an exact-date/week-of entry)
#' @param values Named list/vector of category hours, names matching HOURS_CATEGORY_COLS
#' @export
update_hours_entry <- function(pool, tbl, id, start_date, end_date, values) {
  cols <- c("start_date", "end_date", HOURS_CATEGORY_COLS)
  set_clause <- paste(sprintf("%s = $%d", cols, seq_along(cols)), collapse = ", ")
  params <- unname(c(
    list(as.character(start_date), if (is.null(end_date)) NA else as.character(end_date)),
    as.list(values[HOURS_CATEGORY_COLS]),
    list(id)
  ))

  dbExecute(pool, sprintf(
    "UPDATE %s SET %s WHERE id = $%d", tbl, set_clause, length(cols) + 1
  ), params = params)

  invisible(NULL)
}

#' Delete an hours entry
#' @param pool Database connection pool
#' @param tbl Hours table name (see get_table_name)
#' @param id Row id to delete
#' @export
delete_hours_entry <- function(pool, tbl, id) {
  dbExecute(pool, sprintf("DELETE FROM %s WHERE id = $1", tbl), params = list(id))
  invisible(NULL)
}
