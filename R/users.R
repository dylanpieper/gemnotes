box::use(
  DBI[dbGetQuery, dbExecute],
)

#' Look up a user by their Google 'sub' claim (stable per Google account)
#' @param pool Database connection
#' @param sub Google 'sub' claim from the verified ID token
#' @return One-row data.frame, or a zero-row data.frame if no account exists yet
#' @export
get_user_by_google_sub <- function(pool, sub) {
  dbGetQuery(
    pool,
    "SELECT * FROM users WHERE google_sub = $1",
    params = list(sub)
  )
}

#' Create a new user row from a completed signup wizard submission.
#' A goal of 0 means that category isn't tracked -- there's no separate
#' on/off flag, R/db.R and R/licensure.R already derive tracking purely
#' from goal > 0 (same as the original single-user config.yml did).
#' @param pool Database connection
#' @param sub Google 'sub' claim
#' @param email Google account email
#' @param name Google account display name
#' @param total_hours_goal,therapy_hours_goal,relational_hours_goal,supervision_individual_goal,supervision_group_goal,admin_hours_goal Licensure goals
#' @return The new user's UUID (character)
#' @export
create_user <- function(pool, sub, email, name,
                         total_hours_goal, therapy_hours_goal, relational_hours_goal,
                         supervision_individual_goal, supervision_group_goal, admin_hours_goal) {
  result <- dbGetQuery(
    pool,
    "INSERT INTO users (
      google_sub, email, name,
      total_hours_goal, therapy_hours_goal, relational_hours_goal,
      supervision_individual_goal, supervision_group_goal, admin_hours_goal,
      policy_accepted_at
    ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, now())
    RETURNING id",
    params = list(
      sub, email, name,
      total_hours_goal, therapy_hours_goal, relational_hours_goal,
      supervision_individual_goal, supervision_group_goal, admin_hours_goal
    )
  )
  result$id[1]
}

#' Get the tracking goals for a user, in the shape R/db.R and R/licensure.R expect
#' @param pool Database connection
#' @param user_id UUID of the signed-in user
#' @return Named list of goal fields
#' @export
get_user_config <- function(pool, user_id) {
  row <- dbGetQuery(
    pool,
    "SELECT * FROM users WHERE id = $1",
    params = list(user_id)
  )
  list(
    total_hours_goal = as.integer(row$total_hours_goal[1]),
    therapy_hours_goal = as.integer(row$therapy_hours_goal[1]),
    relational_hours_goal = as.integer(row$relational_hours_goal[1]),
    supervision_individual_goal = as.integer(row$supervision_individual_goal[1]),
    supervision_group_goal = as.integer(row$supervision_group_goal[1]),
    admin_hours_goal = as.integer(row$admin_hours_goal[1])
  )
}

#' Update the tracking goals for an existing user
#' @param pool Database connection
#' @param user_id UUID of the signed-in user
#' @param total_hours_goal,therapy_hours_goal,relational_hours_goal,supervision_individual_goal,supervision_group_goal,admin_hours_goal Licensure goals
#' @export
update_user_config <- function(pool, user_id,
                                total_hours_goal, therapy_hours_goal, relational_hours_goal,
                                supervision_individual_goal, supervision_group_goal, admin_hours_goal) {
  dbExecute(
    pool,
    "UPDATE users SET
      total_hours_goal = $2, therapy_hours_goal = $3, relational_hours_goal = $4,
      supervision_individual_goal = $5, supervision_group_goal = $6, admin_hours_goal = $7
    WHERE id = $1",
    params = list(
      user_id,
      total_hours_goal, therapy_hours_goal, relational_hours_goal,
      supervision_individual_goal, supervision_group_goal, admin_hours_goal
    )
  )
  invisible(NULL)
}

#' Permanently delete a user's account. Cascades to their hours rows via
#' the hours.user_id foreign key (ON DELETE CASCADE) -- this is the "nuke" option.
#' @param pool Database connection
#' @param user_id UUID of the account to delete
#' @export
delete_user <- function(pool, user_id) {
  dbExecute(pool, "DELETE FROM users WHERE id = $1", params = list(user_id))
}
