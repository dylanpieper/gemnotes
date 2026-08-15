box::use(
  yaml[read_yaml],
)

CONFIG_PATH <- "config.yml"
PROD_CONFIG_PATH <- "config_prod.yml"

#' @export
DEFAULTS <- list(
  app_title = "Notes",
  db_host = "",
  db_port = 5432,
  db_name = "postgres",
  db_user = "postgres",
  table_name = "hours",
  total_hours_goal = 4000,
  therapy_hours_goal = 1000,
  relational_hours_goal = 500,
  supervision_individual_goal = 150,
  supervision_group_goal = 50,
  admin_hours_goal = 2800,
  quotes = list(
    "I love you.<br>- Dylan Pieper",
    "You are loved.<br>- Dylan Pieper",
    "Love yourself.<br>- Dylan Pieper"
  )
)

#' Get app settings and licensure hour goals
#' Prefers config_prod.yml (gitignored, self-contained) over config.yml when
#' present, so it stands alone as the only config file needed on prod.
#' A goal of 0 means that category isn't tracked and is omitted from the app
#' @return Named list of settings, with hour goals coerced to integers
#' @export
get_config <- function() {
  path <- if (file.exists(PROD_CONFIG_PATH)) PROD_CONFIG_PATH else CONFIG_PATH
  if (!file.exists(path)) {
    return(DEFAULTS)
  }

  cfg <- read_yaml(path)
  cfg <- utils::modifyList(DEFAULTS, cfg)
  lapply(cfg, function(x) if (is.character(x) || is.list(x)) x else as.integer(x))
}
