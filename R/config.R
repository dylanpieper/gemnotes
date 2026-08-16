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
  google_client_id = "",
  quotes = list(
    "I love you.<br>- Dylan Pieper",
    "You are loved.<br>- Dylan Pieper",
    "Love yourself.<br>- Dylan Pieper"
  )
)

#' Get global app settings (licensure hour goals now live per-user in the
#' users table -- see R/users.R -- not here)
#' @return Named list of settings
#' @export
get_config <- function() {
  path <- if (file.exists(PROD_CONFIG_PATH)) PROD_CONFIG_PATH else CONFIG_PATH
  if (!file.exists(path)) {
    return(DEFAULTS)
  }

  cfg <- read_yaml(path)
  cfg <- utils::modifyList(DEFAULTS, cfg)
  cfg$db_port <- as.integer(cfg$db_port)
  cfg
}
