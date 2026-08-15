#' @export
format_number <- function(x) {
  if (is.null(x) || is.na(x)) return("0")
  format(round(x, 0), big.mark = ",")
}
