box::use(
  quarto[quarto_render],
)

#' Render the therapy hours sign-off report to a PDF
#' @param data Data frame of hours rows to include
#' @param start_date Report period start (Date)
#' @param end_date Report period end (Date)
#' @param clinician_name Name shown on the report (signed-in user's Google profile name)
#' @param output_file Destination path for the rendered PDF
#' @export
render_hours_report <- function(data, start_date, end_date, clinician_name, output_file) {
  tmp_dir <- tempfile("hours_report_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  data_path <- file.path(tmp_dir, "data.rds")
  saveRDS(data, data_path)

  qmd_path <- file.path(tmp_dir, "report.qmd")
  file.copy("report.qmd", qmd_path, overwrite = TRUE)

  quarto_render(
    input = qmd_path,
    output_file = "report.pdf",
    execute_params = list(
      data_path = data_path,
      start_date = as.character(start_date),
      end_date = as.character(end_date),
      clinician_name = clinician_name
    ),
    quiet = TRUE
  )

  file.copy(file.path(tmp_dir, "report.pdf"), output_file, overwrite = TRUE)
}
