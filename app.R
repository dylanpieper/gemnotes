box::use(
  shiny[shinyApp],
  R / config,
  R / utils,
  R / db,
  R / auth,
  R / users,
  R / licensure,
  R / ui_components,
  R / plots,
  R / report,
  R / dashboard,
  R / server,
)

cfg <- config$get_config()

shinyApp(
  ui = dashboard$build_ui(cfg$app_title, cfg$google_client_id, ui_components),
  server = function(input, output, session) {
    server$server(
      input,
      output,
      session,
      config,
      db,
      auth,
      users,
      licensure,
      ui_components,
      utils,
      plots,
      report
    )
  }
)
