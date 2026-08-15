box::use(
  shiny[shinyApp, onStop],
  R / config,
  R / utils,
  R / db,
  R / licensure,
  R / ui_components,
  R / plots,
  R / report,
  R / dashboard,
  R / server,
)

app_title <- config$get_config()$app_title

pool <- db$connect(config)

onStop(function() {
  db$disconnect(pool)
})

shinyApp(
  ui = dashboard$build_ui(app_title, ui_components),
  server = function(input, output, session) {
    server$server(
      input,
      output,
      session,
      pool,
      config,
      db,
      licensure,
      ui_components,
      utils,
      plots,
      report
    )
  }
)
