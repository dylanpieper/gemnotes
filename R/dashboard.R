box::use(
  shiny[
    tagList,
    tags,
    conditionalPanel,
    div,
    h2,
    passwordInput,
    actionButton,
    sliderInput,
    uiOutput,
    span
  ],
  shinyjs[useShinyjs],
  waiter[use_waiter, waiter_preloader, spin_heart],
  bsicons[bs_icon],
  bslib[card, card_header, card_body, page_navbar, navbar_options, nav_panel, page_fillable],
  plotly[plotlyOutput],
)

visualization_cards <- list(
  div(
    class = "row mb-4",
    div(
      class = "col-6",
      card(
        height = "450px",
        full_screen = TRUE,
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center w-100",
            div(
              class = "d-flex align-items-center gap-2",
              bs_icon("bar-chart-fill"),
              "Monthly Work Hours"
            ),
            div(
              class = "d-flex align-items-center gap-2",
              sliderInput(
                inputId = "months_slider",
                label = NULL,
                min = 1,
                max = 24,
                value = 6,
                step = 1,
                width = "200px",
                post = " months"
              )
            )
          )
        ),
        card_body(
          plotlyOutput("work_hours_plot", height = "100%")
        )
      )
    ),
    div(
      class = "col-6",
      card(
        height = "450px",
        full_screen = TRUE,
        card_header(
          div(
            class = "d-flex align-items-center gap-2",
            bs_icon("pie-chart-fill"),
            "Overall Work Hours"
          )
        ),
        card_body(
          plotlyOutput("work_hours_pie", height = "100%")
        )
      )
    )
  )
)

app_css <- tags$style(
  "
  .progress-bar.bg-custom-pink {
    background-color: #C11C84 !important;
  }
  .navbar-brand {
    display: flex !important;
    align-items: center;
    height: 100%;
  }
  .navbar .nav-link {
    display: flex;
    align-items: center;
  }
  .gem-logo {
    flex-shrink: 0;
  }
  #login_container {
    background-color: #fdf0f7;
  }
  #login_container .card {
    border-top: 4px solid #C11C84;
  }
  #login_container .btn-primary {
    background-color: #C11C84;
    border-color: #C11C84;
  }
  #login_container .btn-primary:hover {
    background-color: #9c1568;
    border-color: #9c1568;
  }
"
)

gem_png_white <- "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"
gem_png_pink <- "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"

gem_logo <- function(size = 20, variant = c("white", "pink")) {
  variant <- match.arg(variant)
  src <- if (variant == "white") gem_png_white else gem_png_pink
  tags$img(
    class = "gem-logo",
    src = paste0("data:image/png;base64,", src),
    width = size,
    height = size,
    style = "display: block;"
  )
}

#' @param app_title App name shown on the login screen and navbar
#' @param ui_components ui_components module (see R/ui_components.R)
#' @export
build_ui <- function(app_title, ui_components) {
tagList(
  useShinyjs(),
  use_waiter(),
  tags$head(
    tags$link(
      rel = "shortcut icon",
      href = "data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'><text y='80' font-size='80'>💎</text></svg>"
    ),
    tags$script(
      "
      $(document).ready(function() {
        // Capture Enter key on password field
        $('#password').on('keyup', function(e) {
          if(e.keyCode == 13) {
            Shiny.setInputValue('validate_password', true, {priority: 'event'});
          }
        });
      });
    "
    )
  ),
  app_css,
  waiter_preloader(html = spin_heart(), color = "#C11C84"),

  conditionalPanel(
    condition = "!output.is_authenticated",
    div(
      id = "login_container",
      class = "container-fluid vh-100 d-flex align-items-center justify-content-center",
      style = "position: fixed; top: 0; left: 0; right: 0; bottom: 0; z-index: 1000;",
      div(
        class = "card shadow-sm",
        style = "max-width: 300px; width: 100%;",
        div(
          class = "card-body p-4",
          div(
            class = "d-flex align-items-center justify-content-center gap-2 mb-4",
            gem_logo(28, "pink"),
            h2(app_title, class = "mb-0")
          ),
          passwordInput("password", "Password"),
          div(
            class = "d-grid gap-2",
            actionButton(
              "login",
              "Login",
              class = "btn btn-primary btn-lg"
            )
          ),
          div(
            id = "login_error",
            class = "alert alert-danger mt-3",
            style = "display: none;",
            "Incorrect password"
          )
        )
      )
    )
  ),

  conditionalPanel(
    condition = "output.is_authenticated",
    page_navbar(
      title = div(
        class = "d-flex align-items-center gap-2",
        gem_logo(24, "white"),
        span(app_title)
      ),
      window_title = app_title,
      navbar_options = navbar_options(bg = "#C11C84", underline = TRUE),
      nav_panel(
        title = "Dashboard",
        page_fillable(
          div(
            class = "container-fluid px-4 py-3",
            div(
              class = "row",
              div(
                class = "col-md-4",
                div(
                  id = "licensure_progress_wrapper",
                  card(
                    fill = FALSE,
                    full_screen = TRUE,
                    card_header(
                      div(
                        class = "d-flex align-items-center gap-2",
                        bs_icon("trophy-fill"),
                        "Licensure Progress"
                      )
                    ),
                    card_body(
                      uiOutput("licensure_progress")
                    )
                  )
                )
              ),
              div(
                class = "col-md-8",
                div(
                  class = "row mb-4",
                  div(
                    class = "col-md-6",
                    div(
                      id = "hours_summary_wrapper",
                      card(
                        height = "340px",
                        card_header(
                          div(
                            class = "d-flex justify-content-between align-items-center w-100",
                            div(
                              class = "d-flex align-items-center gap-2",
                              bs_icon("clock-fill"),
                              "Hours Summary"
                            ),
                            uiOutput("hours_view_toggle", inline = TRUE)
                          )
                        ),
                        uiOutput("hours_summary")
                      )
                    )
                  ),
                  div(
                    class = "col-md-6",
                    card(
                      height = "340px",
                      id = "poetry_card",
                      card_header(
                        "Hi, My Love"
                      ),
                      card_body(
                        class = "d-flex flex-column align-items-center justify-content-center",
                        div(
                          div(
                            id = "quote-text",
                            style = "text-align: center; transition: opacity 0.5s;",
                            uiOutput("current_quote"),
                            div(
                              class = "mt-4",
                              actionButton(
                                "generate_btn",
                                "❤️",
                                class = "btn-outline-primary btn-sm"
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                !!!visualization_cards
              )
            )
          )
        )
      ),
      nav_panel(title = "Track Hours", ui_components$track_hours_ui),
      nav_panel(title = "Export Report", ui_components$export_report_ui)
    )
  )
)
}
