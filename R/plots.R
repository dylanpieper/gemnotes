box::use(
  shiny[req, observe, updateSliderInput],
  plotly[renderPlotly, plot_ly, add_trace, layout],
  MetBrewer[met.brewer],
  DBI[dbGetQuery],
  stats[setNames],
)

#' @export
server_plots <- function(input, output, session, authenticated, monthly_breakdown, pool, tbl) {
  date_range_query <- sprintf("
    SELECT
      MIN(date_trunc('month', start_date))::date as min_date,
      MAX(date_trunc('month', start_date))::date as max_date
    FROM %s", tbl)

  observe({
    req(authenticated())
    if (is.null(input$months_slider)) {
      date_range <- dbGetQuery(pool, date_range_query)
      min_date <- as.Date(date_range$min_date)
      max_date <- as.Date(date_range$max_date)

      if (is.na(min_date) || is.na(max_date)) {
        return() # brand-new account, zero rows -- leave the slider at its default range
      }

      months_range <- as.numeric(difftime(max_date, min_date, units = "days") / 30)

      updateSliderInput(
        session,
        "months_slider",
        min = 1,
        max = ceiling(months_range),
        value = min(6, ceiling(months_range))
      )
    }
  })
  output$work_hours_plot <- renderPlotly({
    req(authenticated(), monthly_breakdown())

    data <- monthly_breakdown()

    if (nrow(data) == 0) {
      return(
        plot_ly() |> layout(
          annotations = list(list(
            text = "No hours logged yet",
            showarrow = FALSE,
            font = list(size = 16, color = "#999"),
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5,
            xanchor = "center",
            yanchor = "middle"
          )),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
      )
    }

    stacking_order <- c(
      "exam_prep",
      "cont_ed",
      "staff_meetings",
      "letters",
      "emails",
      "session_plan",
      "case_notes",
      "consultation",
      "supervision_group",
      "supervision_individual",
      "relational_family",
      "relational_couple",
      "individual"
    )

    cassatt_colors <- met.brewer("Signac", 13, "continuous")

    colors <- setNames(cassatt_colors, c(
      "individual", "relational_couple", "relational_family",
      "supervision_individual", "supervision_group", "consultation",
      "case_notes", "session_plan", "emails", "letters",
      "staff_meetings", "cont_ed", "exam_prep"
    ))

    labels <- c(
      "individual" = "Individual Therapy",
      "relational_couple" = "Couple Therapy",
      "relational_family" = "Family Therapy",
      "supervision_individual" = "Individual Supervision",
      "supervision_group" = "Group Supervision",
      "consultation" = "Consultation",
      "case_notes" = "Case Notes",
      "session_plan" = "Session Planning",
      "emails" = "Emails",
      "letters" = "Mail / Letters",
      "staff_meetings" = "Staff Meetings",
      "cont_ed" = "Continuing Education",
      "exam_prep" = "Exam Preparation"
    )

    data <- data[order(data$month, decreasing = FALSE), ]

    data$month_label <- format(data$month, "%b %Y")

    data$month_label <- factor(data$month_label, levels = unique(data$month_label))

    p <- plot_ly(data, x = ~month_label)

    for (cat in stacking_order) {
      if (any(data[[cat]] > 0)) {
        p <- add_trace(
          p,
          y = as.numeric(data[[cat]]),
          type = "bar",
          name = labels[cat],
          marker = list(color = colors[cat]),
          hovertemplate = paste0(
            "%{fullData.name}: %{y:.0f} hrs",
            "<extra></extra>"
          )
        )
      }
    }

    p |> layout(
      barmode = "stack",
      showlegend = FALSE,
      xaxis = list(
        title = "",
        tickangle = -45,
        tickfont = list(size = 12)
      ),
      yaxis = list(
        title = "Hours",
        tickfont = list(size = 12)
      ),
      margin = list(b = 80),
      hovermode = "closest",
      hoverlabel = list(bgcolor = "white")
    )
  })

  output$work_hours_pie <- renderPlotly({
    req(authenticated(), monthly_breakdown())

    data <- monthly_breakdown()

    cassatt_colors <- met.brewer("Signac", 13, "continuous")

    colors <- setNames(cassatt_colors, c(
      "individual", "relational_couple", "relational_family",
      "supervision_individual", "supervision_group", "consultation",
      "case_notes", "session_plan", "emails", "letters",
      "staff_meetings", "cont_ed", "exam_prep"
    ))

    labels <- c(
      "individual" = "Individual Therapy",
      "relational_couple" = "Couple Therapy",
      "relational_family" = "Family Therapy",
      "supervision_individual" = "Individual Supervision",
      "supervision_group" = "Group Supervision",
      "consultation" = "Consultation",
      "case_notes" = "Case Notes",
      "session_plan" = "Session Planning",
      "emails" = "Emails",
      "letters" = "Mail / Letters",
      "staff_meetings" = "Staff Meetings",
      "cont_ed" = "Continuing Education",
      "exam_prep" = "Exam Preparation"
    )

    hour_columns <- c(
      "individual", "relational_couple", "relational_family",
      "supervision_individual", "supervision_group", "consultation",
      "case_notes", "session_plan", "emails", "letters",
      "staff_meetings", "cont_ed", "exam_prep"
    )

    total_by_category <- sapply(hour_columns, function(cat) {
      sum(data[[cat]], na.rm = TRUE)
    })

    non_zero_categories <- total_by_category > 0
    total_by_category <- total_by_category[non_zero_categories]
    colors_subset <- colors[non_zero_categories]
    labels_subset <- labels[non_zero_categories]

    if (length(total_by_category) == 0) {
      return(
        plot_ly() |> layout(
          annotations = list(list(
            text = "No hours logged yet",
            showarrow = FALSE,
            font = list(size = 16, color = "#999"),
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5,
            xanchor = "center",
            yanchor = "middle"
          )),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
      )
    }

    size_order <- order(total_by_category, decreasing = TRUE)
    total_by_category <- total_by_category[size_order]
    colors_subset <- colors_subset[size_order]
    labels_subset <- labels_subset[size_order]

    total_hours <- sum(total_by_category)
    percentages <- total_by_category / total_hours * 100

    ordered_labels <- unname(labels_subset[names(total_by_category)])
    label_text <- ifelse(
      percentages >= 5,
      paste0(ordered_labels, "<br>", round(percentages, 1), "%"),
      ""
    )

    plot_ly(
      labels = ordered_labels,
      values = total_by_category,
      type = "pie",
      hole = 0.5,
      sort = FALSE,
      domain = list(x = c(0.15, 0.85), y = c(0.1, 0.9)),
      marker = list(colors = unname(colors_subset)),
      text = label_text,
      textposition = "outside",
      textinfo = "text",
      hovertemplate = paste0(
        "%{label}: %{value:.0f} hrs (%{percent:.2f}%)",
        "<extra></extra>"
      )
    ) |>
      layout(
        showlegend = FALSE,
        margin = list(t = 60, b = 60, l = 110, r = 110)
      )
  })
}
