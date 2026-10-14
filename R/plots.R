# Chart functions
# ===============
#
# Each function takes a data frame already computed by R/data.R and returns
# a plotly chart. Like data.R, they know nothing about Shiny.


#' Line chart of one indicator over time, one line per university.
#'
#' df: output of indicators_by_year(); y: name of the column to plot.
plot_trend <- function(df, y, y_label) {
  p <- plotly::plot_ly()

  # One trace per university, in the fixed order of UNI_COLORS, so each
  # university always gets its own colour and the legend order never changes
  for (uni in intersect(names(UNI_COLORS), unique(df$university))) {
    d <- df %>% dplyr::filter(university == uni) %>% dplyr::arrange(year)

    p <- p %>%
      plotly::add_trace(
        x = d$year, y = d[[y]],
        type = "scatter", mode = "lines+markers",
        name = uni,
        line = list(color = UNI_COLORS[[uni]], width = 2),
        marker = list(color = UNI_COLORS[[uni]], size = 8),
        # %{y} is the value of the point; <extra></extra> hides the trace
        # name box that plotly adds by default
        hovertemplate = paste0(uni, ": %{y}<extra></extra>")
      )
  }

  p %>%
    plotly::layout(
      # One tooltip with the three universities for the year under the mouse
      hovermode = "x unified",
      xaxis = list(title = "", dtick = 1, showgrid = FALSE, fixedrange = TRUE),
      yaxis = list(title = y_label, rangemode = "tozero", gridcolor = GRID_COLOR,
                   zeroline = FALSE, fixedrange = TRUE),
      legend = list(orientation = "h", x = 0, y = 1.12),
      margin = list(t = 40),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = TEXT_MUTED)
    ) %>%
    # Keep only the useful toolbar buttons (download as PNG)
    plotly::config(displaylogo = FALSE,
                   modeBarButtons = list(list("toImage")))
}


#' Stacked areas of open access routes over time, one panel per university.
#'
#' df: output of oa_routes_by_year(). Small multiples (one panel per
#' university, same y axis) are easier to compare than three stacks in one
#' chart.
plot_oa_routes <- function(df) {
  # A route with no works in a year is missing from df; add it with 0%, so
  # every area has a value for every year and the stack has no gaps
  df <- df %>%
    tidyr::complete(university, year, oa_status = names(OA_ROUTES),
                    fill = list(n = 0, pct = 0))

  universities <- intersect(names(UNI_COLORS), unique(df$university))

  panels <- lapply(universities, function(uni) {
    d <- df %>% dplyr::filter(university == uni)
    p <- plotly::plot_ly()

    for (route in names(OA_ROUTES)) {
      dr <- d %>% dplyr::filter(oa_status == route) %>% dplyr::arrange(year)
      label <- tools::toTitleCase(route)

      p <- p %>%
        plotly::add_trace(
          x = dr$year, y = dr$pct,
          type = "scatter", mode = "lines",
          # Traces with the same stackgroup are stacked on top of each other
          stackgroup = "one",
          fillcolor = OA_ROUTES[[route]],
          # Thin line in the background colour: a small gap between areas
          line = list(width = 0.5, color = "#fcfcfb"),
          name = label,
          # One legend entry per route, shared by the three panels
          legendgroup = route,
          showlegend = uni == universities[1],
          hovertemplate = paste0(label, ": %{y:.1f}%<extra></extra>")
        )
    }

    p %>%
      plotly::layout(
        # Panel title, placed relative to the panel ("paper" coordinates)
        annotations = list(list(
          text = paste0("<b>", uni, "</b>"), x = 0, y = 1.06,
          xref = "paper", yref = "paper", xanchor = "left",
          showarrow = FALSE, font = list(size = 13)
        ))
      )
  })

  plotly::subplot(panels, nrows = 1, shareY = TRUE, margin = 0.03) %>%
    plotly::layout(
      hovermode = "x unified",
      yaxis = list(title = "% of works", range = c(0, 100), gridcolor = GRID_COLOR,
                   fixedrange = TRUE),
      legend = list(orientation = "h", x = 0, y = 1.18, traceorder = "normal"),
      margin = list(t = 70),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = TEXT_MUTED)
    ) %>%
    plotly::config(displaylogo = FALSE, modeBarButtons = list(list("toImage")))
}


#' 100% stacked horizontal bars: open access routes over the whole period,
#' one bar per university.
#'
#' df: output of oa_routes_total().
plot_oa_routes_total <- function(df) {
  universities <- intersect(names(UNI_COLORS), unique(df$university))
  p <- plotly::plot_ly()

  for (route in names(OA_ROUTES)) {
    dr <- df %>%
      dplyr::filter(oa_status == route) %>%
      # One value per university in the fixed order, 0 if the route is absent
      dplyr::right_join(tibble::tibble(university = universities), by = "university") %>%
      dplyr::mutate(pct = dplyr::coalesce(pct, 0))
    label <- tools::toTitleCase(route)

    p <- p %>%
      plotly::add_trace(
        x = dr$pct, y = dr$university,
        type = "bar", orientation = "h",
        name = label,
        marker = list(color = OA_ROUTES[[route]],
                      line = list(color = "#fcfcfb", width = 1)),
        hovertemplate = paste0(label, ": %{x:.1f}%<extra></extra>")
      )
  }

  p %>%
    plotly::layout(
      barmode = "stack",
      xaxis = list(title = "% of works", range = c(0, 100), gridcolor = GRID_COLOR,
                   fixedrange = TRUE),
      # categoryarray fixes the order of the bars: UDC on top
      yaxis = list(title = "", categoryorder = "array",
                   categoryarray = rev(universities), fixedrange = TRUE),
      legend = list(orientation = "h", x = 0, y = 1.25, traceorder = "normal"),
      margin = list(t = 50),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = TEXT_MUTED)
    ) %>%
    plotly::config(displaylogo = FALSE, modeBarButtons = list(list("toImage")))
}
