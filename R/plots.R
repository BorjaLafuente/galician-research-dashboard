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
