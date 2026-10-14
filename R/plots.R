# Chart functions
# ===============
#
# Each function takes a data frame already computed by R/data.R and returns
# a plotly chart. Like data.R, they know nothing about Shiny.


#' Line chart of one indicator over time, one line per university.
#'
#' df: output of indicators_by_year(); y: name of the column to plot.
#' provisional_from: if a year is given, the line from that year onwards is
#' dashed, to flag values that are not final yet (recent citation data).
plot_trend <- function(df, y, y_label, provisional_from = NULL) {
  p <- plotly::plot_ly()

  # One set of traces per university, in the fixed order of UNI_COLORS, so
  # each university always gets its own colour and the legend order never
  # changes
  for (uni in intersect(names(UNI_COLORS), unique(df$university))) {
    d <- df %>% dplyr::filter(university == uni) %>% dplyr::arrange(year)
    color <- UNI_COLORS[[uni]]

    # Without a provisional period, the solid line covers every year
    cut <- if (is.null(provisional_from)) max(d$year) else provisional_from
    solid <- d %>% dplyr::filter(year <= cut)
    dashed <- d %>% dplyr::filter(year >= cut)

    p <- p %>%
      # Solid line: the one shown in the legend
      plotly::add_trace(
        x = solid$year, y = solid[[y]],
        type = "scatter", mode = "lines",
        name = uni, legendgroup = uni,
        line = list(color = color, width = 2),
        hoverinfo = "skip"
      ) %>%
      # Points with the tooltip, one per year. Kept separate from the lines
      # so the year where solid and dashed meet appears only once.
      # %{y} is the value; <extra></extra> hides the trace name box
      plotly::add_trace(
        x = d$year, y = d[[y]],
        type = "scatter", mode = "markers",
        name = uni, legendgroup = uni, showlegend = FALSE,
        marker = list(color = color, size = 8),
        hovertemplate = paste0(uni, ": %{y}<extra></extra>")
      )

    if (nrow(dashed) > 1) {
      p <- p %>%
        plotly::add_trace(
          x = dashed$year, y = dashed[[y]],
          type = "scatter", mode = "lines",
          name = uni, legendgroup = uni, showlegend = FALSE,
          line = list(color = color, width = 2, dash = "dot"),
          hoverinfo = "skip"
        )
    }
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
    # Keep only the useful toolbar button (download as PNG)
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
          # y = 1 with yanchor = "bottom": the title sits just above the
          # top edge of the panel, never on top of the data
          text = paste0("<b>", uni, "</b>"), x = 0, y = 1,
          xref = "paper", yref = "paper", xanchor = "left", yanchor = "bottom",
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
      # ticksuffix adds a little space between the labels and the bars
      yaxis = list(title = "", categoryorder = "array",
                   categoryarray = rev(universities), fixedrange = TRUE,
                   ticksuffix = "  "),
      bargap = 0.35,
      legend = list(orientation = "h", x = 0, y = 1.3, traceorder = "normal"),
      margin = list(t = 40, b = 30),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = TEXT_MUTED)
    ) %>%
    plotly::config(displaylogo = FALSE, modeBarButtons = list(list("toImage")))
}


#' Dot plot: one row per research field, one dot per university.
#'
#' df: output of indicators_by_field(); x: column to plot. Fields are sorted
#' by their total number of works (largest at the top), so the order does
#' not change when the user switches indicator. A grey line joins the lowest
#' and highest university in each field, which makes the gap easy to see.
plot_fields <- function(df, x, x_label) {
  df <- df %>% dplyr::filter(!is.na(.data[[x]]))

  # Field order for the y axis: plotly draws the first category at the
  # bottom, so the smallest field goes first
  field_order <- df %>%
    dplyr::distinct(field, field_works) %>%
    dplyr::arrange(field_works) %>%
    dplyr::pull(field)

  ranges <- df %>%
    dplyr::group_by(field) %>%
    dplyr::summarise(low = min(.data[[x]]), high = max(.data[[x]]), .groups = "drop")

  p <- plotly::plot_ly() %>%
    plotly::add_segments(
      data = ranges, x = ~low, xend = ~high, y = ~field, yend = ~field,
      line = list(color = GRID_COLOR, width = 4),
      hoverinfo = "skip", showlegend = FALSE
    )

  for (uni in intersect(names(UNI_COLORS), unique(df$university))) {
    d <- df %>% dplyr::filter(university == uni)

    p <- p %>%
      plotly::add_trace(
        x = d[[x]], y = d$field,
        type = "scatter", mode = "markers",
        name = uni,
        # White ring around each dot, so overlapping dots stay distinct
        marker = list(color = UNI_COLORS[[uni]], size = 12,
                      line = list(color = "#fcfcfb", width = 2)),
        # customdata carries extra values for the tooltip
        customdata = d$n_works,
        hovertemplate = paste0("<b>%{y}</b><br>", uni, ": %{x} ",
                               "(%{customdata:,} works)<extra></extra>")
      )
  }

  p %>%
    plotly::layout(
      xaxis = list(title = x_label, rangemode = "tozero", gridcolor = GRID_COLOR,
                   zeroline = FALSE, fixedrange = TRUE, side = "top"),
      yaxis = list(title = "", categoryorder = "array", categoryarray = field_order,
                   showgrid = FALSE, fixedrange = TRUE),
      legend = list(orientation = "h", x = 0, y = -0.02, yanchor = "top"),
      margin = list(l = 10, t = 40),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = TEXT_MUTED)
    ) %>%
    plotly::config(displaylogo = FALSE, modeBarButtons = list(list("toImage")))
}
