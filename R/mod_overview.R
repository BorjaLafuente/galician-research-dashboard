# Overview tab
# ============
#
# Summary cards for the selected period and the evolution of one indicator
# chosen by the user.
#
# A Shiny module is a pair of functions, UI and server, that share an id.
# NS(id) adds that id as a prefix to every input and output, so the same
# names ("trend", "indicator"...) can be reused in other modules without
# clashing.


overview_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # One line describing the current selection, so every number on the page
    # can be read in context
    shiny::uiOutput(ns("selection")),

    # Summary cards, built in the server because their values depend on the
    # filters
    shiny::uiOutput(ns("cards")),

    bslib::card(
      bslib::card_header("Evolution"),
      shiny::radioButtons(
        ns("indicator"), label = NULL,
        choices = TREND_INDICATORS, selected = "n_works", inline = TRUE
      ),
      plotly::plotlyOutput(ns("trend"), height = "380px"),
      bslib::card_footer(
        "Citation indicators of the last one or two years are provisional: ",
        "recent works have had little time to be cited."
      )
    )
  )
}


#' works: reactive with the works matching the sidebar filters
#' filters: reactive list with the filter values (to describe the selection)
overview_server <- function(id, works, filters) {
  shiny::moduleServer(id, function(input, output, session) {

    output$selection <- shiny::renderUI({
      f <- filters()
      type_labels <- c(article = "articles", review = "reviews",
                       "conference-paper" = "conference papers")
      fields <- if (length(f$fields) == 0) "All fields" else paste(f$fields, collapse = ", ")

      types <- paste(type_labels[f$types], collapse = " and ")
      # First letter in capitals: "Articles and reviews"
      types <- paste0(toupper(substr(types, 1, 1)), substring(types, 2))

      shiny::p(
        class = "text-muted mb-1",
        paste(
          types,
          paste0(f$years[1], "\u2013", f$years[2]),
          fields,
          sep = " \u00b7 "
        )
      )
    })

    # validate(need(...)) shows a message instead of an error when the
    # filters leave nothing to show
    totals <- shiny::reactive({
      shiny::validate(shiny::need(nrow(works()) > 0, "No publications match the filters."))
      indicators_total(works())
    })

    by_year <- shiny::reactive({
      shiny::validate(shiny::need(nrow(works()) > 0, "No publications match the filters."))
      indicators_by_year(works())
    })

    output$cards <- shiny::renderUI({
      t <- totals()
      bslib::layout_column_wrap(
        width = 1 / 4,
        kpi_card("Publications", t, "n_works", format_number,
                 scale_max = max(t$n_works)),
        kpi_card("Open access", t, "pct_oa", format_pct),
        kpi_card("Intl. collaboration", t, "pct_international", format_pct),
        kpi_card("Top 10% most cited", t, "pct_top10", format_pct)
      )
    })

    output$trend <- plotly::renderPlotly({
      label <- names(TREND_INDICATORS)[TREND_INDICATORS == input$indicator]
      plot_trend(by_year(), input$indicator, label)
    })
  })
}


# --- Helpers -----------------------------------------------------------------

format_number <- function(x) format(x, big.mark = ",")
format_pct <- function(x) paste0(format(x, nsmall = 1), "%")

#' A card with one indicator and one row per university.
#'
#' Each row has a coloured dot (identity), a thin bar whose length encodes
#' the value, and the value itself in normal text colour, so the three
#' universities can be compared at a glance. Bars of percentages go from 0 to
#' 100; bars of counts go from 0 to scale_max (the largest value in the card).
kpi_card <- function(title, totals, column, formatter, scale_max = 100) {
  rows <- lapply(seq_len(nrow(totals)), function(i) {
    uni <- totals$university[i]
    value <- totals[[column]][i]
    color <- UNI_COLORS[[uni]]

    shiny::div(
      class = "kpi-row",
      shiny::span(class = "kpi-dot", style = paste0("background:", color, ";")),
      shiny::span(class = "kpi-label", uni),
      shiny::div(
        class = "kpi-track",
        shiny::div(class = "kpi-bar",
                   style = paste0("width:", round(100 * value / scale_max, 1), "%; ",
                                  "background:", color, ";"))
      ),
      shiny::strong(class = "kpi-value", formatter(value))
    )
  })

  bslib::card(
    bslib::card_header(title),
    bslib::card_body(rows, gap = "0.5rem")
  )
}
