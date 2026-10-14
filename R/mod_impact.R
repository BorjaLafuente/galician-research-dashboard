# Impact tab
# ==========
#
# Citation impact over time and by research field. As explained in the
# OpenAlex repository (docs/findings.md), OpenAlex's citation indicators are
# about twice as high as those of the CWTS Leiden Ranking, so they are
# meant for comparing the three universities, not as absolute levels.


impact_ui <- function(id) {
  ns <- shiny::NS(id)

  # One card with two tabs, so each view gets the full width without making
  # the page long: over time, and by research field
  bslib::navset_card_underline(
    title = "Citation impact",

    bslib::nav_panel(
      "Over time",
      shiny::radioButtons(
        ns("indicator"), label = NULL,
        choices = IMPACT_INDICATORS, selected = "pct_top10", inline = TRUE
      ),
      plotly::plotlyOutput(ns("trend"), height = "400px"),
      shiny::p(
        class = "text-muted small mt-2",
        "Dotted line: the last year is provisional, as recent works have had ",
        "little time to be cited. OpenAlex indicators are useful to compare ",
        "the three universities, not as absolute levels."
      )
    ),

    bslib::nav_panel(
      "By research field",
      # Indicator and number of fields side by side
      shiny::div(
        class = "d-flex flex-wrap align-items-center gap-4",
        shiny::radioButtons(
          ns("field_indicator"), label = NULL,
          choices = IMPACT_INDICATORS[c("Top 10% most cited (%)", "Median FWCI")],
          selected = "pct_top10", inline = TRUE
        ),
        # width = "auto": the label takes the space it needs, in one line
        shiny::checkboxInput(ns("all_fields"), "Show all fields",
                             value = FALSE, width = "auto")
      ),
      # The chart height depends on the number of fields, so the output is
      # created in the server
      shiny::uiOutput(ns("fields_container")),
      shiny::p(
        class = "text-muted small mt-2",
        paste0("Fields sorted by number of works (the ", TOP_FIELDS, " largest ",
               "unless all fields are shown). A university is shown in a ",
               "field only with at least ", MIN_WORKS_FIELD,
               " works with citation data.")
      )
    )
  )
}


#' works: reactive with the works matching the sidebar filters
impact_server <- function(id, works) {
  shiny::moduleServer(id, function(input, output, session) {

    has_works <- shiny::reactive({
      shiny::validate(shiny::need(nrow(works()) > 0, "No publications match the filters."))
      works()
    })

    output$trend <- plotly::renderPlotly({
      by_year <- indicators_by_year(has_works())
      label <- names(IMPACT_INDICATORS)[IMPACT_INDICATORS == input$indicator]

      # Dash the last segment only if the selection reaches the last year
      provisional_from <- if (max(by_year$year) >= PROVISIONAL_YEAR) PROVISIONAL_YEAR - 1 else NULL

      plot_trend(by_year, input$indicator, label, provisional_from)
    })

    by_field <- shiny::reactive({
      out <- indicators_by_field(has_works())

      # Unless the user asks for all of them, keep the largest fields only
      if (!isTRUE(input$all_fields)) {
        largest <- out %>%
          dplyr::distinct(field, field_works) %>%
          dplyr::slice_max(field_works, n = TOP_FIELDS, with_ties = FALSE) %>%
          dplyr::pull(field)
        out <- out %>% dplyr::filter(field %in% largest)
      }
      out
    })

    # About 24 pixels per field, plus room for the axis and the legend
    output$fields_container <- shiny::renderUI({
      n_fields <- dplyr::n_distinct(by_field()$field)
      plotly::plotlyOutput(session$ns("fields"),
                           height = paste0(24 * n_fields + 110, "px"))
    })

    output$fields <- plotly::renderPlotly({
      label <- names(IMPACT_INDICATORS)[IMPACT_INDICATORS == input$field_indicator]
      plot_fields(by_field(), input$field_indicator, label)
    })
  })
}
