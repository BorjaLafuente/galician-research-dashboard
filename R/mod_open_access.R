# Open access tab
# ===============
#
# How the works are made open: route by route over time, and the overall mix
# for the selected period.


open_access_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::card(
      bslib::card_header("Open access routes over time"),
      plotly::plotlyOutput(ns("routes_by_year"), height = "420px"),
      bslib::card_footer(
        "Each work is assigned a single route, the most open one available ",
        "(OpenAlex classification). Green therefore only counts works with no ",
        "other open version."
      )
    ),
    bslib::card(
      bslib::card_header("Open access routes, whole period"),
      plotly::plotlyOutput(ns("routes_total"), height = "260px")
    )
  )
}


#' works: reactive with the works matching the sidebar filters
open_access_server <- function(id, works) {
  shiny::moduleServer(id, function(input, output, session) {

    has_works <- shiny::reactive({
      shiny::validate(shiny::need(nrow(works()) > 0, "No publications match the filters."))
      works()
    })

    output$routes_by_year <- plotly::renderPlotly({
      plot_oa_routes(oa_routes_by_year(has_works()))
    })

    output$routes_total <- plotly::renderPlotly({
      plot_oa_routes_total(oa_routes_total(has_works()))
    })
  })
}
