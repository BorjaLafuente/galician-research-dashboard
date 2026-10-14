# Collaboration tab
# =================
#
# Who the universities publish with: partner countries and partner
# institutions. The data are counts by OpenAlex (see the OpenAlex repository,
# src/extract_collaboration.py), not individual works, so they cover articles
# and reviews only and cannot be split by research field.


collaboration_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::navset_card_underline(
      title = "Collaboration",

      bslib::nav_panel(
        "Countries",
        bslib::layout_columns(
          col_widths = c(8, 4),
          plotly::plotlyOutput(ns("country_map"), height = "420px"),
          plotly::plotlyOutput(ns("top_countries"), height = "420px")
        ),
        shiny::p(
          class = "text-muted small mt-2",
          "Share of the university's works with at least one author from each ",
          "country. A work with authors from several countries counts for each ",
          "of them, so shares add up to more than 100%."
        )
      ),

      bslib::nav_panel(
        "Institutions",
        plotly::plotlyOutput(ns("institution_map"), height = "420px"),
        DT::DTOutput(ns("institution_table")),
        shiny::p(
          class = "text-muted small mt-2",
          "Map: the ", TOP_PARTNERS, " partner institutions with most shared works. ",
          "Ministries, funders and other bodies whose OpenAlex counts are inflated ",
          "by their dependent institutions are counted by direct affiliation only ",
          "(see the OpenAlex repository, docs/findings.md)."
        )
      )
    )
  )
}


#' university: reactive with the university chosen in the sidebar
#' years: reactive with the year range chosen in the sidebar
collaboration_server <- function(id, university, years,
                                 partner_countries, partner_institutions) {
  shiny::moduleServer(id, function(input, output, session) {

    countries <- shiny::reactive({
      partner_countries_total(partner_countries, university(), years())
    })

    institutions <- shiny::reactive({
      partner_institutions_total(partner_institutions, university(), years())
    })

    output$country_map <- plotly::renderPlotly({
      plot_country_map(countries())
    })

    output$top_countries <- plotly::renderPlotly({
      plot_top_countries(countries(), university())
    })

    output$institution_map <- plotly::renderPlotly({
      top <- institutions() %>%
        dplyr::slice_max(n_works, n = TOP_PARTNERS, with_ties = FALSE)
      plot_institution_map(top, university())
    })

    output$institution_table <- DT::renderDT({
      table <- institutions() %>%
        dplyr::transmute(
          Institution = name,
          Type = tools::toTitleCase(type),
          Country = country_code,
          City = city,
          Works = n_works,
          `Counted by` = count_basis
        )

      DT::datatable(
        table,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          order = list(list(4, "desc")),   # Works, largest first (from 0)
          language = list(search = "Find an institution:")
        )
      )
    })
  })
}
