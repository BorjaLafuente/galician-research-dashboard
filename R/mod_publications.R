# Publications tab
# ================
#
# The list of works behind every indicator, with the sidebar filters
# applied: searchable, sortable, with links to the original publication and
# a CSV download.


publications_ui <- function(id) {
  ns <- shiny::NS(id)

  # fill = FALSE: the card grows with the table instead of squeezing it
  # into a fixed height with its own scroll bar
  bslib::card(
    fill = FALSE,
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      shiny::textOutput(ns("n_pubs"), inline = TRUE),
      shiny::downloadButton(ns("download"), "Download CSV",
                            class = "btn-sm btn-outline-primary")
    ),
    bslib::card_body(
      fill = FALSE, fillable = FALSE,
      DT::DTOutput(ns("table"))
    )
  )
}


#' works: reactive with the works matching the sidebar filters
#' publications: one row per work, prepared once at start-up
#'   (prepare_publications() in R/data.R)
publications_server <- function(id, works, publications) {
  shiny::moduleServer(id, function(input, output, session) {

    # Select the prepared rows of the works that pass the filters. %in% on
    # IDs is very fast, so this is the only work done when a filter moves.
    pubs <- shiny::reactive({
      publications[publications$work_id %in% works()$work_id, ]
    })

    output$n_pubs <- shiny::renderText({
      paste(format(nrow(pubs()), big.mark = ","), "publications")
    })

    output$table <- DT::renderDT({
      table <- pubs() %>%
        dplyr::select(Title = title_html, Year = year, Type = type_label,
                      Source = source_name, Field = field,
                      Universities = universities, `Open access` = oa_label,
                      FWCI = fwci, `Top 10%` = top10_label)

      DT::datatable(
        table,
        # Title is already safe HTML (see prepare_publications())
        escape = FALSE,
        rownames = FALSE,
        options = list(
          pageLength = 15,
          # Page elements: f = search box, t = table, i = "Showing 1 to 15
          # of ...", p = page buttons
          dom = "ftip",
          autoWidth = TRUE,
          scrollX = TRUE,
          # Title column wider than the rest (DT counts columns from 0)
          columnDefs = list(list(width = "35%", targets = 0)),
          language = list(search = "Search all columns:")
        )
      )
    })

    # The CSV contains the same works as the table, with plain columns
    # (DOI as a full link) instead of HTML
    output$download <- shiny::downloadHandler(
      filename = function() paste0("galician_publications_", Sys.Date(), ".csv"),
      content = function(file) {
        pubs() %>%
          dplyr::transmute(
            work_id, title, year, type, source = source_name, field,
            universities, oa_status, fwci, is_top10,
            doi = dplyr::if_else(is.na(doi), NA_character_,
                                 paste0("https://doi.org/", doi))
          ) %>%
          utils::write.csv(file, row.names = FALSE, na = "")
      }
    )
  })
}
