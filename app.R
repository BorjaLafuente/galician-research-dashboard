# Galician research dashboard
# ===========================
#
# Research output, open access, impact and collaboration of the three
# Galician universities (UDC, USC, UVIGO), 2015-2025, from OpenAlex data.
#
# The files in R/ are loaded automatically by Shiny:
#   R/data.R     reads data/app.sqlite and computes the indicators
#   R/theme.R    colours, labels and theme
#   R/plots.R    chart functions
#   R/mod_*.R    one module per tab
#
# Run locally from RStudio (Run App) or with:  shiny::runApp()

library(shiny)
library(bslib)
library(dplyr)

# Read the data once, when the app starts. Every user session shares it.
app_data <- load_app_data("data/app.sqlite")

ALL_FIELDS <- sort(unique(na.omit(app_data$works_uni$field)))
YEAR_RANGE <- range(app_data$works_uni$year)


# --- User interface ----------------------------------------------------------

ui <- page_navbar(
  title = "Galician research dashboard",
  theme = app_theme,

  # The sidebar is shared by every tab: the same filters apply everywhere
  # Styles of the summary cards (R/mod_overview.R) and small adjustments
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css")),

  sidebar = sidebar(
    width = 280,

    checkboxGroupInput(
      "universities", "Universities",
      choices = names(UNI_COLORS), selected = names(UNI_COLORS)
    ),

    # sep = "" shows 2015 instead of 2,015
    sliderInput(
      "years", "Publication years",
      min = YEAR_RANGE[1], max = YEAR_RANGE[2],
      value = YEAR_RANGE, step = 1, sep = ""
    ),

    checkboxGroupInput(
      "types", "Document types",
      choices = c("Articles" = "article", "Reviews" = "review",
                  "Conference papers" = "conference-paper"),
      selected = c("article", "review")
    ),

    # Empty selection = all fields
    selectizeInput(
      "fields", "Research fields",
      choices = ALL_FIELDS, multiple = TRUE,
      options = list(placeholder = "All fields")
    )
  ),

  nav_panel("Overview", overview_ui("overview")),

  nav_spacer(),
  nav_item(
    tags$a("Source code", href = "https://github.com/BorjaLafuente/galician-research-dashboard",
           target = "_blank")
  ),

  # HTML() keeps the text and the link together: listing them as separate
  # pieces would add a space before the full stop after the link
  footer = div(
    class = "text-muted small px-3 py-2",
    HTML(paste0(
      'Personal project based on open data from ',
      '<a href="https://openalex.org" target="_blank">OpenAlex</a>. ',
      'Figures are not official statistics of any of the universities. ',
      'Data built on ', app_data$metadata[["built_on"]], '.'
    ))
  )
)


# --- Server ------------------------------------------------------------------

server <- function(input, output, session) {

  # Works matching the sidebar filters. Every module receives this reactive,
  # so the filtering logic exists in one place only.
  works <- reactive({
    filter_works(
      app_data$works_uni,
      universities = input$universities,
      years = input$years,
      types = input$types,
      fields = input$fields
    )
  })

  # The filter values themselves, to describe the selection in each tab
  filters <- reactive({
    list(universities = input$universities, years = input$years,
         types = input$types, fields = input$fields)
  })

  overview_server("overview", works, filters)
}

shinyApp(ui, server)
