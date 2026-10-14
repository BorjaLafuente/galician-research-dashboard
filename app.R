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
#   R/mod_*.R    one module per tab (overview, open access...)
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

  # By default bslib squeezes every card to fit the window height, so a tall
  # chart shrinks the others. With fillable = FALSE each card keeps its own
  # height and the page scrolls instead.
  fillable = FALSE,

  # The sidebar is shared by every tab: the same filters apply everywhere
  # Styles of the summary cards (R/mod_overview.R) and small adjustments
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css")),

  # id = "tab": the name of the open tab is available as input$tab, which
  # lets the sidebar show only the filters that apply to that tab
  id = "tab",

  # The sidebar is shared by every tab
  sidebar = sidebar(
    width = 280,

    # conditionalPanel() shows its content only when the JavaScript
    # condition is true. It runs in the browser, so the sidebar changes
    # instantly when the user switches tab.
    conditionalPanel(
      "input.tab !== 'Collaboration'",
      checkboxGroupInput(
        "universities", "Universities",
        choices = names(UNI_COLORS), selected = names(UNI_COLORS)
      )
    ),

    # Collaboration maps show one university at a time
    conditionalPanel(
      "input.tab === 'Collaboration'",
      radioButtons(
        "collab_university", "University",
        choices = names(UNI_COLORS), selected = "UDC"
      )
    ),

    # sep = "" shows 2015 instead of 2,015
    sliderInput(
      "years", "Publication years",
      min = YEAR_RANGE[1], max = YEAR_RANGE[2],
      value = YEAR_RANGE, step = 1, sep = ""
    ),

    # Collaboration data are counts of articles and reviews by OpenAlex, with
    # no research field, so these two filters do not apply to that tab
    conditionalPanel(
      "input.tab !== 'Collaboration'",
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

    conditionalPanel(
      "input.tab === 'Collaboration'",
      p(class = "text-muted small",
        "Collaboration data cover articles and reviews, with no breakdown ",
        "by research field.")
    )
  ),

  nav_panel("Overview", overview_ui("overview")),
  nav_panel("Open access", open_access_ui("open_access")),
  nav_panel("Impact", impact_ui("impact")),
  nav_panel("Collaboration", collaboration_ui("collaboration")),

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
  open_access_server("open_access", works)
  impact_server("impact", works)
  collaboration_server("collaboration",
                       university = reactive(input$collab_university),
                       years = reactive(input$years),
                       app_data$partner_countries, app_data$partner_institutions)
}

shinyApp(ui, server)
