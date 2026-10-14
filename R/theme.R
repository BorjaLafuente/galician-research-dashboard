# Colours, labels and theme shared by the whole app
# =================================================

# Corporate colour of each university (R names deeppink3, dodgerblue3,
# darkgreen), the same as in the figures of the OpenAlex repository, checked
# for colour-blind safety. Each university keeps its colour in every chart,
# whatever the filters: colour follows the university, never its position.
UNI_COLORS <- c(UDC = "#CD1076", USC = "#1874CD", UVIGO = "#006400")

UNI_NAMES <- c(
  UDC = "Universidade da Coruña",
  USC = "Universidade de Santiago de Compostela",
  UVIGO = "Universidade de Vigo"
)

# Indicators that can be plotted over time: label shown in the app -> column
# returned by indicators_by_year() in R/data.R
TREND_INDICATORS <- c(
  "Publications" = "n_works",
  "Open access (%)" = "pct_oa",
  "International collaboration (%)" = "pct_international",
  "Top 10% most cited (%)" = "pct_top10",
  "Median FWCI" = "median_fwci"
)

# Recessive greys for text and grid lines, so the data stands out
TEXT_MUTED <- "#52514e"
GRID_COLOR <- "#e4e3df"

# The navigation bar is dark navy (set in www/styles.css): a neutral colour,
# so it never competes with the three university colours

app_theme <- bslib::bs_theme(
  version = 5,
  bg = "#fcfcfb",
  fg = "#0b0b0b",
  primary = "#1874CD",
  # Inter, a clean sans-serif font, linked from Google Fonts (local = FALSE:
  # the browser downloads it, nothing is stored in the app). If it cannot be
  # loaded, the browser falls back to the system font.
  base_font = bslib::font_collection(
    bslib::font_google("Inter", local = FALSE), "system-ui", "sans-serif"
  ),
  "font-size-base" = "0.95rem"
)
