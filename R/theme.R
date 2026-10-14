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

# Indicators of the Impact tab
IMPACT_INDICATORS <- c(
  "Top 10% most cited (%)" = "pct_top10",
  "Top 1% most cited (%)" = "pct_top1",
  "Median FWCI" = "median_fwci"
)

# Last year in the data. Its citation indicators are provisional: works
# published a few months ago have had little time to be cited, so in charts
# the line from the previous year to this one is dashed.
PROVISIONAL_YEAR <- 2025

# Minimum number of works to show a field's citation indicators: with fewer,
# one or two highly cited papers change the percentage a lot
MIN_WORKS_FIELD <- 30

# Number of research fields shown by default in the Impact tab (the largest)
TOP_FIELDS <- 12

# Number of partner institutions shown on the map
TOP_PARTNERS <- 50

# Sequential scale for maps: one hue, from light (few works) to dark (many)
MAP_SCALE <- list(c(0, "#dbe9f6"), c(1, "#0b3d75"))

# Grey for countries with no co-authored works
MAP_LAND <- "#ecebe7"

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

# Open access routes, from most open to closed, with the colours used in the
# OpenAlex repository figures (checked for colour-blind safety). Closed is a
# neutral grey, so the open routes stand out.
OA_ROUTES <- c(
  gold = "#2a78d6",
  diamond = "#eb6834",
  hybrid = "#1baf7a",
  bronze = "#eda100",
  green = "#e87ba4",
  closed = "#c9c8c3"
)
