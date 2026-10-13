# Check: do the app's indicators match the OpenAlex repository?
#
# Computes the indicators with the functions in R/data.R and compares them
# with the CSV files that sql/02, 04 and 05 wrote in the OpenAlex repository.
# Same data, two independent implementations (SQL there, dplyr here): if
# they agree, both are very likely correct.
#
# Run from the project root:  Rscript scripts/check_indicators.R

suppressPackageStartupMessages(library(dplyr))
source("R/data.R")

RESULTS <- "../openalex-galician-universities/results"

app_data <- load_app_data()

ours <- app_data$works_uni %>%
  filter_works(universities = c("UDC", "USC", "UVIGO"), years = c(2015, 2025)) %>%
  indicators_by_year()

open_access <- read.csv(file.path(RESULTS, "02_open_access.csv"))
international <- read.csv(file.path(RESULTS, "04_international_collaboration.csv"))
impact <- read.csv(file.path(RESULTS, "05_citation_impact.csv"))

# One row per university and year with both versions side by side
comparison <- ours %>%
  inner_join(open_access %>% select(university, year, sql_n_works = n_works,
                                    sql_pct_oa = pct_oa),
             by = c("university", "year")) %>%
  inner_join(international %>% select(university, year,
                                      sql_pct_international = pct_int_art_rev),
             by = c("university", "year")) %>%
  inner_join(impact %>% select(university, year, sql_n_with_fwci = n_works,
                               sql_median_fwci = median_fwci,
                               sql_pct_top10 = pct_top10, sql_pct_top1 = pct_top1),
             by = c("university", "year"))

cat("Rows compared:", nrow(comparison), "(expected 33: 3 universities x 11 years)\n\n")

# Largest absolute difference for each indicator. Differences of 0.1 can
# appear in percentages: SQLite rounds halves away from zero (12.25 -> 12.3)
# and R rounds them to the nearest even digit (12.25 -> 12.2).
max_diff <- function(a, b) max(abs(a - b))

checks <- tibble::tribble(
  ~indicator,           ~max_difference,
  "n_works",            max_diff(comparison$n_works, comparison$sql_n_works),
  "pct_oa",             max_diff(comparison$pct_oa, comparison$sql_pct_oa),
  "pct_international",  max_diff(comparison$pct_international, comparison$sql_pct_international),
  "n_with_fwci",        max_diff(comparison$n_with_fwci, comparison$sql_n_with_fwci),
  "median_fwci",        max_diff(comparison$median_fwci, comparison$sql_median_fwci),
  "pct_top10",          max_diff(comparison$pct_top10, comparison$sql_pct_top10),
  "pct_top1",           max_diff(comparison$pct_top1, comparison$sql_pct_top1)
)

print(checks)
