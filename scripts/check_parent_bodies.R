# Check: which institutions fall below the 10% direct-share rule?
#
# Lists the largest university-institution pairs whose lineage count is
# replaced by the direct count in build_app_data.R, to confirm they are
# umbrella bodies or inflated lineage counts and not real partners.
# Exploration only: nothing is written.
#
# Run from the project root:  Rscript scripts/check_parent_bodies.R

suppressPackageStartupMessages({
  library(DBI)
  library(dplyr)
  library(dbplyr)
})

src <- dbConnect(RSQLite::SQLite(), "../openalex-galician-universities/data/openalex.db")
on.exit(dbDisconnect(src), add = TRUE)

removed <- tbl(src, "collab_institutions") %>%
  group_by(university, institution_id) %>%
  summarise(
    n_works = sum(n_works, na.rm = TRUE),
    n_direct = sum(n_works_direct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(direct_share = as.numeric(n_direct) / as.numeric(n_works)) %>%
  filter(direct_share < 0.10) %>%
  left_join(tbl(src, "institutions"), by = "institution_id") %>%
  select(university, name, type, country_code, n_works, n_direct) %>%
  arrange(desc(n_works)) %>%
  collect()

cat("Pairs below the rule:", nrow(removed), "\n")
cat("Pairs with 10 or more works:", sum(removed$n_works >= 10), "\n\n")

# print(n = 30) shows 30 rows instead of the default 10
removed %>% print(n = 30)
