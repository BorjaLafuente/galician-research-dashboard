# Build the data used by the dashboard
# =====================================
#
# Reads the SQLite database produced by the openalex-galician-universities
# pipeline and writes data/app.sqlite: a smaller database with only what the
# app needs, already filtered and joined, so the app itself stays simple and
# fast.
#
# Run from the project root (the folder with app.R), with the OpenAlex
# repository cloned next to this one:
#
#   github/
#     openalex-galician-universities/   <- source: data/openalex.db
#     galician-research-dashboard/      <- this project
#
#   Rscript scripts/build_app_data.R

# suppressPackageStartupMessages() hides the usual "objects are masked"
# messages when the packages load
suppressPackageStartupMessages({
  library(DBI)
  library(dplyr)
  library(dbplyr)
})

SOURCE_DB <- "../openalex-galician-universities/data/openalex.db"
APP_DB <- "data/app.sqlite"

# OpenAlex ID of each university, used to drop each university from its own
# list of partners (the other two stay: they are real partners of each other)
OWN_IDS <- tibble(
  university = c("UDC", "USC", "UVIGO"),
  institution_id = c("I11019714", "I200284239", "I6289922")
)

# When less than 10% of an institution's works with a university come from
# direct affiliations, its lineage count is not reliable and the direct count
# is used instead. See docs/findings.md in the OpenAlex repository.
MIN_DIRECT_SHARE <- 0.10


# --- Connect to the source database -----------------------------------------

if (!file.exists(SOURCE_DB)) {
  stop("Source database not found: ", SOURCE_DB,
       "\nRun the openalex-galician-universities pipeline first.")
}

src <- dbConnect(RSQLite::SQLite(), SOURCE_DB)

# on.exit() runs when the script ends, even after an error: the R equivalent
# of try/finally, so the connection is always closed
on.exit(dbDisconnect(src), add = TRUE)

# Check that the source database has every table this script needs, so a
# missing step of the OpenAlex pipeline gives a clear error instead of a
# cryptic one halfway through
required <- c("works", "work_universities", "collab_countries",
              "collab_institutions", "institutions")
missing <- setdiff(required, dbListTables(src))

if (length(missing) > 0) {
  stop("Tables missing in ", SOURCE_DB, ": ", paste(missing, collapse = ", "),
       "\nRun src/extract_collaboration.py and src/load.py in the OpenAlex repository.")
}


# --- 1. Publications ---------------------------------------------------------
# tbl() does not read the table: it creates a reference to it. dplyr verbs on
# that reference are translated to SQL by dbplyr and run inside SQLite. Only
# collect() brings the result into R.

works <- tbl(src, "works") %>%
  select(work_id, doi, title, year, type, is_oa, oa_status, source_name,
         domain, field, fwci, is_top10, is_top1, is_international) %>%
  collect()

work_universities <- tbl(src, "work_universities") %>%
  collect()

cat("works:", nrow(works), "rows\n")
cat("work_universities:", nrow(work_universities), "rows\n")


# --- 2. Partner countries ----------------------------------------------------
# The row for Spain (ES) holds the university's total number of works each
# year. It is turned into a column (n_total) so every country row carries its
# own denominator, and then removed from the list of partners.

countries <- tbl(src, "collab_countries")

totals <- countries %>%
  filter(country_code == "ES") %>%
  select(university, year, n_total = n_works)

partner_countries <- countries %>%
  filter(country_code != "ES") %>%
  inner_join(totals, by = c("university", "year")) %>%
  select(university, year, country_code, country_name, n_works, n_total) %>%
  collect()

cat("partner_countries:", nrow(partner_countries), "rows\n")


# --- 3. Partner institutions -------------------------------------------------
# Each institution comes with two counts (see the OpenAlex repository):
#   n_works         by lineage: includes works of its child institutions
#   n_works_direct  direct affiliation only
# Lineage is the better count for large organisations (works with any CSIC
# institute count for CSIC), but it is inflated for bodies that are mostly
# reached through others: ministries, funders, or universities above a lab
# that joins large physics consortia. For those, the direct count is used.

collab <- tbl(src, "collab_institutions")

# Direct share of each institution with each university, 2015-2025.
# as.numeric() is needed: in SQLite, an integer divided by an integer is an
# integer division (2 / 547 = 0), the same issue as in the median query.
# dbplyr translates as.numeric() into CAST(... AS REAL).
count_basis <- collab %>%
  group_by(university, institution_id) %>%
  summarise(
    total = sum(n_works, na.rm = TRUE),
    total_direct = sum(n_works_direct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    direct_share = as.numeric(total_direct) / as.numeric(total),
    count_basis = if_else(direct_share >= MIN_DIRECT_SHARE, "lineage", "direct")
  ) %>%
  select(university, institution_id, count_basis)

# Choose the reliable count for each institution, drop rows left with no
# works, add the name, type and location of each institution, and drop each
# university from its own list. OWN_IDS is a small table in R, not in the
# database, so that last anti_join (rows with no match in OWN_IDS) runs
# after collect().
partner_institutions <- collab %>%
  inner_join(count_basis, by = c("university", "institution_id")) %>%
  mutate(n_works = if_else(count_basis == "lineage", n_works, n_works_direct)) %>%
  filter(n_works > 0) %>%
  inner_join(tbl(src, "institutions"), by = "institution_id") %>%
  select(university, year, institution_id, name, type, country_code, city,
         latitude, longitude, n_works, count_basis) %>%
  collect() %>%
  anti_join(OWN_IDS, by = c("university", "institution_id"))

cat("partner_institutions:", nrow(partner_institutions), "rows\n")

n_direct <- count_basis %>% filter(count_basis == "direct") %>% count() %>% pull(n)
cat("  university-institution pairs counted by direct affiliation:", n_direct, "\n")


# --- 4. Metadata -------------------------------------------------------------
# Dates shown in the app footer, so users know how recent the data is

metadata <- tibble(
  key = c("collab_extracted_on", "built_on"),
  value = c(
    countries %>% summarise(d = max(extracted_on, na.rm = TRUE)) %>% pull(d),
    as.character(Sys.Date())
  )
)


# --- 5. Write the app database -----------------------------------------------
# The file is created from scratch every time, so it never keeps stale tables

if (file.exists(APP_DB)) invisible(file.remove(APP_DB))

app <- dbConnect(RSQLite::SQLite(), APP_DB)
on.exit(dbDisconnect(app), add = TRUE)

dbWriteTable(app, "works", works)
dbWriteTable(app, "work_universities", work_universities)
dbWriteTable(app, "partner_countries", partner_countries)
dbWriteTable(app, "partner_institutions", partner_institutions)
dbWriteTable(app, "metadata", metadata)

# Indexes on the columns the app filters by most often
# (invisible() hides the 0 that dbExecute() returns)
invisible(dbExecute(app, "CREATE INDEX idx_wu_university ON work_universities (university)"))
invisible(dbExecute(app, "CREATE INDEX idx_pi_university_year ON partner_institutions (university, year)"))

size_mb <- round(file.size(APP_DB) / 1024^2, 1)
cat("\nSaved", APP_DB, "(", size_mb, "MB )\n")
