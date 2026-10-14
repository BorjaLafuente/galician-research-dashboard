# Data access and indicators
# ==========================
#
# Functions that read data/app.sqlite and compute the indicators shown in the
# app. They know nothing about Shiny: they take data frames and filter values
# and return data frames, so they can be tested on their own (see
# scripts/check_indicators.R).
#
# Shiny loads every file in R/ automatically when the app starts, so app.R
# can use these functions without source().


# --- Loading -----------------------------------------------------------------

#' Read every table the app needs into memory.
#'
#' The database is small (about 28 MB), so it is read once when the app
#' starts. Filtering data frames in memory is then faster than sending a new
#' SQL query each time the user moves a filter.
load_app_data <- function(path = "data/app.sqlite") {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  works <- DBI::dbReadTable(con, "works")
  work_universities <- DBI::dbReadTable(con, "work_universities")

  # One row per (work, university): the format every indicator needs, since
  # indicators are always computed per university. A work shared by two
  # universities appears twice (full counting).
  works_uni <- work_universities %>%
    dplyr::inner_join(works, by = "work_id")

  metadata <- DBI::dbReadTable(con, "metadata")

  list(
    works_uni = works_uni,
    partner_countries = DBI::dbReadTable(con, "partner_countries"),
    partner_institutions = DBI::dbReadTable(con, "partner_institutions"),
    # Named vector: metadata_value["built_on"]
    metadata = setNames(metadata$value, metadata$key)
  )
}


# --- Filtering ---------------------------------------------------------------

#' Keep the works matching the filters chosen in the app.
#'
#' fields = NULL means all fields (no filter).
filter_works <- function(works_uni, universities, years,
                         types = c("article", "review"), fields = NULL) {
  out <- works_uni %>%
    dplyr::filter(
      university %in% universities,
      year >= years[1], year <= years[2],
      type %in% types
    )

  if (!is.null(fields) && length(fields) > 0) {
    out <- out %>% dplyr::filter(field %in% fields)
  }

  out
}


# --- Indicators --------------------------------------------------------------

#' Percentage of TRUE values (1) in a 0/1 column, ignoring missing values.
pct <- function(x) {
  round(100 * mean(x, na.rm = TRUE), 1)
}

#' Main indicators, one row per group.
#'
#' The groups are whatever the data frame is grouped by before calling it:
#' university and year for the time series, or only university for the
#' summary cards.
#'
#' Citation indicators use only the works with an FWCI value, as in
#' sql/05_citation_impact.sql of the OpenAlex repository.
summarise_indicators <- function(df) {
  df %>%
    dplyr::summarise(
      n_works = dplyr::n(),
      pct_oa = pct(is_oa),
      pct_international = pct(is_international),
      n_with_fwci = sum(!is.na(fwci)),
      median_fwci = round(stats::median(fwci, na.rm = TRUE), 2),
      pct_top10 = pct(is_top10[!is.na(fwci)]),
      pct_top1 = pct(is_top1[!is.na(fwci)]),
      .groups = "drop"
    )
}

#' Indicators per university and year (time series).
indicators_by_year <- function(df) {
  df %>%
    dplyr::group_by(university, year) %>%
    summarise_indicators()
}

#' Indicators per university for the whole selected period (summary cards).
indicators_total <- function(df) {
  df %>%
    dplyr::group_by(university) %>%
    summarise_indicators()
}

#' Share of works by open access route, per university and year.
#'
#' Returns one row per university, year and route (long format), which is
#' what a stacked chart needs.
oa_routes_by_year <- function(df) {
  df %>%
    dplyr::filter(!is.na(oa_status)) %>%
    dplyr::count(university, year, oa_status, name = "n") %>%
    dplyr::group_by(university, year) %>%
    dplyr::mutate(pct = round(100 * n / sum(n), 1)) %>%
    dplyr::ungroup()
}


#' Share of works by open access route over the whole selected period,
#' per university.
oa_routes_total <- function(df) {
  df %>%
    dplyr::filter(!is.na(oa_status)) %>%
    dplyr::count(university, oa_status, name = "n") %>%
    dplyr::group_by(university) %>%
    dplyr::mutate(pct = round(100 * n / sum(n), 1)) %>%
    dplyr::ungroup()
}


# --- Collaboration -----------------------------------------------------------

#' Works with each partner country over the selected period.
#'
#' pct_works is the share of the university's works with at least one author
#' from that country. Full counting: a work with authors from France and
#' Italy counts for both, so shares add up to more than 100%.
partner_countries_total <- function(partner_countries, universities, years) {
  partner_countries %>%
    dplyr::filter(
      university %in% universities,
      year >= years[1], year <= years[2]
    ) %>%
    dplyr::group_by(university, country_code, country_name) %>%
    dplyr::summarise(
      n_works = sum(n_works),
      n_total = sum(n_total),
      .groups = "drop"
    ) %>%
    dplyr::mutate(pct_works = round(100 * n_works / n_total, 1)) %>%
    dplyr::arrange(university, dplyr::desc(n_works))
}

#' Works with each partner institution over the selected period.
#'
#' n_works uses the count chosen in build_app_data.R for each institution
#' (by lineage, or by direct affiliation for bodies whose lineage count is
#' inflated).
partner_institutions_total <- function(partner_institutions, universities,
                                       years, types = NULL) {
  out <- partner_institutions %>%
    dplyr::filter(
      university %in% universities,
      year >= years[1], year <= years[2]
    )

  if (!is.null(types) && length(types) > 0) {
    out <- out %>% dplyr::filter(type %in% types)
  }

  out %>%
    dplyr::group_by(university, institution_id, name, type, country_code,
                    city, latitude, longitude, count_basis) %>%
    dplyr::summarise(n_works = sum(n_works), .groups = "drop") %>%
    dplyr::arrange(university, dplyr::desc(n_works))
}
