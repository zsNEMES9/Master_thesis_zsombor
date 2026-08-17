################################################################################
##  NAMSOR GENDER IMPUTATION
##  Replaces "unknown" / "andy" gender labels using the NamSor API.
##  Requires: httr2, countrycode  (install if missing)
##
##  Usage:
##    1. Set NAMSOR_API_KEY in your environment (see CONFIG below).
##       Get a key free at https://namsor.app/
##    2. Source this file AFTER df_raw is loaded in the analysis script.
##    3. It writes a cache file "namsor_cache.csv" so the API is only
##       called once — re-runs just reload the cache.
##
##  The script is a no-op when there is nothing to impute: if every row already
##  carries a resolved gender label, it prints a message and exits without
##  touching the API, the cache or the data. That is the normal state of the
##  distributed dataset, whose gender column was resolved on an earlier pass.
################################################################################

library(httr2)
library(countrycode)   # converts country names → ISO-2 codes

# ── CONFIG ────────────────────────────────────────────────────────────────────
#
#  The API key is read from the environment, never hardcoded, so that this file
#  is safe to commit. Set it in ~/.Renviron (then restart R):
#
#      NAMSOR_API_KEY=your_key_here
#
#  or for a single session:  Sys.setenv(NAMSOR_API_KEY = "your_key_here")

NAMSOR_API_KEY <- Sys.getenv("NAMSOR_API_KEY", unset = "")
CACHE_FILE     <- "namsor_cache.csv"
BATCH_SIZE     <- 100              # NamSor max per request

# ── HELPER: call NamSor genderNameGeoBatch ────────────────────────────────────

namsor_batch <- function(batch_df, api_key) {
  # batch_df must have columns: row_id, first_name, country_iso2

  body <- list(
    personalNames = lapply(seq_len(nrow(batch_df)), function(i) {
      list(
        id          = as.character(batch_df$row_id[i]),
        firstName   = batch_df$first_name[i],
        lastName    = "",
        countryIso2 = batch_df$country_iso2[i]
      )
    })
  )

  resp <- request("https://v2.namsor.com/NamSorAPIv2/api2/json/genderGeoBatch") |>
    req_headers(
      "X-API-KEY"    = api_key,
      "Content-Type" = "application/json",
      "Accept"       = "application/json"
    ) |>
    req_body_json(body) |>
    req_error(is_error = \(r) FALSE) |>   # handle errors manually
    req_perform()

  if (resp_status(resp) != 200) {
    warning(sprintf("NamSor API error %d: %s", resp_status(resp), resp_body_string(resp)))
    return(NULL)
  }

  parsed <- resp_body_json(resp)

  tibble(
    row_id         = as.integer(map_chr(parsed$personalNames, "id")),
    namsor_gender  = map_chr(parsed$personalNames, "likelyGender"),
    namsor_scale   = map_dbl(parsed$personalNames, "genderScale"),  # -1=female, +1=male
    namsor_score   = map_dbl(parsed$personalNames, "score")
  )
}

# ── SKIP GUARD ────────────────────────────────────────────────────────────────
#
#  Nothing below should run unless there is actually something to impute.
#  Without this guard, a dataset with zero unresolved labels produces
#  n_batches = 0, so the request loop never executes, the results list is
#  empty, and the empty-results check further down reports "All API calls
#  failed" even though no call was ever attempted.

gender_col_present <- all(c("first_name", "gender") %in% names(df_raw))
n_to_impute <- if (gender_col_present) {
  sum(df_raw$gender %in% c("unknown", "andy") | is.na(df_raw$gender))
} else {
  0L
}

if (n_to_impute == 0) {

  cat(sprintf(
    "No unresolved gender labels (%s) — skipping NamSor imputation.\n",
    if (gender_col_present) "0 unknown/andy rows" else "first_name/gender column absent"
  ))

} else {

  # ── MAIN ────────────────────────────────────────────────────────────────────

  ## 1. Identify unknown-gender rows
  unknown_rows <- df_raw |>
    mutate(row_id = row_number()) |>
    filter(gender %in% c("unknown", "andy") | is.na(gender)) |>
    select(row_id, first_name, country)

  cat(sprintf("Rows with unknown gender: %d\n", nrow(unknown_rows)))

  ## 2. Convert country names to ISO-2 codes
  unknown_rows <- unknown_rows |>
    mutate(
      country_iso2 = countrycode(country, origin = "country.name", destination = "iso2c"),
      country_iso2 = replace_na(country_iso2, "")   # NamSor accepts empty string
    )

  ## 3. Deduplicate: only unique first_name × country combinations
  unique_names <- unknown_rows |>
    distinct(first_name, country_iso2) |>
    mutate(row_id = row_number())   # temporary ID for dedup lookup

  cat(sprintf("Unique name × country combinations: %d\n", nrow(unique_names)))

  ## 4. Load cache or call API
  if (file.exists(CACHE_FILE)) {

    cat("Cache found — loading without calling API.\n")
    cached <- read_csv(CACHE_FILE, show_col_types = FALSE)

  } else {

    if (!nzchar(NAMSOR_API_KEY))
      stop("NAMSOR_API_KEY is not set. Add it to ~/.Renviron and restart R, ",
           "or supply namsor_cache.csv in the working directory.")

    cat("No cache — calling NamSor API...\n")

    n_batches <- ceiling(nrow(unique_names) / BATCH_SIZE)
    results   <- vector("list", n_batches)

    for (i in seq_len(n_batches)) {
      idx   <- ((i - 1) * BATCH_SIZE + 1):min(i * BATCH_SIZE, nrow(unique_names))
      batch <- unique_names[idx, ]

      results[[i]] <- namsor_batch(batch, NAMSOR_API_KEY)

      cat(sprintf("  Batch %d / %d done\n", i, n_batches))
      if (i < n_batches) Sys.sleep(0.3)   # stay within rate limit
    }

    # Join results back onto unique_names (first_name + country_iso2)
    results_ok <- Filter(Negate(is.null), results)
    if (length(results_ok) == 0)
      stop("Every NamSor request returned an error — check the API key, the ",
           "connection, and any warnings printed above.")
    cached <- bind_rows(results_ok) |>
    left_join(unique_names, by = "row_id") |>
    select(first_name, country_iso2, namsor_gender, namsor_scale, namsor_score)

    write_csv(cached, CACHE_FILE)
    cat(sprintf("Results cached to %s\n", CACHE_FILE))
  }

  ## 5. Map NamSor output → same coding as existing gender column
  #    NamSor returns: "male", "female", "unknown"
  namsor_to_gender <- function(g, scale) {
    case_when(
      g == "female" & abs(scale) >= 0.6 ~ "female",
      g == "female" & abs(scale) <  0.6 ~ "mostly_female",
      g == "male"   & abs(scale) >= 0.6 ~ "male",
      g == "male"   & abs(scale) <  0.6 ~ "mostly_male",
      TRUE                               ~ "unknown"
    )
  }

  cached <- cached |>
    mutate(gender_imputed = namsor_to_gender(namsor_gender, namsor_scale))

  ## 6. Build a lookup: first_name × country_iso2 → imputed gender
  gender_lookup <- cached |>
    select(first_name, country_iso2, gender_imputed)

  ## 7. Update only the unknown/andy rows in df_raw
  df_raw <- df_raw |>
    mutate(
      country_iso2 = countrycode(country, origin = "country.name", destination = "iso2c"),
      gender = case_when(
        gender %in% c("unknown", "andy") ~
          coalesce(
            gender_lookup$gender_imputed[
              match(paste(first_name, country_iso2),
                    paste(gender_lookup$first_name, gender_lookup$country_iso2))
            ],
            gender   # keep "unknown" if NamSor also couldn't resolve it
          ),
        TRUE ~ gender   # leave all other values untouched
      )
    ) |>
    select(-country_iso2)

  ## 8. Persist the resolved labels
  #  NOTE: this overwrites the input file in place, so the imputation is a
  #  one-way step — after a successful run the source CSV carries resolved
  #  labels and the guard above will skip on every subsequent run. Keep a
  #  pristine copy of upwork_pooled.csv outside the working directory.
  write_csv(df_raw, "upwork_pooled.csv")

}
