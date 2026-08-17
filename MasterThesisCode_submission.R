################################################################################
##  BORDERLESS BY DESIGN? GEOGRAPHIC ORIGIN AND ALGORITHMIC RANKING
##  IN ONLINE LABOR MARKETS
##
##  Analysis script for the master's thesis of the same title.
##
##  Author      : Zsombor Nemes
##  Institution : Rotterdam School of Management, Erasmus University
##  Submitted   : 15 August 2026
##
##  ---------------------------------------------------------------------------
##  WHAT THIS FILE DOES
##  ---------------------------------------------------------------------------
##  Tests whether a freelancer's geographic origin predicts their position in
##  Upwork talent-search results. The outcome, rank_global, is the position a
##  profile holds within a top-20 result set, so LOWER NUMBERS ARE BETTER
##  PLACEMENTS and a negative Global South coefficient is an advantage.
##
##  Four hypotheses are tested against five specifications:
##
##    Model 1   m1          H1  direct effect of origin        Table 7, col. 1
##    Model 1b  m1b         H1  within-keyword check           Table 7, col. 2
##    Model 2   m2          H2  verifiability interaction      Table 8
##    Model 3   m3          H3  profile richness interaction   Table 9
##    Model 4   ob_result   H4  Oaxaca-Blinder, omega = 0.5    Table 10
##
##  ---------------------------------------------------------------------------
##  SECTIONS
##  ---------------------------------------------------------------------------
##    0   Packages.
##    1   Data loading. Profile URLs are hashed on read.
##    2   Variable construction: earnings tier, platform quality signals, the
##        profile richness index, and the result-set cell used as the stratum.
##    3   Control vectors and the formula helper that every model reads.
##    4   Descriptive statistics and design validation: sample composition,
##        Tables 5-6, Figure 2, the permutation structure of the outcome, its
##        reproducibility across the two collection waves, and the missingness
##        screens. Closes with an unconditional baseline for context.
##    5   Model 1, together with the regression assumption checks and the
##        earnings-tier non-display diagnostic.
##    6   Model 1b.
##    7   Model 2.
##    8   Model 3.
##    9   Model 4, with a check on whether the decomposed gap is non-zero.
##   10   Supplementary analyses NOT reported in the thesis. Retained so the
##        repository shows what was examined; each block says so in its header.
##   11   Regression tables, captioned and filed with the thesis numbering.
##   12   Figures, likewise.
##   13   Closing note on the limitation the design cannot resolve.
##
##  ---------------------------------------------------------------------------
##  TO REPRODUCE
##  ---------------------------------------------------------------------------
##  Requires upwork_pooled.csv and genderize_namsor.R in the working directory.
##  All output is written to ./figures/. The control vector is set in one place,
##  at `controls_primary` in Section 3.
################################################################################


# ── 0. PACKAGES ───────────────────────────────────────────────────────────────

library(tidyverse)      # data wrangling + ggplot2
library(fixest)         # fast OLS / FE with clustered SE  (feols)
library(sandwich)       # vcovCL for non-fixest models
library(lmtest)         # coeftest
library(oaxaca)         # Blinder-Oaxaca decomposition
library(quantreg)       # quantile regression (rq)
library(modelsummary)   # regression tables (modelsummary / modelplot)
library(ggplot2)        # figures (already via tidyverse, but explicit)
library(scales)         # label helpers for ggplot
library(gt)
library(flextable)
library(car)
library(digest)

dir.create("figures", showWarnings = FALSE)   # output folder for figures


# ── 1. DATA LOADING ──────────────────────────────────────────────────────────

df_raw <- read_csv("upwork_pooled.csv", show_col_types = FALSE)
source("genderize_namsor.R")

df_raw <- df_raw |>
  mutate(profile_url = sapply(profile_url, digest, algo = "sha256"))

cat(sprintf("Raw data: %d rows × %d columns\n", nrow(df_raw), ncol(df_raw)))


# ── 2. VARIABLE CONSTRUCTION ─────────────────────────────────────────────────

## 2.1  Earnings tier → numeric → ordinal rank --------------------------------
#
#  Original values: "$0", "$100+", "$1K+", "$10K+", "$100K+", "$1M+", etc.,
#  plus a handful of exact dollar amounts (e.g. "$62", "$300").
#  Strategy: extract the numeric threshold, convert K/M suffixes, then
#  dense_rank() to produce a clean ordinal scale (1 = lowest earnings).

parse_earnings_num <- function(x) {
  x <- str_trim(as.character(x))
  dplyr::case_when(
    is.na(x) | x == "" ~ NA_real_,
    str_detect(x, "\\$[0-9]+M") ~
      as.numeric(str_extract(x, "[0-9]+")) * 1e6,
    str_detect(x, "\\$[0-9]+K") ~
      as.numeric(str_extract(x, "[0-9]+")) * 1e3,
    str_detect(x, "\\$[0-9]+") ~
      as.numeric(str_extract(x, "[0-9]+")),
    TRUE ~ NA_real_
  )
}


## 2.2  Build analysis dataset ------------------------------------------------

# Helper: binarise a continuous disclosure component at its sample mean.
# Follows Galperin & Greppi (2019), who dichotomise profile completeness at
# the sample average (80%) rather than standardising it. See the
# profile-richness construction below.
above_mean <- function(x) as.integer(x > mean(x, na.rm = TRUE))

df <- df_raw |>
  mutate(

    # -- Earnings tier ------------------------------------------------------
    #  Upwork displays cumulative earnings as a threshold badge ("$10K+") and
    #  withholds it for 526 of the 3,974 observations. Those 526 are NOT
    #  freelancers without completed work: they score at or above the sample on
    #  every platform-generated quality signal, and are more likely to hold a
    #  badge (85.2% vs 67.7%) and Top Rated Plus status (34.0% vs 24.5%). See
    #  thesis Table A5, p. 73. Filling them at the floor of the earnings scale
    #  would therefore assign the lowest value on the scale to freelancers who
    #  outperform the sample on all of it.
    #
    #  Non-display is also close to a single event across three fields: the
    #  indicator for a withheld earnings tier correlates r = 0.962 with a
    #  withheld job count and r = 0.736 with withheld hours worked (Table A5,
    #  Panel B). Non-display of the Job Success Score is a separate event and
    #  keeps its own indicator.
    #
    #  Three consequences for the construction below:
    #    (a) earnings enter as log10 of the displayed dollar threshold. The
    #        alternative, a dense rank over the 63 distinct thresholds, mixes
    #        exact amounts ($5, $43, $62) with tier floors ($1K+, $10K+) and so
    #        stretches the sparse bottom of the distribution relative to its
    #        dense middle. The two correlate r = 0.990, so this is a
    #        clarification of scale rather than a change of measure.
    #    (b) withheld earnings, job count and hours collapse into ONE indicator,
    #        `no_history_shown`, because they are one event.
    #    (c) the filled value is the observed MEDIAN. The indicator then carries
    #        the level shift while the continuous term carries the gradient,
    #        and the two are close to orthogonal rather than near-collinear.
    #
    #  `earnings_ordinal` is constructed but unused; it is retained only so the
    #  zero-fill specification remains estimable from this file.
    earnings_num     = parse_earnings_num(earnings_tier),
    earnings_missing = as.integer(is.na(earnings_num)),
    earnings_ordinal = replace_na(dense_rank(earnings_num), 0),  # zero-fill vector only
    earnings_log10   = log10(replace_na(earnings_num,
                                        median(earnings_num, na.rm = TRUE)) + 1),

    # ── Badge → ordered integer (NA = no badge) ─────────────────────────────
    badge_clean = replace_na(badge, "No Badge"),
    badge_ord   = ordered(
      badge_clean,
      levels = c("No Badge", "Rising Talent", "Top Rated", "Top Rated Plus")
    ),
    badge_num = as.integer(badge_ord),   # 1 = No badge … 4 = Top Rated Plus

    # ── JSS: missing flag + MEDIAN fill ─────────────────────────────────────
    #  Zero-filling JSS makes jss_filled exactly 0 whenever jss_missing is
    #  1, so the two are mechanically linked and their VIFs reach 16.7 and
    #  15.8. Median-filling breaks that link: the dummy
    #  carries the level shift, the continuous term carries the gradient,
    #  and the two are close to orthogonal. Coefficients on jss are
    #  interpretable in the same way; only the collinearity disappears.
    jss_missing = as.integer(is.na(jss)),
    jss_filled  = replace_na(jss, 0),                              # zero-fill vector
    jss_med     = replace_na(jss, median(jss, na.rm = TRUE)),       # reported vector

    # ── Volume signals: median fill + log-transform ────────────────────────
    #  Same argument as for JSS. The zero-filled objects are retained so that
    #  the alternative vector remains estimable.
    total_jobs_filled = replace_na(total_jobs,    0),               # zero-fill vector
    hours_filled      = replace_na(hours_worked,  0),               # zero-fill vector
    reviews_filled    = replace_na(review_count,  0),
    log_total_jobs    = log1p(total_jobs_filled),                   # zero-fill vector
    log_hours         = log1p(hours_filled),                        # zero-fill vector
    log_reviews       = log1p(reviews_filled),
    log_total_jobs_m  = log1p(replace_na(total_jobs,
                                         median(total_jobs,   na.rm = TRUE))),
    log_hours_m       = log1p(replace_na(hours_worked,
                                         median(hours_worked, na.rm = TRUE))),

    # ── ONE dummy for the joint "no work history displayed" event ──────────
    #  Missing earnings tier, missing total_jobs and missing hours_worked are
    #  the same underlying event (r = 0.962 and 0.736 between the indicators).
    #  Separate zero-fills with a single dummy would force that dummy to
    #  absorb three artificial floors at once. One dummy, one event.
    no_history_shown = as.integer(is.na(earnings_num) |
                                  is.na(total_jobs)   |
                                  is.na(hours_worked)),

    # ── Hourly rate: log-transform (right-skewed) ───────────────────────────
    log_rate = log1p(replace_na(rate_usd_hr, median(rate_usd_hr, na.rm = TRUE))),

    # ── Years on platform: fill rare NA with median ──────────────────────────
    years_on_platform = replace_na(
      years_on_platform,
      median(years_on_platform, na.rm = TRUE)
    ),

    # ── Profile richness components (voluntary disclosure only) ─────────────
    #  Three components, each with complete coverage and genuine variance in
    #  BOTH scraping waves. Two former components were dropped:
    #
    #   has_photo    — 3,959 of 3,972 profiles (99.7%) display a photo. A
    #                  near-constant cannot carry moderating variation, and it
    #                  was the source of the z-score blow-up documented below.
    #
    #   overview_len — captures page layout, not profile content. The Phase 2
    #                  selector (div.d-flex.justify-space-between) is a generic
    #                  layout class. In March it returned a bimodal artefact:
    #                  two tight bands, 122-158 and 241-280, with NOTHING in
    #                  between, the upper band almost exactly double the lower
    #                  (a block rendered once vs twice). In June the markup
    #                  consolidated and it returned the constant 136 for all
    #                  1,988 observations (sd = 0.00). It correlates -0.09 with
    #                  the freelancer's own card bio in March, where a genuine
    #                  overview length would correlate strongly positive.
    #                  Self-description length is measured by bio_len_card
    #                  instead, which is stable across waves (r = 0.964 for the
    #                  1,239 profiles appearing in both).
    bio_card       = replace_na(bio_len_card,
                                median(bio_len_card,       na.rm = TRUE)),
    skills_card    = replace_na(skills_count_card,
                                median(skills_count_card,  na.rm = TRUE)),
    video          = replace_na(has_video, 0),

    # ── Gender dummies ───────────────────────────────────────────────────────
    #  "andy" = androgynous (genderize.io category); treat as unknown.
    gender_female  = case_when(
      gender %in% c("female",       "mostly_female") ~ 1L,
      gender %in% c("male",         "mostly_male")   ~ 0L,
      TRUE ~ NA_integer_
    ),
    gender_unknown = as.integer(gender %in% c("unknown", "andy")),

    # ── Factors ─────────────────────────────────────────────────────────────
    keyword_f        = factor(keyword),
    profile_f        = factor(profile_url),
    scrape_session   = as.integer(scrape_session),   # already 0/1
    income_factor    = factor(
      income_group_wb,
      levels = c("High income", "Upper middle income",
                 "Lower middle income", "Low income")
    ),
    verif_factor = factor(verif_group, levels = c("medium", "high", "low"))

  ) |>
  # -- Profile richness index: additive disclosure count (0-3) ----------------
  #
  #  A single continuous Richness_i term, as required by the Model 3 equation in
  #  thesis Section 4.3.4 and the conceptual model in Section 3.2, where the
  #  index is a control except when interacted with the Global South indicator
  #  to test H3.
  #
  #  CONSTRUCTION -- following Galperin & Greppi (2019). Their profile
  #  completeness measure is a platform-computed weighted percentage that they
  #  dichotomise at the sample average (80%) and enter as a dummy. Upwork
  #  publishes no equivalent percentage, so the count is assembled directly from
  #  its constituent disclosures using the same sample-average cut rule:
  #
  #     richness_index = I(bio_len_card      > mean)  # self-description length
  #                    + I(skills_count_card > mean)  # breadth of listed skills
  #                    + has_video                    # intro video present
  #
  #  Range 0-3, mean ~1.24. No standardisation, so no component can dominate the
  #  sum through skew. The H3 interaction is read per additional disclosure
  #  element.
  #
  #  SCOPE -- all three components are VOLUNTARY DISCLOSURE. Platform-generated
  #  performance signals (badge, earnings tier, JSS, job counts) are deliberately
  #  excluded; they enter the models as separate controls. Galperin & Greppi's
  #  central result is that the two information types behave differently --
  #  validated work history and feedback attenuate the foreign penalty
  #  (-0.119 -> -0.079; -0.164 -> -0.052) while self-reported profile
  #  information does not (-0.122 -> -0.135). Keeping the index purely
  #  self-reported is what makes H3 a clean test of that contrast.
  #
  #  TWO COMPONENTS ARE DELIBERATELY ABSENT, both for measurement reasons
  #  documented in the component block above: has_photo is present on 99.7% of
  #  profiles and carries no moderating variation, and overview_len measures
  #  page layout rather than profile content (bimodal in March, constant at 136
  #  for all 1,988 June observations). Self-description length is measured by
  #  bio_len_card, which is stable across waves at r = 0.964.
  mutate(
    r_bio    = above_mean(bio_card),
    r_skills = above_mean(skills_card),
    r_video  = as.integer(video),
    richness_index = r_bio + r_skills + r_video
  )

# ── Diagnostic: profile richness index (additive disclosure count) ───────────
#
#  Confirms every component carries real variance in BOTH waves — the failure
#  that disqualified has_photo and overview_len from the index.

cat("\n─────────────────────────────────────────────────────────\n")
cat("  DIAGNOSTIC: Profile richness index (0-3 disclosure count)\n")
cat("─────────────────────────────────────────────────────────\n")

richness_components <- df |>
  select(profile_url, scrape_session, bio_card, skills_card, video,
         r_bio, r_skills, r_video, richness_index)

cat("\n-- Sample-mean cut points --\n")
cat(sprintf("   bio_len_card      > %.0f characters\n", mean(df$bio_card)))
cat(sprintf("   skills_count_card > %.1f skills\n",     mean(df$skills_card)))

cat("\n-- Raw component summary --\n")
print(summary(richness_components |> select(bio_card, skills_card, video)))

cat("\n-- Share above cut / present, by component --\n")
print(richness_components |>
        summarise(across(c(r_bio, r_skills, r_video), mean)) |>
        round(3))

cat("\n-- VARIANCE CHECK by scraping wave (0 = March, 1 = June) --\n")
cat("   Every component must vary in both waves. A component with sd = 0 in\n")
cat("   either wave is a session artefact and must not enter the index.\n")
print(richness_components |>
        group_by(scrape_session) |>
        summarise(across(c(bio_card, skills_card, video),
                         list(sd = ~sd(.x), n_distinct = ~n_distinct(.x))),
                  .groups = "drop"))

cat("\n-- richness_index distribution --\n")
print(table(richness_components$richness_index))
print(summary(richness_components$richness_index))

cat("\n-- Correlation with the components it is built from --\n")
print(round(cor(df$richness_index,
                df[, c("bio_card", "skills_card", "video")]), 3))

# -- Result-set cell and within-cell percentile rank ---------------------------
#
#  percent_rank is computed within the RESULT SET actually served to the client,
#  keyword x scraping session, rather than within keyword alone. Pooling the two
#  waves would mix two separate result pages into a single ranking, so that each
#  rank value appeared roughly twice inside each group and percent_rank's
#  min-rank tie handling compressed the scale.
#
#  `cell` is also the stratum used by the supplementary analyses in Section 10
#  -- the permutation test, the result-set fixed effects and the top-k
#  conditional logit -- because it is the set within which the algorithm
#  allocated the twenty positions.

df <- df |>
  mutate(cell = interaction(keyword, scrape_session, drop = TRUE)) |>
  group_by(cell) |>
  mutate(
    rank_pct  = percent_rank(rank_global),        # 0 = best, 1 = worst in cell
    cell_size = n()
  ) |>
  ungroup() |>
  mutate(
    top5  = as.integer(rank_global <= 5),
    top10 = as.integer(rank_global <= 10)
  )

cat(sprintf("Analysis dataset: %d rows × %d columns\n", nrow(df), ncol(df)))


# -- 3. CONTROL VECTORS AND FORMULA HELPER ------------------------------------
#
#  Defined here, ahead of every model, so that no specification below can read a
#  control vector before it exists.
#
#  Used across all OLS/FE models.  gender_female has NAs for gender-unknown
#  observations; those rows are dropped automatically by feols (listwise).
#  gender_unknown is a separate flag retained in the model.

#  NOTE — completeness_card, log_bio_card and log_skills_card were removed
#  from this vector when richness_index was rebuilt as a disclosure count.
#  richness_index is now constructed FROM bio_len_card and skills_count_card
#  (r = 0.63 and 0.57 respectively), so retaining the logs would place the
#  index and its own components in the same regression. completeness_card was
#  additionally a double-count: it is defined as
#  has_photo + I(bio>50) + I(skills>3) + I(badge!=NA) + I(earnings!=NA), and
#  badge_num and earnings_ordinal already enter separately. richness_index is
#  now the single measure of voluntary disclosure, matching the Richness_i
#  term in the Sec. 4.3.4 Model 3 equation.

#  GENDER -- read before using gender_female.
#  Gender is inferred from first names by an external name-inference service.
#  The field contains zero unknowns across all 3,974 rows, which is not
#  plausible for a sample that is 18.5% Pakistan, 11.9% India and 4.5% Nigeria,
#  so it is a control of unknown validity and is treated as such in the thesis
#  limitations. It is retained because it is inert (coefficient ~ -0.08,
#  t ~ -0.35). gender_unknown is constant at 0; it is dropped from the vector
#  explicitly rather than left to be auto-removed for collinearity, so that the
#  routine modelling notice does not mask a measurement problem. See the closing
#  note at the end of this file.


controls <- c(                       # zero-fill vector, retained for comparison
  "jss_filled", "jss_missing",
  "log_total_jobs", "log_reviews", "log_hours",
  "log_rate", "earnings_ordinal", "earnings_missing", "badge_num",
  "years_on_platform", "richness_index",
  "scrape_session",
  "gender_female", "gender_unknown"
)

#  Reported vector: median-filled continuous terms, ONE joint missingness
#  dummy, log10 earnings in place of a 0-63 dense rank, gender_unknown out.
controls_v3 <- c(
  "jss_med", "jss_missing",
  "log_total_jobs_m", "log_reviews", "log_hours_m",
  "log_rate", "earnings_log10", "no_history_shown", "badge_num",
  "years_on_platform", "richness_index",
  "scrape_session",
  "gender_female"
)

#  ── THE PRIMARY VECTOR SWITCH ───────────────────────────────────────────────
#  Every model downstream reads `controls_primary`. It is set to the vector
#  the thesis reports; setting it to `controls` instead runs the entire file
#  on the zero-fill vector. Nothing else needs to change.
#
#  Two blocks stay pinned to the zero-fill vector regardless of this switch,
#  because their purpose is to document the difference between the rules:
#    - m1_lm and the VIF comparison in the assumption checks (Section 5.2),
#    - m1_v2vec in Section 5.1.
controls_primary <- controls_v3

make_fml <- function(lhs, rhs_extra = NULL, fe = NULL, ctrl = controls_primary) {
  # Helper: build a fixest formula string
  rhs <- paste(c(rhs_extra, ctrl), collapse = " + ")
  if (!is.null(fe)) {
    as.formula(paste(lhs, "~", rhs, "|", fe))
  } else {
    as.formula(paste(lhs, "~", rhs))
  }
}

################################################################################
##  4.  DESCRIPTIVE STATISTICS AND DESIGN VALIDATION
##
##  Corresponds to thesis Section 5.1 (pp. 41-45). Produces Table 5 (sample
##  composition), Table 6 (descriptive statistics), Figure 2 (correlation
##  heatmap) and the inputs to Figures 3-5, together with the checks that
##  validate the sampling design: the within-cell permutation structure, the
##  cross-wave reproducibility of the ordering, and the missingness screens.
##
##  Model 0, the unconditional baseline, is estimated at the end of this section
##  rather than with the reported models. It is not one of the five
##  specifications in thesis Table 4 and is not reported as a result; it is
##  included as unconditional context for the Global South coefficient.
################################################################################

cat("\n─────────────────────────────────────────────────────────\n")
cat("  SECTION 4: DESCRIPTIVE STATISTICS AND DESIGN VALIDATION\n")
cat("─────────────────────────────────────────────────────────\n")

## 4.1  Sample overview
cat(sprintf("Observations      : %d\n", nrow(df)))
cat(sprintf("Unique profiles   : %d\n", n_distinct(df$profile_url)))
cat(sprintf("Keywords          : %d\n", n_distinct(df$keyword)))
cat(sprintf("Countries         : %d\n", n_distinct(df$country_wb)))
cat(sprintf("ISCO sub-major grp: %d\n", n_distinct(df$isco_group)))
cat(sprintf("Sessions          : %d (0=March 2026, 1=June 2026)\n",
            n_distinct(df$scrape_session)))

## 4.2a Geographic composition
cat("\n── Geographic distribution ──\n")
df |>
  count(income_group_wb, global_south, name = "n") |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  arrange(global_south, income_group_wb) |>
  print()

## 4.2  Table 5: Sample Composition by Income Group and Geographic Origin (p. 41)
tbl5_data <- df |>
  mutate(geographic_origin = ifelse(global_south == 1, "Global South", "Global North")) |>
  count(income_group_wb, geographic_origin, name = "n") |>
  mutate(`Share (%)` = round(100 * n / sum(n), 1)) |>
  rename(
    `Income Group`      = income_group_wb,
    `Geographic Origin` = geographic_origin,
    `N`                 = n
  ) |>
  arrange(`Income Group`, `Geographic Origin`)

flextable::flextable(tbl5_data) |>
  flextable::set_caption(
    caption = "Table 5. Sample Composition by Income Group and Geographic Origin"
  ) |>
  flextable::theme_booktabs() |>
  flextable::bold(part = "header") |>
  flextable::fontsize(size = 10, part = "all") |>
  flextable::autofit() |>
  flextable::save_as_docx(path = "figures/table5_sample_composition.docx")
cat("  Saved: figures/table5_sample_composition.docx\n")

## 4.3  Mean rank by geographic origin
cat("\n── Mean search rank by geographic origin (1 = top position) ──\n")
rank_by_origin <- df |>
  group_by(global_south) |>
  summarise(
    n           = n(),
    mean_rank   = mean(rank_global),
    sd_rank     = sd(rank_global),
    median_rank = median(rank_global),
    .groups     = "drop"
  )
print(rank_by_origin)

raw_gap <- diff(rank_by_origin$mean_rank[order(rank_by_origin$global_south)])
cat(sprintf("\nRaw rank gap (GS − GN): %.3f positions (positive = GS disadvantaged)\n", raw_gap))

## 4.4  Mean rank by income group (four-tier)
cat("\n── Mean rank by World Bank income group ──\n")
rank_by_income <- df |>
  group_by(income_group_wb) |>
  summarise(
    n         = n(),
    mean_rank = round(mean(rank_global), 2),
    sd_rank   = round(sd(rank_global),   2),
    .groups   = "drop"
  ) |>
  arrange(mean_rank)
print(rank_by_income)

## 4.5  Mean rank by verifiability group × geographic origin
cat("\n── Mean rank by verifiability group and geographic origin ──\n")
rank_by_verif <- df |>
  group_by(verif_group, global_south) |>
  summarise(
    n         = n(),
    mean_rank = round(mean(rank_global), 2),
    .groups   = "drop"
  ) |>
  pivot_wider(names_from = global_south,
              values_from = c(n, mean_rank),
              names_prefix = "GS") |>
  mutate(gap = round(mean_rank_GS1 - mean_rank_GS0, 2))
print(rank_by_verif)

## 4.6  Table 6: Descriptive Statistics (p. 42) -------------------------------
#
#  Saved to Word as a plain three-line (booktabs) table: variable names in
#  column 1, then p50 / mean / sd / max / min — matching the layout of a
#  standard Stata-style "summarize, detail" table.

#  Table 6 describes the variables that actually enter the reported models
#  and nothing else. The zero-fill constructions are deliberately not shown:
#  the estimates are invariant to the imputation rule, so listing both sets
#  in the main descriptives table adds clutter without carrying an argument.
#  The contrast between the two rules is made by the VIF comparison instead.
tbl6_vars <- df |>
  select(
    "Search rank (1-20)"          = rank_global,
    "Global South (0/1)"          = global_south,
    "JSS (0-100, median-filled)"  = jss_med,
    "JSS missing (dummy)"         = jss_missing,
    "Log(total jobs)"             = log_total_jobs_m,
    "Log(review count)"           = log_reviews,
    "Log(hours worked)"           = log_hours_m,
    "Log(hourly rate)"            = log_rate,
    "Log10(earnings threshold)"   = earnings_log10,
    "No work history shown (dummy)" = no_history_shown,
    "Badge level (1-4)"           = badge_num,
    "Years on platform"           = years_on_platform,
    "Profile richness index"      = richness_index
  )

## Helper: a "plain" three-line table (no bold header/body, no shading) -------
#  Mirrors the classic Stata summary-statistics layout: thin rule above and
#  below the header, thin rule at the bottom, left-aligned variable names,
#  right-aligned numeric columns.

save_plain_table <- function(data, caption, path, first_col_width = 2.2) {
  ft <- flextable::flextable(data) |>
    flextable::set_caption(caption = caption) |>
    flextable::theme_booktabs() |>
    flextable::fontsize(size = 10, part = "all") |>
    flextable::align(j = 1, align = "left",  part = "all") |>
    flextable::align(j = 2:ncol(data), align = "right", part = "all") |>
    flextable::width(j = 1, width = first_col_width) |>
    flextable::autofit()
  flextable::save_as_docx(ft, path = path)
  cat(sprintf("  Saved: %s\n", path))
}

tbl6_data <- tibble(Variable = names(tbl6_vars)) |>
  mutate(
    p50  = map_dbl(tbl6_vars, median, na.rm = TRUE),
    mean = map_dbl(tbl6_vars, mean,   na.rm = TRUE),
    sd   = map_dbl(tbl6_vars, sd,     na.rm = TRUE),
    max  = map_dbl(tbl6_vars, max,    na.rm = TRUE),
    min  = map_dbl(tbl6_vars, min,    na.rm = TRUE)
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

save_plain_table(
  data    = tbl6_data,
  caption = "Table 6. Descriptive Statistics",
  path    = "figures/table6_descriptive_statistics.docx"
)

## 4.7  Figure 2: Pairwise Correlation Heatmap (p. 42) ------------------------
#
#  A pairwise-correlation *table* becomes unreadable once enough variables
#  are involved (here, all regression variables in the control vector plus
#  the outcome and treatment — 18 columns). A heatmap conveys the same
#  information in one compact, single-page figure: colour shows the sign
#  and strength of each pairwise correlation, with the value overlaid as
#  text for precision. This replaces the earlier flextable/Word version,
#  which stayed unreadable even after landscape orientation and font
#  shrinking once wider than a dozen or so variables.
#
#  Note: this is a descriptive complement, not the primary multicollinearity
#  diagnostic — Variance Inflation Factors (see "REGRESSION ASSUMPTION
#  CHECKS" under Model 1, above) remain the formal test for that.

#  Built from the reported control vector, so the heatmap shows the
#  correlation structure of the variables actually in the models. Under the
#  zero-fill rule the same figure carries the imputation artefact directly:
#  JSS-vs-JSS-missing and earnings-vs-earnings-missing appear as near-perfect
#  negative correlations, which is what VIFs of 16-17 report from the other
#  side. Those cells sitting near zero is the check that the rule works.
corr_vars <- df |>
  select(
    "Search rank"         = rank_global,
    "Global South"        = global_south,
    "JSS"                 = jss_med,
    "JSS missing"         = jss_missing,
    "Log(total jobs)"     = log_total_jobs_m,
    "Log(reviews)"        = log_reviews,
    "Log(hours)"          = log_hours_m,
    "Log(hourly rate)"    = log_rate,
    "Log10(earnings)"     = earnings_log10,
    "No history shown"    = no_history_shown,
    "Badge level"         = badge_num,
    "Years on platform"   = years_on_platform,
    "Richness index"      = richness_index,
    "Session (June=1)"    = scrape_session,
    "Female"              = gender_female
    # gender_unknown omitted: constant (0) in the final sample -> sd = 0,
    # which produces an all-NA row/column in the correlation matrix.
  )

corr_mat_full <- cor(corr_vars, use = "pairwise.complete.obs")
var_order     <- colnames(corr_mat_full)

corr_long <- corr_mat_full |>
  as.data.frame() |>
  rownames_to_column("Var1") |>
  pivot_longer(-Var1, names_to = "Var2", values_to = "r") |>
  mutate(
    Var1 = factor(Var1, levels = var_order),
    Var2 = factor(Var2, levels = rev(var_order))
  )

p_corr <- ggplot(corr_long, aes(x = Var1, y = Var2, fill = r)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", r)), size = 2.3) +
  scale_fill_gradient2(
    low = "#C62828", mid = "white", high = "#1565C0",
    midpoint = 0, limits = c(-1, 1), name = "Pearson r"
  ) +
  labs(
    title    = "Figure 2. Pairwise Correlation Heatmap",
    subtitle = "Pairwise-complete Pearson correlations among regression variables",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x       = element_text(angle = 45, hjust = 1),
    panel.grid        = element_blank(),
    plot.title        = element_text(face = "bold"),
    plot.subtitle     = element_text(colour = "grey40")
  ) +
  coord_fixed()

ggsave("figures/fig2_correlation_heatmap.png", p_corr,
       width = 11, height = 9.5, dpi = 300)
cat("  Saved: figures/fig2_correlation_heatmap.png\n")

## 4.8  Welch t-test: rank_global by global_south
cat("\n── Welch t-test: rank_global ~ global_south ──\n")
t_res <- t.test(rank_global ~ global_south, data = df)
print(t_res)


# -- 4.9  DESIGN CHECK: the outcome is a within-cell permutation ---------------
#
#  rank_global is not a free-running random variable. Each keyword x session
#  cell contains the top 20 results, so the ranks within a cell are a
#  PERMUTATION of 1..20: exactly one profile holds each position, the cell mean
#  is fixed at 10.5 by construction, and one freelancer can only move up if
#  another moves down. Thesis Section 4.1.2 (p. 29) sets this out; Section 5.1
#  (p. 42) reports the realised data conforming to it.
#
#  Three consequences, all stated in the thesis:
#
#   1. The marginal distribution of rank_global is forced to be near-uniform on
#      1..20, so its standard deviation is pinned at sqrt((20^2 - 1)/12) = 5.766
#      whatever the covariates do. The observed value is 5.771. That is the
#      design, not an empirical finding.
#
#   2. Almost none of the variance lies BETWEEN cells (0.074 of a total 33.304,
#      or 0.22%), so keyword fixed effects have close to nothing to absorb. This
#      is why adding keyword dummies drives adjusted R2 negative in Model 1b and
#      Model 3.
#
#   3. A low R2 is therefore not evidence of a misspecified model. The ceiling
#      is structural and is reported as such.
#
#  What a low R2 does indicate here is addressed by the reproducibility check
#  immediately below.

cat("\n─────────────────────────────────────────────────────────\n")
cat("  STRUCTURAL DIAGNOSTIC: within-cell permutation check\n")
cat("─────────────────────────────────────────────────────────\n")

cell_struct <- df |>
  group_by(cell) |>
  summarise(n = n(), n_unique_ranks = n_distinct(rank_global), .groups = "drop")

cat(sprintf("  Result-set cells (keyword x session) : %d\n", nrow(cell_struct)))
cat(sprintf("  Cells with exactly 20 observations   : %d (%.1f%%)\n",
            sum(cell_struct$n == 20), 100 * mean(cell_struct$n == 20)))
cat(sprintf("  Cells with NO duplicated rank value  : %d (%.1f%%)\n",
            sum(cell_struct$n == cell_struct$n_unique_ranks),
            100 * mean(cell_struct$n == cell_struct$n_unique_ranks)))

var_total   <- var(df$rank_global)
var_between <- var(ave(df$rank_global, df$cell, FUN = mean))
cat(sprintf("\n  Total variance of rank_global        : %.3f\n", var_total))
cat(sprintf("  Variance BETWEEN cells               : %.3f (%.2f%% of total)\n",
            var_between, 100 * var_between / var_total))
cat(sprintf("  Observed sd                          : %.3f\n", sd(df$rank_global)))
cat(sprintf("  sd implied by a uniform permutation  : %.3f\n", sqrt((20^2 - 1) / 12)))
cat("\n  -> If between-cell variance is ~0 and observed sd matches the\n")
cat("     permutation benchmark, the outcome is a forced permutation and the\n")
cat("     R2 ceiling is structural. Say so in the Results chapter.\n")


# -- 4.10  DESIGN CHECK: is the ordering reproducible across waves? ------------
#
#  A structural R2 ceiling explains why R2 is small. It does not explain why it
#  is near zero. Two readings are possible:
#
#    (a) the within-page ordering is close to random, personalised or A/B
#        tested, in which case no covariate could predict it; or
#    (b) the ordering is systematic and reproducible, but the variables
#        collected here are not the inputs the algorithm uses.
#
#  The March/June panel distinguishes them. Under (a) the cross-wave
#  correlation would be near zero and the mean absolute rank change would
#  approach the independence benchmark E|U1 - U2| = 6.65 for two draws from
#  U{1..20}. The realised values -- r = 0.678, mean |change| 3.05, 65.7%
#  retention across 1,298 profile-keyword pairs -- support (b), and are reported
#  in thesis Section 5.1 (p. 42).
#
#  Reading (b) locates the limitation in the covariate set. Inputs the design
#  cannot observe include paid Boost and sponsored placement, the availability
#  badge, response rate and activity recency, Connects expenditure, client-side
#  personalisation, and the platform's proprietary internal quality score.

cat("\n─────────────────────────────────────────────────────────\n")
cat("  IS THE RANKING REPRODUCIBLE ACROSS WAVES?\n")
cat("─────────────────────────────────────────────────────────\n")

rank_panel <- df |>
  select(profile_url, keyword, scrape_session, rank_global) |>
  pivot_wider(names_from = scrape_session, values_from = rank_global,
              names_prefix = "wave") |>
  filter(!is.na(wave0), !is.na(wave1))

cat(sprintf("  profile x keyword pairs observed in both waves : %d\n",
            nrow(rank_panel)))
cat(sprintf("  Pearson  r(rank March, rank June)             : %.3f\n",
            cor(rank_panel$wave0, rank_panel$wave1)))
cat(sprintf("  Spearman r                                    : %.3f\n",
            cor(rank_panel$wave0, rank_panel$wave1, method = "spearman")))
cat(sprintf("  Mean |rank change|                            : %.2f\n",
            mean(abs(rank_panel$wave0 - rank_panel$wave1))))
cat("  Independence benchmark E|U1-U2|, U ~ U{1..20}  : 6.65\n")

retention <- df |>
  group_by(keyword) |>
  summarise(
    kept = length(intersect(profile_url[scrape_session == 0],
                            profile_url[scrape_session == 1])) /
           pmax(1, n_distinct(profile_url[scrape_session == 0])),
    .groups = "drop"
  )
cat(sprintf("  Mean top-20 retention March -> June           : %.3f\n",
            mean(retention$kept)))
cat("\n  -> High correlation + low |change| + high retention = the ordering is\n")
cat("     systematic and reproducible. The near-zero R2 is then a statement\n")
cat("     about the COVARIATE SET, not about the algorithm being noisy.\n")
cat("     Candidate omitted inputs to name in the Discussion: paid Boost /\n")
cat("     sponsored placement, the availability badge, recent response rate\n")
cat("     and recency of activity, Connects expenditure, client-side\n")
cat("     personalisation, and Upwork's proprietary internal quality score.\n")


# -- 4.11  MEASUREMENT CHECK: cross-wave stability of every control -----------
#
#  Because one collected field (overview_len) turned out to be a layout artefact
#  rather than a measured attribute, the same forensic standard is applied to
#  every regressor before any of them is used. Two questions, both checked here:
#
#    VALUE stability       -- does the recorded value agree across waves?
#    MISSINGNESS stability -- is a field absent for the same profile in both
#                             waves (a genuine profile property) or does it flip
#                             (a collection artefact)?
#
#  Thesis Section 5.1 (p. 42) reports the result: status flips for fewer than 5%
#  of profiles and recorded values correlate 0.81 to 1.00 between waves, so
#  non-display is a property of the profile and an imputation rule is
#  defensible on that ground.

cat("\n─────────────────────────────────────────────────────────\n")
cat("  CROSS-WAVE STABILITY OF CONTROLS\n")
cat("─────────────────────────────────────────────────────────\n")

both_wave_ids <- df |>
  group_by(profile_url) |>
  filter(n_distinct(scrape_session) == 2) |>
  pull(profile_url) |>
  unique()

stability_vars <- c("jss", "total_jobs", "hours_worked", "rate_usd_hr",
                    "review_count", "bio_len_card", "skills_count_card",
                    "years_on_platform")

stab <- map_dfr(stability_vars, function(v) {
  w <- df |>
    filter(profile_url %in% both_wave_ids) |>
    select(profile_url, scrape_session, value = all_of(v)) |>
    distinct(profile_url, scrape_session, .keep_all = TRUE) |>
    pivot_wider(names_from = scrape_session, values_from = value,
                names_prefix = "w")
  wv <- w |> filter(!is.na(w0), !is.na(w1))
  tibble(
    variable       = v,
    n_value_pairs  = nrow(wv),
    r_value        = if (nrow(wv) > 10) cor(wv$w0, wv$w1) else NA_real_,
    pct_miss_flips = 100 * mean(is.na(w$w0) != is.na(w$w1))
  )
})
print(stab |> mutate(across(where(is.numeric), ~ round(.x, 3))))
cat("\n  -> r_value near 1 = the field is measured consistently.\n")
cat("     pct_miss_flips near 0 = missingness is a genuine profile property,\n")
cat("     not scraper noise, and an imputation rule is therefore defensible\n")
cat("     ON THAT GROUND (though not necessarily substantively — see 2.2).\n")


## 4.12  Missingness summary (thesis Table A4, p. 71)
cat("\n── Missingness in key raw variables ──\n")
df_raw |>
  select(jss, total_jobs, hours_worked, rate_usd_hr,
         badge, review_count, overview_len,
         skills_count_profile, skills_count_card, bio_len_card,
         has_video, has_photo) |>
  summarise(across(everything(),
                   ~ sprintf("%d (%.1f%%)", sum(is.na(.)), 100 * mean(is.na(.))))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "missing") |>
  print()

## 4.13  Degenerate-variance screen
#  Missingness alone does not detect the failure mode that disqualified
#  overview_len: the field was fully POPULATED in June but took a single
#  value (136) for all 1,988 observations. Any variable that is constant, or
#  near-constant, within a scraping wave is a session artefact rather than a
#  measured attribute and must not enter an index or a control vector.
cat("\n── Degenerate-variance screen by scraping wave ──\n")
cat("   Flags fields that are (near-)constant within a wave.\n")
df_raw |>
  select(scrape_session, overview_len, has_photo, has_video,
         bio_len_card, skills_count_card, skills_count_profile) |>
  group_by(scrape_session) |>
  summarise(across(everything(),
                   ~ sprintf("sd=%.2f, distinct=%d",
                             sd(.x, na.rm = TRUE), n_distinct(.x, na.rm = TRUE))),
            .groups = "drop") |>
  pivot_longer(-scrape_session, names_to = "variable", values_to = "spread") |>
  pivot_wider(names_from = scrape_session, values_from = spread,
              names_prefix = "wave_") |>
  print()


# -- 4.14  MODEL 0: unconditional baseline ------------------------------------
#
#  Rank_(i,k) = alpha + beta1 GlobalSouth_(i) + epsilon_(i,k)
#
#  NOT one of the five specifications in thesis Table 4 (p. 40) and not reported
#  as a result. Estimated here as unconditional context for the controlled
#  estimate in Model 1: it is the regression equivalent of the raw group means
#  in Section 5.1. SE clustered at profile URL level.

cat("\n─────────────────────────────────────────────────────────\n")
cat("  MODEL 0 — Baseline OLS (H1)\n")
cat("─────────────────────────────────────────────────────────\n")

m0 <- feols(rank_global ~ global_south,
            data    = df,
            cluster = ~profile_url)
summary(m0)

################################################################################
##  5.  MODEL 1 -- OLS WITH FULL CONTROLS  (H1 primary)
##
##  Rank_(i,k) = alpha + beta1 GlobalSouth_(i) + gamma'X_(i) + epsilon_(i,k)
##
##  The primary test of H1. Thesis Section 4.3.2 (p. 37-38); reported in
##  Table 7, col. 1 (p. 46). SE clustered at profile URL level.
##
##  Lower rank numbers denote better placement, so H1 predicts a POSITIVE
##  coefficient on global_south. State the direction in words wherever this
##  estimate is described: a negative coefficient is a Global South advantage,
##  not a penalty.
##
##  This section also carries the three diagnostics the thesis attaches to
##  Model 1: the regression assumption checks (Sec. 4.3.2, p. 38), the
##  control-vector comparison, and the earnings-tier non-display diagnostic
##  (thesis Table A5, p. 73).
################################################################################

cat("\n─────────────────────────────────────────────────────────\n")
cat("  MODEL 1 — OLS with full controls (H1 primary)\n")
cat("─────────────────────────────────────────────────────────\n")

m1 <- feols(make_fml("rank_global", "global_south"),
            data    = df,
            cluster = ~profile_url)
summary(m1)


# -- 5.1  The same specification under the zero-fill control vector ------------
#
#  m1 runs on controls_primary. This block re-fits it on the zero-filled vector
#  so the two imputation rules can be compared directly. The point is
#  pre-emptive: a reader is entitled to ask whether the imputation rule was
#  chosen to produce a particular answer, and a stable global_south estimate
#  across the two rules answers it.
#
#  m1_v3 is an alias for m1, so that the blocks below name explicitly which of
#  the two vectors they are reading.

m1_v3    <- m1
m1_v2vec <- feols(make_fml("rank_global", "global_south", ctrl = controls),
                  data    = df,
                  cluster = ~profile_url)

cat("\n── Model 1 under the zero-fill control vector, for comparison ──\n")
summary(m1_v2vec)

cat("\n── Does the imputation rule move the H1 estimate? ──\n")
cat(sprintf("  Zero-fill vector, dense-rank earnings  : %+.4f (SE %.4f, p = %.3f)\n",
            coef(m1_v2vec)["global_south"], fixest::se(m1_v2vec)["global_south"],
            fixest::pvalue(m1_v2vec)["global_south"]))
cat(sprintf("  Median-fill vector, log10 earnings     : %+.4f (SE %.4f, p = %.3f)\n",
            coef(m1)["global_south"], fixest::se(m1)["global_south"],
            fixest::pvalue(m1)["global_south"]))
cat("  -> Report both. A stable estimate means the imputation rule affected\n")
cat("     the CONTROL coefficients, not the H1 result.\n")


# -- 5.2  REGRESSION ASSUMPTION CHECKS (Model 1) ------------------------------
#
#  Thesis Section 4.3.2 (p. 38) specifies four diagnostics on the least-squares
#  equivalent of the Model 1 fit: Breusch-Pagan for heteroscedasticity, variance
#  inflation factors, a residual normality test, and Cook's distance. All four
#  are run below. They are diagnostics, not results, and none of them appears as
#  a numbered table or figure in the thesis.
#
#  Two of the four are, in the thesis's own words, "reported but not relied
#  upon", and the code says so at each one:
#
#    - Residual normality. The outcome is a discrete permutation of the integers
#      1..20, so the residuals cannot be normal however well specified the
#      model. The Q-Q plot will always show the step pattern of a discrete
#      uniform.
#    - Cook's distance. The 4/n convention flags a fraction of any well-behaved
#      fit, so the maximum is reported against the conventional threshold of 1
#      instead.
#
#  Clustered standard errors are retained regardless of the Breusch-Pagan result
#  as the more conservative choice.
#
#  VIFs are reported for BOTH control vectors, as the thesis specifies, because
#  the comparison is itself the argument for the imputation rule: a zero-filled
#  variable equals zero exactly when its indicator equals one, making the pair
#  near-collinear by construction. Note that global_south carries a VIF of 1.59
#  throughout, so the H1 estimate was never the affected quantity -- the
#  platform-signal controls were.

cat("\n─────────────────────────────────────────────────────────\n")
cat("  ASSUMPTION CHECKS (Model 1)\n")
cat("─────────────────────────────────────────────────────────\n")

# Refit as lm for bptest and vif (feols doesn't support these directly)
m1_lm <- lm(as.formula(paste(
  "rank_global ~ global_south +",
  paste(controls, collapse = " + ")
)), data = df |> drop_na(all_of(c("rank_global", "global_south", controls))))

resid_m2  <- residuals(m1_lm)
fitted_m2 <- fitted(m1_lm)

# 1. Diagnostic plots
png("figures/diagnostic_assumption_checks.png", width = 1400, height = 1200, res = 130)
par(mfrow = c(2, 2))

plot(fitted_m2, resid_m2,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted",
     pch = 16, cex = 0.3, col = "steelblue")
abline(h = 0, lty = 2, col = "red")
lines(lowess(fitted_m2, resid_m2), col = "red", lwd = 1.5)

qqnorm(resid_m2, main = "Normal Q-Q",
       pch = 16, cex = 0.3, col = "steelblue")
qqline(resid_m2, col = "red", lwd = 1.5)

plot(fitted_m2, sqrt(abs(resid_m2)),
     xlab = "Fitted values", ylab = "sqrt(|Residuals|)",
     main = "Scale-Location",
     pch = 16, cex = 0.3, col = "steelblue")
lines(lowess(fitted_m2, sqrt(abs(resid_m2))), col = "red", lwd = 1.5)

hist(resid_m2, breaks = 50, main = "Residual Distribution",
     xlab = "Residuals", col = "steelblue", border = "white")

dev.off()
cat("Diagnostic plots saved to figures/diagnostic_assumption_checks.png\n")

# 2. Breusch-Pagan test (heteroscedasticity)
cat("\n── Breusch-Pagan test ──\n")
bp <- bptest(m1_lm)
print(bp)
if (bp$p.value < 0.05) cat("  -> Heteroscedasticity detected. Clustered SEs in feols already address this.\n")

# 3. VIF (multicollinearity)
cat("\n── Variance Inflation Factors ──\n")

# Identify and drop aliased (zero-variance after listwise deletion) variables
alias_info  <- alias(m1_lm)
aliased_vars <- rownames(alias_info$Complete)
if (length(aliased_vars) > 0)
  cat(sprintf("Aliased variables removed before VIF: %s\n",
              paste(aliased_vars, collapse = ", ")))

formula_vif <- update(formula(m1_lm),
                      paste(". ~ . -", paste(aliased_vars, collapse = " - ")))
m1_lm_vif  <- lm(formula_vif, data = m1_lm$model)

vif_vals <- vif(m1_lm_vif)
print(round(vif_vals, 2))
cat(sprintf("Max VIF: %.2f  |  Mean VIF: %.2f\n", max(vif_vals), mean(vif_vals)))
if (max(vif_vals) > 10) cat("  -> VIF > 10: severe multicollinearity in flagged variable(s).\n")
if (max(vif_vals) > 5)  cat("  -> VIF > 5: moderate multicollinearity — interpret affected coefficients cautiously.\n")


# 3b. VIF under the current control vector ------------------------------------
#  Thesis Section 4.3.2 (p. 38) specifies that VIFs be reported for both the
#  current control vector and the superseded zero-filled one. Under the
#  zero-filled vector they reach 16.7 (jss_filled), 15.8 (jss_missing), 17.5
#  (earnings_ordinal) and 8.3 (earnings_missing). Those values are not a
#  property of the data: a zero-filled variable equals zero exactly when its
#  indicator equals one, so the pair is a near-linear dependency by
#  construction, and median filling removes it. global_south carries a VIF of
#  1.59 under both vectors, so the H1 estimate was never the affected quantity.
cat("\n── VIF under the reported control vector ──\n")
m1_lm_v3 <- lm(as.formula(paste("rank_global ~ global_south +",
                                paste(controls_v3, collapse = " + "))),
               data = df |> drop_na(all_of(c("rank_global", "global_south",
                                             controls_v3))))
vif_v3 <- vif(m1_lm_v3)
print(round(vif_v3, 2))
cat(sprintf("Max VIF (reported vector): %.2f  |  Mean VIF: %.2f\n",
            max(vif_v3), mean(vif_v3)))

# 4. Normality of residuals ---------------------------------------------------
#  keep this in the appendix, but do not draw a conclusion from it.
#  The outcome is a discrete permutation of the integers 1..20, so the
#  residuals CANNOT be normal no matter how well specified the model is; the
#  Q-Q plot will always show the stepped pattern of a discrete uniform.
#  Shapiro-Wilk on n = 3,974 additionally rejects for negligible departures,
#  so a small p-value here carries no information. The relevant question is
#  whether inference is valid, and that is settled by the permutation test in
#  Section 5d, which assumes nothing about the residual distribution.
cat("\n── Normality test (reported for completeness; see note in code) ──\n")
if (length(resid_m2) <= 5000) {
  sw <- shapiro.test(resid_m2)
  cat(sprintf("Shapiro-Wilk: W = %.4f, p = %.4f\n", sw$statistic, sw$p.value))
} else {
  ks <- ks.test(scale(resid_m2), "pnorm")
  cat(sprintf("Kolmogorov-Smirnov: D = %.4f, p = %.4f\n", ks$statistic, ks$p.value))
}
cat("  -> Uninformative by construction: a permutation outcome cannot produce\n")
cat("     normal residuals. Rely on Section 5d for inference, not on the CLT.\n")

# 5. Influential observations (Cook's distance) -------------------------------
#  the 4/n rule flags roughly 2-5% of observations in ANY
#  well-behaved model; a count like "96 obs (2.4%)" is what a clean model looks
#  like, not a warning. Report the maximum Cook's D against the conventional
#  cutoff of 1 instead, which is the value that would actually indicate a
#  single observation driving the fit.
cat("\n── Influential observations ──\n")
cooks_d   <- cooks.distance(m1_lm)
threshold <- 4 / nrow(m1_lm$model)
n_inf     <- sum(cooks_d > threshold, na.rm = TRUE)
cat(sprintf("Cook's D > 4/n (threshold = %.4f): %d obs (%.1f%%) [expected 2-5%% in any model]\n",
            threshold, n_inf, 100 * n_inf / nrow(m1_lm$model)))
cat(sprintf("Max Cook's D: %.4f  (conventional concern threshold = 1)\n",
            max(cooks_d, na.rm = TRUE)))
cat(sprintf("Observations with Cook's D > 1: %d\n", sum(cooks_d > 1, na.rm = TRUE)))


# -- 5.3  DIAGNOSTIC: non-display of the earnings tier -------------------------
#
#  Reported in the thesis as Table A5, "Platform-Generated Quality Signals by
#  Display Status of the Earnings Tier" (p. 73). This block produces it.
#
#  The diagnostic establishes two things that together justify the imputation
#  rule set in Section 2.2. First, non-display does not identify freelancers
#  without completed work: the non-displaying group scores at least as well on
#  every platform-generated quality signal. Second, non-display of the earnings
#  tier, the job count and hours worked is close enough to a single event
#  (r = 0.962 and 0.736) to justify one shared indicator.
#
#  The third step is the reason the rule matters rather than merely differing.
#  Under zero-filling, the three affected fields are all floored for the same
#  526 observations while a single dummy covers the event, so that dummy absorbs
#  three artificial floors at once and returns a coefficient of the opposite
#  sign to the raw difference in means. It is an intercept correction for a
#  coding decision, not an effect. Removing the floored regressors flips the
#  sign back. This is why the reported models median-fill and use one indicator,
#  and why no substantive claim is attached to the non-display term.

cat("\n─────────────────────────────────────────────────────────\n")
cat("  DIAGNOSTIC: is earnings_missing a suppression artefact?\n")
cat("─────────────────────────────────────────────────────────\n")

cat("\n-- Step 1: who are the 'no earnings tier' freelancers? --\n")
cat("   Non-display might be read as newcomers with no work. Check it.\n")
print(df |>
        group_by(earnings_missing) |>
        summarise(
          n            = n(),
          mean_rank    = mean(rank_global),
          mean_jss     = mean(jss, na.rm = TRUE),
          mean_reviews = mean(review_count, na.rm = TRUE),
          pct_badged   = 100 * mean(badge_num > 1),
          pct_top_plus = 100 * mean(badge_num == 4),
          mean_bio     = mean(bio_len_card, na.rm = TRUE),
          mean_years   = mean(years_on_platform),
          .groups      = "drop"
        ) |>
        mutate(across(where(is.numeric), ~ round(.x, 2))))

cat("\n-- Step 2: are the three missingness events the same event? --\n")
miss_cor <- df |>
  transmute(
    m_earn  = earnings_missing,
    m_jobs  = as.integer(is.na(total_jobs)),
    m_hours = as.integer(is.na(hours_worked)),
    m_jss   = jss_missing
  )
print(round(cor(miss_cor), 3))

cat("\n-- Step 3: raw vs fitted, and what happens when the floors are removed --\n")
raw_diff <- mean(df$rank_global[df$earnings_missing == 1]) -
            mean(df$rank_global[df$earnings_missing == 0])
cat(sprintf("   RAW mean rank difference (missing - present) : %+.3f  (worse)\n",
            raw_diff))
#  Read from m1_v2vec, not m1: under the reported vector the term is no
#  longer in the model at all, which is the point of the median-fill rule.
cat(sprintf("   Model 1 fitted coefficient (zero-fill)       : %+.3f\n",
            coef(m1_v2vec)["earnings_missing"]))

m1_nofloor <- feols(
  rank_global ~ global_south + jss_filled + jss_missing + log_reviews +
    log_rate + earnings_missing + badge_num + years_on_platform +
    richness_index + scrape_session + gender_female,
  data = df, cluster = ~keyword
)
cat(sprintf("   Same model minus the 3 zero-filled terms     : %+.3f (t = %.2f)\n",
            coef(m1_nofloor)["earnings_missing"],
            coef(m1_nofloor)["earnings_missing"] / fixest::se(m1_nofloor)["earnings_missing"]))
cat("\n   -> If the sign flips between the last two lines, that coefficient\n")
cat("      was measuring the imputation rule, not the freelancers.\n")

################################################################################
##  6.  MODEL 1b -- KEYWORD FIXED EFFECTS  (H1, within-keyword)
##
##  Rank_(i,k) = alpha_(k) + beta1 GlobalSouth_(i) + gamma'X_(i) + epsilon_(i,k)
##
##  The same specification as Model 1 with keyword fixed effects, so the
##  geographic coefficient is identified only from freelancers competing for the
##  same search term. Thesis Section 4.3.2 (p. 38); reported alongside Model 1
##  in Table 7, col. 2 (p. 46), as a within-keyword check rather than a separate
##  test of H1. SE clustered at keyword level.
##
##  Adjusted R2 goes negative here. That is the permutation structure of the
##  outcome (Section 4.9), not a misspecification: keyword fixed effects cost
##  degrees of freedom and have almost no between-cell variance to absorb.
################################################################################

m1b <- feols(
  as.formula(paste(
    "rank_global ~ global_south +",
    paste(controls_primary, collapse = " + "),
    "| keyword"
  )),
  data    = df,
  cluster = ~keyword
)
summary(m1b)

################################################################################
##  7.  MODEL 2 -- VERIFIABILITY INTERACTION  (H2)
##
##  Rank_(i,k) = alpha + beta1 GlobalSouth_(i) + beta2 LowVerif_(k)
##             + beta3 (GlobalSouth_(i) x LowVerif_(k)) + gamma'X_(i) + eps
##
##  Thesis Section 4.3.3 (p. 38); reported in Table 8 (p. 47).
##
##  LowVerif is 1 for keywords in the low-verifiability tier and 0 for the high
##  tier. The headline specification drops the medium tier to sharpen the
##  contrast, leaving 3,654 of 3,974 observations. Keyword fixed effects cannot
##  be used: the verifiability indicator is constant within keyword and
##  therefore collinear with them, so the dummy takes their place. SE clustered
##  at keyword level.
##
##  H2 predicts beta3 > 0: a larger geographic penalty where output is harder to
##  verify, the pattern expected if the algorithm leans on group-level proxies
##  when individual signals are less informative.
################################################################################

cat("\n─────────────────────────────────────────────────────────\n")
cat("  MODEL 2 — Verifiability interaction (H2)\n")
cat("─────────────────────────────────────────────────────────\n")

df_hl <- df |> filter(verif_group %in% c("high", "low"))
cat(sprintf("  Sample (high + low verif only): %d observations\n", nrow(df_hl)))

m2 <- feols(
  make_fml("rank_global", c("global_south * low_verif")),
  data    = df_hl,
  cluster = ~keyword
)
summary(m2)

################################################################################
##  8.  MODEL 3 -- PROFILE RICHNESS INTERACTION  (H3)
##
##  Rank_(i,k) = alpha_(k) + beta1 GlobalSouth_(i) + beta2 Richness_(i)
##             + beta3 (GlobalSouth_(i) x Richness_(i)) + gamma'X_(i) + eps
##
##  Thesis Section 4.3.4 (p. 38); reported in Table 9 (p. 48).
##
##  richness_index belongs to the control vector, so it is removed from the
##  standalone control list when it enters the interaction. Keyword fixed
##  effects included; SE clustered at keyword level.
##
##  H3 predicts beta3 < 0: a richer profile narrows the geographic penalty.
################################################################################

cat("\n─────────────────────────────────────────────────────────\n")
cat("  MODEL 3 — Profile richness interaction (H3)\n")
cat("─────────────────────────────────────────────────────────\n")

controls_no_richness <- setdiff(controls_primary, "richness_index")

m3_fml <- as.formula(paste(
  "rank_global ~ global_south * richness_index +",
  paste(controls_no_richness, collapse = " + "),
  "| keyword"
))

m3 <- feols(m3_fml, data = df, cluster = ~keyword)
summary(m3)

################################################################################
##  9.  MODEL 4 -- OAXACA-BLINDER DECOMPOSITION  (H4)
##
##  Decomposes the mean rank gap into an explained component (differences in
##  observable characteristics) and an unexplained component (differences in the
##  returns to those characteristics). Thesis Section 4.3.5; reported in
##  Table 10 (p. 49) at the balanced weighting, omega = 0.5.
##
##  ORIENTATION -- read this before interpreting any sign in this section. The
##  oaxaca package decomposes group0 - group1, which here is Global North minus
##  Global South. Higher rank numbers are worse positions. A Global South
##  DISADVANTAGE therefore requires a NEGATIVE unexplained component in this
##  parameterisation, which is what H4 predicts. A positive unexplained
##  component indicates returns favouring Global South freelancers.
##
##  SE are bootstrapped. The replication counts are set immediately below,
##  ahead of the oaxaca() call, so that they take effect.
################################################################################

# -- Bootstrap replications ---------------------------------------------------
#  Set here, above the oaxaca() call and above the quantile regressions in
#  Section 10, so both read the intended value. The thesis reports 500
#  replications for the decomposition (Table 10 note, p. 49).
#
#  On the choice of R: the Monte Carlo standard error of a bootstrap p-value
#  near 0.05 at R = 500 is sqrt(0.05 * 0.95 / 500) = 0.010, large enough to move
#  a borderline result across the 0.05 line on the draw alone. Seeding makes a
#  number reproducible; it does not make it precise. The quantile regressions in
#  Section 10 therefore use a higher count.
ob_R <- 500
qr_R <- 2000

cat("\n─────────────────────────────────────────────────────────\n")
cat("  MODEL 4 — Oaxaca-Blinder decomposition (H4)\n")
cat("─────────────────────────────────────────────────────────\n")
cat(sprintf("  Bootstrap replications: OB R = %d, QR R = %d\n", ob_R, qr_R))

# gender_unknown causes NA coefficients within groups -> remove from OB controls
ob_controls <- setdiff(controls_primary, "gender_unknown")   # no-op under the reported vector

# oaxaca cannot handle NA rows — build complete-case subset
ob_vars <- c("rank_global", "global_south", ob_controls)
df_ob   <- df |>
  select(all_of(ob_vars)) |>
  drop_na()

cat(sprintf("  Complete-case sample for OB: %d observations\n", nrow(df_ob)))

ob_formula <- as.formula(paste(
  "rank_global ~",
  paste(ob_controls, collapse = " + "),
  "| global_south"
))

set.seed(86)
ob_result <- oaxaca(ob_formula, data = df_ob, R = ob_R)

cat("\n── Twofold decomposition ──\n")
print(ob_result$twofold$overall)

cat("\n── Threefold decomposition ──\n")
print(ob_result$threefold$overall)

cat("\n── Top-10 variable contributions to the explained component ──\n")
expl_vars <- ob_result$twofold$variables[[1]] |>
  as.data.frame() |>
  rownames_to_column("variable") |>
  arrange(desc(abs(`coef(explained)`))) |>
  head(10)
print(expl_vars)

cat("\n── Top-10 variable contributions to the unexplained component ──\n")
unexpl_vars <- ob_result$twofold$variables[[2]] |>
  as.data.frame() |>
  rownames_to_column("variable") |>
  arrange(desc(abs(`coef(explained)`))) |>
  head(10)
print(unexpl_vars)


# -- 9.1  Is there a gap to decompose? -----------------------------------------
#
#  This check governs how Table 10 can be read, and the thesis states its result
#  in the text at Section 5.2.4 (pp. 48-49) before presenting any component.
#
#  Oaxaca-Blinder splits an observed mean gap into an endowment part and a
#  coefficient part. The gap being split here is +0.177 rank positions in the
#  Global North minus Global South orientation (SE 0.188, Welch t = 0.940,
#  p = 0.347): it is not distinguishable from zero. Decomposing a null gap is
#  arithmetically valid but the two components must sum to approximately
#  nothing, so they offset -- a larger unexplained component is necessarily
#  accompanied by an equal and opposite explained component, and neither is
#  separately evidence of differential treatment.
#
#  The block below prints the twofold components with 95% confidence intervals
#  at every weighting, and flags whether any interval excludes zero. None does,
#  which is what the thesis reports.

cat("\n─────────────────────────────────────────────────────────\n")
cat("  H4 SANITY CHECK: is there a gap to decompose?\n")
cat("─────────────────────────────────────────────────────────\n")

cat(sprintf("\n  Total raw gap (GN - GS)     : %+.4f rank positions\n",
            -diff(rank_by_origin$mean_rank[order(rank_by_origin$global_south)])))
cat(sprintf("  Welch t / p for that gap    : t = %.3f, p = %.4f\n",
            t_res$statistic, t_res$p.value))
cat(sprintf("  SE of the raw gap           : %.4f\n",
            diff(t_res$conf.int) / (2 * 1.96)))

ob_ci <- ob_result$twofold$overall |>
  as.data.frame() |>
  transmute(
    omega       = group.weight,
    explained   = round(`coef(explained)`, 3),
    exp_lo      = round(`coef(explained)`   - 1.96 * `se(explained)`,   3),
    exp_hi      = round(`coef(explained)`   + 1.96 * `se(explained)`,   3),
    unexplained = round(`coef(unexplained)`, 3),
    unexp_lo    = round(`coef(unexplained)` - 1.96 * `se(unexplained)`, 3),
    unexp_hi    = round(`coef(unexplained)` + 1.96 * `se(unexplained)`, 3),
    unexp_sig05 = ifelse(sign(`coef(unexplained)` - 1.96 * `se(unexplained)`) ==
                         sign(`coef(unexplained)` + 1.96 * `se(unexplained)`),
                         "YES", "no")
  )
cat("\n  Twofold components with 95% CIs. Does any weighting produce an\n")
cat("  interval that excludes zero? See the last column.\n\n")
print(ob_ci)
cat("\n  -> If unexp_sig05 is 'no' in every row, H4 cannot be reported as\n")
cat("     supported. This is what thesis Section 5.2.4 (p. 49) reports.\n")


# -- 9.2  Record of the bootstrap settings actually used -----------------------
#  Printed so the replication counts in the methodology chapter can be copied
#  from the log rather than recalled. ob_R and qr_R are set in Section 9 above.
cat(sprintf("\n  Bootstrap replications used: OB R = %d, QR R = %d\n", ob_R, qr_R))
cat("  Thesis Table 10 (p. 49) reports 500 replications for the decomposition.\n")

################################################################################
##                                                                            ##
##  10.  SUPPLEMENTARY / EXPLORATORY ANALYSES                                 ##
##       BEYOND WHAT IS REPORTED IN THE SUBMITTED THESIS                      ##
##                                                                            ##
################################################################################
##
##  Nothing in this section appears in the submitted thesis -- not in the text,
##  not as a numbered table or figure, and not in Appendices A through D. It is
##  retained because it was run during the analysis and a public repository
##  should show what was examined, not only what was reported.
##
##  A reader comparing this file against the PDF should expect to find no
##  counterpart to any block below. That is intentional and is not an omission
##  from the thesis.
##
##  Contents:
##   10.1  Within-cell permutation test for H1
##   10.2  Minimum detectable effect and equivalence bounds
##   10.3  Clustering robustness sweep
##   10.4  Result-set fixed effects and top-k conditional logit
##   10.5  Percentile-rank outcome
##   10.6  Origin x scraping-session interaction
##   10.7  Verifiability interaction across all three tiers
##   10.8  Quantile regression at Q25 / Q50 / Q75
##   10.9  Multiple-testing accounting
##  10.10  Income-group ordinal specifications
##  10.11  RTI robustness covariate
##
##  Two of these warrant a note on how the thesis DOES handle the same concern,
##  so the difference is not mistaken for an oversight:
##
##   - The thesis states the H1 and H2 findings as bounds ("the upper confidence
##     limit rules out any disadvantage larger than 0.100 positions", p. 46;
##     "permits an amplification of at most 1.180 positions", p. 47). That
##     framing is in the thesis; the formal power calculation in 10.2 that
##     motivated it is not.
##   - The thesis reports the permutation STRUCTURE of the outcome (Sections
##     4.1.2 and 5.1) and uses it to explain the R2 ceiling and to discount the
##     normality diagnostic. The permutation TEST in 10.1 is not reported.
##
##  Objects defined in this section that are consumed by the supplementary
##  tables and figures in Sections 11 and 12: m1_cellfe, m_top5, m_top10,
##  m1_pct, m1b_pct, m_temporal, m_temporal_fe, m2_full, qr_coef_table,
##  qr_summaries, m_b1 through m_b7, m_c1, m_c2.
################################################################################

# -- 10.1  Within-cell permutation test for H1 ---------------------------------
#  NOT IN THE SUBMITTED THESIS.
#
#  Explored as a design-based alternative to the clustered standard errors, on
#  the reasoning that cluster-robust SEs are derived under sampling variation in
#  which each rank could in principle take any value, whereas within a result
#  set the 20 positions are handed out exactly once each. That constraint
#  removes variance the sandwich estimator still assumes is present, which makes
#  the clustered SE conservative.
#
#  Implementation holds the design matrix fixed -- preserving the panel
#  structure, the repeated profiles and every covariate value -- and permutes
#  the observed ranks within each result-set cell, refitting each time.
#
#  Not carried into the thesis for two reasons that are worth recording. The
#  sharp null being inverted is "within a result set the ordering is
#  exchangeable across profiles", which is stronger than "global_south has no
#  effect". And global_south is an attribute rather than an assigned treatment,
#  so this is a within-cell permutation test against a design-based reference
#  distribution, not randomization inference recovering an experimental
#  randomisation. The thesis reports the clustered result, which is the more
#  conservative of the two.

cat("\n─────────────────────────────────────────────────────────\n")
cat("  WITHIN-CELL PERMUTATION TEST FOR H1\n")
cat("─────────────────────────────────────────────────────────\n")

ri_B <- 2000
set.seed(20260804)

ri_data <- df |>
  select(rank_global, cell, global_south, all_of(controls_v3)) |>
  drop_na()

ri_fit  <- feols(make_fml("rank_global", "global_south", ctrl = controls_v3),
                 data = ri_data)
ri_obs  <- coef(ri_fit)["global_south"]
ri_cellidx <- split(seq_len(nrow(ri_data)), ri_data$cell)

ri_null <- numeric(ri_B)
for (b in seq_len(ri_B)) {
  y <- ri_data$rank_global
  for (ix in ri_cellidx) y[ix] <- sample(y[ix])
  tmp <- ri_data; tmp$rank_global <- y
  ri_null[b] <- coef(feols(make_fml("rank_global", "global_south",
                                    ctrl = controls_v3),
                           data = tmp, notes = FALSE))["global_south"]
  if (b %% 250 == 0) cat(sprintf("    %d / %d permutations\n", b, ri_B))
}

ri_p <- mean(abs(ri_null) >= abs(ri_obs))
cat(sprintf("\n  Observed beta(global_south)          : %+.4f\n", ri_obs))
cat(sprintf("  Permutation p-value (two-sided, B=%d): %.4f\n", ri_B, ri_p))
cat(sprintf("  sd of the permutation null           : %.4f\n", sd(ri_null)))
cat(sprintf("  Clustered SE for comparison          : %.4f\n",
            fixest::se(m1_v3)["global_south"]))
cat(sprintf("  95%% permutation null interval        : [%+.3f, %+.3f]\n",
            quantile(ri_null, 0.025), quantile(ri_null, 0.975)))
cat("\n  -> If the null sd is smaller than the clustered SE, the clustered\n")
cat("     inference was conservative and the permutation p-value is the\n")
cat("     design-appropriate one. Report both, and be explicit that this\n")
cat("     moves the H1 result AGAINST the null rather than towards it.\n")

png("figures/diagnostic_permutation_null.png", width = 1200, height = 800, res = 140)
hist(ri_null, breaks = 50, col = "steelblue", border = "white",
     main = "Within-cell permutation null for beta(Global South)",
     xlab = "beta(Global South) under the sharp null")
abline(v = ri_obs, col = "red", lwd = 2)
dev.off()
cat("  Saved: figures/diagnostic_permutation_null.png\n")


# -- 10.2  Minimum detectable effect and equivalence bounds --------------------
#  NOT IN THE SUBMITTED THESIS, though the framing it produced is.
#
#  Explored to convert the null H1 result into a bounded claim, on the reasoning
#  that a null with no power statement leaves the reader unable to tell whether
#  the study found no penalty or was unable to detect one.
#
#  The bound survived into the thesis and the calculation did not. Sections
#  5.2.1 and 5.2.2 state the finding as an upper confidence limit -- no
#  disadvantage larger than 0.100 positions, or 0.5% of a result page (p. 46) --
#  which is the ci[2] quantity computed below. The minimum-detectable-effect
#  line at 80% power is not reported anywhere in the thesis.
#
#  Reported for both control vectors so the bound can be shown to be invariant
#  to the imputation rule. m1_cellfe is estimated in Section 10.4 and its bound
#  is not computed here.

cat("\n─────────────────────────────────────────────────────────\n")
cat("  POWER: MINIMUM DETECTABLE EFFECT AND EQUIVALENCE BOUNDS\n")
cat("─────────────────────────────────────────────────────────\n")

mde_report <- function(model, label) {
  b  <- coef(model)["global_south"]
  s  <- fixest::se(model)["global_south"]
  ci <- c(b - 1.96 * s, b + 1.96 * s)
  mde <- (qnorm(0.975) + qnorm(0.80)) * s
  cat(sprintf("\n  %s\n", label))
  cat(sprintf("    beta = %+.3f   SE = %.3f   95%% CI = [%+.3f, %+.3f]\n",
              b, s, ci[1], ci[2]))
  cat(sprintf("    MDE at 80%% power / alpha = 0.05 : %.3f rank positions (%.1f%% of a 20-slot page)\n",
              mde, 100 * mde / 20))
  cat(sprintf("    Upper equivalence bound: the data rule out any Global South\n"))
  cat(sprintf("    DISADVANTAGE larger than %+.3f rank positions.\n", ci[2]))
}
#  m1 uses the reported vector; m1_v2vec the zero-fill one. Shown together so
#  the bound can be seen to be invariant to the imputation rule.
mde_report(m1,       "Model 1  (reported vector)")
mde_report(m1_v2vec, "Model 1  (zero-fill vector)")
#  (m1_cellfe is estimated later, in Section 5g; its bound is reported there.)
cat("\n  -> Write the H1 finding as a bound, not as an absence:\n")
cat("     'we can exclude a Global South ranking penalty larger than roughly\n")
cat("      one tenth of one position out of twenty', not 'no effect found'.\n")


# -- 10.3  Clustering robustness sweep -----------------------------------------
#  NOT IN THE SUBMITTED THESIS.
#
#  Explored because global_south varies at country level in a concentrated
#  sample (United States 21.5%, Pakistan 18.5%, India 11.9%), so clustering on
#  profile_url treats two Pakistani freelancers as independent draws when the
#  treatment they carry is a country attribute -- a Moulton concern.
#
#  The thesis reports only the two clustering choices actually used in the
#  specifications it presents: profile URL for Model 1, keyword for Models 1b,
#  2 and 3 (Sections 4.3.2 to 4.3.4). The sweep across country, cell and two-way
#  clustering below is additional and was not carried into the reported results.

cat("\n─────────────────────────────────────────────────────────\n")
cat("  CLUSTERING ROBUSTNESS FOR beta(Global South)\n")
cat("─────────────────────────────────────────────────────────\n")

clust_specs <- list(
  "profile_url"         = ~profile_url,
  "keyword"             = ~keyword,
  "country_wb"          = ~country_wb,
  "cell (kw x session)" = ~cell,
  "two-way profile^kw"  = ~profile_url + keyword,
  "two-way country^kw"  = ~country_wb + keyword
)
for (nm in names(clust_specs)) {
  mm <- feols(make_fml("rank_global", "global_south", ctrl = controls_v3),
              data = df, cluster = clust_specs[[nm]])
  cat(sprintf("  %-22s beta = %+.4f  SE = %.4f  p = %.3f\n",
              nm, coef(mm)["global_south"], fixest::se(mm)["global_south"],
              fixest::pvalue(mm)["global_south"]))
}
cat("\n  -> Report the most conservative of these as the headline SE.\n")


# -- 10.4  Result-set fixed effects and top-k conditional logit ----------------
#  NOT IN THE SUBMITTED THESIS.
#
#  Explored as specifications that respect the permutation structure of the
#  outcome more directly than OLS does. OLS on a forced permutation treats
#  "position 3 vs 4" and "position 19 vs 20" as the same one-unit move and
#  ignores that the 20 slots are allocated jointly within a cell.
#
#   (i) Result-set fixed effects absorb keyword x session rather than keyword,
#       which is the set within which the allocation actually happened, and
#       removes any March/June composition shift.
#  (ii) Top-k placement as a conditional logit stratified by result set asks
#       whether a profile reached the top 5 of its own result set, using the
#       cell as the choice set and making no cardinality assumption about the
#       rank scale.
#
#  A rank-ordered (Plackett-Luce) logit over each cell is the textbook model for
#  the full ordering; it is noted as the natural extension but not fitted, as it
#  requires a package outside this script's dependencies.
#
#  The thesis reports Models 1 and 1b with keyword fixed effects and does not
#  present either specification below.

cat("\n─────────────────────────────────────────────────────────\n")
cat("  ALTERNATIVE OUTCOME SPECIFICATIONS\n")
cat("─────────────────────────────────────────────────────────\n")

cat("\n-- (i) Result-set (keyword x session) fixed effects --\n")
m1_cellfe <- feols(
  make_fml("rank_global", "global_south", fe = "cell", ctrl = controls_v3),
  data = df, cluster = ~keyword
)
summary(m1_cellfe)

cat("\n-- (ii) Top-5 placement, conditional logit stratified by result set --\n")
m_top5 <- feglm(
  as.formula(paste("top5 ~ global_south +",
                   paste(controls_v3, collapse = " + "), "| cell")),
  data = df, family = binomial(), cluster = ~keyword
)
summary(m_top5)

cat("\n-- (ii-b) Top-10 placement, same design --\n")
m_top10 <- feglm(
  as.formula(paste("top10 ~ global_south +",
                   paste(controls_v3, collapse = " + "), "| cell")),
  data = df, family = binomial(), cluster = ~keyword
)
summary(m_top10)

cat("\n-- Descriptive: Global South share by position band --\n")
print(df |>
        mutate(band = cut(rank_global, c(0, 5, 10, 15, 20),
                          labels = c("1-5", "6-10", "11-15", "16-20"))) |>
        group_by(band) |>
        summarise(n = n(), gs_share = round(mean(global_south), 3),
                  .groups = "drop"))
cat(sprintf("   Overall Global South share: %.3f\n", mean(df$global_south)))
cat("   -> A flat profile across bands is itself the H1 answer, in a form a\n")
cat("      reader can check by eye and that needs no distributional assumption.\n")


# -- 10.5  Percentile-rank outcome ---------------------------------------------
#  NOT IN THE SUBMITTED THESIS.
#
#  Explored as a scale check on Models 1 and 1b, re-estimating both with the
#  within-cell percentile rank in place of the raw 1-20 position. `rank_pct` is
#  computed over keyword x scraping session, which is the result set actually
#  served, rather than pooling the two waves.
#
#  The thesis reports the raw rank throughout and does not present a
#  percentile-rank specification.

cat("\n── M2 robustness: percentile rank outcome ──\n")
m1_pct <- feols(
  as.formula(paste(
    "rank_pct ~ global_south +",
    paste(controls_primary, collapse = " + ")
  )),
  data    = df,
  cluster = ~profile_url
)
summary(m1_pct)

cat("\n── M3a robustness: percentile rank outcome ──\n")
m1b_pct <- feols(
  as.formula(paste(
    "rank_pct ~ global_south +",
    paste(controls_primary, collapse = " + "),
    "| keyword"
  )),
  data    = df,
  cluster = ~keyword
)
summary(m1b_pct)


# -- 10.6  Origin x scraping-session interaction -------------------------------
#  NOT IN THE SUBMITTED THESIS.
#
#  Explored to check whether the geographic coefficient moved between the March
#  and June waves. In the thesis scrape_session enters every specification as a
#  control (Section 4.3.1, p. 37) and is never interacted with origin.

cat("\n─────────────────────────────────────────────────────────\n")
cat("  TEMPORAL ROBUSTNESS — global_south x scrape_session\n")
cat("─────────────────────────────────────────────────────────\n")

m_temporal <- feols(
  as.formula(paste(
    "rank_global ~ global_south * scrape_session +",
    paste(setdiff(controls_primary, "scrape_session"), collapse = " + ")
  )),
  data    = df,
  cluster = ~profile_url
)
summary(m_temporal)

cat("\n── Temporal: keyword FE ──\n")
m_temporal_fe <- feols(
  as.formula(paste(
    "rank_global ~ global_south * scrape_session +",
    paste(setdiff(controls_primary, "scrape_session"), collapse = " + "),
    "| keyword"
  )),
  data    = df,
  cluster = ~keyword
)
summary(m_temporal_fe)


# -- 10.7  Verifiability interaction across all three tiers --------------------
#  NOT IN THE SUBMITTED THESIS.
#
#  Explored as a complement to Model 2, retaining the medium-verifiability tier
#  and taking the high tier as the reference. Thesis Section 4.3.3 (p. 38)
#  designates the two-tier contrast as the headline specification and drops the
#  320 medium-tier observations to sharpen it; Table 8 (p. 47) reports that
#  specification alone.

cat("\n── Model 2 (all verif groups, high-verif = reference) ──\n")

df_verif_ref_high <- df |>
  mutate(verif_factor = relevel(verif_factor, ref = "high"))

m2_full <- feols(
  make_fml("rank_global",
           c("global_south * verif_factor")),
  data    = df_verif_ref_high,
  cluster = ~keyword
)
summary(m2_full)


# -- 10.8  Quantile regression at Q25 / Q50 / Q75 ------------------------------
#  NOT IN THE SUBMITTED THESIS.
#
#  Explored to see whether the geographic coefficient varied across the rank
#  distribution rather than shifting its mean. Bootstrap SEs are computed once
#  per quantile with a seed set immediately beforehand, because summary(se =
#  "boot") draws fresh samples on every call and two unseeded calls otherwise
#  return different SEs for the same fitted model.
#
#  Three reasons this was not carried into the thesis, all of which apply to the
#  one estimate here that reaches p < 0.05:
#
#   1. Quantiles of a discrete outcome are not well separated. rank_global takes
#      20 values with roughly 200 observations at each, and the 25th percentile
#      sits on a mass point, so the quantile is not uniquely identified; rq()
#      returns one vertex of a solution set and the bootstrap wanders over it.
#   2. Bootstrap noise. The Q25 p-value has been observed to move across the
#      0.05 threshold between calls at R = 500. Seeding fixes the number without
#      making it precise.
#   3. Multiplicity. This is one estimate among roughly two dozen fitted across
#      four hypotheses, which is the expected yield under a complete null. See
#      Section 10.9.
#
#  NOTE ON DIRECTION if these estimates are ever discussed: Q25 is the TOP of
#  the rank distribution, since lower rank numbers are better positions. A
#  negative coefficient there describes a modest Global South advantage among
#  well-placed profiles, not a penalty concentrated at the bottom.

cat("\n─────────────────────────────────────────────────────────\n")
cat("  QUANTILE REGRESSION — penalty across the rank distribution\n")
cat("─────────────────────────────────────────────────────────\n")

qr_controls <- setdiff(controls_primary, "gender_unknown")   # no-op under the reported vector

qr_formula <- as.formula(paste(
  "rank_global ~ global_south +",
  paste(qr_controls, collapse = " + ")
))

taus <- c(0.25, 0.50, 0.75)

qr_models <- lapply(taus, function(tau) {
  rq(qr_formula, tau = tau, data = df, method = "fn", na.action = na.omit)
})
names(qr_models) <- paste0("Q", taus * 100)

set.seed(42)
qr_summaries <- lapply(qr_models, function(m)
  summary(m, se = "boot", R = qr_R, bsmethod = "xy"))

for (nm in names(qr_models)) {
  cat(sprintf("\n── Quantile regression at %s ──\n", nm))
  s <- qr_summaries[[nm]]
  cat(sprintf(
    "  global_south: coef = %.4f  SE = %.4f  t = %.2f  p = %.4f\n",
    s$coefficients["global_south", "Value"],
    s$coefficients["global_south", "Std. Error"],
    s$coefficients["global_south", "t value"],
    s$coefficients["global_south", "Pr(>|t|)"]
  ))
}

cat("\n── Full quantile regression table (global_south coefficient) ──\n")
qr_coef_table <- lapply(seq_along(qr_models), function(i) {
  s <- qr_summaries[[i]]
  tibble(
    quantile  = taus[i],
    coef      = s$coefficients["global_south", "Value"],
    se        = s$coefficients["global_south", "Std. Error"],
    t_stat    = s$coefficients["global_south", "t value"],
    p_value   = s$coefficients["global_south", "Pr(>|t|)"],
    ci_low    = coef - 1.96 * se,
    ci_high   = coef + 1.96 * se
  )
}) |> bind_rows()
print(qr_coef_table)


# -- 10.9  Multiple-testing accounting -----------------------------------------
#  NOT IN THE SUBMITTED THESIS.
#
#  Explored because this script fits roughly 20 feols models and 3 quantile
#  regressions across four hypotheses without pre-registration. Under a complete
#  null and 23 independent tests, the probability of at least one p < 0.05 is
#  1 - 0.95^23 = 0.69, so finding one is the expected outcome rather than a
#  discovery.
#
#  The thesis handles this by designation rather than by correction, which is
#  the first of the two options below: Section 4.3.6 names one primary
#  specification per hypothesis in advance -- Model 1 for H1, Model 2 for H2,
#  Model 3 for H3 and the balanced twofold decomposition for H4 -- and reports
#  those five as the confirmatory set. The adjusted p-values computed below are
#  not reported.
#
#  The alternative, a stepdown correction across the family, is noted as a
#  pointer: the wildrwolf package implements Romano-Wolf for fixest objects and
#  accounts for correlation between tests, making it far less punitive than
#  Bonferroni. It is not a dependency of this script.

cat("\n─────────────────────────────────────────────────────────\n")
cat("  MULTIPLE-TESTING ACCOUNTING\n")
cat("─────────────────────────────────────────────────────────\n")

primary_tests <- tribble(
  ~hypothesis, ~specification,             ~p_value,
  "H1", "Model 1, global_south",           fixest::pvalue(m1)["global_south"],
  "H2", "Model 2, GS x low_verif",         fixest::pvalue(m2)["global_south:low_verif"],
  "H3", "Model 3, GS x richness",          fixest::pvalue(m3)["global_south:richness_index"],
  "H4", "OB unexplained, omega = 0.5",
        2 * pnorm(-abs(ob_result$twofold$overall[3, "coef(unexplained)"] /
                        ob_result$twofold$overall[3, "se(unexplained)"]))
) |>
  mutate(
    p_bonferroni = pmin(1, p_value * n()),
    p_holm       = p.adjust(p_value, method = "holm"),
    p_bh_fdr     = p.adjust(p_value, method = "BH")
  )
print(primary_tests |> mutate(across(where(is.numeric), ~ round(.x, 4))))

n_tests <- 23
cat(sprintf("\n  Tests fitted in this script (approx.)          : %d\n", n_tests))
cat(sprintf("  P(at least one p < 0.05 | complete null)      : %.3f\n",
            1 - 0.95^n_tests))
cat("  -> State this in the Results chapter and designate the four primary\n")
cat("     specifications above BEFORE discussing any exploratory result.\n")


# -- 10.10  SUPPLEMENTARY -- Income-group ordinal specifications ---------------
#  NOT IN THE SUBMITTED THESIS.
#
#  This block has no relationship to the thesis's Appendix B, which documents
#  the web-scraping implementation. It was labelled "Appendix B" during
#  development and is renamed here so the correspondence is not implied.
#
#  The four-tier ordinal income variable is mentioned once in the thesis (p. 30)
#  as "retained as an additional specification". None of the seven models below
#  appears as a thesis table, and none is in Appendix A.
#
#  TWO CAVEATS ON READING THESE ESTIMATES, both of which are why the block stays
#  supplementary:
#
#   1. It is a relabelling, not a robustness check on the geographic
#      classification. In this dataset global_south is definitionally identical
#      to "not high income": the crosstab printed below is perfectly block
#      diagonal, with all 1,497 high-income observations coded Global North and
#      all 2,477 others coded Global South. A finer scale on the same partition
#      cannot corroborate the partition. A genuine robustness check would vary
#      the CUT rather than its granularity -- a continuous log GDP per capita
#      term, or a Western-core versus rest split that reclassifies high-income
#      non-Western origins such as Chile, the Gulf states and parts of Central
#      Europe.
#
#   2. The Low income cell holds n = 33 across a handful of profiles. Its
#      coefficient and its apparent position as best-ranked group are noise.
#      The thesis flags the same thing at Section 5.1 (p. 41) and in the
#      discussion of Figure 4 (p. 44), where the low-income confidence interval
#      spans more than two rank positions.
#
#  Note also the concentration: the Global North group is 57% United States and
#  the Global South group roughly 49% Pakistan and India, so the contrast is in
#  large part a US-versus-South-Asia comparison.

cat("\n─────────────────────────────────────────────────────────\n")
cat("  SUPPLEMENTARY — Income-group ordinal specifications (not in thesis)\n")
cat("─────────────────────────────────────────────────────────\n")

cat("\n-- Is global_south separately identified from income group? --\n")
print(table(df$income_group_wb, df$global_south))
cat("   (A block-diagonal table means Appendix B is a relabelling, not a\n")
cat("    robustness check. Say so rather than letting it read as corroboration.)\n")
cat(sprintf("\n   n in the Low income cell: %d  -> not interpretable, flag in every table.\n",
            sum(df$income_group_wb == "Low income")))

## B1: Ordinal (continuous) specification
m_b1 <- feols(rank_global ~ income_group_ordinal,
              data = df, cluster = ~profile_url)

m_b2 <- feols(
  make_fml("rank_global", "income_group_ordinal"),
  data = df, cluster = ~profile_url
)

m_b3 <- feols(
  as.formula(paste(
    "rank_global ~ income_group_ordinal +",
    paste(controls_primary, collapse = " + "),
    "| keyword"
  )),
  data = df, cluster = ~keyword
)

cat("\n── B1: Baseline (ordinal) ──\n"); summary(m_b1)
cat("\n── B2: Full controls (ordinal) ──\n"); summary(m_b2)
cat("\n── B3: Keyword FE (ordinal) ──\n"); summary(m_b3)

## B4: Factor specification — separate dummy for each income group
m_b4 <- feols(
  make_fml("rank_global", "income_factor"),
  data = df, cluster = ~profile_url
)
cat("\n── B4: Income group as factor (High income = reference) ──\n")
summary(m_b4)

m_b5 <- feols(
  as.formula(paste(
    "rank_global ~ income_factor +",
    paste(controls_primary, collapse = " + "),
    "| keyword"
  )),
  data = df, cluster = ~keyword
)

## B6: Verifiability interaction with ordinal income (H2 robustness)
m_b6 <- feols(
  as.formula(paste(
    "rank_global ~ income_group_ordinal * low_verif +",
    paste(controls_primary, collapse = " + ")
  )),
  data    = df_hl,
  cluster = ~keyword
)
cat("\n── B6: Verifiability interaction with ordinal income ──\n")
summary(m_b6)

## B7: Profile richness interaction with ordinal income (H3 robustness)
m_b7 <- feols(
  as.formula(paste(
    "rank_global ~ income_group_ordinal * richness_index +",
    paste(controls_no_richness, collapse = " + "),
    "| keyword"
  )),
  data = df, cluster = ~keyword
)
cat("\n── B7: Profile richness interaction with ordinal income ──\n")
summary(m_b7)


# -- 10.11  SUPPLEMENTARY -- RTI robustness covariate --------------------------
#  NOT IN THE SUBMITTED THESIS.
#
#  This block has no relationship to the thesis's Appendix C, which documents
#  the output-verifiability classification. It was labelled "Appendix C" during
#  development and is renamed here so the correspondence is not implied. The
#  distinction matters because the thesis's Appendix C contains its own
#  "Table C1" (Output-Verifiability Tier by ISCO Group, p. 78), a different
#  table entirely.
#
#  Routine Task Intensity itself is not absent from the thesis: rti_score is
#  listed in Table A4, the data-completeness table (p. 72), and RTI scores
#  appear as a column of thesis Table C1, where they inform the verifiability
#  coding. What is absent is the pair of models below, which add rti_score to
#  the Model 1 control vector as a robustness covariate. Those are not reported.
#
#  A keyword-FE variant is not estimable: rti_score is constant within keyword,
#  since each keyword maps to exactly one ISCO group, so it is perfectly
#  collinear with the fixed effects and is silently dropped. Occupational task
#  content is already absorbed by the keyword FE in Model 1b.

cat("\n─────────────────────────────────────────────────────────\n")
cat("  SUPPLEMENTARY — RTI robustness covariate (not in thesis)\n")
cat("─────────────────────────────────────────────────────────\n")

controls_rti <- c(controls_primary, "rti_score")

m_c1 <- feols(
  make_fml("rank_global", c("global_south")),
  data    = df,
  cluster = ~profile_url
)   # same as m1 — repeated for side-by-side table

m_c2 <- feols(
  as.formula(paste(
    "rank_global ~ global_south +",
    paste(controls_rti, collapse = " + ")
  )),
  data    = df,
  cluster = ~profile_url
)

#  NOTE: a "C3" specification adding rti_score under keyword fixed effects
#  is not estimable in any meaningful sense: rti_score is constant within
#  keyword (each keyword maps to exactly one ISCO group), so it is perfectly
#  collinear with the keyword FE — fixest silently drops it and the model
#  reproduces Model 1b coefficient-for-coefficient. C3 is therefore omitted;
#  occupational task content is already absorbed by the keyword FE in 3a.

cat("\n── C1: Model 1 baseline (no RTI, for comparison) ──\n")
summary(m_c1)
cat("\n── C2: Model 1 + RTI score ──\n")
summary(m_c2)

################################################################################
##  11.  REGRESSION TABLES
##
##  Saved to Word (.docx) with booktabs formatting; t-statistics in parentheses.
##  Table numbering and caption text follow the SUBMITTED THESIS, so that a file
##  in ./figures/ can be matched to a page in the PDF without translation:
##
##    table5_sample_composition.docx      Table 5   p. 41   (Section 4)
##    table6_descriptive_statistics.docx  Table 6   p. 42   (Section 4)
##    table7_h1_ols.docx                  Table 7   p. 46
##    table8_h2_verif.docx                Table 8   p. 47
##    table9_h3_richness.docx             Table 9   p. 48
##    table10_h4_oaxaca.docx              Table 10  p. 49
##    table11_hypothesis_summary.docx     Table 11  p. 50
##
##  Files prefixed supp_ have no counterpart in the thesis and are numbered in
##  an independent S-series so they cannot be confused with a thesis table.
##
##  Thesis Tables 1-4 and Appendix C's Table C1 are prose or hand-built
##  exhibits, not output of this script, and nothing here is captioned with
##  those numbers.
################################################################################

cat("\n─────────────────────────────────────────────────────────\n")
cat("  SECTION 11: REGRESSION TABLES\n")
cat("─────────────────────────────────────────────────────────\n")

# ── Shared settings ───────────────────────────────────────────────────────────

coef_labels <- c(
  "global_south"                        = "Global South",
  "income_group_ordinal"                = "Income group (ordinal)",
  "income_factorUpper middle income"    = "Upper middle income",
  "income_factorLower middle income"    = "Lower middle income",
  "income_factorLow income"             = "Low income",
  "low_verif"                           = "Low verifiability",
  "global_south:low_verif"              = "Global South x Low verif",
  "richness_index"                      = "Profile richness index",
  "global_south:richness_index"         = "Global South x Richness",
  "verif_factorlow"                     = "Low verifiability",
  "verif_factorhigh"                    = "High verifiability",
  "global_south:verif_factorlow"        = "Global South x Low verif",
  "global_south:verif_factorhigh"       = "Global South x High verif",
  "jss_filled"                          = "JSS",
  "jss_med"                             = "JSS",
  "jss_missing"                         = "JSS missing (dummy)",
  "log_total_jobs"                      = "Log(total jobs)",
  "log_total_jobs_m"                    = "Log(total jobs)",
  "log_reviews"                         = "Log(review count)",
  "log_hours"                           = "Log(hours worked)",
  "log_hours_m"                         = "Log(hours worked)",
  "log_rate"                            = "Log(hourly rate)",
  "earnings_ordinal"                    = "Earnings tier (ordinal)",
  "earnings_log10"                      = "Log10(earnings threshold)",
  "no_history_shown"                    = "No work history shown (dummy)",
  "earnings_missing"                    = "Earnings tier missing (dummy)",
  "badge_num"                           = "Badge level (1-4)",
  "years_on_platform"                   = "Years on platform",
  "scrape_session"                      = "Session (June = 1)",
  "gender_female"                       = "Female",
  "gender_unknown"                      = "Gender unknown",
  "rti_score"                           = "RTI score (O*NET 2017)"
)

gof_labels <- tribble(
  ~raw,              ~clean,          ~fmt,
  "nobs",            "Observations",  0,
  "r.squared",       "R2",            3,
  "adj.r.squared",   "R2 adjusted",   3,
  "rmse",            "RMSE",          3,
  "FE: keyword",     "Keyword FE",    0,
  "FE: cell",        "Result-set FE", 0    # keyword x session absorption
)

star_levels <- c("*" = 0.10, "**" = 0.05, "***" = 0.01)
star_note   <- "Robust t-statistics in parentheses. * p<0.10, ** p<0.05, *** p<0.01."

# ── Helper: save a modelsummary regression table to Word ─────────────────────
#
#  Applies booktabs theme, bold headers, 10pt font, then saves to .docx.

save_reg_table <- function(models, title, notes, path, ...) {
  modelsummary(
    models,
    statistic  = "statistic",    # t-statistics in parentheses (not SEs)
    fmt        = "%.3f",
    stars      = star_levels,
    coef_map   = coef_labels,
    gof_map    = gof_labels,
    title      = title,
    notes      = notes,
    output     = "flextable",
    ...
  ) |>
    flextable::theme_booktabs() |>
    flextable::bold(part = "header") |>
    flextable::bold(j = 1, part = "body") |>
    flextable::fontsize(size = 10, part = "all") |>
    flextable::autofit() |>
    flextable::save_as_docx(path = path)
  cat(sprintf("  Saved: %s\n", path))
}

## Table 7: H1 -- OLS estimates of the geographic rank penalty (p. 46) ---------
#  Two columns, matching the submitted thesis: Model 1 and Model 1b. The
#  baseline (m0) and result-set FE (m1_cellfe) specifications are output
#  separately as Supplementary Table S1; neither is a column of thesis Table 7.
save_reg_table(
  models = list(
    "(1) Full controls" = m1,
    "(1b) Keyword FE"   = m1b
  ),
  title  = "Table 7. OLS Estimates of the Geographic Rank Penalty (H1)",
  notes  = paste(
    "Dep. var.: rank_global (1 = top position, 20 = bottom).",
    "Controls are the vector described in Section 4.3.1.",
    "SE clustered at profile URL level (col 1) and keyword level (col 2).",
    star_note
  ),
  path   = "figures/table7_h1_ols.docx"
)

## Table 8: H2 -- verifiability interaction (p. 47) ----------------------------
#  One column, matching the submitted thesis: the high- and low-verifiability
#  sample. The all-tiers variant (m2_full) is output separately as
#  Supplementary Table S4.
save_reg_table(
  models = list(
    "High- and low-verifiability sample" = m2
  ),
  title  = "Table 8. Verifiability Interaction (H2)",
  notes  = paste(
    "Dep. var.: rank_global (1 = top position, 20 = bottom).",
    "Estimated on the high- and low-verifiability observations only.",
    "Low verifiability is an indicator equal to 1 for keywords in the low tier",
    "and 0 for keywords in the high tier, so the Global South coefficient is the",
    "origin gap within the high tier and the interaction is the additional gap",
    "in the low tier. Controls are the vector described in Section 4.3.1.",
    "Keyword fixed effects are collinear with the verifiability indicator and",
    "are therefore replaced by it. SE clustered at keyword level.",
    star_note
  ),
  path   = "figures/table8_h2_verif.docx"
)

## Table 9: H3 -- profile richness interaction (p. 48) -------------------------
save_reg_table(
  models = list(
    "(3) Richness interaction" = m3
  ),
  title  = "Table 9. Profile Richness Interaction (H3)",
  notes  = paste(
    "Keyword FE included. Controls included but not shown.",
    "SE clustered at keyword level.",
    star_note
  ),
  path   = "figures/table9_h3_richness.docx"
)

## Table 10: H4 -- Oaxaca-Blinder decomposition (p. 49) ------------------------
#
#  The oaxaca package does not produce a modelsummary-compatible object,
#  so we build the table manually from the twofold$overall matrix and
#  format it with flextable directly.

ob_overall <- ob_result$twofold$overall

ob_tbl <- ob_overall |>
  as.data.frame() |>
  rownames_to_column("Weighting scheme") |>
  select(
    `Weighting scheme`,
    `Explained (coef.)`    = `coef(explained)`,
    `Explained (SE)`       = `se(explained)`,
    `Unexplained (coef.)`  = `coef(unexplained)`,
    `Unexplained (SE)`     = `se(unexplained)`
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

flextable::flextable(ob_tbl) |>
  flextable::set_caption(
    caption = "Table 10. Oaxaca-Blinder Twofold Decomposition of the Geographic Rank Gap (H4)"
  ) |>
  flextable::add_footer_lines(
    values = paste(
      "Bootstrap SE, 500 replications, seed 86.",
      "Decomposed gap = mean rank (Global North) - mean rank (Global South);",
      "higher rank number = worse visibility.",
      "A positive unexplained component indicates differential returns",
      "favouring Global South freelancers (GS ranked better than their",
      "observables predict under GN returns), not a GS disadvantage.",
      star_note
    )
  ) |>
  flextable::theme_booktabs() |>
  flextable::bold(part = "header") |>
  flextable::bold(j = 1, part = "body") |>
  flextable::fontsize(size = 10, part = "all") |>
  flextable::autofit() |>
  flextable::save_as_docx(path = "figures/table10_h4_oaxaca.docx")
cat("  Saved: figures/table10_h4_oaxaca.docx\n")


## Table 11: Summary of hypothesis test outcomes (p. 50) -----------------------
#
#  Computed from the fitted model objects rather than transcribed, so that it
#  cannot drift out of agreement with Tables 7 through 10. The Outcome column is
#  derived from the p-values, not asserted.
#
#  A hand-typed summary table is the single most fragile exhibit in a thesis:
#  it is written once, early, and then silently contradicts the regression
#  tables a few pages earlier if any estimate moves. Building it from the same
#  objects that produce Tables 7-10 removes the failure mode entirely.
#
#  PREDICTED SIGNS -- these are set against the orientation of the outcome, not
#  against intuition, and the H4 row is the one that inverts:
#    H1  rank rises with worse visibility, so H1 predicts POSITIVE.
#    H2  the penalty is larger where output is hard to verify, so POSITIVE.
#    H3  richness attenuates the penalty, so NEGATIVE.
#    H4  the oaxaca package decomposes group0 - group1 = GN - GS. A Global
#        South disadvantage means GS carries the higher, worse rank number,
#        which makes GN - GS negative. H4 therefore predicts a NEGATIVE
#        unexplained component in this parameterisation.

h_get <- function(model, term) {
  c(est = unname(coef(model)[term]),
    se  = unname(fixest::se(model)[term]),
    p   = unname(fixest::pvalue(model)[term]))
}
h1 <- h_get(m1, "global_south")
h2 <- h_get(m2, "global_south:low_verif")
h3 <- h_get(m3, "global_south:richness_index")
h4 <- c(est = unname(ob_result$twofold$overall[3, "coef(unexplained)"]),
        se  = unname(ob_result$twofold$overall[3, "se(unexplained)"]))
h4["p"] <- 2 * pnorm(-abs(h4["est"] / h4["se"]))

verdict <- function(est, p, predicted_sign) {
  if (p >= 0.10)                          "Not supported"
  else if (sign(est) != predicted_sign)   "Rejected (sign opposite to prediction)"
  else if (p < 0.05)                      "Supported"
  else                                    "Weakly supported (p < 0.10)"
}
fmt <- function(v, ci = TRUE) {
  s <- sprintf("%+.3f (SE %.3f, p = %.3f)", v["est"], v["se"], v["p"])
  if (ci) s <- sprintf("%s; 95%% CI [%+.3f, %+.3f]", s,
                       v["est"] - 1.96 * v["se"], v["est"] + 1.96 * v["se"])
  s
}

tbl11_data <- tibble(
  `Hyp.` = c("H1", "H2", "H3", "H4"),
  `Statement (abridged)` = c(
    "GS profiles occupy lower rank positions than GN, ceteris paribus.",
    "The rank penalty is larger in low-verifiability categories.",
    "Profile richness attenuates the GS rank penalty.",
    "A significant share of the gap is unexplained by observables."
  ),
  #  PREDICTED SIGNS -- set against the orientation of the outcome, not against
  #  intuition. H1: rank rises with worse visibility, so H1 predicts POSITIVE.
  #  H2: the penalty is larger where output is hard to verify, so POSITIVE.
  #  H3: richness attenuates the penalty, so NEGATIVE. H4: the oaxaca package
  #  decomposes group0 - group1 = GN - GS, and a Global South disadvantage means
  #  GS carries the higher, worse rank number, which makes GN - GS negative --
  #  so H4 predicts a NEGATIVE unexplained component in this parameterisation.
  Outcome = c(
    verdict(h1["est"], h1["p"],  1),
    verdict(h2["est"], h2["p"],  1),
    verdict(h3["est"], h3["p"], -1),
    verdict(h4["est"], h4["p"], -1)
  ),
  `Key evidence` = c(
    paste("Model 1, Global South:", fmt(h1)),
    paste("Model 2, GS x Low verif:", fmt(h2)),
    paste("Model 3, GS x Richness:", fmt(h3)),
    paste("Oaxaca-Blinder unexplained (omega = 0.5):", fmt(h4))
  )
)
cat("\n── Table 11 computed from the fitted model objects ──\n")
print(tbl11_data |> select(`Hyp.`, Outcome, `Key evidence`), width = 200)
cat("\n  These are generated from the same objects as Tables 7-10, so any\n")
cat("  disagreement with those tables is a real error.\n")

flextable::flextable(tbl11_data) |>
  flextable::set_caption(caption = "Table 11. Summary of Hypothesis Test Outcomes") |>
  flextable::add_footer_lines(
    values = "* p<0.10, ** p<0.05, *** p<0.01."
  ) |>
  flextable::theme_booktabs() |>
  flextable::bold(part = "header") |>
  flextable::bold(j = 1, part = "body") |>
  flextable::fontsize(size = 10, part = "all") |>
  flextable::width(j = 2, width = 2.5) |>
  flextable::width(j = 4, width = 3.0) |>
  flextable::autofit() |>
  flextable::save_as_docx(path = "figures/table11_hypothesis_summary.docx")
cat("  Saved: figures/table11_hypothesis_summary.docx\n")

# =============================================================================
#  SUPPLEMENTARY TABLES -- NOT IN THE SUBMITTED THESIS
#  Numbered in an independent S-series so they cannot be mistaken for a thesis
#  table. Each corresponds to a block in Section 10.
# =============================================================================

## Supplementary Table S1: additional H1 specifications -------------------------
#  NOT IN THE SUBMITTED THESIS. The unconditional baseline and the result-set
#  fixed-effects specification, split out of the H1 table so that Table 7
#  reproduces the submitted thesis column for column. Both are estimated
#  elsewhere in this file (Sections 4.14 and 10.4); this block only tabulates
#  them.
save_reg_table(
  models = list(
    "(0) Baseline"       = m0,
    "(1c) Result-set FE" = m1_cellfe
  ),
  title  = "Supplementary Table S1. Additional H1 Specifications (not in the submitted thesis)",
  notes  = paste(
    "Dep. var.: rank_global (1 = top position, 20 = bottom).",
    "Col 1 is the unconditional baseline of Section 4.14; col 2 absorbs the",
    "keyword x session result set, the unit within which the twenty positions",
    "were actually allocated. Neither is a column of thesis Table 7.",
    "SE clustered at profile URL level (col 1) and keyword level (col 2).",
    star_note
  ),
  path   = "figures/supp_tableS1_h1_additional_specs.docx"
)

## Supplementary Table S2: imputation-rule sensitivity -------------------------
#  NOT IN THE SUBMITTED THESIS. The H1 estimate under the zero-fill vector
#  and under the vector the thesis reports, side by side. Answers the
#  question of whether the result depends on the imputation rule.
save_reg_table(
  models = list(
    "(1) Median fill" = m1,
    "(2) Zero fill"   = m1_v2vec
  ),
  title  = "Supplementary Table S2. Sensitivity of the H1 Estimate to the Missing-Data Rule (not in the submitted thesis)",
  notes  = paste(
    "Both columns are the Model 1 specification; they differ only in how",
    "structurally absent fields are handled. Col 2 zero-fills JSS, job counts,",
    "hours and earnings and adds separate dummies; col 1 median-fills them,",
    "uses log10 of the displayed earnings threshold, and collapses the",
    "co-occurring missingness into a single indicator.",
    "SE clustered at profile URL level.",
    star_note
  ),
  path   = "figures/supp_tableS2_imputation_sensitivity.docx"
)

## Supplementary Table S3: top-k placement ------------------------------------
#  NOT IN THE SUBMITTED THESIS. Tabulates the conditional logits from
#  Section 10.4.
save_reg_table(
  models = list(
    "Top-5 placement"  = m_top5,
    "Top-10 placement" = m_top10
  ),
  title  = "Supplementary Table S3. Top-k Placement, Conditional Logit Stratified by Result Set (not in the submitted thesis)",
  notes  = paste(
    "Dep. var.: indicator for reaching the top 5 / top 10 of the result set.",
    "Result-set (keyword x session) fixed effects; the cell is the choice set",
    "within which the algorithm allocated positions.",
    "Coefficients are log-odds. SE clustered at keyword level.",
    star_note
  ),
  path   = "figures/supp_tableS3_topk_logit.docx"
)

## Supplementary Table S4: verifiability interaction, all three tiers -----------
#  NOT IN THE SUBMITTED THESIS. Split out of the H2 table so that Table 8
#  reproduces the submitted thesis. Estimated in Section 10.7.
save_reg_table(
  models = list(
    "(2+) All verifiability tiers" = m2_full
  ),
  title  = "Supplementary Table S4. Verifiability Interaction Across All Three Tiers (not in the submitted thesis)",
  notes  = paste(
    "All observations retained, high verifiability = reference category.",
    "Thesis Table 8 reports the two-tier contrast only, per Section 4.3.3.",
    "Controls included but not shown. SE clustered at keyword level.",
    star_note
  ),
  path   = "figures/supp_tableS4_verif_all_tiers.docx"
)

## Supplementary Table S5: quantile regression ---------------------------------
#  NOT IN THE SUBMITTED THESIS. Tabulates the quantile regressions from
#  Section 10.8. See that block on why these estimates are not reported.
tblS5_data <- qr_coef_table |>
  mutate(
    Quantile      = paste0("Q", round(quantile * 100)),
    Coefficient   = round(coef,    3),
    `Std. Error`  = round(se,      3),
    `t-stat`      = round(t_stat,  3),
    `p-value`     = round(p_value, 3),
    `95% CI`      = sprintf("[%.3f, %.3f]", ci_low, ci_high),
    Sig           = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE           ~ ""
    )
  ) |>
  select(Quantile, Coefficient, Sig, `Std. Error`, `t-stat`, `p-value`, `95% CI`)

flextable::flextable(tblS5_data) |>
  flextable::set_caption(
    caption = "Supplementary Table S5. Quantile Regression: Geographic Penalty Across the Rank Distribution (not in the submitted thesis)"
  ) |>
  flextable::add_footer_lines(
    values = paste(
      "Coefficient on Global South at each quantile of rank_global.",
      "Bootstrap SE (R = 500, xy-pair method).",
      "Full control vector included; other coefficients not shown.",
      star_note
    )
  ) |>
  flextable::theme_booktabs() |>
  flextable::bold(part = "header") |>
  flextable::bold(j = 1, part = "body") |>
  flextable::fontsize(size = 10, part = "all") |>
  flextable::autofit() |>
  flextable::save_as_docx(path = "figures/supp_tableS5_quantile_regression.docx")
cat("  Saved: figures/supp_tableS5_quantile_regression.docx\n")

## Supplementary Table S6: income-group ordinal specifications -----------------
#  NOT IN THE SUBMITTED THESIS, and unrelated to the thesis Appendix B
#  (Web-Scraping Implementation). Tabulates Section 10.10.
save_reg_table(
  models = list(
    "(B1) Baseline"    = m_b1,
    "(B2) Controls"    = m_b2,
    "(B3) Keyword FE"  = m_b3,
    "(B4) Factor"      = m_b4,
    "(B5) Factor + FE" = m_b5
  ),
  title  = "Supplementary Table S6. Income-Group Ordinal Specifications (not in the submitted thesis)",
  notes  = paste(
    "Dep. var.: rank_global. B1-B3: income_group_ordinal (continuous).",
    "B4-B5: income group as factor dummies (High income = reference).",
    "SE clustered at profile URL level (B1-B2, B4) and keyword level (B3, B5).",
    star_note
  ),
  path   = "figures/supp_tableS6_income_ordinal.docx"
)

## Supplementary Table S7: RTI robustness covariate ----------------------------
#  NOT IN THE SUBMITTED THESIS, and unrelated to the thesis Appendix C
#  (Output-Verifiability Classification), which contains its own Table C1 on
#  a different subject. Tabulates Section 10.11.
save_reg_table(
  models = list(
    "(1) No RTI"    = m_c1,
    "(C2) With RTI" = m_c2
  ),
  title  = "Supplementary Table S7. RTI Score as Additional Control (not in the submitted thesis)",
  notes  = paste(
    "RTI = O*NET 2017 Routine Task Intensity (Lewandowski et al., 2022).",
    "SE clustered at profile URL level.",
    "A keyword-FE variant (former C3) is omitted: rti_score is constant",
    "within keyword and perfectly collinear with the fixed effects.",
    star_note
  ),
  path   = "figures/supp_tableS7_rti_covariate.docx"
)

cat("\n── All tables saved to figures/ ──\n")

################################################################################
##  12.  FIGURES
##
##  Numbering follows the SUBMITTED THESIS. The thesis contains five numbered
##  figures; three are produced here and one in Section 4:
##
##    fig2_correlation_heatmap.png   Figure 2   p. 42   (produced in Section 4)
##    fig3_rank_distribution.png     Figure 3   p. 43
##    fig4_rank_by_income.png        Figure 4   p. 44
##    fig5_verif_gap.png             Figure 5   p. 45
##
##  Figure 1 (Conceptual Model, p. 25) is not produced by this script.
##
##  Files prefixed diagnostic_ or supp_ are NOT numbered figures in the thesis
##  and are deliberately kept out of the Figure 1-5 sequence.
##
##  SIGN CONVENTION for every figure below: rank_global runs 1-20 with LOWER =
##  BETTER position, so a negative coefficient on global_south is a Global South
##  advantage, not a penalty. Each caption states the direction in words. Two
##  conventions are in use and both are labelled explicitly where they appear:
##  the Oaxaca figure plots GN - GS, the verifiability figure plots GS - GN.
################################################################################

cat("\n─────────────────────────────────────────────────────────\n")
cat("  SECTION 12: FIGURES\n")
cat("─────────────────────────────────────────────────────────\n")

theme_thesis <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(size = 13, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 10, colour = "grey40", hjust = 0),
    plot.caption  = element_text(size = 9,  colour = "grey50", hjust = 0),
    axis.title    = element_text(size = 11),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

## Figure 3: Rank distribution by geographic origin (p. 43)
p1 <- df |>
  mutate(origin = factor(global_south, labels = c("Global North", "Global South"))) |>
  ggplot(aes(x = rank_global, fill = origin)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.80, colour = "white") +
  scale_fill_manual(values = c("Global North" = "#1565C0",
                               "Global South" = "#C62828"),
                    name = "") +
  scale_x_continuous(breaks = seq(1, 20, 2)) +
  labs(
    title    = "Figure 3. Distribution of Search Rank Positions by Geographic Origin",
    subtitle = "Positions 1-20; lower values indicate greater visibility",
    x        = "Search rank position (1 = top)",
    y        = "Count",
    caption  = "N = 3,974 profile-keyword-session observations."
  ) +
  theme_thesis
ggsave("figures/fig3_rank_distribution.png", p1,
       width = 9, height = 5, dpi = 300)
cat("  Saved: figures/fig3_rank_distribution.png\n")

## Figure 4: Mean rank by income group, with 95% CI (p. 44)
p2 <- rank_by_income |>
  mutate(
    se_mean   = sd_rank / sqrt(n),
    income_f  = fct_reorder(income_group_wb, mean_rank)
  ) |>
  ggplot(aes(x = income_f, y = mean_rank, fill = income_f)) +
  geom_col(alpha = 0.85, colour = "white") +
  geom_errorbar(aes(ymin = mean_rank - 1.96 * se_mean,
                    ymax = mean_rank + 1.96 * se_mean),
                width = 0.25, colour = "grey30") +
  geom_text(aes(label = sprintf("%.2f", mean_rank), y = mean_rank + 0.3),
            size = 3.5) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1, guide = "none") +
  labs(
    title    = "Figure 4. Mean Search Rank Position by World Bank Income Group",
    subtitle = "Lower rank number = better visibility",
    x        = "Income group",
    y        = "Mean rank position",
    caption  = "Error bars = +/-1.96 x SE."
  ) +
  theme_thesis +
  theme(legend.position = "none")
ggsave("figures/fig4_rank_by_income.png", p2,
       width = 8, height = 5, dpi = 300)
cat("  Saved: figures/fig4_rank_by_income.png\n")

## Figure 5: Raw geographic rank gap by verifiability group (p. 45)
verif_gap_df <- df |>
  group_by(verif_group, global_south) |>
  summarise(
    mean_rank = mean(rank_global),
    var_rank  = var(rank_global),
    n         = n(),
    .groups   = "drop"
  ) |>
  pivot_wider(names_from  = global_south,
              values_from = c(mean_rank, var_rank, n)) |>
  mutate(
    gap = mean_rank_1 - mean_rank_0,
    se  = sqrt(var_rank_1 / n_1 + var_rank_0 / n_0),
    verif_label = factor(verif_group,
                         levels = c("high", "medium", "low"),
                         labels = c("High\nverifiability",
                                    "Medium\nverifiability",
                                    "Low\nverifiability"))
  )

p5 <- ggplot(verif_gap_df, aes(x = verif_label, y = gap, fill = verif_label)) +
  geom_col(alpha = 0.85, colour = "white") +
  geom_errorbar(aes(ymin = gap - 1.96 * se, ymax = gap + 1.96 * se),
                width = 0.25, colour = "grey30") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_fill_manual(values = c("#1B5E20", "#F57F17", "#B71C1C"), guide = "none") +
  labs(
    title    = "Figure 5. Raw Geographic Rank Gap by Verifiability Category (H2)",
    subtitle = "Gap = mean rank (GS) - mean rank (GN); positive = GS disadvantaged",
    x        = "Verifiability category",
    y        = "Mean rank gap (positions)",
    caption  = "Unadjusted means. Error bars = +/-1.96 x SE."
  ) +
  theme_thesis +
  theme(legend.position = "none")
ggsave("figures/fig5_verif_gap.png", p5,
       width = 7, height = 5, dpi = 300)
cat("  Saved: figures/fig5_verif_gap.png\n")


## Supplementary and diagnostic figures ----------------------------------------
#  NOT NUMBERED FIGURES IN THE THESIS. Captions below are deliberately
#  unnumbered so they cannot be read as belonging to the Figure 1-5 sequence.

## Supplementary figure: coefficient plot — geographic penalty across H1 models
#  NOT A NUMBERED FIGURE IN THE THESIS.
p3 <- modelplot(
  list("(0) Baseline"      = m0,
       "(1) Full controls" = m1,
       "(1b) Keyword FE"   = m1b),
  coef_map    = c("global_south" = "Global South"),
  conf_level  = 0.95,
  color       = c("#0D47A1", "#1565C0", "#42A5F5")
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(
    title   = "Geographic Rank Penalty Across Model Specifications (H1)",
    x       = "Coefficient on Global South (rank positions higher, 95% CI)",
    y       = "Model",
    caption = "Positive coefficient: GS freelancers ranked lower (worse visibility)."
  ) +
  theme_thesis
ggsave("figures/supp_fig_h1_coefficient_plot.png", p3,
       width = 8, height = 4, dpi = 300)
cat("  Saved: figures/supp_fig_h1_coefficient_plot.png\n")

## Supplementary figure: Oaxaca-Blinder decomposition bar chart
#  NOT A NUMBERED FIGURE IN THE THESIS. Thesis Table 10 (p. 49) reports the
#  decomposition in tabular form.
ob_rows <- ob_result$twofold$overall
# Row 3 = omega 0.5 (balanced weighting scheme)
coef_exp   <- ob_rows[3, "coef(explained)"]
se_exp     <- ob_rows[3, "se(explained)"]
coef_unexp <- ob_rows[3, "coef(unexplained)"]
se_unexp   <- ob_rows[3, "se(unexplained)"]

#  SE OF THE TOTAL GAP. The independence formula sqrt(se_exp^2 +
#  se_unexp^2) assumes the two components are independent. They are
#  not: they are constrained to sum to the observed gap, so they are strongly
#  NEGATIVELY correlated, and the independence formula overstates the error
#  bar substantially. The total gap is just the raw difference in means, whose
#  SE is already known exactly from the Welch test in Section 3.7 (0.188).
#  Use that instead of manufacturing one.
#
#  SIGN CONVENTION. This figure defines the gap as GN - GS while thesis
#  Figure 5 defines it as GS - GN, and both are labelled "gap".
#  Whichever is chosen, use it EVERYWHERE. The least error-prone option for a
#  reader is to abandon "gap in rank" entirely and plot visibility
#  (21 - rank), so that up always means better; failing that, put the
#  direction in the axis title of every single figure, not only the subtitle.
ob_total_se <- diff(t_res$conf.int) / (2 * 1.96)   # exact SE of the raw gap

ob_plot_df <- tibble(
  component = c("Total gap\n(GN - GS)",
                "Explained\n(endowments)",
                "Unexplained\n(coefficients)"),
  coef = c(coef_exp + coef_unexp, coef_exp, coef_unexp),
  se   = c(ob_total_se, se_exp, se_unexp)
) |>
  mutate(component = factor(component, levels = component))

cat(sprintf("\n  Decomposition figure, total-gap SE: %.4f (exact, from the Welch test)\n",
            ob_total_se))
cat(sprintf("       The independence formula sqrt(se_exp^2 + se_unexp^2) = %.4f assumes\n",
            sqrt(se_exp^2 + se_unexp^2)))
cat("       independence between two components that must sum to a fixed total.\n")

p4 <- ggplot(ob_plot_df, aes(x = component, y = coef, fill = component)) +
  geom_col(alpha = 0.85, colour = "white") +
  geom_errorbar(
    aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se),
    width = 0.25, colour = "grey30"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_text(aes(label = sprintf("%.3f", coef),
                y     = coef + sign(coef) * 0.15),
            size = 3.5) +
  scale_fill_manual(
    values = c("grey50", "#1565C0", "#C62828"),
    guide  = "none"
  ) +
  labs(
    title    = "Oaxaca-Blinder Decomposition of the Geographic Rank Gap (H4)",
    subtitle = "Gap = mean rank (GN) - mean rank (GS); lower rank number = better position, so a positive bar means Global South ranked better",
    x        = "",
    y        = "Rank positions",
    caption  = paste0("Bootstrap SE (R = 500). Balanced twofold weighting (omega = 0.5).",
                      "\nPositive unexplained component = differential returns favouring",
                      " GS freelancers, unexplained by observables.")
  ) +
  theme_thesis +
  theme(legend.position = "none")
ggsave("figures/supp_fig_oaxaca_decomposition.png", p4,
       width = 8, height = 5, dpi = 300)
cat("  Saved: figures/supp_fig_oaxaca_decomposition.png\n")

## Supplementary figure: quantile regression — penalty across rank distribution
#  NOT A NUMBERED FIGURE IN THE THESIS. Q25 is the TOP of the rank
#  distribution, since lower rank numbers are better positions.
p6 <- qr_coef_table |>
  mutate(quantile_label = paste0("Q", round(quantile * 100))) |>
  ggplot(aes(x = quantile_label, y = coef)) +
  geom_point(size = 4, colour = "#1565C0") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.15, colour = "#1565C0") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(
    title    = "Quantile Regression: Geographic Penalty Across Rank Distribution",
    subtitle = "Coefficient on Global South at Q25, Q50, Q75 of rank_global",
    x        = "Quantile of search rank distribution",
    y        = "Coefficient on Global South (rank positions)",
    caption  = "Error bars = +/-1.96 x bootstrap SE (R = 500, xy-pair method)."
  ) +
  theme_thesis
ggsave("figures/supp_fig_quantile_regression.png", p6,
       width = 7, height = 5, dpi = 300)
cat("  Saved: figures/supp_fig_quantile_regression.png\n")

## Supplementary figure: profile richness x geographic origin (H3)
#  NOT A NUMBERED FIGURE IN THE THESIS.
#  richness_index is now an integer count taking only the values 0-3, so the
#  prediction grid is those four points rather than a 50-point continuum
#  across the 5th-95th percentile range (which would imply fractional
#  disclosure elements that cannot occur).
richness_seq <- sort(unique(df$richness_index))

#  The grid is built from controls_no_richness itself rather than from a
#  hardcoded list of names, so it cannot break when the control vector
#  changes. Every control is held at its sample mean
#  (median for the discrete ones), whatever the vector happens to contain.
#  Dummies are held at 0 rather than at their means so the plotted profile is
#  an actual freelancer configuration rather than a fractional average of one.
hold_at <- function(v) {
  x <- df[[v]]
  if (all(x %in% c(0, 1), na.rm = TRUE)) return(0L)         # dummies -> 0
  if (is.integer(x) || all(x == round(x), na.rm = TRUE))
    return(median(x, na.rm = TRUE))                          # counts -> median
  mean(x, na.rm = TRUE)                                      # continuous -> mean
}

pred_df <- expand_grid(
  richness_index = richness_seq,
  global_south   = c(0L, 1L)
)
for (v in controls_no_richness) pred_df[[v]] <- hold_at(v)
pred_df$keyword <- df$keyword[1]   # placeholder for FE prediction

cat("\n  Richness marginal-effects grid holds these controls fixed:\n")
print(tibble(control = controls_no_richness,
             held_at = map_dbl(controls_no_richness, ~ as.numeric(hold_at(.x)))) |>
        mutate(held_at = round(held_at, 3)))

# Estimate version without FE for marginal effect illustration only.
m3_no_fe <- feols(
  as.formula(paste(
    "rank_global ~ global_south * richness_index +",
    paste(controls_no_richness, collapse = " + ")
  )),
  data = df, cluster = ~keyword
)

pred_df$predicted <- predict(m3_no_fe, newdata = pred_df)

p7 <- pred_df |>
  mutate(origin = factor(global_south, labels = c("Global North", "Global South"))) |>
  ggplot(aes(x = richness_index, y = predicted, colour = origin, fill = origin)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("Global North" = "#1565C0",
                                 "Global South" = "#C62828"), name = "") +
  scale_fill_manual(values   = c("Global North" = "#1565C0",
                                 "Global South" = "#C62828"), name = "") +
  labs(
    title    = "Marginal Effect of Profile Richness by Geographic Origin (H3)",
    subtitle = "Predicted rank_global; other variables held at sample means",
    x        = "Profile richness index",
    y        = "Predicted search rank position (lower = better)",
    caption  = paste("From Model 3 (no FE variant) for comparability of predictions.",
                     "\nLower rank number = better position. The interaction is positive,",
                     "\nso the Global South line rises with disclosure: richness narrows a",
                     "\nGlobal South advantage rather than a penalty.")
  ) +
  theme_thesis
ggsave("figures/supp_fig_richness_marginal_effects.png", p7,
       width = 8, height = 5, dpi = 300)
cat("  Saved: figures/supp_fig_richness_marginal_effects.png\n")


# =============================================================================
#  CLOSING NOTE -- THE LIMITATION NO SPECIFICATION IN THIS FILE CAN REPAIR
#  Corresponds to the discussion of the sampling frame in the thesis.
# =============================================================================
#
#  SELECTION ON THE OUTCOME. The sample is the top 20 results per keyword per
#  session, so every freelancer in it has already cleared the platform's
#  inclusion threshold. Every model in this file therefore estimates a quantity
#  conditional on inclusion: given that a profile reached the first two pages,
#  does origin predict its position within them? If the algorithm disadvantages
#  Global South freelancers by keeping them OUT of the top 20 rather than by
#  ordering them lower once inside, this design is structurally blind to it --
#  and the exclusion margin is the more plausible location for such an effect,
#  because it is where the bulk of any visibility loss would occur. The thesis
#  conditions every estimate on this frame explicitly (Section 5.2, p. 45).
#
#  WHY IT BEARS ON THE SIGN OF THE ESTIMATES. The results lean weakly and
#  consistently towards a Global South advantage. Read naively that says the
#  platform favours Global South freelancers. Read against the selection
#  structure it says something different and more defensible: if Global South
#  profiles face a higher bar to enter the top 20, then those observed inside it
#  are drawn from further out in the right tail of their group's unobserved
#  quality distribution than their Global North counterparts are. Better
#  unobservables inside the sample produce exactly the small negative
#  coefficient found here. A within-top-20 advantage is the arithmetic signature
#  of a stricter inclusion threshold, not evidence against discrimination.
#
#  That reading cannot be tested on these data. It is an interpretation the
#  design permits but cannot adjudicate, and it is presented as such. What can
#  be stated firmly is the bound: conditional on appearing in the top 20, Global
#  South origin carries no ranking penalty larger than about a tenth of a
#  position out of twenty (thesis Section 5.2.1, p. 46).
#
#  WHAT WOULD SETTLE IT, if a further collection wave becomes possible:
#    - Scrape deeper (top 100+) and model P(reach the top 20) directly.
#    - Record the total result count per keyword, and per keyword x country
#      using the platform's location filter. That supplies the denominator, so
#      the observed top-20 country composition can be tested against the pool
#      composition. This is the cleanest available audit statistic and it
#      targets the margin that matters.
#    - Flag sponsored and Boosted placements and the availability badge, which
#      are paid or activity-driven positions currently pooled with organic ones.
#    - Vary the client-side vantage point (location, logged-in state) to measure
#      how much of the ordering is personalisation.
#
#  FRAMING. This is an observational audit, not an experiment. The origin signal
#  was never randomised, so algorithmic discrimination is not identified and the
#  coefficient is a conditional association. The contribution is a bounded null
#  on the ordering margin together with a documented measurement framework.
#
#  MEASUREMENT CAVEATS carried in the thesis limitations and repeated here
#  because they bear on how the control vector should be read:
#    - Gender is inferred from first names with no reported accuracy. The field
#      contains zero unknowns across all 3,974 rows, which is not plausible for
#      a sample that is 18.5% Pakistan, 11.9% India and 4.5% Nigeria. Name-based
#      inference is least accurate precisely for the South Asian and West
#      African names that make up most of the Global South group, so the
#      measurement error is correlated with the treatment. gender_female is
#      retained as an inert control (coefficient ~ -0.08); gender_unknown is
#      dropped from the vector rather than left to be auto-removed for
#      collinearity, so that the notice does not mask the data-integrity issue.
#    - Only the top 20 positions are observed, per the note above.
# =============================================================================


# -- DONE ---------------------------------------------------------------------

cat("\n=============================================================\n")
cat("  Analysis complete. Output written to ./figures/\n")
cat("\n  THESIS TABLES\n")
cat("    table5_sample_composition.docx       Table 5   p. 41\n")
cat("    table6_descriptive_statistics.docx   Table 6   p. 42\n")
cat("    table7_h1_ols.docx                   Table 7   p. 46\n")
cat("    table8_h2_verif.docx                 Table 8   p. 47\n")
cat("    table9_h3_richness.docx              Table 9   p. 48\n")
cat("    table10_h4_oaxaca.docx               Table 10  p. 49\n")
cat("    table11_hypothesis_summary.docx      Table 11  p. 50\n")
cat("\n  THESIS FIGURES\n")
cat("    fig2_correlation_heatmap.png         Figure 2  p. 42\n")
cat("    fig3_rank_distribution.png           Figure 3  p. 43\n")
cat("    fig4_rank_by_income.png              Figure 4  p. 44\n")
cat("    fig5_verif_gap.png                   Figure 5  p. 45\n")
cat("\n  DIAGNOSTIC IMAGES (not numbered figures in the thesis)\n")
cat("    diagnostic_assumption_checks.png\n")
cat("    diagnostic_permutation_null.png\n")
cat("\n  SUPPLEMENTARY (not in the submitted thesis)\n")
cat("    supp_tableS1_h1_additional_specs.docx\n")
cat("    supp_tableS2_imputation_sensitivity.docx\n")
cat("    supp_tableS3_topk_logit.docx\n")
cat("    supp_tableS4_verif_all_tiers.docx\n")
cat("    supp_tableS5_quantile_regression.docx\n")
cat("    supp_tableS6_income_ordinal.docx\n")
cat("    supp_tableS7_rti_covariate.docx\n")
cat("    supp_fig_h1_coefficient_plot.png\n")
cat("    supp_fig_oaxaca_decomposition.png\n")
cat("    supp_fig_quantile_regression.png\n")
cat("    supp_fig_richness_marginal_effects.png\n")
cat("=============================================================\n")
