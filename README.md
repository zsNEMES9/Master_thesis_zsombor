# Borderless by Design? Geographic Origin and Algorithmic Ranking in Online Labor Markets

Data collection and analysis code for the master's thesis of the same title.

**Author:** Zsombor Nemes
**Institution:** Rotterdam School of Management, Erasmus University
**Submitted:** 15 August 2026

---

## What this study does

The study tests whether a freelancer's geographic origin predicts their position in Upwork talent-search results. Search result pages were collected for 92 occupational keywords in two waves (March and June 2026), yielding 3,974 profile–keyword–session observations across the top 20 positions of each result set.

The outcome, `rank_global`, is the position a profile holds within a top-20 result set. **Lower numbers are better placements**, so a negative coefficient on Global South origin indicates an advantage, not a penalty.

Four hypotheses are tested against five specifications:

| Model | Object | Hypothesis | Reported as |
|---|---|---|---|
| Model 1 | `m1` | H1 — direct effect of origin | Table 7, col. 1 |
| Model 1b | `m1b` | H1 — within-keyword check | Table 7, col. 2 |
| Model 2 | `m2` | H2 — verifiability interaction | Table 8 |
| Model 3 | `m3` | H3 — profile richness interaction | Table 9 |
| Model 4 | `ob_result` | H4 — Oaxaca-Blinder decomposition | Table 10 |

---

## Repository contents

### Data collection (Python)

| File | Purpose |
|---|---|
| `Upwork_scraper_final.py` | Phase 1 scraper — search result cards |
| `upwork_scraper_finalv2.py` | Revised Phase 1 scraper used for the second wave |
| `Phase2_scraper.py` | Phase 2 scraper — individual profile pages |
| `merge_datasets.py` | Merges Phase 1 and Phase 2 output into the pooled dataset |

### Data preparation

| File | Purpose |
|---|---|
| `add_global_south.py` | Adds World Bank income group and the Global South indicator |
| `add_verifiability.py` | Applies the output-verifiability classification to the pooled data |
| `ilo_data.r` | ILO / ISCO-08 occupational data used for keyword classification |
| `genderize_namsor.R` | Resolves gender labels from first names via the NamSor API |

#### A note on the verifiability classification

The output-verifiability tiers were coded **by hand**, not derived by the script.
Each of the 100 search keywords was scored by the author against the two-step
test set out in Appendix C.2 of the thesis:

- **Step 1** — does an external specification exist that is the primary and
  disqualifying axis of the deliverable's quality?
- **Step 2** — does relational, cultural or aesthetic judgement also
  substantially drive the verdict?

with the mechanical rule (1,0) → High, (0,1) → Low, (1,1) → Medium.

`add_verifiability.py` contains no classification logic. It reads the completed
coding from the classification workbook, applies it to the pooled observations,
and refuses to run if the coding is internally inconsistent — it checks that
every keyword is present and unique, that the scoring rule reproduces the stated
tier for all 100 keywords, and that observation counts reconcile.

The full coding, with each keyword's Step 1 and Step 2 scores and both the
keyword-level and ISCO-group-level tiers, is printed in **Table C2** of the
thesis (pp. 79–81), so the classification can be checked independently of this
repository.

### Analysis (R)

| File | Purpose |
|---|---|
| `MasterThesisCode_submission.R` | Full analysis — descriptives, five reported models, supplementary analyses, all tables and figures |

### Configuration

The scrapers write to paths defined near the top of each file. These are set to
relative defaults and will almost certainly need changing for your setup:

| Setting | File | Purpose |
|---|---|---|
| `THESIS_DIR` | `Upwork_scraper_final.py`, `upwork_scraper_finalv2.py`, `Phase2_scraper.py` | Output directory for scraped pages |
| `--user-data-dir` | same three files | Chrome profile directory, kept persistent so cookies and session state carry across runs |

`ilo_data.r` no longer calls `setwd()`; set your working directory before sourcing it.

### Pipeline order

```
Upwork_scraper_final.py / upwork_scraper_finalv2.py   →  search cards
Phase2_scraper.py                                     →  profile pages
merge_datasets.py                                     →  pooled dataset
add_global_south.py, add_verifiability.py, ilo_data.r →  classification variables
genderize_namsor.R                                    →  gender labels
MasterThesisCode_submission.R                         →  analysis, tables, figures
```

---

## Data availability

**The analysis dataset is not included in this repository.**

`upwork_pooled.csv` contains freelancer first names and countries collected from public Upwork profiles. Although profile URLs are SHA-256 hashed at load time, those two fields together remain potentially identifying, so the dataset is withheld rather than published.

The collection instrument is fully documented here and in Appendix B of the thesis, so the dataset can be regenerated. Note that search rankings change continuously; a fresh collection will not reproduce these exact figures.

**The classification workbook is not included either.** `add_verifiability.py`
reads `ISCO_Verifiability_Classification_v5.xlsx` and will stop with an error if
it is absent, so that step of the pipeline cannot be re-run from this repository
alone. The workbook holds no information beyond what Table C2 of the thesis
already prints in full — every keyword, both step scores and both tiers — so it
is omitted rather than duplicated.

For access to the analysis dataset or the classification workbook for
verification purposes, contact the author.

---

## Reproducing the analysis

### Requirements

R 4.x with the following packages:

```r
install.packages(c(
  "tidyverse", "fixest", "sandwich", "lmtest", "oaxaca", "quantreg",
  "modelsummary", "scales", "gt", "flextable", "car", "digest"
))
```

### Running it

1. Place `upwork_pooled.csv` in the same directory as `MasterThesisCode_submission.R`.
2. Set R's working directory to that folder.
3. Run:

   ```r
   source("MasterThesisCode_submission.R")
   ```

All output is written to `./figures/` — 7 thesis tables, 4 thesis figures, 2 diagnostic images and 11 supplementary files.

### Reproducibility note

Three results are stochastic and seeded: the within-cell permutation test (`set.seed(20260804)`), the Oaxaca-Blinder bootstrap (`set.seed(86)`, 500 replications) and the quantile-regression bootstrap (`set.seed(42)`, 2,000 replications). These reproduce exactly against the same R and package versions. R 3.6 changed the default `sample()` algorithm, so a different R major version will shift all three.

---

## API key setup

`genderize_namsor.R` uses the [NamSor](https://namsor.app/) API to infer gender from first names.

**Most users will never need a key.** The script checks whether any labels are unresolved and exits without calling the API if there are none, which is the case for the completed dataset.

If you do need it, the key is read from the environment, never from the source:

```bash
cp .Renviron.example .Renviron
# edit .Renviron and add your key
# restart R
```

`.Renviron` is git-ignored. Never commit it.

---

## Output map

Generated files correspond to the thesis as follows.

### Thesis tables

| File | Thesis | Page |
|---|---|---|
| `table5_sample_composition.docx` | Table 5 — Sample Composition by Income Group and Geographic Origin | 41 |
| `table6_descriptive_statistics.docx` | Table 6 — Descriptive Statistics | 42 |
| `table7_h1_ols.docx` | Table 7 — OLS Estimates of the Geographic Rank Penalty (H1) | 46 |
| `table8_h2_verif.docx` | Table 8 — Verifiability Interaction (H2) | 47 |
| `table9_h3_richness.docx` | Table 9 — Profile Richness Interaction (H3) | 48 |
| `table10_h4_oaxaca.docx` | Table 10 — Oaxaca-Blinder Twofold Decomposition (H4) | 49 |
| `table11_hypothesis_summary.docx` | Table 11 — Summary of Hypothesis Test Outcomes | 50 |

### Thesis figures

| File | Thesis | Page |
|---|---|---|
| `fig2_correlation_heatmap.png` | Figure 2 — Pairwise Correlation Heatmap | 42 |
| `fig3_rank_distribution.png` | Figure 3 — Rank Distribution by Geographic Origin | 43 |
| `fig4_rank_by_income.png` | Figure 4 — Mean Rank by World Bank Income Group | 44 |
| `fig5_verif_gap.png` | Figure 5 — Raw Geographic Rank Gap by Verifiability Category | 45 |

Figure 1 (Conceptual Model) is not produced by this script.

### Not in the thesis

Files prefixed `supp_` or `diagnostic_` have no counterpart in the submitted thesis. They are retained so the repository shows what was examined, not only what was reported. Section 10 of the analysis script contains the corresponding code, and each block states in its header that it is not reported.

---

## Ethics and collection

Data were collected from publicly visible Upwork search results and profile pages. No account credentials, private data or paid proxy services were used. Profile URLs are hashed before analysis. Collection methodology, request pacing and failure handling are documented in Appendix B of the thesis.

---

## Citation

```
Nemes, Z. (2026). Borderless by Design? Geographic Origin and Algorithmic
Ranking in Online Labor Markets. Master's thesis, Rotterdam School of
Management, Erasmus University.
```

## Licence

MIT — see `LICENSE`.
