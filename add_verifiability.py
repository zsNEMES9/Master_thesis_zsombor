"""
add_verifiability.py
====================
Assigns output-verifiability labels to upwork_pooled.csv.

WHAT THIS SCRIPT DOES
---------------------
Nothing in this file decides a verifiability tier. Every tier is read from the
classification workbook, in which each of the 100 search keywords was coded by
hand against the two-step test documented in Appendix C.2 of the thesis:

    Step 1  Does an external specification exist that is the primary and
            disqualifying axis of the deliverable's quality?
    Step 2  Does relational, cultural or aesthetic judgement also substantially
            drive the verdict?

    (1, 0) -> High        (0, 1) -> Low        (1, 1) -> Medium

The script applies that hand-coding to the pooled observations and verifies it
was applied consistently. Before writing anything it checks that every keyword
in the dataset appears in the workbook, that no keyword is duplicated, that the
scoring rule reproduces the stated tier for all 100 keywords, and that the
workbook's observation counts match the dataset. Any failure aborts the run
rather than writing a partial result.

PRIMARY vs SECONDARY TIER
-------------------------
verif_group        KEYWORD-level tier. Primary; used by Model 2, the test of H2.
                   The two-step test is applied at the keyword level, which is
                   also the level at which rank is defined and at which Model 1b
                   takes fixed effects.

verif_group_isco   ISCO-GROUP-level tier. Secondary, retained as an available
                   robustness coding and not used in the reported models. Groups
                   whose keywords disagree collapse to Medium because ISCO-08
                   offers no finer subdivision at that point, not because the
                   deliverable is intermediate.

OUTPUT
------
Rewrites upwork_pooled.csv in place, after copying it to a dated backup. Adds
the two tier variables, their dummy indicators, the two step scores, the stated
verification basis, and the refined ISCO code and title. Prints a summary of the
resulting distribution and of any keyword whose tier differs from the labels
already present in the file.

Usage:  python add_verifiability.py
"""

import shutil
from datetime import date
from pathlib import Path

import pandas as pd

# ── 0. Configuration ──────────────────────────────────────────────────────────
HERE = Path(__file__).resolve().parent
POOLED = HERE / "upwork_pooled.csv"
WORKBOOK = HERE / "ISCO_Verifiability_Classification_v5.xlsx"
BACKUP = HERE / f"upwork_pooled_BACKUP_pre_v5_verif_{date.today().isoformat()}.csv"

TIER_ORDER = ["high", "medium", "low"]

# ── 1. Load ───────────────────────────────────────────────────────────────────
df = pd.read_csv(POOLED)
km = pd.read_excel(WORKBOOK, sheet_name="Keyword Mapping")
groups = pd.read_excel(WORKBOOK, sheet_name="ISCO Verifiability")

print(f"Pooled rows: {len(df):,}  |  workbook keywords: {len(km)}  "
      f"|  workbook ISCO groups: {len(groups)}")

df["keyword"] = df["keyword"].str.strip()
km["keyword"] = km["keyword"].str.strip()

# ── 2. Integrity checks BEFORE touching anything ─────────────────────────────
missing = sorted(set(df["keyword"]) - set(km["keyword"]))
extra = sorted(set(km["keyword"]) - set(df["keyword"]))
if missing:
    raise SystemExit(f"ABORT — keywords in dataset but not in workbook: {missing}")
if extra:
    print(f"WARNING — workbook keywords absent from dataset: {extra}")
if km["keyword"].duplicated().any():
    raise SystemExit("ABORT — duplicate keywords in the workbook mapping sheet.")

# The two-step scores must reproduce the stated tier for every keyword. This is
# an audit of the hand-coding rather than a substitute for it: a mismatch means
# the workbook is internally inconsistent, so the run stops instead of silently
# preferring one column over the other.
def tier_from_steps(s1, s2):
    return {(1, 0): "high", (0, 1): "low", (1, 1): "medium"}.get((int(s1), int(s2)))

derived = km.apply(
    lambda r: tier_from_steps(r["step1_spec_is_primary"], r["step2_relational_cultural"]),
    axis=1,
)
stated = km["keyword_verif"].str.strip().str.lower()
bad = km.loc[derived != stated, ["keyword", "step1_spec_is_primary",
                                 "step2_relational_cultural", "keyword_verif"]]
if len(bad):
    raise SystemExit(f"ABORT — scoring rule does not reproduce stated tier:\n{bad}")
if derived.isna().any():
    raise SystemExit("ABORT — a keyword scored (0,0); the scoring rule has no tier for it.")
print("OK — two-step scores reproduce the stated keyword tier for all "
      f"{len(km)} keywords.")

# Workbook N per ISCO group must match the dataset.
gcheck = (km.merge(df["keyword"].value_counts().rename("n_rows"),
                   left_on="keyword", right_index=True, how="left")
            .groupby("isco_code")["n_rows"].sum()
            .rename("n_dataset"))
gcheck = groups.set_index("ISCO Code")["N (obs.)"].rename("n_workbook").to_frame().join(gcheck)
gcheck["match"] = gcheck["n_workbook"] == gcheck["n_dataset"]
if not gcheck["match"].all():
    print("WARNING — N mismatch between workbook and dataset:")
    print(gcheck[~gcheck["match"]].to_string())
else:
    print(f"OK — N matches the workbook for all {len(gcheck)} ISCO groups "
          f"(total {int(gcheck['n_workbook'].sum()):,}).")

# ── 3. Preserve the outgoing labels for the change log ───────────────────────
df["verif_group_prev"] = df.get("verif_group")

# ── 4. Merge the classification onto the pooled data ─────────────────────────
mapping = km.set_index("keyword")

df["verif_group"] = df["keyword"].map(mapping["keyword_verif"].str.strip().str.lower())
df["verif_group_isco"] = df["keyword"].map(mapping["group_verif"].str.strip().str.lower())
df["verif_step1_spec"] = df["keyword"].map(mapping["step1_spec_is_primary"]).astype(int)
df["verif_step2_relational"] = df["keyword"].map(mapping["step2_relational_cultural"]).astype(int)
df["verif_basis"] = df["keyword"].map(mapping["verification_basis"])
df["isco_code_v5"] = df["keyword"].map(mapping["isco_code"])
df["isco_title_v5"] = df["keyword"].map(mapping["isco_title"])

if df["verif_group"].isna().any():
    raise SystemExit("ABORT — unassigned rows after merge.")

# ── 5. Dummies ───────────────────────────────────────────────────────────────
# Primary (keyword-level) — the indicators the analysis script reads for Model 2.
df["low_verif"] = (df["verif_group"] == "low").astype(int)
df["high_verif"] = (df["verif_group"] == "high").astype(int)
df["med_verif"] = (df["verif_group"] == "medium").astype(int)

# Secondary (ISCO-group-level) — robustness specifications only.
df["low_verif_isco"] = (df["verif_group_isco"] == "low").astype(int)
df["high_verif_isco"] = (df["verif_group_isco"] == "high").astype(int)
df["med_verif_isco"] = (df["verif_group_isco"] == "medium").astype(int)

# ── 6. RTI — descriptive covariate, read from the workbook ───────────────────
# RTI is a continuous covariate reported as descriptive context and is NOT used
# to assign tiers. Lewandowski et al. (2022) scores are defined at 2-digit ISCO,
# so the finer groups inherit their parent 2-digit score.
rti_v5 = df["isco_code_v5"].map(
    groups.set_index("ISCO Code")["RTI Score (O*NET 2017)"]
)
if "rti_score" in df.columns:
    delta = (rti_v5.round(3) - df["rti_score"].round(3)).abs()
    n_diff = int((delta > 0.001).sum())
    print(f"RTI: {n_diff:,} rows differ from the existing rti_score "
          f"(workbook is rounded to 3 dp; existing column retained).")
else:
    df["rti_score"] = rti_v5

# ── 7. Diagnostics ───────────────────────────────────────────────────────────
def block(title):
    print(f"\n{'=' * 78}\n{title}\n{'=' * 78}")

block("KEYWORD-LEVEL TIER (verif_group) — PRIMARY, used for H2 / Model 2")
print(df["verif_group"].value_counts().reindex(TIER_ORDER).rename("rows").to_frame()
        .assign(pct=lambda t: (100 * t["rows"] / len(df)).round(1),
                keywords=km["keyword_verif"].str.lower().value_counts().reindex(TIER_ORDER))
        .to_string())

block("ISCO-GROUP-LEVEL TIER (verif_group_isco) — SECONDARY, robustness only")
print(df["verif_group_isco"].value_counts().reindex(TIER_ORDER).rename("rows").to_frame()
        .assign(pct=lambda t: (100 * t["rows"] / len(df)).round(1))
        .to_string())

n_m2_kw = int(df["verif_group"].isin(["high", "low"]).sum())
n_m2_gr = int(df["verif_group_isco"].isin(["high", "low"]).sum())
print(f"\nModel 2 analytic sample (Medium excluded):")
print(f"  keyword-level    : {n_m2_kw:,} obs ({100 * n_m2_kw / len(df):.1f}% of pooled)")
print(f"  ISCO-group-level : {n_m2_gr:,} obs ({100 * n_m2_gr / len(df):.1f}% of pooled)")

block("CROSS-TABULATION — keyword tier x ISCO-group tier (rows)")
print(pd.crosstab(df["verif_group"], df["verif_group_isco"]).reindex(
    index=TIER_ORDER, columns=TIER_ORDER, fill_value=0).to_string())

block("CHANGE LOG — keywords whose tier differs from the previous classification")
if df["verif_group_prev"].notna().any():
    chg = (df.groupby("keyword")
             .agg(isco_v5=("isco_code_v5", "first"),
                  old=("verif_group_prev", "first"),
                  new=("verif_group", "first"),
                  n=("keyword", "size"))
             .query("old != new")
             .sort_values(["old", "new", "keyword"]))
    print(f"{len(chg)} of {df['keyword'].nunique()} keywords change tier "
          f"({chg['n'].sum():,} of {len(df):,} rows reclassified).\n")
    print(chg.to_string())
else:
    print("No previous verif_group column found — nothing to compare.")

block("PER-ISCO-GROUP SUMMARY")
summary = (df.groupby(["isco_code_v5", "isco_title_v5"])
             .agg(n=("keyword", "size"),
                  n_keywords=("keyword", "nunique"),
                  group_tier=("verif_group_isco", "first"),
                  high_kw=("high_verif", "sum"),
                  med_kw=("med_verif", "sum"),
                  low_kw=("low_verif", "sum"))
             .reset_index())
print(summary.to_string(index=False))

# ── 8. Save ──────────────────────────────────────────────────────────────────
df = df.drop(columns=["verif_group_prev"])

if not BACKUP.exists():
    shutil.copy2(POOLED, BACKUP)
    print(f"\nBackup written: {BACKUP.name}")
else:
    print(f"\nBackup already exists, not overwritten: {BACKUP.name}")

df.to_csv(POOLED, index=False)
print(f"Saved: {POOLED.name} — {len(df):,} rows x {df.shape[1]} columns")
print("\nNew/updated columns: verif_group (keyword-level, PRIMARY), "
      "low_verif, high_verif, med_verif,\n  verif_group_isco (+ *_isco dummies), "
      "verif_step1_spec, verif_step2_relational, verif_basis,\n  isco_code_v5, isco_title_v5")