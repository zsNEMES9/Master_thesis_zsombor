import pandas as pd
import numpy as np

# ── 1. Load datasets ──────────────────────────────────────────────────────────
df  = pd.read_csv("upwork_pooled.csv")
rti = pd.read_csv("rti_country_specific_survey_predicted.csv")

print(f"Pooled rows: {len(df)}")

# ── 2. Extract O*NET 2017 RTI scores (continuous robustness variable) ─────────
# RTI is retained as a continuous control variable in robustness regressions.
# It is NOT used to assign verifiability tiers (see §4.3 of thesis for rationale:
# RTI conflates non-routine analytical with non-routine interpersonal — two task
# types that have opposite verifiability profiles under ALM 2003).
onet17_cols = [c for c in rti.columns if c.startswith("onet17_rti_isco2d")]
onet17_row  = rti[onet17_cols].iloc[0]

rti_lookup = {
    int(col.replace("onet17_rti_isco2d", "")): score
    for col, score in onet17_row.items()
}

print(f"RTI scores available for {len(rti_lookup)} ISCO 2-digit groups")

# ── 3. Parse ISCO group number from pooled data ───────────────────────────────
df["isco_num"] = df["isco_group"].str.extract(r"(\d+)$").astype(int)

# ── 4. Map RTI scores (continuous robustness variable) ───────────────────────
df["rti_score"] = df["isco_num"].map(rti_lookup)

unmatched_rti = df[df["rti_score"].isna()]["isco_group"].unique()
if len(unmatched_rti) > 0:
    print(f"ISCO groups with no O*NET RTI score (armed forces / agriculture): {unmatched_rti}")
    print("Assigned median RTI for these groups.")
    df["rti_score"] = df["rti_score"].fillna(df["rti_score"].median())

# ── 5. Manual verifiability classification — ALM (2003) task framework ────────
# Classification based on Autor, Levy & Murnane (2003) ALM task content:
#
# HIGH verifiability — Non-routine Cognitive Analytical (NRCA) + technical output:
#   Outputs objectively testable against explicit criteria (code runs/fails,
#   engineering spec met/not, data model accuracy measurable).
#   ISCO: 21 Science/Engineering, 25 ICT, 35 ICT Technicians,
#         43 Numerical Clerks, 71-74 Engineering trades, 81-83 Plant/Transport ops
#
# LOW verifiability — Non-routine Cognitive Interpersonal (NRCI):
#   Output quality assessed through subjective, culturally-inflected criteria;
#   not evaluable without domain expertise or long-term observation.
#   ISCO: 22 Health, 23 Teaching, 26 Legal/Cultural, 32 Health Associates,
#         51 Personal Services, 53 Personal Care, 94 Food Preparation
#
# MEDIUM — Mixed or context-dependent task content.
# (Source: ISCO_Verifiability_Classification.xlsx for full rationale + citations)

HIGH_VERIF = {21, 25, 35, 43, 71, 72, 74, 81, 82, 83}
LOW_VERIF  = {22, 23, 26, 32, 51, 53, 94}

def assign_verif(code):
    if code in HIGH_VERIF:
        return "high"
    elif code in LOW_VERIF:
        return "low"
    else:
        return "medium"

df["verif_group"] = df["isco_num"].apply(assign_verif)

# ── 6. Binary indicators ──────────────────────────────────────────────────────
df["low_verif"]  = (df["verif_group"] == "low").astype(int)
df["high_verif"] = (df["verif_group"] == "high").astype(int)
df["med_verif"]  = (df["verif_group"] == "medium").astype(int)

# ── 7. Verification ───────────────────────────────────────────────────────────
print("\nVerifiability classification per ISCO group (sorted by tier):")
summary = (df[["isco_group", "isco_num", "verif_group", "rti_score"]]
           .drop_duplicates()
           .sort_values(["verif_group", "isco_num"]))
print(summary.to_string(index=False))

print("\nVerifiability group distribution (ISCO groups):")
print(df.groupby("verif_group")["isco_num"].nunique().rename("n_groups"))

print("\nVerifiability group distribution (rows):")
print(df["verif_group"].value_counts())

print(f"\nHigh verif groups ({len(HIGH_VERIF)}): {sorted(HIGH_VERIF)}")
print(f"Low  verif groups ({len(LOW_VERIF)}):  {sorted(LOW_VERIF)}")

# ── 8. Save ───────────────────────────────────────────────────────────────────
df.to_csv("upwork_pooled.csv", index=False)
print(f"\nSaved: upwork_pooled.csv — now {df.shape[1]} columns")
