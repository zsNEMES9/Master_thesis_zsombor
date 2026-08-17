import pandas as pd

# ── 1. Load pooled dataset and World Bank classification ──────────────────────
df = pd.read_csv("upwork_pooled.csv")
wb = pd.read_excel("CLASS_2025_10_07.xlsx", sheet_name="List of economies")
wb = wb[["Economy", "Income group"]].dropna(subset=["Economy"])

print(f"Pooled rows:              {len(df)}")
print(f"Unique countries in data: {df['country'].nunique()}")

# ── 2. Harmonise country names to World Bank spelling ─────────────────────────
name_map = {
    "Czech Republic":                        "Czechia",
    "Egypt":                                 "Egypt, Arab Rep.",
    "Macedonia":                             "North Macedonia",
    "North Macedonia":                       "North Macedonia",
    "Palestinian Territories":               "West Bank and Gaza",
    "Slovakia":                              "Slovak Republic",
    "South Korea":                           "Korea, Rep.",
    "Turkey":                                "Türkiye",
    "Venezuela":                             "Venezuela, RB",
    "Kyrgyzstan":                            "Kyrgyz Republic",
    "Congo, the Democratic Republic of the": "Congo, Dem. Rep.",
}
df["country_wb"] = df["country"].replace(name_map)

# ── 3. Left join on country name ──────────────────────────────────────────────
df = df.merge(
    wb,
    left_on="country_wb",
    right_on="Economy",
    how="left"
).drop(columns="Economy").rename(columns={"Income group": "income_group_wb"})

# ── 4. Manual assignments ─────────────────────────────────────────────────────
# Taiwan:    not a WB member           → High income (well-documented)
# Jersey:    British Crown dependency  → High income (well-documented)
# Ethiopia:  WB cell is blank in file  → Low income (confirmed by IDA lending status)
# Venezuela: WB cell is blank in file  → Upper middle income (WB FY2026 historical classification)
manual = {
    "Taiwan":    "High income",
    "Jersey":    "High income",
    "Ethiopia":  "Low income",
    "Venezuela": "Upper middle income",
}
for country, group in manual.items():
    df.loc[df["country"] == country, "income_group_wb"] = group

# ── 5. Verify no unmatched countries remain ───────────────────────────────────
unmatched = df[df["income_group_wb"].isna()]["country"].unique()
if len(unmatched) == 0:
    print("All countries matched successfully.")
else:
    print(f"Still unmatched — fix before proceeding: {unmatched}")

# ── 6. Create binary Global South indicator ───────────────────────────────────
# 1 = Global South (upper-middle / lower-middle / low income)
# 0 = Global North (high income)
df["global_south"] = (df["income_group_wb"] != "High income").astype(int)

# ── 7. Create 4-tier ordinal income variable (robustness spec) ────────────────
income_ordinal_map = {
    "High income":          1,
    "Upper middle income":  2,
    "Lower middle income":  3,
    "Low income":           4,
}
df["income_group_ordinal"] = df["income_group_wb"].map(income_ordinal_map)

# ── 8. Verification ───────────────────────────────────────────────────────────
print("\nIncome group distribution:")
summary = (
    df.groupby("income_group_wb")
    .agg(n_rows=("global_south", "count"),
         global_south=("global_south", "first"),
         ordinal=("income_group_ordinal", "first"))
    .sort_values("ordinal")
)
print(summary.to_string())

print(f"\nGlobal South (1): {df['global_south'].sum()} rows")
print(f"Global North (0): {(df['global_south'] == 0).sum()} rows")

# ── 9. Save ───────────────────────────────────────────────────────────────────
df.to_csv("upwork_pooled.csv", index=False)
print(f"\nSaved: upwork_pooled.csv — now {df.shape[1]} columns")
