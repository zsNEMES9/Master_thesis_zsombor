import pandas as pd

# ── 1. Load originals (read-only — this script never writes back to them) ─────
df_march = pd.read_csv("upwork_phase2_completed_20260608_0200.csv")
df_june  = pd.read_csv("upwork_with_rates_20260612_0207.csv")

print(f"March rows loaded: {len(df_march)}")
print(f"June  rows loaded: {len(df_june)}")

# ── 2. Inspect within-session duplicates before removing ──────────────────────
# These are profiles Upwork placed on both page 1 and page 2 for the same keyword.
# rank_global is our dependent variable — one observation cannot have two DV values,
# so we keep only the first (lowest rank = highest position = more economically meaningful).
dup_march = df_march[df_march.duplicated(subset=["keyword", "profile_url"], keep=False)]
dup_june  = df_june[df_june.duplicated(subset=["keyword", "profile_url"], keep=False)]

print(f"\nWithin-session duplicates in March: {len(dup_march)} rows "
      f"({len(dup_march)//2} profiles × 2 pages)")
print(f"Within-session duplicates in June:  {len(dup_june)} rows "
      f"({len(dup_june)//2} profiles × 2 pages)")

print("\nMarch duplicates (keyword | rank_global | page):")
print(dup_march[["keyword", "rank_global", "page"]]
      .sort_values(["keyword", "rank_global"])
      .to_string(index=False))

print("\nJune duplicates (keyword | rank_global | page):")
print(dup_june[["keyword", "rank_global", "page"]]
      .sort_values(["keyword", "rank_global"])
      .to_string(index=False))

# ── 3. Keep first appearance (lowest rank_global) ─────────────────────────────
df_march_clean = (df_march
                  .sort_values("rank_global")
                  .drop_duplicates(subset=["keyword", "profile_url"], keep="first"))

df_june_clean  = (df_june
                  .sort_values("rank_global")
                  .drop_duplicates(subset=["keyword", "profile_url"], keep="first"))

print(f"\nMarch rows after dedup: {len(df_march_clean)} (removed {len(df_march) - len(df_march_clean)})")
print(f"June  rows after dedup: {len(df_june_clean)}  (removed {len(df_june)  - len(df_june_clean)})")

# ── 4. Vertical stack ─────────────────────────────────────────────────────────
df_pooled = pd.concat([df_march_clean, df_june_clean], ignore_index=True)

print(f"\nPooled rows: {len(df_pooled)}")
print(f"Sessions:    {df_pooled['session'].nunique()} ({list(df_pooled['session'].unique())})")

# ── 5. Add scrape_session binary ──────────────────────────────────────────────
# 0 = March 2026 (baseline), 1 = June 2026
df_pooled["scrape_session"] = (df_pooled["session"] == "20260610_0324").astype(int)

print(f"\nSession distribution:\n{df_pooled['scrape_session'].value_counts().to_string()}")

# ── 6. Verify triple-key uniqueness ───────────────────────────────────────────
n_dupes = df_pooled.duplicated(subset=["profile_url", "keyword", "session"]).sum()
print(f"\nDuplicate rows on (profile_url, keyword, session): {n_dupes}")
assert n_dupes == 0, "Unexpected duplicates — investigate before proceeding."

# ── 7. Save pooled file (originals are untouched) ─────────────────────────────
output_path = "upwork_pooled.csv"
df_pooled.to_csv(output_path, index=False)
print(f"\nSaved: {output_path}")
print(f"Final shape: {df_pooled.shape[0]} rows × {df_pooled.shape[1]} columns")
