"""
upwork_scraper.py  —  Geographic Bias in Upwork Search Rankings
Master's Thesis  |  RSM Rotterdam

All thesis variables collected:

PHASE 1 — from search card (fast, no extra page load):
    search rank, country, JSS, hourly rate, earnings tier,
    total jobs (hourly + fixed), hours worked, badge (Top Rated etc.),
    skills count (visible + hidden), bio length, has photo,
    keyword searched, ISCO group, session tag

PHASE 2 — from individual profile page (slower, run after Phase 1):
    number of reviews, years on platform (member since),
    certifications count, total skills on profile,
    has video intro, overview length

DEPENDENCIES:
    py -3.11 -m pip install undetected-chromedriver selenium pandas beautifulsoup4

USAGE:
    Diagnostic:   FULL_RUN = False  →  1 keyword, 1 page, no Phase 2
    Production:   FULL_RUN = True   →  all keywords, 2 pages, Phase 2 enabled
"""

import os
import re
import time
import random
import logging
import pandas as pd
from datetime import datetime
from bs4 import BeautifulSoup
import undetected_chromedriver as uc

# ─────────────────────────────────────────────────────────────
#  CONFIG
# ─────────────────────────────────────────────────────────────
THESIS_DIR   = r"./upwork_scraper_test"  # set to your own output directory
FULL_RUN     = True     # False = diagnostic (1 keyword, 1 page)
PAGES        = 2         # pages per keyword in full run (~10 profiles/page)
RUN_PHASE2   = True      # set False to skip profile deep-scrape even in full run

SESSION_TAG  = datetime.now().strftime("%Y%m%d_%H%M")

# ── ISCO group labels for each keyword ───────────────────────
# Used as a categorical fixed-effect variable in your regression
KEYWORD_META = {
    # Female-dominated
    "Online Health Coach":              "ISCO_53",
    "Child Development Consultant":     "ISCO_53",
    "Elder Care Advisor":               "ISCO_53",
    "Household Management Consultant":  "ISCO_91",
    "Medical Writer":                   "ISCO_22",
    "Health Content Creator":           "ISCO_22",
    "Nutrition Consultant":             "ISCO_22",
    "Telehealth Advisor":               "ISCO_22",
    "Virtual Assistant":                "ISCO_41",
    "Data Entry Specialist":            "ISCO_41",
    "Executive Assistant":              "ISCO_41",
    "Medical Transcriptionist":         "ISCO_32",
    "Pharmaceutical Content Writer":    "ISCO_32",
    "Clinical Research Assistant":      "ISCO_32",
    "Customer Support Representative":  "ISCO_42",
    "CRM Specialist":                   "ISCO_42",
    "Live Chat Support Specialist":     "ISCO_42",
    "Online Tutor":                     "ISCO_23",
    "E-learning Content Developer":     "ISCO_23",
    "Curriculum Designer":              "ISCO_23",
    "Language Teacher":                 "ISCO_23",
    "Bookkeeper":                       "ISCO_33",
    "Payroll Specialist":               "ISCO_33",
    "HR Administrator":                 "ISCO_33",
    "Recruitment Coordinator":          "ISCO_33",
    "Food Content Writer":              "ISCO_94",
    "Transcriptionist":                 "ISCO_44",
    "Subtitler Captioner":              "ISCO_44",
    "Travel Content Writer":            "ISCO_51",
    "Life Coach":                       "ISCO_51",
    "Home Organization Consultant":     "ISCO_91",
    "Wellness Content Writer":          "ISCO_22",
    "Test Prep Coach":                  "ISCO_23",
    # Male-dominated
    "CAD Designer":                     "ISCO_72",
    "Logistics Consultant":             "ISCO_83",
    "Civil Engineering Consultant":     "ISCO_71",
    "Electrical Engineering Consultant":"ISCO_74",
    "Cybersecurity Consultant":         "ISCO_01",
    "IT Infrastructure Consultant":     "ISCO_02",
    "Strategic Planning Consultant":    "ISCO_03",
    "Process Engineering Consultant":   "ISCO_81",
    "Automation Programmer":            "ISCO_82",
    "Mining Analyst":                   "ISCO_92",
    "Sustainability Consultant":        "ISCO_96",
    "AgriTech Consultant":              "ISCO_62",
    "IT Help Desk Technician":          "ISCO_35",
    "Software Tester":                  "ISCO_35",
    "Telecommunications Consultant":    "ISCO_35",
    "Data Scientist":                   "ISCO_21",
    "Machine Learning Engineer":        "ISCO_21",
    "NLP Engineer":                     "ISCO_21",
    "Statistician":                     "ISCO_21",
    "Quantitative Analyst":             "ISCO_21",
    "Software Developer":               "ISCO_25",
    "Web Developer":                    "ISCO_25",
    "Mobile App Developer":             "ISCO_25",
    "Blockchain Developer":             "ISCO_25",
    "DevOps Engineer":                  "ISCO_25",
    "Cybersecurity Analyst":            "ISCO_25",
    "Cloud Solutions Architect":        "ISCO_25",
    "Game Developer":                   "ISCO_25",
    "Business Strategy Consultant":     "ISCO_11",
    "Project Manager":                  "ISCO_11",
    "Supply Chain Consultant":          "ISCO_13",
    "Operations Manager":               "ISCO_13",
    "AgriTech Data Analyst":            "ISCO_61",
    # Mixed
    "Content Writer":                   "ISCO_26",
    "Copywriter":                       "ISCO_26",
    "Translator":                       "ISCO_26",
    "Interpreter":                      "ISCO_26",
    "Ghostwriter":                      "ISCO_26",
    "Technical Writer":                 "ISCO_26",
    "Grant Writer":                     "ISCO_26",
    "Scriptwriter":                     "ISCO_26",
    "Localization Specialist":          "ISCO_26",
    "UX UI Designer":                   "ISCO_26",
    "Video Editor":                     "ISCO_26",
    "Photographer":                     "ISCO_26",
    "E-commerce Specialist":            "ISCO_52",
    "Sales Copywriter":                 "ISCO_52",
    "Financial Data Analyst":           "ISCO_43",
    "Accountant":                       "ISCO_43",
    "Contract Drafter":                 "ISCO_34",
    "Compliance Specialist":            "ISCO_34",
    "Event Planning Consultant":        "ISCO_14",
    "Travel Tourism Consultant":        "ISCO_14",
    "Marketing Manager Consultant":     "ISCO_12",
    "Financial Manager Consultant":     "ISCO_12",
    "Graphic Designer":                 "ISCO_73",
    "Logo Designer":                    "ISCO_73",
    "Illustrator":                      "ISCO_73",
    "Brand Identity Designer":          "ISCO_73",
    "Fashion Designer":                 "ISCO_75",
    "Interior Designer":                "ISCO_75",
    "HR Consultant":                    "ISCO_24",
    "Recruiter":                        "ISCO_24",
    "Financial Analyst":                "ISCO_24",
    "Social Commerce Specialist":       "ISCO_95",
    "Food Systems Consultant":          "ISCO_63",
    "Affiliate Marketing Specialist":   "ISCO_52",
}

KEYWORDS = list(KEYWORD_META.keys())   # 100 keywords

# ─────────────────────────────────────────────────────────────
#  LOGGING
# ─────────────────────────────────────────────────────────────
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s | %(levelname)s | %(message)s",
    datefmt="%H:%M:%S",
)
log = logging.getLogger(__name__)


# ─────────────────────────────────────────────────────────────
#  DRIVER
# ─────────────────────────────────────────────────────────────
def build_driver():
    options = uc.ChromeOptions()
    options.add_argument("--window-size=1440,900")
    options.add_argument("--no-sandbox")
    options.add_argument("--disable-dev-shm-usage")
    options.add_argument("--no-first-run")
    options.add_argument("--no-default-browser-check")
    options.add_argument(r"--user-data-dir=.\ChromeAutomationProfile")
    options.add_argument("--profile-directory=Default")
    driver = uc.Chrome(options=options)
    time.sleep(3)
    driver.switch_to.window(driver.window_handles[0])
    return driver


# ─────────────────────────────────────────────────────────────
#  CLOUDFLARE WAIT
# ─────────────────────────────────────────────────────────────
def wait_for_cloudflare(driver, timeout=60):
    deadline = time.time() + timeout
    while time.time() < deadline:
        title = driver.title.lower()
        if "just a moment" not in title and "attention required" not in title:
            return True
        log.info("  ⏳ Cloudflare — waiting (tick checkbox if needed)...")
        time.sleep(3)
    log.warning("  🚨 Cloudflare timeout.")
    return False


# ─────────────────────────────────────────────────────────────
#  DOM SNAPSHOT
# ─────────────────────────────────────────────────────────────
def dump_snapshot(driver, label):
    out_dir = THESIS_DIR if os.path.isdir(THESIS_DIR) else os.getcwd()
    path = os.path.join(out_dir, f"DEBUG_{label}_{SESSION_TAG}.html")
    with open(path, "w", encoding="utf-8") as f:
        f.write(driver.page_source)
    log.info(f"📄 Snapshot → {path}")


# ─────────────────────────────────────────────────────────────
#  PHASE 1 — EXTRACT FROM SEARCH CARD
#  All fields confirmed against live Upwork DOM 2026-03-19
# ─────────────────────────────────────────────────────────────
def extract_card(card, rank, keyword, page):

    # ── Country ──────────────────────────────────────────────
    el      = card.select_one("p.location")
    country = el.get_text(strip=True) if el else "N/A"

    # ── JSS ──────────────────────────────────────────────────
    el      = card.select_one('[data-test="UpI18n"]')
    raw     = el.get_text(strip=True) if el else ""
    jss     = re.sub(r"[^\d.]", "", raw.split()[0]) if raw else "N/A"

    # ── Hourly rate ───────────────────────────────────────────
    el      = card.select_one('[data-test="rate-per-hour"]')
    rate    = re.sub(r"[^\d.]", "", el.get_text(strip=True)) if el else "N/A"

    # ── Badge (Top Rated Plus / Top Rated / Rising Talent) ────
    el      = card.select_one('[data-test="FreelancerTileTopRated"]')
    raw     = el.get_text(strip=True) if el else ""
    badge   = (
        "Top Rated Plus" if "Top Rated Plus" in raw else
        "Top Rated"      if "Top Rated"      in raw else
        "Rising Talent"  if "Rising Talent"  in raw else
        "N/A"
    )

    # ── Earnings tier + total jobs + hours worked ─────────────
    # The earnings popup contains all three: "$100K+ earned | 11 hourly jobs | 1 fixed price job | 8938 hours worked"
    el           = card.select_one('[data-test="freelancer-tile-earnings"]')
    earn_raw     = el.get_text(strip=True) if el else ""

    earnings_tier = earn_raw.split("earned")[0].strip() if "earned" in earn_raw else "N/A"

    hourly_m  = re.search(r"(\d+)\s*hourly job",      earn_raw)
    fixed_m   = re.search(r"(\d+)\s*fixed price job", earn_raw)
    hours_m   = re.search(r"(\d+)\s*hours worked",    earn_raw)

    hourly_jobs  = int(hourly_m.group(1)) if hourly_m else None
    fixed_jobs   = int(fixed_m.group(1))  if fixed_m  else None
    hours_worked = int(hours_m.group(1))  if hours_m  else "N/A"

    total_jobs = (
        hourly_jobs + fixed_jobs
        if hourly_jobs is not None and fixed_jobs is not None
        else hourly_jobs or fixed_jobs or "N/A"
    )

    # ── Skills count (visible + hidden "+N") ──────────────────
    el         = card.select_one('[data-test="FreelancerTileSkills"]')
    skills_raw = el.get_text(strip=True) if el else ""
    skills_clean = re.sub(r"Start of list\.|End of list\.", "", skills_raw).strip()
    extra_m    = re.search(r"\+(\d+)", skills_clean)
    extra_n    = int(extra_m.group(1)) if extra_m else 0
    # Approximate visible skill count by splitting on known camelCase / caps patterns
    visible_text = re.sub(r"\+\d+$", "", skills_clean).strip()
    # Each skill is typically 1–4 words — use character density heuristic:
    # average ~12 chars per skill tag based on inspection
    skills_visible = max(1, round(len(visible_text) / 12)) if visible_text else 0
    skills_count   = skills_visible + extra_n

    # ── Bio snippet length (proxy for profile completeness) ───
    el       = card.select_one('[data-test="UpCLineClamp"]')
    bio_len  = len(el.get_text(strip=True)) if el else 0

    # ── Has profile photo ─────────────────────────────────────
    avatar    = card.select_one('[data-test="FreelancerAvatar"] img')
    has_photo = int(
        avatar is not None and
        "profile-portraits" in (avatar.get("src") or "")
    )

    # ── Freelancer name (for gender estimation later) ─────────
    name_el = card.select_one('[data-test="UpLineClamp"]')
    name    = name_el.get_text(strip=True) if name_el else "N/A"
    # Extract first name only
    first_name = name.split()[0] if name != "N/A" else "N/A"
    # Remove trailing dot (e.g. "Ronald T." → first_name = "Ronald")
    first_name = first_name.rstrip(".")

    # ── Profile URL ───────────────────────────────────────────
    link = card.select_one('a[href*="/freelancers/"]')
    if link:
        href = link["href"]
        profile_url = href if href.startswith("http") else "https://www.upwork.com" + href
    else:
        profile_url = "N/A"

    # ── Profile completeness score (0–5, from search card) ────
    # +1 for each: has_photo, bio present, skills >3, badge, earnings badge
    completeness_card = (
        has_photo +
        int(bio_len > 50) +
        int(skills_count > 3) +
        int(badge != "N/A") +
        int(earnings_tier != "N/A")
    )

    return {
        # Identifiers
        "session"            : SESSION_TAG,
        "keyword"            : keyword,
        "isco_group"         : KEYWORD_META.get(keyword, "N/A"),
        "page"               : page,
        "rank_page"          : rank,
        "rank_global"        : (page - 1) * 10 + rank,
        # Key IV
        "country"            : country,
        # Primary quality controls
        "jss"                : jss,
        "total_jobs"         : total_jobs,
        "hours_worked"       : hours_worked,
        # Rate (secondary DV + control)
        "rate_usd_hr"        : rate,
        # Earnings badge (ordinal)
        "earnings_tier"      : earnings_tier,
        # Badge
        "badge"              : badge,
        # Profile richness moderators (from card)
        "skills_count_card"  : skills_count,
        "bio_len_card"       : bio_len,
        "has_photo"          : has_photo,
        "completeness_card"  : completeness_card,
        # For gender estimation
        "first_name"         : first_name,
        # For Phase 2 join
        "profile_url"        : profile_url,
    }


# ─────────────────────────────────────────────────────────────
#  PHASE 1 — SCRAPE SEARCH PAGE
# ─────────────────────────────────────────────────────────────
def scrape_search_page(driver, keyword, page):
    url = (
        f"https://www.upwork.com/search/profiles/"
        f"?q={keyword.replace(' ', '+')}&page={page}&per_page=10"
    )
    log.info(f"  → {url}")
    driver.get(url)

    if not wait_for_cloudflare(driver):
        dump_snapshot(driver, f"CF_BLOCKED_{keyword.replace(' ', '_')}_p{page}")
        return []

    time.sleep(random.uniform(3, 5))

    soup  = BeautifulSoup(driver.page_source, "html.parser")
    cards = soup.select('[data-test="FreelancerTile"]')

    if not cards:
        log.warning("  ⚠️  No cards — dumping snapshot")
        dump_snapshot(driver, f"NO_CARDS_{keyword.replace(' ', '_')}_p{page}")
        return []

    log.info(f"  ✅ {len(cards)} cards")
    results = []
    for i, card in enumerate(cards, 1):
        try:
            results.append(extract_card(card, i, keyword, page))
        except Exception as e:
            log.warning(f"  Card {i} error: {e}")
    return results


# ─────────────────────────────────────────────────────────────
#  PHASE 2 — PROFILE DEEP SCRAPE
#  Adds: reviews, member_since_year, cert_count,
#        skills_count_profile, has_video, overview_len
# ─────────────────────────────────────────────────────────────
def deep_scrape_profile(driver, profile_url):
    extras = {
        "review_count"        : "N/A",
        "member_since_year"   : "N/A",
        "years_on_platform"   : "N/A",
        "cert_count"          : "N/A",
        "skills_count_profile": "N/A",
        "has_video"           : "N/A",
        "overview_len"        : "N/A",
    }
    if not profile_url or profile_url == "N/A" or not profile_url.startswith("http"):
        return extras
    try:
        driver.get(profile_url)
        if not wait_for_cloudflare(driver):
            return extras
        time.sleep(random.uniform(3, 5))
        soup = BeautifulSoup(driver.page_source, "html.parser")

        # ── Reviews ──────────────────────────────────────────
        for sel in ['[data-test="total-feedback"]', '[class*="totalFeedback"]',
                    '[data-qa="feedback-count"]', 'span[class*="reviews"]']:
            el = soup.select_one(sel)
            if el:
                extras["review_count"] = re.sub(r"[^\d]", "", el.get_text())
                break

        # ── Member since year ─────────────────────────────────
        full_text = soup.get_text()
        member_m  = re.search(r"Member since\s+((?:January|February|March|April|May|June|July|August|September|October|November|December)\s+\d{4})", full_text)
        if not member_m:
            member_m = re.search(r"Member since[^\d]*(20\d{2})", full_text)
        if member_m:
            year_str = re.search(r"20\d{2}", member_m.group(1) if len(member_m.groups()) > 0 else member_m.group(0))
            if year_str:
                year = int(year_str.group())
                extras["member_since_year"] = year
                extras["years_on_platform"] = datetime.now().year - year

        # ── Certifications ────────────────────────────────────
        for sel in ['[data-test="certification"]', '[class*="Certification"]',
                    '[class*="certification"]']:
            certs = soup.select(sel)
            if certs:
                extras["cert_count"] = len(certs)
                break
        if extras["cert_count"] == "N/A":
            extras["cert_count"] = 0

        # ── Skills on profile ─────────────────────────────────
        for sel in ['[data-test="Skill"]', '[class*="skill-tag"]',
                    'span[class*="skillName"]', '[class*="o-skill"]']:
            skills = soup.select(sel)
            if skills:
                extras["skills_count_profile"] = len(skills)
                break

        # ── Video intro ───────────────────────────────────────
        has_video = bool(
            soup.select_one('[class*="video"]') or
            soup.select_one('[class*="Video"]') or
            soup.find("video")
        )
        extras["has_video"] = int(has_video)

        # ── Overview / bio length ─────────────────────────────
        for sel in ['[data-test="description"]', '[class*="Description"]',
                    '[class*="overview"]', '[data-qa="overview"]']:
            bio = soup.select_one(sel)
            if bio:
                extras["overview_len"] = len(bio.get_text(strip=True))
                break

    except Exception as e:
        log.warning(f"  Deep scrape error for {profile_url}: {e}")
    return extras


# ─────────────────────────────────────────────────────────────
#  SAVE
# ─────────────────────────────────────────────────────────────
def save(df, label):
    out_dir = THESIS_DIR if os.path.isdir(THESIS_DIR) else os.getcwd()
    path    = os.path.join(out_dir, f"upwork_{label}_{SESSION_TAG}.csv")
    df.to_csv(path, index=False, encoding="utf-8-sig")
    log.info(f"💾 {len(df)} rows → {path}")
    return path


# ─────────────────────────────────────────────────────────────
#  MAIN
# ─────────────────────────────────────────────────────────────
def main():
    log.info("=" * 60)
    log.info(f"  Upwork Scraper | Session: {SESSION_TAG}")
    log.info(f"  Full run: {FULL_RUN} | Phase 2: {RUN_PHASE2}")
    log.info(f"  Keywords: {len(KEYWORDS)} | Pages: {PAGES}")
    log.info(f"  Expected max rows: ~{len(KEYWORDS) * PAGES * 10}")
    log.info("=" * 60)

    driver = build_driver()

    keywords_to_run = KEYWORDS if FULL_RUN else KEYWORDS[:1]
    pages_to_run    = PAGES    if FULL_RUN else 1

    # Warm-up
    log.info("🌐 Loading Upwork homepage...")
    driver.get("https://www.upwork.com")
    if not wait_for_cloudflare(driver):
        log.error("Blocked on homepage.")
        input("Press Enter to quit...")
        driver.quit()
        return
    time.sleep(random.uniform(4, 6))
    if not FULL_RUN:
        dump_snapshot(driver, "homepage")

    # ── Phase 1 ───────────────────────────────────────────────
    all_rows  = []
    total_kw  = len(keywords_to_run)

    for ki, kw in enumerate(keywords_to_run, 1):
        log.info(f"\n📌 [{ki}/{total_kw}] {kw!r}  ({KEYWORD_META.get(kw,'?')})")
        for pg in range(1, pages_to_run + 1):
            rows = scrape_search_page(driver, kw, pg)
            all_rows.extend(rows)
            if not FULL_RUN:
                dump_snapshot(driver, f"search_{kw.replace(' ', '_')}_p{pg}")
            time.sleep(random.uniform(4, 8))

        # Checkpoint every 10 keywords
        if FULL_RUN and ki % 10 == 0:
            checkpoint_df = pd.DataFrame(all_rows)
            save(checkpoint_df, f"checkpoint_kw{ki}")
            log.info(f"  💾 Checkpoint saved at keyword {ki}")

    if not all_rows:
        log.error("❌ Zero rows. Check DEBUG snapshots.")
        input("Press Enter to quit...")
        driver.quit()
        return

    df_p1 = pd.DataFrame(all_rows)
    log.info(f"\n📊 Phase 1: {len(df_p1)} rows | {df_p1['country'].nunique()} countries")
    log.info(
        f"   N/A rates — country:{(df_p1['country']=='N/A').mean():.0%}"
        f" | jss:{(df_p1['jss']=='N/A').mean():.0%}"
        f" | rate:{(df_p1['rate_usd_hr']=='N/A').mean():.0%}"
        f" | total_jobs:{(df_p1['total_jobs']=='N/A').mean():.0%}"
    )

    print("\n── Sample (first 10 rows) ──")
    print(df_p1[[
        "rank_global", "keyword", "isco_group", "country",
        "jss", "rate_usd_hr", "earnings_tier", "badge",
        "total_jobs", "hours_worked", "skills_count_card"
    ]].head(10).to_string())

    save(df_p1, "phase1_search")

    # ── Phase 2 ───────────────────────────────────────────────
    if FULL_RUN and RUN_PHASE2:
        log.info(f"\n🔍 Phase 2: visiting {df_p1['profile_url'].nunique()} profiles...")
        deep_rows = []
        urls = [u for u in df_p1["profile_url"].unique() if u != "N/A"]

        for j, url in enumerate(urls, 1):
            log.info(f"  [{j}/{len(urls)}] {url}")
            ext = deep_scrape_profile(driver, url)
            ext["profile_url"] = url
            deep_rows.append(ext)

            # Checkpoint every 50 profiles
            if j % 50 == 0:
                p2_tmp = pd.DataFrame(deep_rows)
                df_tmp = df_p1.merge(p2_tmp, on="profile_url", how="left")
                save(df_tmp, f"phase2_checkpoint_{j}")

            time.sleep(random.uniform(3, 6))

        df_p2  = pd.DataFrame(deep_rows)
        df_out = df_p1.merge(df_p2, on="profile_url", how="left")

        log.info(f"\n📊 Phase 2 complete. Final dataset: {len(df_out)} rows")
        log.info(
            f"   N/A rates — reviews:{(df_out['review_count']=='N/A').mean():.0%}"
            f" | years:{(df_out['years_on_platform']=='N/A').mean():.0%}"
        )
        save(df_out, "phase2_merged")
    else:
        if not FULL_RUN:
            log.info("\n⏩ Phase 2 skipped (FULL_RUN=False).")
        else:
            log.info("\n⏩ Phase 2 skipped (RUN_PHASE2=False).")

    log.info("\n✅ Done.")
    input("\nPress Enter to close browser...")
    driver.quit()


if __name__ == "__main__":
    main()