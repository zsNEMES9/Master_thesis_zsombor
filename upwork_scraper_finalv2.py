"""
upwork_scraper_final.py  —  Geographic Bias in Upwork Search Rankings
Master's Thesis  |  RSM Rotterdam

OUTPUT COLUMNS:
    session, keyword, isco_group, page, rank_page, rank_global,
    country, jss, total_jobs, hours_worked, rate_usd_hr,
    earnings_tier, badge, skills_count_card, bio_len_card,
    completeness_card, first_name, profile_url,
    has_photo, review_count, member_since_year, years_on_platform,
    overview_len, gender, skills_count_profile, has_video

PHASE 1 — from search card:
    search rank, country, JSS, earnings tier, total jobs, hours worked,
    badge, skills count (card), bio length, has photo,
    keyword, ISCO group, session tag
    NOTE: rate_usd_hr is NOT scraped from the card (Upwork removed it in
    2026). It is collected in Phase 2 from the profile page JS data blob.

PHASE 2 — from individual profile page (Nuxt JS data blob + HTML fallbacks):
    rate_usd_hr, has_photo (more reliable via og:image), review_count,
    member_since_year, years_on_platform, skills_count_profile,
    has_video, overview_len

GENDER — offline, via gender_guesser on first_name

MODES:
    FULL_RUN = False  →  1 keyword, 1 page, no Phase 2 (diagnostic)
    FULL_RUN = True   →  all keywords, 2 pages, Phase 2 enabled

    python upwork_scraper_final.py --test       # 1 keyword, 1 page, Phase 1+2, save CSV
    python upwork_scraper_final.py --test-url URL  # single profile smoke-test

CRASH RECOVERY:
    Phase 1 checkpointed every 10 keywords  → upwork_phase1_checkpoint_kw*.csv
    Phase 2 checkpointed every 50 profiles  → phase2_progress_*.csv
    Re-running automatically resumes from latest progress file.
    To start fully fresh: delete all phase2_progress_*.csv files.

DEPENDENCIES:
    pip install undetected-chromedriver selenium pandas beautifulsoup4 gender_guesser
"""

import os
import re
import sys
import time
import random
import logging
import argparse
import pandas as pd
import gender_guesser.detector as gender_lib
from datetime import datetime
from bs4 import BeautifulSoup
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By

try:
    import undetected_chromedriver as uc
    _UC_AVAILABLE = True
except ImportError:
    _UC_AVAILABLE = False


# ─────────────────────────────────────────────────────────────
#  CONFIG  — edit these before running
# ─────────────────────────────────────────────────────────────
THESIS_DIR       = r".\upwork_scraper_test"  # set to your own output directory
FULL_RUN         = False      # False = diagnostic (1 keyword, 1 page, no Phase 2)
PAGES            = 2          # pages per keyword in full run (~10 profiles/page)
RUN_PHASE2       = True       # set False to collect Phase 1 only
CHECKPOINT_EVERY = 50         # Phase 2 checkpoint interval (profiles)

SESSION_TAG = datetime.now().strftime("%Y%m%d_%H%M")

# ── ISCO group labels ─────────────────────────────────────────
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

# Final column order
FINAL_COLUMNS = [
    "session", "keyword", "isco_group", "page", "rank_page", "rank_global",
    "country", "jss", "total_jobs", "hours_worked", "rate_usd_hr",
    "earnings_tier", "badge", "skills_count_card", "bio_len_card",
    "completeness_card", "first_name", "profile_url",
    "has_photo", "review_count", "member_since_year", "years_on_platform",
    "overview_len", "gender", "skills_count_profile", "has_video",
]


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
#  GENDER  (offline, no API calls)
# ─────────────────────────────────────────────────────────────
_gender_detector = gender_lib.Detector(case_sensitive=False)


def genderize(first_name: str) -> str:
    """Return one of: male | female | mostly_male | mostly_female | andy | unknown"""
    if not first_name or pd.isna(first_name):
        return "unknown"
    name = str(first_name).strip().split()[0]
    name = re.sub(r"[^A-Za-zÀ-ÿ\-']", "", name)
    if not name:
        return "unknown"
    return _gender_detector.get_gender(name)


# ─────────────────────────────────────────────────────────────
#  DRIVER
# ─────────────────────────────────────────────────────────────
def build_driver():
    """
    Build undetected Chrome.
    version_main intentionally omitted — auto-detects installed Chrome,
    preventing breakage on every Chrome auto-update.
    """
    if not _UC_AVAILABLE:
        raise RuntimeError("undetected_chromedriver not installed. "
                           "Run: pip install undetected-chromedriver")
    options = uc.ChromeOptions()
    options.add_argument("--window-size=1440,900")
    options.add_argument("--no-sandbox")
    options.add_argument("--disable-dev-shm-usage")
    options.add_argument("--no-first-run")
    options.add_argument("--no-default-browser-check")
    options.add_argument(r"--user-data-dir=.\ChromeAutomationProfile")
    options.add_argument("--profile-directory=Default")
    driver = uc.Chrome(options=options, version_main=149)
    time.sleep(3)
    driver.switch_to.window(driver.window_handles[0])
    log.info("  ✅ Chrome started")
    return driver


def is_driver_alive(driver) -> bool:
    try:
        _ = driver.title
        return True
    except Exception:
        return False


# ─────────────────────────────────────────────────────────────
#  CLOUDFLARE WAIT
# ─────────────────────────────────────────────────────────────
def wait_for_cloudflare(driver, timeout: int = 60) -> bool:
    deadline = time.time() + timeout
    while time.time() < deadline:
        title = driver.title.lower()
        if "just a moment" not in title and "attention required" not in title:
            return True
        log.info("  ⏳ Cloudflare — waiting (tick checkbox if needed)...")
        time.sleep(3)
    log.warning("  🚨 Cloudflare timeout")
    return False


# ─────────────────────────────────────────────────────────────
#  DOM SNAPSHOT  (diagnostic)
# ─────────────────────────────────────────────────────────────
def dump_snapshot(driver, label: str):
    out_dir = THESIS_DIR if os.path.isdir(THESIS_DIR) else os.getcwd()
    path = os.path.join(out_dir, f"DEBUG_{label}_{SESSION_TAG}.html")
    with open(path, "w", encoding="utf-8") as f:
        f.write(driver.page_source)
    log.info(f"📄 Snapshot → {path}")


# ─────────────────────────────────────────────────────────────
#  PHASE 1 — EXTRACT FROM SEARCH CARD
#  Confirmed against live Upwork DOM 2026-03
#  NOTE: rate_usd_hr is NOT extracted here — Upwork removed it from
#  search cards in 2026. It is collected in Phase 2 from the profile page.
# ─────────────────────────────────────────────────────────────
def extract_card(card, rank: int, keyword: str, page: int) -> dict:

    # ── Country ──────────────────────────────────────────────
    el      = card.select_one("p.location")
    country = el.get_text(strip=True) if el else "N/A"

    # ── JSS ──────────────────────────────────────────────────
    el   = card.select_one('[data-test="UpI18n"]')
    raw  = el.get_text(strip=True) if el else ""
    jss  = re.sub(r"[^\d.]", "", raw.split()[0]) if raw else "N/A"

    # ── Badge ────────────────────────────────────────────────
    el  = card.select_one('[data-test="FreelancerTileTopRated"]')
    raw = el.get_text(strip=True) if el else ""
    badge = (
        "Top Rated Plus" if "Top Rated Plus" in raw else
        "Top Rated"      if "Top Rated"      in raw else
        "Rising Talent"  if "Rising Talent"  in raw else
        "N/A"
    )

    # ── Earnings tier + total jobs + hours worked ─────────────
    el       = card.select_one('[data-test="freelancer-tile-earnings"]')
    earn_raw = el.get_text(strip=True) if el else ""

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

    # ── Skills count (card) ───────────────────────────────────
    el         = card.select_one('[data-test="FreelancerTileSkills"]')
    skills_raw = el.get_text(strip=True) if el else ""
    skills_clean = re.sub(r"Start of list\.|End of list\.", "", skills_raw).strip()
    extra_m    = re.search(r"\+(\d+)", skills_clean)
    extra_n    = int(extra_m.group(1)) if extra_m else 0
    visible_text = re.sub(r"\+\d+$", "", skills_clean).strip()
    skills_visible = max(1, round(len(visible_text) / 12)) if visible_text else 0
    skills_count_card = skills_visible + extra_n

    # ── Bio snippet length (from card) ────────────────────────
    el      = card.select_one('[data-test="UpCLineClamp"]')
    bio_len = len(el.get_text(strip=True)) if el else 0

    # ── Has profile photo (from card — overridden by Phase 2) ─
    avatar    = card.select_one('[data-test="FreelancerAvatar"] img')
    has_photo = int(
        avatar is not None and
        "profile-portraits" in (avatar.get("src") or "")
    )

    # ── Name + first name ─────────────────────────────────────
    name_el    = card.select_one('[data-test="UpLineClamp"]')
    name       = name_el.get_text(strip=True) if name_el else "N/A"
    first_name = name.split()[0].rstrip(".") if name != "N/A" else "N/A"

    # ── Profile URL ───────────────────────────────────────────
    link = card.select_one('a[href*="/freelancers/"]')
    if link:
        href = link["href"]
        profile_url = href if href.startswith("http") else "https://www.upwork.com" + href
    else:
        profile_url = "N/A"

    # ── Profile completeness score (from card, 0–5) ───────────
    completeness_card = (
        has_photo +
        int(bio_len > 50) +
        int(skills_count_card > 3) +
        int(badge != "N/A") +
        int(earnings_tier != "N/A")
    )

    return {
        "session"          : SESSION_TAG,
        "keyword"          : keyword,
        "isco_group"       : KEYWORD_META.get(keyword, "N/A"),
        "page"             : page,
        "rank_page"        : rank,
        "rank_global"      : (page - 1) * 10 + rank,
        "country"          : country,
        "jss"              : jss,
        "total_jobs"       : total_jobs,
        "hours_worked"     : hours_worked,
        "rate_usd_hr"      : None,       # collected in Phase 2 from profile page JS
        "earnings_tier"    : earnings_tier,
        "badge"            : badge,
        "skills_count_card": skills_count_card,
        "bio_len_card"     : bio_len,
        "completeness_card": completeness_card,
        "first_name"       : first_name,
        "profile_url"      : profile_url,
        # Phase 2 placeholders — filled after profile scrape
        "has_photo"           : has_photo,   # overwritten by Phase 2
        "review_count"        : None,
        "member_since_year"   : None,
        "years_on_platform"   : None,
        "overview_len"        : None,
        "gender"              : None,
        "skills_count_profile": None,
        "has_video"           : None,
    }


# ─────────────────────────────────────────────────────────────
#  PHASE 1 — SCRAPE SEARCH PAGE
# ─────────────────────────────────────────────────────────────
def scrape_search_page(driver, keyword: str, page: int) -> list:
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
#  PHASE 2 — NUXT JS HELPERS
# ─────────────────────────────────────────────────────────────

def _find_nuxt_js(soup: BeautifulSoup) -> str:
    """
    Return the largest <script> block containing Upwork profile data.
    Tries multiple fingerprint pairs so we fall back gracefully if the
    primary key name changes between Upwork deployments.
    """
    FINGERPRINTS = [
        ("memberSince", "totalHoursActual"),
        ("memberSince", "skills"),
        ("hourlyRate",  "memberSince"),
        ("hourlyRate",  "skills"),
        ("memberSince", "certifications"),
        ("freelancerProfile", "skills"),
        ("profileData", "skills"),
        ("upworkProfile", "certifications"),
        ("memberSince",),
        ("hourlyRate",),
    ]
    candidates = []
    for script in soup.find_all("script"):
        t = script.get_text()
        for prints in FINGERPRINTS:
            if all(fp in t for fp in prints):
                candidates.append((len(t), t))
                break
    if candidates:
        candidates.sort(reverse=True)
        return candidates[0][1]
    return ""


def _get_hourly_rate(nuxt_js: str, soup: BeautifulSoup):
    """
    Extract hourly rate in USD from the profile page JS data blob.

    Current Upwork JS schema (confirmed 2026-06):
        hourlyRate:{amount:50,currencyCode:"USD"}     ← inline integer
        hourlyRate:{amount:"50.00",currencyCode:...}  ← quoted string
        hourlyRate:{amount:cb,currencyCode:...}        ← variable ref (skip)

    Returns float (e.g. 45.0) or None if the freelancer has no rate set.
    """
    if nuxt_js:
        # Pattern 1 — inline integer/float: hourlyRate:{amount:50,...}
        m = re.search(
            r'hourlyRate\s*:\s*\{[^}]{0,60}amount\s*:\s*([\d.]+)', nuxt_js
        )
        if m:
            try:
                return float(m.group(1))
            except ValueError:
                pass

        # Pattern 2 — quoted string: hourlyRate:{amount:"50.00",...}
        m = re.search(
            r'hourlyRate\s*:\s*\{[^}]{0,60}amount\s*:\s*"([\d.]+)"', nuxt_js
        )
        if m:
            try:
                return float(m.group(1))
            except ValueError:
                pass

        # Pattern 3 — JSON-style: "hourlyRate":{"amount":50,...}
        m = re.search(
            r'"hourlyRate"\s*:\s*\{[^}]{0,60}"amount"\s*:\s*"?([\d.]+)"?',
            nuxt_js,
        )
        if m:
            try:
                return float(m.group(1))
            except ValueError:
                pass

        # Pattern 4 — hourlyChargeRate variant
        m = re.search(
            r'hourlyChargeRate\s*:\s*\{[^}]{0,60}amount\s*:\s*"?([\d.]+)"?',
            nuxt_js,
        )
        if m:
            try:
                return float(m.group(1))
            except ValueError:
                pass

        # Pattern 5 — flat chargeRate: chargeRate:"50.00" or chargeRate:50
        m = re.search(r'chargeRate\s*:\s*"?([\d.]+)"?', nuxt_js)
        if m:
            try:
                val = float(m.group(1))
                if val > 0:
                    return val
            except ValueError:
                pass

    # ── HTML fallbacks ────────────────────────────────────────
    for selector in [
        '[data-test="hourly-rate"]',
        '[data-test="rate"]',
        '[data-test="freelancer-rate"]',
        'p.rate',
        '[class*="hourlyRate"]',
        '[class*="HourlyRate"]',
        '[itemprop="priceRange"]',
    ]:
        el = soup.select_one(selector)
        if el:
            m = re.search(r'([\d.]+)', el.get_text())
            if m:
                try:
                    val = float(m.group(1))
                    if val > 0:
                        return val
                except ValueError:
                    pass

    # Last resort — "$XX/hr" anywhere in visible text
    m = re.search(r'\$([\d.]+)\s*/\s*hr', soup.get_text(), re.IGNORECASE)
    if m:
        try:
            return float(m.group(1))
        except ValueError:
            pass

    return None


def _count_skills(nuxt_js: str, soup: BeautifulSoup) -> int:
    """
    Count profile skills from JS data blob, with HTML fallbacks.

    Current Upwork JS schema (confirmed 2026-04):
        skills:[{uid:VAR, id:VAR, name:"wordpress", prettyName:VAR,
                 certificates:[], rank:N, ...}, ...]
    """
    def _extract_array(js: str, key: str) -> str:
        m = re.search(key + r':\[', js)
        if not m:
            return ""
        start = m.end() - 1
        depth = 0
        for i in range(start, min(start + 50_000, len(js))):
            if js[i] == '[':
                depth += 1
            elif js[i] == ']':
                depth -= 1
                if depth == 0:
                    return js[start:i + 1]
        return ""

    if nuxt_js:
        skills_arr = _extract_array(nuxt_js, 'skills')
        if skills_arr:
            # Primary: one 'certificates:[]' per skill object
            count = len(re.findall(r'certificates:\[\]', skills_arr))
            if count:
                return count
            # Fallback within array: one ',rank:' per skill object
            count = len(re.findall(r',rank:', skills_arr))
            if count:
                return count

        # Legacy ontology skills: prefLabel="Skill Name"
        pref_labels = re.findall(r'prefLabel="([^"]{2,80})"', nuxt_js)
        if pref_labels:
            return len(pref_labels)

    # HTML fallbacks
    for selector in [
        '[data-test="FreelancerSkillsList"] li',
        '[data-test="skills-section"] li',
        ".skills-section li",
        '[aria-label*="skill" i]',
        ".air3-token",
    ]:
        els = soup.select(selector)
        if els:
            return len(els)

    return 0


def _has_video(nuxt_js: str, soup: BeautifulSoup) -> int:
    """
    Detect profile intro video or portfolio video.

    Current Upwork JS schema (confirmed 2026-06):
        video:"https://..."            ← intro video (plain URL)
        video:"https:\\u002F\\u002F..."  ← intro video (unicode-escaped)
        videoUrl:"https://..."         ← portfolio attachment (non-null)
    """
    JS_PATTERNS = [
        r'(?<!\w)video:"https',
        r'(?<!\w)video:"https:\\u002F\\u002F',
        r'videoUrl:"https',
        # Legacy fallbacks
        r'videoIntro\s*:\s*"https',
        r'"videoIntro"\s*:\s*"https',
        r'hasVideoIntro\s*:\s*true',
        r'"hasVideoIntro"\s*:\s*true',
        r'introVideo\s*:\s*"https',
        r'"introVideo"\s*:\s*"https',
        r'profileVideo\s*:\s*"https',
        r'"profileVideo"\s*:\s*"https',
        r'"videoUrl"\s*:\s*"https',
        r'"videoEnabled"\s*:\s*true',
    ]
    for pat in JS_PATTERNS:
        if re.search(pat, nuxt_js):
            return 1

    # HTML fallbacks
    if soup.find("video"):
        return 1
    for selector in [
        '[data-test="intro-video"]',
        ".profile-video",
        '[aria-label*="intro video" i]',
    ]:
        if soup.select_one(selector):
            return 1

    return 0


# ─────────────────────────────────────────────────────────────
#  PHASE 2 — WAIT FOR PROFILE LOAD (JS hydration guard)
# ─────────────────────────────────────────────────────────────
_PROFILE_READY_SELECTORS = [
    "h1[itemprop='name']",
    "[data-test='up-talent-profile']",
    ".profile-title",
    "h1.mb-0",
]


def _wait_for_profile_load(driver, timeout: int = 8):
    """
    Wait until a known profile element appears in the DOM.
    Ensures the Nuxt JS data blob is present before reading page_source,
    fixing intermittent field misses caused by reading before JS hydration.
    """
    combined = ", ".join(_PROFILE_READY_SELECTORS)
    try:
        WebDriverWait(driver, timeout).until(
            EC.presence_of_element_located((By.CSS_SELECTOR, combined))
        )
    except Exception:
        pass  # page may still be usable — extra sleep below provides buffer


# ─────────────────────────────────────────────────────────────
#  PHASE 2 — PARSE HTML  (pure, no browser dependency)
# ─────────────────────────────────────────────────────────────
def _parse_profile_html(html: str, profile_url: str = "") -> dict:
    """
    Extract all Phase 2 variables from raw HTML.
    Separated from browser logic so it can be called on saved HTML
    in tests without a live Chrome session.
    """
    result = {
        "profile_url"         : profile_url,
        "rate_usd_hr"         : None,
        "has_photo"           : 0,
        "review_count"        : 0,
        "member_since_year"   : None,
        "years_on_platform"   : None,
        "skills_count_profile": 0,
        "has_video"           : 0,
        "overview_len"        : None,
    }

    soup    = BeautifulSoup(html, "html.parser")
    text    = soup.get_text()
    nuxt_js = _find_nuxt_js(soup)

    if not nuxt_js:
        log.warning("  ⚠️  No Nuxt JS block found — HTML-only fallbacks used")

    # 1. rate_usd_hr — from JS data blob (Upwork removed it from search cards in 2026)
    result["rate_usd_hr"] = _get_hourly_rate(nuxt_js, soup)

    # 2. has_photo — og:image is the most reliable source
    og_img = soup.find("meta", property="og:image")
    if og_img:
        img_url = og_img.get("content", "")
        result["has_photo"] = int("profile-portraits" in img_url)
    elif nuxt_js:
        m = re.search(r'portrait\s*:\s*"(https[^"]+)"', nuxt_js)
        if not m:
            m = re.search(r'"portrait"\s*:\s*"(https[^"]+)"', nuxt_js)
        result["has_photo"] = int(
            bool(m) and "profile-portraits" in (m.group(1) if m else "")
        )

    # 3. review_count — count "Rating is X out of 5" occurrences
    result["review_count"] = len(re.findall(r"Rating is [\d.]+ out of 5", text))

    # 4. member_since_year + years_on_platform
    if nuxt_js:
        for pat in [
            r'memberSince\s*:\s*"(\d{4})',
            r'"memberSince"\s*:\s*"(\d{4})',
            r'memberSince\s*:\s*(\d{4})',
        ]:
            m = re.search(pat, nuxt_js)
            if m:
                year = int(m.group(1))
                result["member_since_year"]  = year
                result["years_on_platform"] = datetime.now().year - year
                break

    # 5. skills_count_profile
    result["skills_count_profile"] = _count_skills(nuxt_js, soup)

    # 6. has_video
    result["has_video"] = _has_video(nuxt_js, soup)

    # 7. overview_len
    bio_el = soup.select_one("div.d-flex.justify-space-between")
    if bio_el:
        result["overview_len"] = len(bio_el.get_text(strip=True))
    else:
        for el in soup.find_all("section", class_="air3-card-section"):
            t = el.get_text(strip=True)
            if len(t) > 150 and not t.startswith("Work history"):
                result["overview_len"] = len(t)
                break

    return result


# ─────────────────────────────────────────────────────────────
#  PHASE 2 — SCRAPE ONE PROFILE
# ─────────────────────────────────────────────────────────────
def scrape_profile(driver, profile_url: str) -> dict:
    """Navigate to a profile page and return parsed Phase 2 fields."""
    blank = {
        "profile_url"         : profile_url,
        "rate_usd_hr"         : None,
        "has_photo"           : None,
        "review_count"        : None,
        "member_since_year"   : None,
        "years_on_platform"   : None,
        "skills_count_profile": None,
        "has_video"           : None,
        "overview_len"        : None,
    }
    if not profile_url or profile_url == "N/A" or not profile_url.startswith("http"):
        return blank

    try:
        driver.get(profile_url)

        if not wait_for_cloudflare(driver):
            log.warning("  Cloudflare block — skipping profile")
            return blank

        # Wait for JS hydration before reading page_source
        _wait_for_profile_load(driver, timeout=8)
        time.sleep(random.uniform(1, 2))

        result = _parse_profile_html(driver.page_source, profile_url)
        return result

    except Exception as e:
        log.warning(f"  Error scraping {profile_url}: {e}")
        return blank


# ─────────────────────────────────────────────────────────────
#  SAVE
# ─────────────────────────────────────────────────────────────
def save(df: pd.DataFrame, label: str) -> str:
    out_dir = THESIS_DIR if os.path.isdir(THESIS_DIR) else os.getcwd()
    path    = os.path.join(out_dir, f"upwork_{label}_{SESSION_TAG}.csv")
    df.to_csv(path, index=False, encoding="utf-8-sig")
    log.info(f"💾 {len(df)} rows → {path}")
    return path


def _reorder_columns(df: pd.DataFrame) -> pd.DataFrame:
    """Reorder to match FINAL_COLUMNS; drop any unrecognised extras."""
    present = [c for c in FINAL_COLUMNS if c in df.columns]
    return df[present]


# ─────────────────────────────────────────────────────────────
#  MERGE PHASE 1 + PHASE 2 + GENDER
# ─────────────────────────────────────────────────────────────
def merge_and_finalise(df_p1: pd.DataFrame, p2_rows: list) -> pd.DataFrame:
    """
    Merge Phase 2 results into the Phase 1 dataframe, assign gender,
    and return the final dataframe in canonical column order.
    """
    # All columns sourced from Phase 2 (these replace the Phase 1 placeholders)
    p2_cols = [
        "rate_usd_hr", "has_photo", "review_count", "member_since_year",
        "years_on_platform", "skills_count_profile", "has_video", "overview_len",
    ]
    df_base = df_p1.drop(columns=[c for c in p2_cols if c in df_p1.columns],
                         errors="ignore")

    p2_df  = pd.DataFrame(p2_rows)
    df_out = df_base.merge(p2_df, on="profile_url", how="left")

    # Gender from first_name
    if "first_name" in df_out.columns:
        df_out["gender"] = df_out["first_name"].apply(genderize)
        counts = df_out["gender"].value_counts()
        log.info("Gender distribution:\n" + counts.to_string())
    else:
        df_out["gender"] = "unknown"

    return _reorder_columns(df_out)


# ─────────────────────────────────────────────────────────────
#  TEST MODE  — 1 keyword × 1 page, full Phase 1 + Phase 2
# ─────────────────────────────────────────────────────────────
def run_quick_test():
    """
    Scrapes 1 page (up to 10 profiles) of the first keyword with full
    Phase 1 + Phase 2 (including hourly rate from JS).
    Prints a fill-rate report and saves upwork_test_*.csv to THESIS_DIR.

    Usage:  python upwork_scraper_final.py --test
    """
    SEP = "=" * 70
    print(f"\n{SEP}")
    print("  TEST MODE  |  1 keyword × 1 page — Phase 1 + Phase 2")
    print(SEP)

    driver = build_driver()
    try:
        driver.get("https://www.upwork.com")
        if not wait_for_cloudflare(driver):
            print("❌ Blocked on homepage.")
            return
        time.sleep(random.uniform(4, 6))

        kw = KEYWORDS[0]
        log.info(f"\n📌 Keyword: {kw!r}  ({KEYWORD_META[kw]})")

        # ── Phase 1 ──────────────────────────────────────────
        rows = scrape_search_page(driver, kw, page=1)
        if not rows:
            print("❌ No cards scraped. Check DEBUG snapshot.")
            return

        df_p1 = pd.DataFrame(rows)
        print(f"\n── Phase 1: {len(df_p1)} profiles ──")
        print(df_p1[["rank_global", "country", "jss", "badge",
                      "total_jobs", "skills_count_card"]].to_string())

        # ── Phase 2 ──────────────────────────────────────────
        log.info("\n🔍 Phase 2: scraping individual profiles...")
        p2_rows = []
        urls    = [r["profile_url"] for r in rows if r["profile_url"] != "N/A"]

        for j, url in enumerate(urls, 1):
            log.info(f"  [{j}/{len(urls)}] {url}")
            p2_rows.append(scrape_profile(driver, url))
            time.sleep(random.uniform(2, 4))

        # ── Merge + gender ────────────────────────────────────
        df_final = merge_and_finalise(df_p1, p2_rows)

        # ── Fill-rate report ──────────────────────────────────
        check_cols = [
            "rate_usd_hr", "has_photo", "review_count", "member_since_year",
            "years_on_platform", "skills_count_profile", "has_video",
            "overview_len", "gender",
        ]
        print("\n── Fill rates ──")
        for col in check_cols:
            if col in df_final.columns:
                filled  = df_final[col].notna()
                nonzero = (df_final[col] != 0).sum() if col != "gender" else "—"
                print(f"  {col:25s}  fill={filled.sum()}/{len(df_final)}"
                      f" ({filled.mean():.0%})  nonzero={nonzero}")

        print("\n── Final data (all columns) ──")
        print(df_final.to_string())

        path = save(df_final, "test")
        print(f"\n✅ Test complete → {path}")

    finally:
        try:
            driver.quit()
        except Exception:
            pass


# ─────────────────────────────────────────────────────────────
#  SINGLE-URL SMOKE TEST  — diagnostic for one profile URL
# ─────────────────────────────────────────────────────────────
def run_url_test(url: str):
    """
    Fetch one profile URL and print all extracted values plus JS
    diagnostics for any field that returned 0 / None.

    Usage:
        python upwork_scraper_final.py --test-url https://www.upwork.com/freelancers/~...
    """
    SEP = "=" * 70
    print(f"\n{SEP}")
    print(f"  SINGLE-URL TEST  |  {url}")
    print(SEP)

    driver = build_driver()
    try:
        print("\n🌐 Loading homepage (Cloudflare warm-up)...")
        driver.get("https://www.upwork.com")
        wait_for_cloudflare(driver)
        time.sleep(random.uniform(3, 5))

        print(f"\n🌐 Fetching: {url}")
        driver.get(url)
        wait_for_cloudflare(driver)

        print("  ⏳ Waiting for profile to fully load...")
        _wait_for_profile_load(driver, timeout=15)
        time.sleep(random.uniform(2, 4))

        html    = driver.page_source
        soup    = BeautifulSoup(html, "html.parser")
        nuxt_js = _find_nuxt_js(soup)
        result  = _parse_profile_html(html, url)

        # Save raw HTML
        html_path = os.path.join(
            THESIS_DIR if os.path.isdir(THESIS_DIR) else os.getcwd(),
            f"test_profile_{SESSION_TAG}.html"
        )
        with open(html_path, "w", encoding="utf-8") as f:
            f.write(html)
        print(f"\n💾 Raw HTML saved → {html_path}")

        # JS block overview
        print(f"\n🧩 JS block: {'FOUND' if nuxt_js else '❌ NOT FOUND — HTML fallbacks only'}")
        if nuxt_js:
            print(f"   Length: {len(nuxt_js):,} chars")
            print(f"   First 400 chars:")
            print("   " + nuxt_js[:400].replace("\n", " "))

        # Extracted values
        print(f"\n{'─'*70}")
        print("📋 EXTRACTED VALUES")
        print(f"{'─'*70}")
        for k, v in result.items():
            if k == "profile_url":
                continue
            flag = "  ⚠️  ZERO/NULL" if (v is None or v == 0) else ""
            print(f"  {k:25s}: {v}{flag}")

        # Diagnostics for zero/null fields
        _DIAG_FIELDS = [
            (
                "rate_usd_hr",
                ["hourlyRate", "hourlyChargeRate", "chargeRate", "amount"],
            ),
            (
                "skills_count_profile",
                ["tagName", "prettyName", "skillName", "skillUid",
                 '"skills"', "label", "skill", "Skill"],
            ),
            (
                "has_video",
                ["video", "Video", "VIDEO", "intro", "Intro", "youtube", "vimeo"],
            ),
            (
                "member_since_year",
                ["memberSince", "MemberSince", "member_since"],
            ),
            (
                "review_count",
                ["Rating is", "totalFeedback", "feedbackCount", "reviews"],
            ),
        ]

        any_broken = False
        for field, kw_hints in _DIAG_FIELDS:
            if result.get(field) is None or result.get(field) == 0:
                any_broken = True
                print(f"\n{'─'*70}")
                print(f"🔍 DIAGNOSTICS: {field}  (= {result.get(field)})")
                print(f"{'─'*70}")
                if not nuxt_js:
                    print("  No JS block — check saved HTML file.")
                    continue
                for kw in kw_hints:
                    idx = nuxt_js.find(kw)
                    if idx != -1:
                        snippet = nuxt_js[max(0, idx - 60): idx + 300]
                        print(f"  keyword='{kw}'")
                        print("  " + "-" * 50)
                        print("  " + snippet.replace("\n", " ")[:400])
                        print()
                    else:
                        print(f"  ❌ '{kw}' not found in JS block")

        if not any_broken:
            print("\n✅ All fields non-zero — patterns working!")

        print(f"\n{SEP}\n✅ URL test complete.\n{SEP}\n")

    finally:
        try:
            driver.quit()
        except Exception:
            pass


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
            df_ck = pd.DataFrame(all_rows)
            save(df_ck, f"phase1_checkpoint_kw{ki}")
            log.info(f"  💾 Phase 1 checkpoint @ keyword {ki}")

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
        f" | total_jobs:{(df_p1['total_jobs']=='N/A').mean():.0%}"
    )

    print("\n── Phase 1 sample (first 10 rows) ──")
    print(df_p1[[
        "rank_global", "keyword", "isco_group", "country",
        "jss", "earnings_tier", "badge",
        "total_jobs", "hours_worked", "skills_count_card",
    ]].head(10).to_string())

    save(df_p1, "phase1_search")

    # ── Phase 2 ───────────────────────────────────────────────
    if not (FULL_RUN and RUN_PHASE2):
        reason = "FULL_RUN=False" if not FULL_RUN else "RUN_PHASE2=False"
        log.info(f"\n⏩ Phase 2 skipped ({reason}).")
        input("\nPress Enter to close browser...")
        driver.quit()
        return

    log.info(f"\n🔍 Phase 2: {df_p1['profile_url'].nunique()} profiles to visit...")

    all_urls = [
        u for u in df_p1["profile_url"].unique()
        if u and u != "N/A" and str(u).startswith("http")
    ]

    # ── Resume from checkpoint if one exists ──────────────────
    out_dir       = THESIS_DIR if os.path.isdir(THESIS_DIR) else os.getcwd()
    scraped_urls  = set()
    progress_rows = []
    progress_file = os.path.join(out_dir, f"phase2_progress_{SESSION_TAG}.csv")

    existing_ckpts = sorted(
        [f for f in os.listdir(out_dir)
         if f.startswith("phase2_progress_") and f.endswith(".csv")],
        reverse=True
    )
    if existing_ckpts:
        resume_path = os.path.join(out_dir, existing_ckpts[0])
        log.info(f"Found checkpoint: {existing_ckpts[0]}")
        prog_df       = pd.read_csv(resume_path, encoding="utf-8-sig")
        progress_rows = prog_df.to_dict("records")
        scraped_urls  = set(prog_df["profile_url"].tolist())
        log.info(f"Resuming — {len(scraped_urls)} already done")

    urls_to_scrape = [u for u in all_urls if u not in scraped_urls]
    log.info(f"Remaining: {len(urls_to_scrape)} profiles")

    total = len(urls_to_scrape)
    for j, url in enumerate(urls_to_scrape, 1):
        log.info(f"  [{j}/{total}] {url}")

        # Auto-restart if Chrome has died
        if not is_driver_alive(driver):
            log.warning("  ⚠️  Chrome dead — restarting...")
            try:
                driver.quit()
            except Exception:
                pass
            time.sleep(5)
            driver = build_driver()
            driver.get("https://www.upwork.com")
            wait_for_cloudflare(driver)
            time.sleep(random.uniform(3, 5))
            log.info("  ✅ Restarted")

        row = scrape_profile(driver, url)
        progress_rows.append(row)

        if j % CHECKPOINT_EVERY == 0:
            pd.DataFrame(progress_rows).to_csv(
                progress_file, index=False, encoding="utf-8-sig"
            )
            log.info(f"  💾 Phase 2 checkpoint: {j}/{total}")

        time.sleep(random.uniform(1, 2))

    # Final progress save
    pd.DataFrame(progress_rows).to_csv(
        progress_file, index=False, encoding="utf-8-sig"
    )
    log.info(f"💾 Phase 2 complete: {len(progress_rows)} profiles scraped")

    try:
        driver.quit()
    except Exception:
        pass

    # ── Merge + finalise ──────────────────────────────────────
    log.info("\n🔀 Merging Phase 1 + Phase 2...")
    df_final = merge_and_finalise(df_p1, progress_rows)

    log.info(f"\n📊 Final: {len(df_final)} rows | {len(df_final.columns)} columns")
    log.info("Fill rates:")
    check_cols = [
        "rate_usd_hr", "has_photo", "review_count", "member_since_year",
        "years_on_platform", "skills_count_profile", "has_video",
        "overview_len", "gender",
    ]
    for col in check_cols:
        if col in df_final.columns:
            filled  = df_final[col].notna()
            nonzero = (df_final[col] != 0).sum() if col != "gender" else "—"
            log.info(
                f"  {col:25s}  fill={filled.sum()}/{len(df_final)}"
                f" ({filled.mean():.0%})  nonzero={nonzero}"
            )

    save(df_final, "phase2_merged")
    log.info("\n✅ Done. Use upwork_phase2_merged_*.csv for R analysis.")
    input("\nPress Enter to close browser...")


# ─────────────────────────────────────────────────────────────
#  ENTRY POINT
# ─────────────────────────────────────────────────────────────
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Upwork Scraper — Thesis RSM")
    parser.add_argument(
        "--test",
        action="store_true",
        help="Quick test: 1 keyword × 1 page, full Phase 1 + Phase 2, save CSV"
    )
    parser.add_argument(
        "--test-url",
        metavar="URL",
        default=None,
        help="Single-URL diagnostic: scrape one profile and print all fields "
             "+ JS diagnostics for zero/null values"
    )
    args = parser.parse_args()

    if args.test:
        run_quick_test()
    elif args.test_url:
        run_url_test(args.test_url)
    else:
        main()
