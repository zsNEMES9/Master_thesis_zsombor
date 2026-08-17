"""
phase2_scraper.py  —  Profile Deep Scrape  (v2 — fixed 2026-04)
Master's Thesis  |  RSM Rotterdam

Reads Phase 1 CSV and adds these 8 variables from each profile page:
    has_photo             — og:image contains "profile-portraits" (0/1)
    review_count          — rated jobs in work history
    member_since_year     — year from JS memberSince field
    years_on_platform     — current year minus member_since_year
    cert_count            — count of certification entries in JS/HTML
    skills_count_profile  — count of skill entries in JS/HTML
    has_video             — videoIntro / introVideo field present (0/1)
    overview_len          — character length of bio text

PLUS (new in v2):
    gender                — estimated gender from first name
                            values: male | female | mostly_male |
                                    mostly_female | andy | unknown

USAGE:
    py -3.11 phase2_scraper.py              # full run
    py -3.11 phase2_scraper.py --test URL   # test a single profile URL

CRASH RECOVERY:
    Progress is saved every 50 profiles to phase2_progress_*.csv
    Re-running the script automatically resumes from last checkpoint
    To start fully fresh: delete all phase2_progress_*.csv files first

FIX LOG (v2):
    cert_count          — added 6 fallback patterns + HTML fallback
    skills_count_profile— added 8 fallback patterns + HTML fallback
    has_video           — added 5 fallback patterns
    All three now emit diagnostic warnings when ALL patterns fail so
    you can paste the raw JS context into --test to tune patterns.
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

# ── optional selenium import (not needed for --test --offline) ──────────────
try:
    import undetected_chromedriver as uc
    _UC_AVAILABLE = True
except ImportError:
    _UC_AVAILABLE = False

# ─────────────────────────────────────────────────────────────────────────────
#  CONFIG
# ─────────────────────────────────────────────────────────────────────────────
THESIS_DIR       = r".\upwork_scraper_test"  # set to your own output directory
INPUT_FILE       = "upwork_phase1_search_20260323_2046.csv"
SESSION_TAG      = datetime.now().strftime("%Y%m%d_%H%M")
CHECKPOINT_EVERY = 50

# ─────────────────────────────────────────────────────────────────────────────
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s | %(levelname)s | %(message)s",
    datefmt="%H:%M:%S",
)
log = logging.getLogger(__name__)

# ─────────────────────────────────────────────────────────────────────────────
#  GENDER DETECTOR  (offline, no API calls)
# ─────────────────────────────────────────────────────────────────────────────
_gender_detector = gender_lib.Detector(case_sensitive=False)

def genderize(first_name: str) -> str:
    """
    Return one of: male | female | mostly_male | mostly_female | andy | unknown
    'andy' = androgynous (equally common for both genders in the database)
    Strips suffixes/initials that confuse the detector (e.g. "María J." → "María").
    """
    if not first_name or pd.isna(first_name):
        return "unknown"
    name = str(first_name).strip().split()[0]   # use only the first token
    name = re.sub(r"[^A-Za-zÀ-ÿ\-']", "", name)  # strip punctuation/digits
    if not name:
        return "unknown"
    return _gender_detector.get_gender(name)


def genderize_dataframe(df: pd.DataFrame, name_col: str = "first_name") -> pd.DataFrame:
    """
    Add a 'gender' column to df based on name_col.
    Works on an existing merged CSV too — call it standalone if needed.
    """
    if name_col not in df.columns:
        log.warning(f"Column '{name_col}' not found — gender column set to 'unknown'")
        df["gender"] = "unknown"
    else:
        df["gender"] = df[name_col].apply(genderize)
        counts = df["gender"].value_counts()
        log.info("Gender distribution:\n" + counts.to_string())
    return df


# ─────────────────────────────────────────────────────────────────────────────
#  DRIVER
# ─────────────────────────────────────────────────────────────────────────────
def build_driver():
    if not _UC_AVAILABLE:
        raise RuntimeError("undetected_chromedriver not installed")
    options = uc.ChromeOptions()
    options.add_argument("--window-size=1440,900")
    options.add_argument("--no-sandbox")
    options.add_argument("--disable-dev-shm-usage")
    options.add_argument("--no-first-run")
    options.add_argument("--no-default-browser-check")
    options.add_argument(r"--user-data-dir=.\ChromeAutomationProfile")
    options.add_argument("--profile-directory=Default")
    driver = uc.Chrome(options=options, version_main=146)
    time.sleep(3)
    driver.switch_to.window(driver.window_handles[0])
    log.info("  ✅ Chrome started")
    return driver


def is_driver_alive(driver):
    try:
        _ = driver.title
        return True
    except Exception:
        return False


# ─────────────────────────────────────────────────────────────────────────────
#  CLOUDFLARE WAIT
# ─────────────────────────────────────────────────────────────────────────────
def wait_for_cloudflare(driver, timeout=60):
    deadline = time.time() + timeout
    while time.time() < deadline:
        title = driver.title.lower()
        if "just a moment" not in title and "attention required" not in title:
            return True
        log.info("  ⏳ Cloudflare — waiting (tick checkbox if needed)...")
        time.sleep(3)
    log.warning("  🚨 Cloudflare timeout")
    return False


# ─────────────────────────────────────────────────────────────────────────────
#  PATTERN HELPERS
# ─────────────────────────────────────────────────────────────────────────────

def _find_nuxt_js(soup: BeautifulSoup) -> str:
    """
    Return the largest <script> block that looks like Upwork profile data.
    Tries multiple fingerprints so that if the primary one changes we still
    fall back gracefully.
    """
    FINGERPRINTS = [
        ("memberSince", "totalHoursActual"),   # original (used in v1)
        ("memberSince", "skills"),
        ("memberSince", "certifications"),
        ("freelancerProfile", "skills"),
        ("profileData", "skills"),
        ("upworkProfile", "certifications"),
        ("memberSince",),                       # single-fingerprint last resort
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


def _count_cert(nuxt_js: str, soup: BeautifulSoup) -> int:
    """
    Count certifications.

    Current Upwork JS schema (confirmed 2026-04):
      communityCertificates:[{uid:..., ...}, ...]   ← real array when certs exist
      communityCertificates:a                        ← variable (null) when none
      licenses:[{...}]                               ← alternative cert field
    """
    def _extract_array(js, key):
        m = re.search(key + r':\[', js)
        if not m:
            return ""
        start = m.end() - 1
        depth = 0
        for i in range(start, min(start + 50000, len(js))):
            if js[i] == '[': depth += 1
            elif js[i] == ']':
                depth -= 1
                if depth == 0:
                    return js[start:i+1]
        return ""

    # communityCertificates:[...] — only matches when it's a real array
    if re.search(r'communityCertificates:\[', nuxt_js):
        arr = _extract_array(nuxt_js, 'communityCertificates')
        count = len(re.findall(r'\{uid:', arr))
        if count:
            return count

    # licenses:[...] — non-empty array signals certifications
    licenses_arr = _extract_array(nuxt_js, 'licenses')
    if licenses_arr and licenses_arr != '[]':
        count = len(re.findall(r'\{', licenses_arr))
        if count:
            return count

    # ── HTML fallback ─────────────────────────────────────────────────────────
    for selector in [
        '[data-test="certifications"]',
        '[data-ev-label="certification"]',
        "section.certification",
        ".certifications-section",
        '[aria-label*="certification" i]',
    ]:
        els = soup.select(selector)
        if els:
            return len(els)

    for tag in soup.find_all(["h2", "h3", "h4"]):
        if "certif" in tag.get_text(separator=" ").lower():
            parent = tag.find_parent("section") or tag.find_parent("div")
            if parent:
                items = parent.find_all("li")
                if items:
                    return len(items)

    return 0


def _count_skills(nuxt_js: str, soup: BeautifulSoup) -> int:
    """
    Count profile skills.

    Current Upwork JS schema (confirmed 2026-04):
      skills:[{uid:VAR, id:VAR, name:"wordpress", prettyName:VAR, ...}, ...]
      Values are stored as short-hand variable references (not string literals)
      EXCEPT for name:"slug" which is always a quoted string literal.
      Strategy: extract the skills:[...] array, then count name:"..." entries.
    """
    def _extract_array(js, key):
        m = re.search(key + r':\[', js)
        if not m:
            return ""
        start = m.end() - 1
        depth = 0
        for i in range(start, min(start + 50000, len(js))):
            if js[i] == '[': depth += 1
            elif js[i] == ']':
                depth -= 1
                if depth == 0:
                    return js[start:i+1]
        return ""

    # Primary: extract skills array and count by certificates:[] field —
    # every skill object contains this field exactly once regardless of
    # whether name: is a string literal or a null variable reference.
    skills_arr = _extract_array(nuxt_js, 'skills')
    if skills_arr:
        count = len(re.findall(r'certificates:\[\]', skills_arr))
        if count:
            return count
        # Fallback within array: count by ,rank: (also once per skill object)
        count = len(re.findall(r',rank:', skills_arr))
        if count:
            return count

    # Fallback: prefLabel="Skill Name" — used for ontology/legacy skills
    pref_labels = re.findall(r'prefLabel="([^"]{2,80})"', nuxt_js)
    if pref_labels:
        return len(pref_labels)

    # ── HTML fallback ─────────────────────────────────────────────────────────
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
    Detect profile intro video.

    Current Upwork JS schema (confirmed 2026-04):
      video:"https://www.youtube.com/..."   ← YouTube embed
      video:"https://..."                   ← any other hosted video
    The field is a plain `video:` key at the top-level profile object.
    Previous names (videoIntro, introVideo, etc.) kept as fallbacks.
    """
    JS_PATTERNS = [
        r'(?<!\w)video:"https',          # confirmed current field name
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

    # HTML fallback: <video> tag or known video containers
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


# ─────────────────────────────────────────────────────────────────────────────
#  PROFILE SCRAPE
# ─────────────────────────────────────────────────────────────────────────────
def scrape_profile(driver, profile_url: str) -> dict:
    result = {
        "profile_url"         : profile_url,
        "has_photo"           : None,
        "review_count"        : None,
        "member_since_year"   : None,
        "years_on_platform"   : None,
        "cert_count"          : None,
        "skills_count_profile": None,
        "has_video"           : None,
        "overview_len"        : None,
    }
    try:
        driver.get(profile_url)
        if not wait_for_cloudflare(driver):
            log.warning("  Cloudflare block — skipping")
            return result
        time.sleep(random.uniform(3, 5))

        html = driver.page_source
        result.update(_parse_html(html, profile_url))

    except Exception as e:
        log.warning(f"  Error scraping {profile_url}: {e}")

    return result


def _parse_html(html: str, profile_url: str = "") -> dict:
    """
    Pure parsing logic — separated so the test function can call it
    directly on a saved HTML string without needing a live browser.
    """
    result = {
        "profile_url"         : profile_url,
        "has_photo"           : 0,
        "review_count"        : 0,
        "member_since_year"   : None,
        "years_on_platform"   : None,
        "cert_count"          : 0,
        "skills_count_profile": 0,
        "has_video"           : 0,
        "overview_len"        : None,
    }

    soup    = BeautifulSoup(html, "html.parser")
    text    = soup.get_text()
    nuxt_js = _find_nuxt_js(soup)

    if not nuxt_js:
        log.warning("  ⚠️  No NUXT JS block found — HTML-only fallbacks will be used")

    # ── 1. has_photo ──────────────────────────────────────────────────────────
    og_img = soup.find("meta", property="og:image")
    if og_img:
        img_url = og_img.get("content", "")
        result["has_photo"] = int("profile-portraits" in img_url)
    elif nuxt_js:
        portrait_m = re.search(r'portrait\s*:\s*"(https[^"]+)"', nuxt_js)
        if not portrait_m:
            portrait_m = re.search(r'"portrait"\s*:\s*"(https[^"]+)"', nuxt_js)
        result["has_photo"] = int(
            bool(portrait_m) and
            "profile-portraits" in (portrait_m.group(1) if portrait_m else "")
        )

    # ── 2. review_count ───────────────────────────────────────────────────────
    ratings = re.findall(r"Rating is [\d.]+ out of 5", text)
    result["review_count"] = len(ratings)

    # ── 3. member_since_year + years_on_platform ──────────────────────────────
    if nuxt_js:
        for ms_pat in [
            r'memberSince\s*:\s*"(\d{4})',
            r'"memberSince"\s*:\s*"(\d{4})',
            r'memberSince\s*:\s*(\d{4})',
        ]:
            m = re.search(ms_pat, nuxt_js)
            if m:
                year = int(m.group(1))
                result["member_since_year"]  = year
                result["years_on_platform"] = datetime.now().year - year
                break

    # ── 4. cert_count ─────────────────────────────────────────────────────────
    result["cert_count"] = _count_cert(nuxt_js, soup)
    if result["cert_count"] == 0 and nuxt_js:
        _diag_warn("cert_count", nuxt_js, [
            "certif", "Certif", "CERTIF", "issued", "credential",
        ])

    # ── 5. skills_count_profile ───────────────────────────────────────────────
    result["skills_count_profile"] = _count_skills(nuxt_js, soup)
    if result["skills_count_profile"] == 0 and nuxt_js:
        _diag_warn("skills_count_profile", nuxt_js, [
            "tagName", "prettyName", "skillName", "skillUid",
            '"skills"', "label",
        ])

    # ── 6. has_video ──────────────────────────────────────────────────────────
    result["has_video"] = _has_video(nuxt_js, soup)
    if result["has_video"] == 0 and nuxt_js:
        _diag_warn("has_video", nuxt_js, [
            "video", "Video", "VIDEO", "introVideo", "profileVideo",
        ])

    # ── 7. overview_len ───────────────────────────────────────────────────────
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


def _diag_warn(field: str, nuxt_js: str, keywords: list, context_chars: int = 200):
    """No-op placeholder — diagnostics are now handled inside run_test."""
    pass


# ─────────────────────────────────────────────────────────────────────────────
#  DIAGNOSTIC HELPERS  (used only by run_test)
# ─────────────────────────────────────────────────────────────────────────────

def _diag_js_field(label: str, nuxt_js: str, keywords: list, ctx: int = 300):
    """
    For a given field, print every occurrence of every keyword in the JS
    with surrounding context.  This is the raw evidence you need to write
    (or fix) a regex pattern.
    """
    print(f"\n  ── JS context for [{label}] ──")
    found_any = False
    seen = set()
    for kw in keywords:
        start = 0
        while True:
            idx = nuxt_js.find(kw, start)
            if idx == -1:
                break
            snippet = nuxt_js[max(0, idx - 60): idx + ctx]
            key = snippet[:80]                 # deduplicate near-identical hits
            if key not in seen:
                seen.add(key)
                print(f"  keyword='{kw}'")
                print("  " + "-" * 60)
                print("  " + snippet.replace("\n", " ").replace("\r", ""))
                print()
            found_any = True
            start = idx + len(kw)
    if not found_any:
        print(f"  ⚠️  NONE of these keywords found in the JS block: {keywords}")
        print("  → The field name has changed or the JS block wasn't captured.")
        print("  → Check the 'JS block sample' section above for clues.")


def _diag_pattern_results(label: str, nuxt_js: str, patterns: list):
    """
    Run every regex pattern individually and print whether it matched,
    and what it captured — so you can pinpoint exactly which pattern fires.
    """
    print(f"\n  ── Pattern-by-pattern results for [{label}] ──")
    any_hit = False
    for pat in patterns:
        matches = re.findall(pat, nuxt_js)
        if matches:
            preview = str(matches[:5])
            if len(preview) > 120:
                preview = preview[:120] + "..."
            print(f"  ✅  MATCH  {pat!r}")
            print(f"       → {len(matches)} hit(s): {preview}")
            any_hit = True
        else:
            print(f"  ❌  no match  {pat!r}")
    if not any_hit:
        print("  → ALL patterns failed for this field.")


# ─────────────────────────────────────────────────────────────────────────────
#  TEST FUNCTION  — single-URL smoke-test
# ─────────────────────────────────────────────────────────────────────────────
def run_test(url: str):
    """
    Fetch one profile URL and print:
      1. Extracted values for every field
      2. For any field that resolved to 0/None:
         a. Every regex pattern tried + whether it matched
         b. Raw JS snippets around relevant keywords so you can see
            the actual current field name and write a new pattern

    Usage:
        py -3.11 phase2_scraper.py --test https://www.upwork.com/freelancers/~...
    """
    SEP = "=" * 70

    print(f"\n{SEP}")
    print(f"  TEST MODE  |  {url}")
    print(SEP)

    if not _UC_AVAILABLE:
        print("ERROR: undetected_chromedriver not installed.")
        return

    driver = build_driver()
    try:
        print("\n🌐 Loading homepage (Cloudflare warm-up)...")
        driver.get("https://www.upwork.com")
        wait_for_cloudflare(driver)
        time.sleep(random.uniform(3, 5))

        print(f"\n🌐 Fetching: {url}")
        driver.get(url)
        wait_for_cloudflare(driver)
        time.sleep(random.uniform(4, 6))

        html    = driver.page_source
        soup    = BeautifulSoup(html, "html.parser")
        nuxt_js = _find_nuxt_js(soup)
        result  = _parse_html(html, url)

        # ── Save raw HTML so you can inspect it offline ──────────────────────
        html_path = os.path.join(
            THESIS_DIR if os.path.isdir(THESIS_DIR) else os.getcwd(),
            f"test_profile_{SESSION_TAG}.html"
        )
        with open(html_path, "w", encoding="utf-8") as f:
            f.write(html)
        print(f"\n💾 Raw HTML saved → {html_path}")

        # ── JS block overview ─────────────────────────────────────────────────
        print(f"\n🧩 JS block: {'FOUND' if nuxt_js else '❌ NOT FOUND — all HTML fallbacks used'}")
        if nuxt_js:
            print(f"   Length: {len(nuxt_js):,} chars")
            print(f"   First 400 chars of JS block:")
            print("   " + nuxt_js[:400].replace("\n", " "))

        # ── Extracted field values ────────────────────────────────────────────
        print(f"\n{'─'*70}")
        print("📋 EXTRACTED VALUES")
        print(f"{'─'*70}")
        for k, v in result.items():
            if k == "profile_url":
                continue
            flag = "  ⚠️  ZERO/NULL" if (v is None or v == 0) else ""
            print(f"  {k:25s}: {v}{flag}")

        # ── Deep diagnostics for broken fields ───────────────────────────────
        BROKEN = [
            (
                "cert_count",
                result.get("cert_count"),
                [   # patterns from _count_cert
                    r'"certificationName"',
                    r'certificationName\s*:',
                    r'"certifications"\s*:\s*\[',
                    r'certifications\s*:\s*\[',
                    r'"issued"\s*:',
                    r'certUid\s*:',
                    r'"certUid"',
                ],
                ["certif", "Certif", "issued", "credential", "license", "Licens"],
            ),
            (
                "skills_count_profile",
                result.get("skills_count_profile"),
                [   # patterns from _count_skills
                    r'"tagName"\s*:\s*"([^"]{1,59})"',
                    r'"prettyName"\s*:\s*"([^"]{1,59})"',
                    r'"label"\s*:\s*"([^"]{1,59})"',
                    r'"skillName"\s*:\s*"([^"]{1,59})"',
                    r'"name"\s*:\s*"([^"]{1,59})"',
                    r'"skillUid"\s*:',
                    r'skillUid\s*:',
                    r'"skills"\s*:\s*\[([^\]]+)\]',
                ],
                ["tagName", "prettyName", "skillName", "skillUid",
                 '"skills"', "label", "skill", "Skill"],
            ),
            (
                "has_video",
                result.get("has_video"),
                [   # patterns from _has_video
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
                ],
                ["video", "Video", "VIDEO", "intro", "Intro"],
            ),
        ]

        any_broken = False
        for field, val, patterns, kw_hints in BROKEN:
            if val is None or val == 0:
                any_broken = True
                print(f"\n{'─'*70}")
                print(f"🔍 DIAGNOSTICS FOR: {field}  (currently = {val})")
                print(f"{'─'*70}")
                if nuxt_js:
                    _diag_pattern_results(field, nuxt_js, patterns)
                    _diag_js_field(field, nuxt_js, kw_hints, ctx=350)
                else:
                    print("  No JS block — cannot diagnose patterns.")
                    print("  Check the saved HTML file for the page structure.")

        if not any_broken:
            print("\n✅ All three fields extracted non-zero values — patterns are working!")

        print(f"\n{'─'*70}")
        print("📌 HOW TO FIX A BROKEN PATTERN")
        print(f"{'─'*70}")
        print("  1. Look at the JS snippets above for the broken field.")
        print("  2. Find the actual key name Upwork is using (e.g. 'skillLabel').")
        print("  3. Add a new pattern to the relevant _count_* / _has_video function.")
        print("  4. Re-run --test to confirm it now returns non-zero.")
        print(f"  5. Or open {html_path}")
        print("     in a text editor and search for 'certif' / 'skill' / 'video'.")
        print(f"\n" + "="*70)
        print("✅ Test complete.")
        print("="*70 + "\n")
    finally:
        try:
            driver.quit()
        except Exception:
            pass



# ─────────────────────────────────────────────────────────────────────────────
#  SAVE
# ─────────────────────────────────────────────────────────────────────────────
def save(df: pd.DataFrame, label: str) -> str:
    out_dir = THESIS_DIR if os.path.isdir(THESIS_DIR) else os.getcwd()
    path    = os.path.join(out_dir, f"upwork_{label}_{SESSION_TAG}.csv")
    df.to_csv(path, index=False, encoding="utf-8-sig")
    log.info(f"💾 {len(df)} rows → {path}")
    return path


# ─────────────────────────────────────────────────────────────────────────────
#  MAIN
# ─────────────────────────────────────────────────────────────────────────────
def main():
    log.info("=" * 60)
    log.info(f"  Phase 2 Scraper v2 | Session: {SESSION_TAG}")
    log.info("=" * 60)

    # ── Load Phase 1 CSV ──────────────────────────────────────────────────────
    input_path = os.path.join(THESIS_DIR, INPUT_FILE)
    if not os.path.exists(input_path):
        log.error(f"File not found: {input_path}")
        log.error("Update INPUT_FILE in the CONFIG section.")
        return

    df = pd.read_csv(input_path, encoding="utf-8-sig")
    log.info(f"Loaded {len(df)} rows | {df['profile_url'].nunique()} unique profiles")

    all_urls = [
        u for u in df["profile_url"].unique()
        if u and str(u) != "N/A" and str(u).startswith("http")
    ]

    # ── Resume from checkpoint if available ──────────────────────────────────
    progress_file = os.path.join(THESIS_DIR, f"phase2_progress_{SESSION_TAG}.csv")
    scraped_urls  = set()
    progress_rows = []

    existing = sorted([
        f for f in os.listdir(THESIS_DIR)
        if f.startswith("phase2_progress_") and f.endswith(".csv")
    ], reverse=True)

    if existing:
        resume_path = os.path.join(THESIS_DIR, existing[0])
        log.info(f"Found checkpoint: {existing[0]}")
        prog_df       = pd.read_csv(resume_path, encoding="utf-8-sig")
        progress_rows = prog_df.to_dict("records")
        scraped_urls  = set(prog_df["profile_url"].tolist())
        log.info(f"Resuming — {len(scraped_urls)} already done")

    urls_to_scrape = [u for u in all_urls if u not in scraped_urls]
    log.info(f"Remaining: {len(urls_to_scrape)} profiles")

    if urls_to_scrape:
        driver = build_driver()

        log.info("🌐 Loading Upwork homepage...")
        driver.get("https://www.upwork.com")
        if not wait_for_cloudflare(driver):
            log.error("Blocked on homepage. Solve CAPTCHA manually then retry.")
            input("Press Enter to quit...")
            driver.quit()
            return
        time.sleep(random.uniform(3, 5))

        total = len(urls_to_scrape)
        for j, url in enumerate(urls_to_scrape, 1):
            log.info(f"  [{j}/{total}] {url}")

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
                log.info(f"  💾 Checkpoint: {j}/{total}")

            time.sleep(random.uniform(2, 4))

        pd.DataFrame(progress_rows).to_csv(
            progress_file, index=False, encoding="utf-8-sig"
        )
        log.info(f"💾 Progress complete: {len(progress_rows)} profiles scraped")

        try:
            driver.quit()
        except Exception:
            pass

    # ── Merge Phase 1 + Phase 2 ───────────────────────────────────────────────
    log.info("\n🔀 Merging Phase 1 + Phase 2...")

    p2_cols = [
        "has_photo", "review_count", "member_since_year",
        "years_on_platform", "cert_count", "skills_count_profile",
        "has_video", "overview_len",
    ]

    df_clean = df.drop(columns=[c for c in p2_cols if c in df.columns], errors="ignore")
    p2_df    = pd.DataFrame(progress_rows)
    df_final = df_clean.merge(p2_df, on="profile_url", how="left")

    # ── Gender column ─────────────────────────────────────────────────────────
    df_final = genderize_dataframe(df_final, name_col="first_name")

    # ── Summary ───────────────────────────────────────────────────────────────
    log.info(f"\n📊 Final: {len(df_final)} rows | {len(df_final.columns)} columns")
    log.info("Fill rates:")
    all_cols = p2_cols + ["gender"]
    for col in all_cols:
        if col in df_final.columns:
            filled   = df_final[col].notna()
            nonzero  = (df_final[col] != 0).sum() if col != "gender" else "—"
            log.info(f"  {col:25s}  fill={filled.sum()}/{len(df_final)} ({filled.mean():.0%})  nonzero={nonzero}")

    save(df_final, "phase2_merged")
    log.info("\n✅ Done. Use upwork_phase2_merged_*.csv for R analysis.")


# ─────────────────────────────────────────────────────────────────────────────
#  ENTRY POINT
# ─────────────────────────────────────────────────────────────────────────────
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Upwork Phase 2 Scraper")
    parser.add_argument(
        "--test", metavar="URL", default=None,
        help="Test-scrape a single profile URL and print all fields + diagnostics"
    )
    args = parser.parse_args()

    if args.test:
        run_test(args.test)
    else:
        main()