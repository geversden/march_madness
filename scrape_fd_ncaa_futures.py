"""
FanDuel NCAA Tournament Futures Scraper + Devig
Scrapes men's tournament advancement markets, devigs, outputs CSV.

Markets scraped:
  - Win National Championship (YES-only → outright devig, sum=1)
  - Reach Championship Game   (YES-only → outright devig, sum=2)
  - Reach Final Four           (YES/NO → 2-way devig per team)
  - Reach Elite Eight          (YES/NO → 2-way devig per team)
  - Reach Sweet Sixteen        (YES/NO → 2-way devig per team)
  - Reach Round of 32          (YES/NO → 2-way devig per team)

Devig methods:
  YES/NO pairs:  multiplicative (equal margin) — p = imp_yes / (imp_yes + imp_no)
  YES-only:      normalize outright market — p_i = imp_i / sum(imp_i) * n_winners

Uses FanDuel's internal JSON API — no browser/Selenium needed.

Requirements:
    pip install requests

Usage:
    python scrape_fd_ncaa_futures.py
    python scrape_fd_ncaa_futures.py --debug       # dump raw JSON
"""

import csv
import json
import logging
import sys
from datetime import datetime, timezone
from pathlib import Path

import requests

logging.basicConfig(level=logging.INFO, format="%(asctime)s [%(levelname)s] %(message)s")
log = logging.getLogger(__name__)

SCRIPT_DIR = Path(__file__).parent

# ─── FanDuel API ──────────────────────────────────────────────────────────────
API_URL = "https://api.sportsbook.fanduel.com/sbapi/content-managed-page"
API_PARAMS = {
    "page": "CUSTOM",
    "customPageId": "ncaab",
    "pbHorizontal": "false",
    "_ak": "FhMFpcPWXMeyZxOx",
    "timezone": "America/New_York",
}
HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/131.0.0.0 Safari/537.36"
    ),
    "Accept": "application/json",
    "Referer": "https://sportsbook.fanduel.com/",
    "X-Sportsbook-Region": "OH",
}

# ─── Market types we care about (men's tournament only) ──────────────────────
# YES markets: one market with 68 runners, each runner = one team with YES odds
# NO markets:  one market with 68 runners, each runner = one team with NO odds
MARKET_TYPES = {
    # YES markets
    "TEAM_TO_WIN_NATIONAL_CHAMPIONSHIP_YES_-_ALL_REGIONS": "champion",
    "TEAM_TO_REACH_NATIONAL_CHAMPIONSHIP_YES_-_ALL_REGIONS": "championship_game",
    "TEAM_TO_REACH_FINAL_4_YES_-_ALL_REGIONS": "final_four",
    "TEAM_TO_REACH_ELITE_8_YES_-_ALL_REGIONS": "elite_eight",
    "TEAM_TO_REACH_SWEET_16_YES_-_ALL_REGIONS": "sweet_sixteen",
    "TEAM_TO_REACH_ROUND_OF_32_YES_-_ALL_REGIONS": "round_of_32",
    # NO markets (paired with YES for devig)
    "TEAM_TO_REACH_FINAL_4_NO_-_ALL_REGIONS": "final_four_no",
    "TEAM_TO_REACH_ELITE_8_NO_-_ALL_REGIONS": "elite_eight_no",
    "TEAM_TO_REACH_SWEET_16_NO_-_ALL_REGIONS": "sweet_sixteen_no",
    "TEAM_TO_REACH_ROUND_OF_32_NO_-_ALL_REGIONS": "round_of_32_no",
}

# Rounds that have YES/NO pairs for 2-way devig
PAIRED_ROUNDS = ["round_of_32", "sweet_sixteen", "elite_eight", "final_four"]

# Rounds that are YES-only (outright devig)
OUTRIGHT_ROUNDS = {
    "champion": 1,             # exactly 1 winner
    "championship_game": 2,    # exactly 2 teams reach title game
}

# ─── FanDuel name → bracket name mapping ─────────────────────────────────────
# Only need entries where names differ; identical names auto-match.
FD_TO_BRACKET = {
    "California Baptist": "Cal Baptist",
    "Connecticut": "UConn",
    "Hawai'i": "Hawaii",
    "Long Island University": "LIU",
    "McNeese": "McNeese State",
    "Pennsylvania": "Penn",
    "Queen's University": "Queens",
    # FanDuel teams NOT in our bracket (First Four losers, etc.) — map to None
    "Howard": None,
    "Lehigh": None,
    "Miami (OH)": None,
    "NC State": None,
}


# ─── Helpers ─────────────────────────────────────────────────────────────────

def american_to_implied(odds: int) -> float:
    """Convert American odds to implied probability (0-1)."""
    if odds >= 0:
        return 100.0 / (odds + 100.0)
    else:
        return (-odds) / (-odds + 100.0)


def get_american_odds(runner: dict) -> int | None:
    """Extract American odds integer from a FanDuel runner dict."""
    try:
        return runner["winRunnerOdds"]["americanDisplayOdds"]["americanOddsInt"]
    except (KeyError, TypeError):
        return None


def resolve_team_name(fd_name: str) -> str | None:
    """Map FanDuel runner name to bracket name. Returns None if not in bracket."""
    if fd_name in FD_TO_BRACKET:
        return FD_TO_BRACKET[fd_name]
    return fd_name


# ─── API fetch ───────────────────────────────────────────────────────────────

def fetch_api_data() -> dict:
    """Fetch the full NCAAB page JSON from FanDuel API."""
    log.info(f"Fetching FanDuel NCAA API...")
    resp = requests.get(API_URL, params=API_PARAMS, headers=HEADERS, timeout=30)
    resp.raise_for_status()
    data = resp.json()
    n_markets = len(data.get("attachments", {}).get("markets", {}))
    log.info(f"  API response OK — {n_markets} markets found")
    return data


# ─── Extract markets ─────────────────────────────────────────────────────────

def extract_markets(data: dict) -> dict[str, dict[str, int]]:
    """
    Parse API response into:
      { market_key: { bracket_team_name: american_odds, ... }, ... }

    market_key is one of: champion, championship_game, final_four, elite_eight,
                          sweet_sixteen, round_of_32, final_four_no, etc.
    """
    api_markets = data.get("attachments", {}).get("markets", {})
    result = {}

    for market_id, market in api_markets.items():
        mt = market.get("marketType", "")
        if mt not in MARKET_TYPES:
            continue

        market_key = MARKET_TYPES[mt]

        # Skip women's markets (check marketName)
        mn = market.get("marketName", "").lower()
        if "women" in mn or "wcbb" in mn.lower():
            continue

        odds_map = {}
        for runner in market.get("runners", []):
            if runner.get("runnerStatus") != "ACTIVE":
                continue
            fd_name = runner.get("runnerName", "").strip()
            bracket_name = resolve_team_name(fd_name)
            if bracket_name is None:
                continue
            odds = get_american_odds(runner)
            if odds is None:
                continue
            odds_map[bracket_name] = odds

        if odds_map:
            result[market_key] = odds_map
            log.info(f"  {market_key}: {len(odds_map)} teams")

    return result


# ─── Devig ───────────────────────────────────────────────────────────────────

def devig_two_way(yes_odds: int, no_odds: int) -> float:
    """Devig a YES/NO pair using multiplicative (equal margin) method.
    Returns the fair YES probability (0-1)."""
    imp_yes = american_to_implied(yes_odds)
    imp_no = american_to_implied(no_odds)
    total = imp_yes + imp_no
    if total <= 0:
        return imp_yes
    return imp_yes / total


def devig_outright(odds_map: dict[str, int], n_winners: int) -> dict[str, float]:
    """Devig an outright market (normalize implied probs to sum = n_winners).
    Returns { team: fair_probability }."""
    implied = {team: american_to_implied(odds) for team, odds in odds_map.items()}
    total = sum(implied.values())
    if total <= 0:
        return implied
    scale = n_winners / total
    return {team: prob * scale for team, prob in implied.items()}


# ─── Build team table ────────────────────────────────────────────────────────

def build_team_table(markets: dict[str, dict[str, int]]) -> list[dict]:
    """Combine all markets into one row per team with devigged probabilities."""

    # Collect all team names from any market
    all_teams = set()
    for odds_map in markets.values():
        all_teams.update(odds_map.keys())
    all_teams = sorted(all_teams)

    # Devig outright markets (YES-only)
    outright_probs = {}
    for round_key, n_winners in OUTRIGHT_ROUNDS.items():
        if round_key in markets:
            outright_probs[round_key] = devig_outright(markets[round_key], n_winners)
            total = sum(outright_probs[round_key].values())
            log.info(f"  Devigged {round_key}: {len(outright_probs[round_key])} teams, "
                     f"sum = {total:.3f} (expect {n_winners})")

    # Devig paired YES/NO markets
    paired_probs = {}
    for round_key in PAIRED_ROUNDS:
        yes_key = round_key
        no_key = f"{round_key}_no"
        if yes_key not in markets:
            continue
        yes_map = markets[yes_key]
        no_map = markets.get(no_key, {})

        round_probs = {}
        for team in yes_map:
            if team in no_map:
                # 2-way devig
                round_probs[team] = devig_two_way(yes_map[team], no_map[team])
            else:
                # No NO line — just use raw implied (slightly overestimated)
                round_probs[team] = american_to_implied(yes_map[team])

        paired_probs[round_key] = round_probs
        total = sum(round_probs.values())
        log.info(f"  Devigged {round_key}: {len(round_probs)} teams, "
                 f"sum = {total:.1f} (expect {2 ** (5 - PAIRED_ROUNDS.index(round_key))})")

    # Merge into rows
    rows = []
    for team in all_teams:
        row = {"team": team}
        # Paired rounds
        for round_key in PAIRED_ROUNDS:
            if round_key in paired_probs and team in paired_probs[round_key]:
                row[round_key] = paired_probs[round_key][team]
            else:
                row[round_key] = None
        # Outright rounds
        for round_key in OUTRIGHT_ROUNDS:
            if round_key in outright_probs and team in outright_probs[round_key]:
                row[round_key] = outright_probs[round_key][team]
            else:
                row[round_key] = None
        rows.append(row)

    return rows


# ─── Load bracket metadata (seed, region) ───────────────────────────────────

def load_bracket_info() -> dict[str, dict]:
    """Load seed/region from sim_results_2026.rds via a small R call,
    or fall back to empty dict if R not available."""
    import subprocess
    try:
        out = subprocess.check_output([
            "Rscript", "-e",
            'sim <- readRDS("sim_results_2026.rds"); '
            'cat(paste(sim$teams$name, sim$teams$seed, sim$teams$region, sep=","), sep="\\n")'
        ], cwd=str(SCRIPT_DIR), timeout=10, text=True)
        info = {}
        for line in out.strip().split("\n"):
            parts = line.split(",")
            if len(parts) == 3:
                info[parts[0]] = {"seed": int(parts[1]), "region": parts[2]}
        return info
    except Exception as e:
        log.warning(f"  Could not load bracket info: {e}")
        return {}


# ─── CSV output ──────────────────────────────────────────────────────────────

ROUND_COLUMNS = ["round_of_32", "sweet_sixteen", "elite_eight",
                 "final_four", "championship_game", "champion"]

CSV_COLUMNS = ["team", "seed", "region"] + ROUND_COLUMNS + ["scraped_at"]


def write_csv(rows: list[dict], bracket_info: dict, scraped_at: datetime) -> Path:
    ts = scraped_at.strftime("%Y%m%d_%H%M%S")
    path = SCRIPT_DIR / f"fd_ncaa_futures_{ts}.csv"

    # Enrich with seed/region, sort by seed then team
    enriched = []
    for row in rows:
        team = row["team"]
        info = bracket_info.get(team, {})
        out = {
            "team": team,
            "seed": info.get("seed", ""),
            "region": info.get("region", ""),
        }
        for col in ROUND_COLUMNS:
            val = row.get(col)
            out[col] = f"{val:.6f}" if val is not None else "NA"
        out["scraped_at"] = scraped_at.isoformat()
        enriched.append(out)

    enriched.sort(key=lambda x: (
        int(x["seed"]) if x["seed"] else 99,
        x["team"]
    ))

    with open(path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=CSV_COLUMNS)
        writer.writeheader()
        writer.writerows(enriched)

    log.info(f"  CSV written: {path}  ({len(enriched)} teams)")
    return path


# Also write a clean "latest" symlink-style file for easy downstream use
def write_latest_csv(rows: list[dict], bracket_info: dict, scraped_at: datetime) -> Path:
    path = SCRIPT_DIR / "fd_ncaa_futures_latest.csv"

    enriched = []
    for row in rows:
        team = row["team"]
        info = bracket_info.get(team, {})
        out = {
            "team": team,
            "seed": info.get("seed", ""),
            "region": info.get("region", ""),
        }
        for col in ROUND_COLUMNS:
            val = row.get(col)
            out[col] = f"{val:.6f}" if val is not None else "NA"
        out["scraped_at"] = scraped_at.isoformat()
        enriched.append(out)

    enriched.sort(key=lambda x: (
        int(x["seed"]) if x["seed"] else 99,
        x["team"]
    ))

    with open(path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=CSV_COLUMNS)
        writer.writeheader()
        writer.writerows(enriched)

    log.info(f"  Latest CSV: {path}")
    return path


# ─── Main ────────────────────────────────────────────────────────────────────

def main():
    scraped_at = datetime.now(timezone.utc)
    debug_mode = "--debug" in sys.argv

    log.info("Starting FanDuel NCAA Tournament futures scrape")

    # Fetch
    data = fetch_api_data()

    if debug_mode:
        debug_path = SCRIPT_DIR / "debug_fd_ncaa_api_response.json"
        with open(debug_path, "w") as f:
            json.dump(data, f, indent=2)
        log.info(f"  Debug JSON dump: {debug_path}")

    # Extract
    markets = extract_markets(data)
    if not markets:
        log.error("No men's tournament markets found!")
        sys.exit(1)

    # Devig
    log.info("Devigging markets...")
    rows = build_team_table(markets)

    # Load bracket info for seed/region
    bracket_info = load_bracket_info()

    # Write CSVs
    write_csv(rows, bracket_info, scraped_at)
    write_latest_csv(rows, bracket_info, scraped_at)

    # Summary
    log.info("=" * 60)
    log.info("Scrape complete. Markets found:")
    for mk in ROUND_COLUMNS:
        has_data = sum(1 for r in rows if r.get(mk) is not None)
        log.info(f"  {mk:25s} {has_data} teams")
    log.info(f"  {'TOTAL TEAMS':25s} {len(rows)}")

    # Print top 10
    champ_rows = sorted(
        [r for r in rows if r.get("champion") is not None],
        key=lambda x: -x["champion"]
    )
    log.info("")
    log.info(f"{'Team':22s} {'Seed':>4s}  {'R32':>6s}  {'S16':>6s}  {'E8':>6s}  "
             f"{'F4':>6s}  {'CG':>6s}  {'Champ':>6s}")
    log.info("-" * 72)
    for r in champ_rows[:15]:
        info = bracket_info.get(r["team"], {})
        seed = info.get("seed", "")
        def fmt(v):
            return f"{100*v:5.1f}%" if v is not None else "   NA "
        log.info(f"{r['team']:22s} {str(seed):>4s}  "
                 f"{fmt(r.get('round_of_32'))}  {fmt(r.get('sweet_sixteen'))}  "
                 f"{fmt(r.get('elite_eight'))}  {fmt(r.get('final_four'))}  "
                 f"{fmt(r.get('championship_game'))}  {fmt(r.get('champion'))}")

    log.info("\nDone")


if __name__ == "__main__":
    main()
