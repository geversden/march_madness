#!/usr/bin/env python3
"""
KenPom Daily Scraper
Pulls AdjEM, AdjO, AdjD for all D1 teams using nodriver to bypass Cloudflare.

Credentials (in priority order):
  1. Environment variables KENPOM_EMAIL / KENPOM_PASSWORD
  2. Local file .kenpom_creds with email=... and password=... lines

Usage:  python3 scrape_kenpom.py
Output: kenpom_data/kenpom_ratings_YYYY-MM-DD.csv
"""

import asyncio
import csv
import os
import re
import sys
from datetime import date
from pathlib import Path

import nodriver as uc
from bs4 import BeautifulSoup


def load_credentials():
    email = os.environ.get("KENPOM_EMAIL", "")
    password = os.environ.get("KENPOM_PASSWORD", "")

    if not email or not password:
        script_dir = Path(__file__).parent
        creds_file = script_dir / ".kenpom_creds"
        if creds_file.exists():
            for line in creds_file.read_text().strip().splitlines():
                line = line.strip()
                if "=" in line:
                    key, val = line.split("=", 1)
                    if key == "email":
                        email = val
                    elif key == "password":
                        password = val

    if not email or not password:
        print("ERROR: KenPom credentials not found.", file=sys.stderr)
        print("Set KENPOM_EMAIL/KENPOM_PASSWORD env vars or create .kenpom_creds", file=sys.stderr)
        sys.exit(1)

    return email, password


def parse_ratings(html: str) -> list[dict]:
    soup = BeautifulSoup(html, "html.parser")
    table = soup.find("table", id="ratings-table")
    if not table:
        table = soup.find("table")
    if not table:
        print("ERROR: Could not find ratings table.", file=sys.stderr)
        sys.exit(1)

    tbody = table.find("tbody")
    rows = tbody.find_all("tr") if tbody else table.find_all("tr")

    ratings = []
    for row in rows:
        cells = row.find_all("td")
        if len(cells) < 9:
            continue

        rank_text = cells[0].get_text(strip=True)
        if not rank_text or not rank_text.isdigit():
            continue

        team_name = cells[1].get_text(strip=True)
        # Remove seed numbers appended during tournament
        team_name = re.sub(r"\s*\d+$", "", team_name)

        try:
            ratings.append({
                "rank": int(rank_text),
                "team": team_name,
                "conf": cells[2].get_text(strip=True),
                "record": cells[3].get_text(strip=True),
                "adj_em": float(cells[4].get_text(strip=True)),
                "adj_o": float(cells[5].get_text(strip=True)),
                "adj_d": float(cells[7].get_text(strip=True)),
            })
        except (ValueError, IndexError):
            continue

    return ratings


async def main():
    email, password = load_credentials()
    script_dir = Path(__file__).parent

    print("Launching browser...")
    browser = await uc.start(headless=True)

    print("Navigating to kenpom.com...")
    page = await browser.get("https://kenpom.com/")

    # Wait for Cloudflare to resolve
    print("Waiting for Cloudflare challenge...")
    for i in range(30):
        await asyncio.sleep(1)
        try:
            title = await page.evaluate("document.title")
            if "Just a moment" not in str(title):
                print(f"  Cloudflare resolved after {i+1}s")
                break
        except Exception:
            pass
    else:
        print("WARNING: Cloudflare may not have resolved after 30s, continuing anyway...")

    await asyncio.sleep(2)

    # Check if we need to login (ratings visible without login on kenpom)
    html = await page.evaluate("document.documentElement.outerHTML")

    if "ratings-table" not in html:
        print("No ratings table found, attempting login...")
        # Try to find and fill login form
        try:
            email_input = await page.find("email", best_match=True)
            await email_input.send_keys(email)
            pw_input = await page.find("password", best_match=True)
            await pw_input.send_keys(password)
            submit = await page.find("submit", best_match=True)
            await submit.click()
            await asyncio.sleep(3)
            html = await page.evaluate("document.documentElement.outerHTML")
        except Exception as e:
            print(f"Login attempt failed: {e}")

    if "ratings-table" not in html:
        print("ERROR: Still no ratings table after login attempt.", file=sys.stderr)
        # Save debug HTML
        debug_file = script_dir / "debug_page.html"
        debug_file.write_text(html)
        print(f"Debug HTML saved to {debug_file}", file=sys.stderr)
        browser.stop()
        sys.exit(1)

    print("Found ratings table, parsing...")
    ratings = parse_ratings(html)
    print(f"  Parsed {len(ratings)} teams")

    browser.stop()

    # Save to CSV
    out_dir = script_dir / "kenpom_data"
    out_dir.mkdir(exist_ok=True)
    today = date.today().isoformat()
    out_file = out_dir / f"kenpom_ratings_{today}.csv"

    with open(out_file, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["rank", "team", "conf", "record", "adj_em", "adj_o", "adj_d"])
        writer.writeheader()
        writer.writerows(ratings)

    print(f"\nSaved {len(ratings)} teams to {out_file}")

    # Quick preview
    print(f"\n{'Rk':>3}  {'Team':<25} {'Conf':<6} {'AdjEM':>6} {'AdjO':>6} {'AdjD':>6}")
    print("-" * 62)
    for r in ratings[:20]:
        print(f"{r['rank']:>3}  {r['team']:<25} {r['conf']:<6} {r['adj_em']:>6.1f} {r['adj_o']:>6.1f} {r['adj_d']:>6.1f}")

    print("\nDone.")


if __name__ == "__main__":
    uc.loop().run_until_complete(main())
