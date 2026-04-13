"""
Download cloud-free Sentinel-2 (L2A) imagery overlapping the Caldor and Dixie
plot convex hulls for two target dates:
  - mid-November 2021  (first post-fire growing season)
  - June 2022          (first-year survey period)

Uses the Copernicus Data Space Ecosystem (CDSE) OData API.
  https://dataspace.copernicus.eu/

Prerequisites:
  pip install requests geopandas shapely

Credentials are read from environment variables:
  COPERNICUS_USER      (email used to register at dataspace.copernicus.eu)
  COPERNICUS_PASSWORD

Output is saved to:
  <datadir>/remote-cbf-prediction/sentinel-imagery/<fire>/<period>/
"""

import os
import zipfile
from pathlib import Path
from datetime import date, timedelta, timezone, datetime

import requests
import geopandas as gpd
from shapely.geometry import mapping
from shapely import to_wkt

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
DATADIR = Path("/ofo-share/repos/derek/early-regen_data")
HULLS_PATH = DATADIR / "remote-cbf-prediction" / "focal_footprints_per_fire.gpkg"
OUTPUT_DIR = DATADIR / "remote-cbf-prediction" / "sentinel-imagery"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

# ---------------------------------------------------------------------------
# Credentials  (set these in your shell before running)
# ---------------------------------------------------------------------------
COPERNICUS_USER = os.environ["COPERNICUS_USER"]
COPERNICUS_PASSWORD = os.environ["COPERNICUS_PASSWORD"]

# ---------------------------------------------------------------------------
# Search parameters
# ---------------------------------------------------------------------------
TARGET_DATES = {
    "nov2021": date(2021, 11, 15),
    "jun2022": date(2022, 6, 15),
}
SEARCH_WINDOW_DAYS = 12   # search ± this many days around each target date
MAX_CLOUD_COVER = 5       # percent

# ---------------------------------------------------------------------------
# CDSE endpoints
# ---------------------------------------------------------------------------
TOKEN_URL = (
    "https://identity.dataspace.copernicus.eu"
    "/auth/realms/CDSE/protocol/openid-connect/token"
)
ODATA_URL = "https://catalogue.dataspace.copernicus.eu/odata/v1/Products"
DOWNLOAD_BASE_URL = "https://download.dataspace.copernicus.eu/odata/v1/Products"


def get_access_token(user: str, password: str) -> str:
    """Obtain a short-lived OAuth2 access token from CDSE."""
    resp = requests.post(
        TOKEN_URL,
        data={
            "grant_type": "password",
            "client_id": "cdse-public",
            "username": user,
            "password": password,
        },
        timeout=30,
    )
    resp.raise_for_status()
    return resp.json()["access_token"]


def search_products(wkt: str, start: date, end: date, max_cloud: float) -> list[dict]:
    """Query CDSE OData for S2MSI2A products intersecting *wkt* in [start, end]."""
    date_filter = (
        f"ContentDate/Start gt {start.isoformat()}T00:00:00.000Z and "
        f"ContentDate/Start lt {end.isoformat()}T23:59:59.000Z"
    )
    cloud_filter = f"Attributes/OData.CSC.DoubleAttribute/any(att:att/Name eq 'cloudCover' and att/OData.CSC.DoubleAttribute/Value le {max_cloud})"
    geo_filter = f"OData.CSC.Intersects(area=geography'SRID=4326;{wkt}')"
    collection_filter = "Collection/Name eq 'SENTINEL-2' and Attributes/OData.CSC.StringAttribute/any(att:att/Name eq 'productType' and att/OData.CSC.StringAttribute/Value eq 'S2MSI2A')"

    full_filter = " and ".join([date_filter, cloud_filter, geo_filter, collection_filter])

    resp = requests.get(
        ODATA_URL,
        params={
            "$filter": full_filter,
            "$orderby": "ContentDate/Start asc",
            "$top": 50,
            "$expand": "Attributes",
        },
        timeout=60,
    )
    resp.raise_for_status()
    return resp.json().get("value", [])


import re


def tile_id(product: dict) -> str:
    """Extract the MGRS tile code (e.g. T11SKC) from the product name."""
    m = re.search(r'_T([0-9]{2}[A-Z]{3})_', product["Name"])
    return m.group(1) if m else product["Id"]


def pick_best_per_tile(products: list[dict], target: date) -> list[dict]:
    """Pick one date for all tiles: the date on which max cloud cover across tiles is lowest.

    Strategy:
      1. Find all unique MGRS tiles represented in the search results.
      2. For each sensing date, collect the products for every tile on that date.
      3. Only consider dates where every required tile has at least one product.
      4. Among those dates, pick the one with lowest max cloud cover across its tiles.
      5. Return one product per tile for that winning date.
    """
    def sensing_date(p: dict) -> date:
        return datetime.fromisoformat(
            p["ContentDate"]["Start"].replace("Z", "+00:00")
        ).date()

    def cloud(p: dict) -> float:
        val = next(
            (a["Value"] for a in p.get("Attributes", []) if a["Name"] == "cloudCover"),
            100.0,
        )
        return float(val)

    all_tiles = {tile_id(p) for p in products}

    # Group: date -> tile -> best (lowest cloud) product
    by_date: dict[date, dict[str, dict]] = {}
    for p in products:
        d = sensing_date(p)
        tid = tile_id(p)
        if d not in by_date:
            by_date[d] = {}
        if tid not in by_date[d] or cloud(p) < cloud(by_date[d][tid]):
            by_date[d][tid] = p

    # Only keep dates that cover every required tile
    complete_dates = {
        d: tile_map
        for d, tile_map in by_date.items()
        if all_tiles.issubset(tile_map.keys())
    }

    if not complete_dates:
        # Fall back: pick date with most tiles covered, then lowest max cloud
        complete_dates = by_date

    # Among complete dates, pick the one with lowest max cloud cover across tiles
    def date_score(item):
        d, tile_map = item
        return max(cloud(p) for p in tile_map.values())

    best_date, best_tiles = min(complete_dates.items(), key=date_score)
    return list(best_tiles.values())


def download_product(product_id: str, dest_dir: Path, token: str) -> Path:
    """Stream-download a zipped product from CDSE into *dest_dir*."""
    dest_dir.mkdir(parents=True, exist_ok=True)
    url = f"{DOWNLOAD_BASE_URL}({product_id})/$value"
    headers = {"Authorization": f"Bearer {token}"}

    with requests.get(url, headers=headers, stream=True, timeout=300) as resp:
        resp.raise_for_status()
        filename = None
        cd = resp.headers.get("Content-Disposition", "")
        if "filename=" in cd:
            filename = cd.split("filename=")[-1].strip().strip('"')
        if not filename:
            filename = f"{product_id}.zip"
        out_path = dest_dir / filename
        with open(out_path, "wb") as f:
            for chunk in resp.iter_content(chunk_size=1 << 20):
                f.write(chunk)

    print(f"  Extracting …")
    with zipfile.ZipFile(out_path, "r") as zf:
        zf.extractall(dest_dir)
    out_path.unlink()

    return dest_dir


# ---------------------------------------------------------------------------
# Load hulls (one feature per fire, WGS-84)
# ---------------------------------------------------------------------------
hulls = gpd.read_file(HULLS_PATH).to_crs(epsg=4326)

# ---------------------------------------------------------------------------
# Main loop: search + download for each fire × target-date combination
# ---------------------------------------------------------------------------
for _, row in hulls.iterrows():
    fire_name = row["Fire"]
    wkt = to_wkt(row["geometry"], rounding_precision=6)

    for period_name, target_date in TARGET_DATES.items():
        start = target_date - timedelta(days=SEARCH_WINDOW_DAYS)
        end = target_date + timedelta(days=SEARCH_WINDOW_DAYS)

        print(f"\n[{fire_name} / {period_name}] Searching {start} – {end} …")

        products = search_products(wkt, start, end, MAX_CLOUD_COVER)

        if not products:
            print("  No products found. Try widening SEARCH_WINDOW_DAYS or MAX_CLOUD_COVER.")
            continue

        best_per_tile = pick_best_per_tile(products, target_date)
        tile_dates = sorted({datetime.fromisoformat(p["ContentDate"]["Start"].replace("Z", "+00:00")).date() for p in best_per_tile})
        print(f"  Selected date(s): {tile_dates} — {len(best_per_tile)} tile(s) to download.")

        fire_out = OUTPUT_DIR / fire_name.lower() / period_name
        token = get_access_token(COPERNICUS_USER, COPERNICUS_PASSWORD)

        for product in best_per_tile:
            sensing_date = datetime.fromisoformat(
                product["ContentDate"]["Start"].replace("Z", "+00:00")
            ).date()
            cloud = next(
                (a["Value"] for a in product.get("Attributes", []) if a["Name"] == "cloudCover"),
                "?",
            )
            print(
                f"  Tile {tile_id(product)}: {product['Name']}\n"
                f"    Date: {sensing_date}, Cloud cover: {cloud}%"
            )
            print(f"    Downloading …")
            out_path = download_product(product["Id"], fire_out, token)
            print(f"    Saved and extracted to {out_path}")

print("\nAll downloads complete.")
