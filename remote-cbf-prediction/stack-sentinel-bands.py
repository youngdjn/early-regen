"""
Stack all Sentinel-2 L2A bands at 10 m resolution for each fire/period.

- All 13 spectral bands (B01–B12, B8A) + SCL are resampled to 10 m with
  nearest-neighbour interpolation.
- If multiple tiles (SAFE directories) are present for a fire/period they are
  mosaicked before stacking.
- Output: one cloud-optimised GeoTIFF per fire/period written to
    <datadir>/remote-cbf-prediction/sentinel-stacks/<fire>/<period>_stack.tif

Band order in output:
  1  B01   2  B02   3  B03   4  B04   5  B05   6  B06   7  B07
  8  B08   9  B8A  10  B09  11  B11  12  B12  13  SCL

Prerequisites:
  pip install rasterio numpy geopandas
"""

from pathlib import Path
import numpy as np
import rasterio
from rasterio.enums import Resampling
from rasterio.merge import merge
from rasterio.mask import mask as rio_mask
from rasterio.warp import reproject, calculate_default_transform, Resampling as WarpResampling
import geopandas as gpd

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
DATADIR = Path("/ofo-share/repos/derek/early-regen_data")
IMAGERY_DIR = DATADIR / "remote-cbf-prediction" / "sentinel-imagery"
STACK_DIR = DATADIR / "remote-cbf-prediction" / "sentinel-stacks"
STACK_DIR.mkdir(parents=True, exist_ok=True)
HULLS_PATH = DATADIR / "remote-cbf-prediction" / "focal_footprints_per_fire.gpkg"
BUFFER_M = 1000  # metres

# ---------------------------------------------------------------------------
# Band definitions: (label, filename suffix inside SAFE)
# ---------------------------------------------------------------------------
BANDS = [
    ("B01", "_B01_60m.jp2"),
    ("B02", "_B02_10m.jp2"),
    ("B03", "_B03_10m.jp2"),
    ("B04", "_B04_10m.jp2"),
    ("B05", "_B05_20m.jp2"),
    ("B06", "_B06_20m.jp2"),
    ("B07", "_B07_20m.jp2"),
    ("B08", "_B08_10m.jp2"),
    ("B8A", "_B8A_20m.jp2"),
    ("B09", "_B09_60m.jp2"),
    ("B11", "_B11_20m.jp2"),
    ("B12", "_B12_20m.jp2"),
    ("SCL", "_SCL_20m.jp2"),
]

TARGET_RES = 10.0  # metres
TARGET_CRS = "EPSG:32610"  # UTM zone 10N, WGS84


# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

def find_band_file(safe_dir: Path, suffix: str) -> Path | None:
    matches = list(safe_dir.rglob(f"IMG_DATA/**/*{suffix}"))
    if not matches:
        return None
    if len(matches) > 1:
        print(f"    WARNING: multiple matches for {suffix}, using {matches[0].name}")
    return matches[0]


def read_resampled(jp2_path: Path, resampling: Resampling = Resampling.bilinear) -> tuple[np.ndarray, dict]:
    """Read a JP2 band, reproject to TARGET_CRS, and resample to TARGET_RES."""
    with rasterio.open(jp2_path) as src:
        dst_transform, dst_w, dst_h = calculate_default_transform(
            src.crs, TARGET_CRS,
            src.width, src.height,
            *src.bounds,
            resolution=TARGET_RES,
        )
        data = np.zeros((dst_h, dst_w), dtype=src.dtypes[0])
        reproject(
            source=rasterio.band(src, 1),
            destination=data,
            src_transform=src.transform,
            src_crs=src.crs,
            dst_transform=dst_transform,
            dst_crs=TARGET_CRS,
            resampling=resampling,
        )
        profile = src.profile.copy()
        profile.update(
            crs=TARGET_CRS,
            transform=dst_transform,
            width=dst_w,
            height=dst_h,
            driver="GTiff",
            count=1,
        )
    return data, profile


def mosaic_tiles(tile_arrays: list[tuple[np.ndarray, dict]], resampling: Resampling = Resampling.bilinear) -> tuple[np.ndarray, dict]:
    """Mosaic multiple (data, profile) pairs for the same band using in-memory files."""
    mem_files = []
    datasets = []
    for data, profile in tile_arrays:
        mf = rasterio.MemoryFile()
        with mf.open(**profile) as ds:
            ds.write(data[np.newaxis, :, :])
        mem_files.append(mf)
        datasets.append(mf.open())

    merged, transform = merge(datasets, resampling=resampling)

    out_profile = datasets[0].profile.copy()
    out_profile.update(
        height=merged.shape[1],
        width=merged.shape[2],
        transform=transform,
        count=1,
    )
    for ds in datasets:
        ds.close()
    for mf in mem_files:
        mf.close()

    return merged[0], out_profile


# ---------------------------------------------------------------------------
# Load fire hulls (used for cropping)
# ---------------------------------------------------------------------------
hulls = gpd.read_file(HULLS_PATH)

# ---------------------------------------------------------------------------
# Main loop
# ---------------------------------------------------------------------------
for fire_dir in sorted(IMAGERY_DIR.iterdir()):
    if not fire_dir.is_dir():
        continue
    fire_name = fire_dir.name

    for period_dir in sorted(fire_dir.iterdir()):
        if not period_dir.is_dir():
            continue
        period_name = period_dir.name

        safe_dirs = sorted(period_dir.glob("*.SAFE"))
        if not safe_dirs:
            print(f"[{fire_name}/{period_name}] No SAFE directories found, skipping.")
            continue

        print(f"\n[{fire_name} / {period_name}] {len(safe_dirs)} tile(s)")

        stacked = []   # list of (data, profile) in band order
        labels  = []
        ref_profile = None

        for band_label, suffix in BANDS:
            resamp = Resampling.nearest if band_label == "SCL" else Resampling.bilinear
            tile_arrays = []
            for safe_dir in safe_dirs:
                jp2 = find_band_file(safe_dir, suffix)
                if jp2 is None:
                    print(f"  WARNING: {band_label} not found in {safe_dir.name}")
                    continue
                data, profile = read_resampled(jp2, resampling=resamp)
                tile_arrays.append((data, profile))

            if not tile_arrays:
                continue

            if len(tile_arrays) > 1:
                print(f"  Mosaicking {len(tile_arrays)} tiles for {band_label} …")
                data, profile = mosaic_tiles(tile_arrays, resampling=resamp)
            else:
                data, profile = tile_arrays[0]

            stacked.append(data)
            labels.append(band_label)
            if ref_profile is None:
                ref_profile = profile

        if not stacked:
            print("  No bands written, skipping.")
            continue

        out_dir = STACK_DIR / fire_name
        out_dir.mkdir(parents=True, exist_ok=True)
        out_path = out_dir / f"{period_name}_stack.tif"

        out_profile = ref_profile.copy()
        out_profile.update(
            count=len(stacked),
            dtype=stacked[0].dtype,
            compress="deflate",
            predictor=2,
            tiled=True,
            blockxsize=512,
            blockysize=512,
            interleave="band",
        )

        with rasterio.open(out_path, "w", **out_profile) as dst:
            for i, (data, label) in enumerate(zip(stacked, labels), start=1):
                dst.write(data, i)
                dst.update_tags(i, name=label)

        print(f"  Wrote {len(stacked)} bands ({', '.join(labels)}) → {out_path}")

        # -------------------------------------------------------------------
        # Crop to fire hull + buffer
        # -------------------------------------------------------------------
        fire_row = hulls[hulls["Fire"].str.lower() == fire_name.lower()]
        if fire_row.empty:
            print(f"  WARNING: no hull found for '{fire_name}', skipping crop.")
            continue

        # Buffer in TARGET_CRS (projected metres), then mask
        buffered = fire_row.to_crs(TARGET_CRS).geometry.buffer(BUFFER_M)
        shapes = [geom.__geo_interface__ for geom in buffered]

        with rasterio.open(out_path) as src:
            cropped, crop_transform = rio_mask(src, shapes, crop=True, all_touched=True)
            crop_profile = src.profile.copy()
            crop_profile.update(
                height=cropped.shape[1],
                width=cropped.shape[2],
                transform=crop_transform,
            )
            band_tags = [src.tags(i) for i in range(1, src.count + 1)]

        cropped_path = out_dir / f"{period_name}_stack_cropped.tif"
        with rasterio.open(cropped_path, "w", **crop_profile) as dst:
            dst.write(cropped)
            for i, tags in enumerate(band_tags, start=1):
                dst.update_tags(i, **tags)

        print(f"  Cropped to fire hull + {BUFFER_M} m → {cropped_path}")

print("\nAll stacks complete.")
