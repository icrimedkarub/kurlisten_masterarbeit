import pandas as pd
import time
import os
from geopy.geocoders import Nominatim

# --- Paths (relative to scripts/) ---
DATA_DIR = "../data"
CACHE_DIR = "../cache"

os.makedirs(CACHE_DIR, exist_ok=True)

cache_file = f"{CACHE_DIR}/geocode_cache.xlsx"

# --- Load cleaned addresses ---
df = pd.read_excel(f"{DATA_DIR}/addresses_clean.xlsx")

# Extract unique addresses
unique_addresses = df["Address"].dropna().unique()

# Geolocator
geolocator = Nominatim(user_agent="kurlisten_geocoder")

# --- Load cache if it exists ---
if os.path.exists(cache_file):
    cache_df = pd.read_excel(cache_file)
else:
    cache_df = pd.DataFrame(columns=["Address", "Latitude", "Longitude"])

# Turn cache into lookup dict
coords = dict(
    zip(
        cache_df["Address"],
        zip(cache_df["Latitude"], cache_df["Longitude"])
    )
)

# --- Geocoding loop ---
for i, addr in enumerate(unique_addresses, start=1):

    if addr in coords and coords[addr] != (None, None):
        print(f"[{i}/{len(unique_addresses)}] SKIP (cached): {addr}")
        continue

    try:
        location = geolocator.geocode(f"{addr}, Baden bei Wien, Austria")
        if location:
            coords[addr] = (location.latitude, location.longitude)
            print(f"[{i}/{len(unique_addresses)}] SUCCESS: {addr}")
        else:
            coords[addr] = (None, None)
            print(f"[{i}/{len(unique_addresses)}] NOT FOUND: {addr}")
    except Exception as e:
        coords[addr] = (None, None)
        print(f"[{i}/{len(unique_addresses)}] ERROR: {addr} ({e})")

    # --- Save cache after every request ---
    pd.DataFrame.from_dict(
        coords,
        orient="index",
        columns=["Latitude", "Longitude"]
    ).reset_index().rename(columns={"index": "Address"}).to_excel(
        cache_file,
        index=False
    )

    time.sleep(1)  # polite pause for Nominatim

# --- Merge results back into full dataset ---
coords_df = pd.DataFrame.from_dict(
    coords,
    orient="index",
    columns=["Latitude", "Longitude"]
).reset_index().rename(columns={"index": "Address"})

df_with_coords = df.merge(coords_df, on="Address", how="left")

# --- Save final output ---
df_with_coords.to_excel(
    f"{DATA_DIR}/addresses_with_coords.xlsx",
    index=False
)