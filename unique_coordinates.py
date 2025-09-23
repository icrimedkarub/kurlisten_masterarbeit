import pandas as pd
import time
import os
from geopy.geocoders import Nominatim

# Load cleaned addresses
df = pd.read_excel("/Users/heima/Desktop/Masterarbeit/Kurlisten/Kurlisten_Excel/addresses_clean.xlsx")

# Extract unique addresses
unique_addresses = df['Address'].dropna().unique()

# Geolocator
geolocator = Nominatim(user_agent="my_geocoder")

# --- Load cache if it exists ---
cache_file = "/Users/heima/Desktop/geocode_cache.xlsx"
if os.path.exists(cache_file):
    cache_df = pd.read_excel(cache_file)
else:
    cache_df = pd.DataFrame(columns=["Address", "Latitude", "Longitude"])

# Turn into dict for fast lookup
coords = dict(zip(cache_df["Address"], zip(cache_df["Latitude"], cache_df["Longitude"])))

# Geocode addresses not yet in cache
for i, addr in enumerate(unique_addresses, start=1):
    if addr in coords and coords[addr] != (None, None):
        print(f"[{i}/{len(unique_addresses)}] SKIP (cached): {addr}")
        continue

    try:
        location = geolocator.geocode(f"{addr}, Baden bei Wien, Austria")
        if location:
            coords[addr] = (location.latitude, location.longitude)
            print(f"[{i}/{len(unique_addresses)}] SUCCESS: {addr} → ({location.latitude:.5f}, {location.longitude:.5f})")
        else:
            coords[addr] = (None, None)
            print(f"[{i}/{len(unique_addresses)}] NOT FOUND: {addr}")
    except Exception as e:
        coords[addr] = (None, None)
        print(f"[{i}/{len(unique_addresses)}] ERROR: {addr} ({e})")

    # Save cache every iteration (so we never lose progress)
    pd.DataFrame.from_dict(coords, orient="index", columns=["Latitude", "Longitude"]) \
        .reset_index().rename(columns={"index": "Address"}) \
        .to_excel(cache_file, index=False)

    time.sleep(1)  # polite pause

# --- Final results DataFrame ---
coords_df = pd.DataFrame.from_dict(coords, orient="index", columns=["Latitude", "Longitude"]).reset_index()
coords_df.rename(columns={"index": "Address"}, inplace=True)

# Merge back into full dataset
df_with_coords = df.merge(coords_df, on="Address", how="left")

# Save output
df_with_coords.to_excel("//Users/heima/Desktop/Masterarbeit/Kurlisten/Kurlisten_Excel/addresses_with_coords_test.xlsx", index=False)
