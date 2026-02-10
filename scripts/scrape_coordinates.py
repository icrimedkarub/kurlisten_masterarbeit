import pandas as pd
from geopy.geocoders import Nominatim
from geopy.extra.rate_limiter import RateLimiter
import re
import time
from tqdm import tqdm

# === FILE PATH ===
file_path = '../data/towns.xlsx'

# === LOAD DATA ===
df = pd.read_excel(file_path)

# Ensure Coordinates column exists
if 'Coordinates' not in df.columns:
    df['Coordinates'] = None

# === SETUP GEOCODER ===
geolocator = Nominatim(user_agent="place_coordinate_fetcher_burak_d")
geocode = RateLimiter(geolocator.geocode, min_delay_seconds=1, max_retries=2)

# === HELPER FUNCTION ===
def extract_place_and_country(location_ref):
    """Extracts the place name and optional country code (in brackets)."""
    match = re.match(r'^(.*?)\s*\((\w{2})\)$', str(location_ref).strip())
    if match:
        place, country_code = match.groups()
        return place.strip(), country_code.upper()
    return location_ref.strip(), None

# === FETCH COORDINATES FUNCTION ===
def fetch_coordinates(place, country=None):
    """Try to fetch coordinates for a place, first with, then without country code."""
    query = f"{place}, {country}" if country else place
    try:
        location = geocode(query)
        if location:
            return f"Point({location.longitude} {location.latitude})"
        # Retry without country if failed the first time
        if country:
            location = geocode(place)
            if location:
                return f"Point({location.longitude} {location.latitude})"
    except Exception as e:
        print(f"Error fetching {query}: {e}")
    return None

# === MAIN LOOP WITH PROGRESS ===
total_rows = df.shape[0]
print(f"🔍 Starting coordinate lookup for {total_rows} rows...")

for idx, row in tqdm(df.iterrows(), total=total_rows, desc="Processing rows"):
    loc = row.get('Location_Reference')
    if pd.isna(loc):
        continue  # skip empty
    if pd.notna(row.get('Coordinates')):
        continue  # skip already processed

    place, country = extract_place_and_country(loc)
    coords = fetch_coordinates(place, country)

    if coords:
        df.at[idx, 'Coordinates'] = coords
        tqdm.write(f"{idx+1}: ✅ {loc} → {coords}")
    else:
        tqdm.write(f"{idx+1}: ⚠️ {loc} → Not found")

    # Safety sleep in case of long runs
    time.sleep(1)

# === SAVE OUTPUT ===
output_file = file_path.replace('.xlsx', '_with_coordinates.xlsx')
df.to_excel(output_file, index=False)
print(f"\n✅ Coordinates added and file saved as:\n{output_file}")
