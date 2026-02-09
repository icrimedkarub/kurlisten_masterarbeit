import pandas as pd
import re
import json
from datetime import datetime

df = pd.read_excel("/Users/heima/Desktop/Projects/aquae_europae/files/towns.xlsx")
df.columns = df.columns.str.strip()
df = df.fillna("")

def parse_coords(point_str):
    match = re.match(r"Point\(([-\d\.]+) ([-\d\.]+)\)", str(point_str))
    if match:
        lng, lat = match.groups()
        return float(lat), float(lng)
    return None, None

df["Latitude"], df["Longitude"] = zip(*df["Coordinates"].apply(parse_coords))

selected = [
    "Name", "Alternative_Names", "Country", "Type",
    "List_Availability", "Progress", "Link",
    "Latitude", "Longitude"
]

data = df[selected].to_dict(orient="records")

with open("/Users/heima/PycharmProjects/aquae_europae/data/towns.json", "w", encoding="utf-8") as f:
    json.dump(data, f, ensure_ascii=False, indent=2)

with open("/Users/heima/PycharmProjects/aquae_europae/data/metadata.json", "w", encoding="utf-8") as f:
    json.dump(
        {"last_updated": datetime.now().strftime("%d.%m.%Y")},
        f,
        indent=2,
        ensure_ascii=False
    )