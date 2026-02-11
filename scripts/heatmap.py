import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.ndimage import gaussian_filter
from PIL import Image

# --- Step 0: Load data ---
df = pd.read_excel("../data/addresses_with_coords.xlsx")
map_img = Image.open("../data/baden_1870.png").convert("RGB")
map_img = np.array(map_img)
ref = pd.read_excel("../data/ref_points.xlsx")

# --- Step 1: Ensure coordinates are floats ---
df['Latitude'] = pd.to_numeric(df['Latitude'].astype(str).str.replace(",", "."), errors='coerce')
df['Longitude'] = pd.to_numeric(df['Longitude'].astype(str).str.replace(",", "."), errors='coerce')
df = df.dropna(subset=['Latitude','Longitude'])

# --- Step 2: Affine transform ---
geo_pts = ref[['lat','lon']].values
img_pts = ref[['x_pix','y_pix']].values
A = np.column_stack((geo_pts, np.ones(len(geo_pts))))
x_coef, _, _, _ = np.linalg.lstsq(A, img_pts[:,0], rcond=None)
y_coef, _, _, _ = np.linalg.lstsq(A, img_pts[:,1], rcond=None)

latlon_array = df[['Latitude','Longitude']].values
df['x_pix'] = x_coef[0]*latlon_array[:,0] + x_coef[1]*latlon_array[:,1] + x_coef[2]
df['y_pix'] = y_coef[0]*latlon_array[:,0] + y_coef[1]*latlon_array[:,1] + y_coef[2]

# --- Step 3: Create weighted heatmap ---
heatmap = np.zeros((map_img.shape[0], map_img.shape[1]))
for x, y, w in zip(df['x_pix'], df['y_pix'], df['Party']):
    x_int, y_int = int(x), int(y)
    if 0 <= x_int < heatmap.shape[1] and 0 <= y_int < heatmap.shape[0]:
        heatmap[y_int, x_int] += w
heatmap = gaussian_filter(heatmap, sigma=15)

# --- Step 4: Log scaling + clipping ---
vmax = np.percentile(heatmap, 99)
heatmap_scaled = np.log1p(np.clip(heatmap, 0, vmax))

# --- Step 5: Overlay heatmap with capped alpha so map remains visible ---
import matplotlib.pyplot as plt
from matplotlib import colors
import numpy as np

fig, ax = plt.subplots(figsize=(10,10))

# Show the basemap
ax.imshow(map_img, origin="upper", interpolation="none")

# --- Scale and boost heatmap ---
vmax = np.percentile(heatmap, 99)
heat_clipped = np.clip(heatmap, 0, vmax)

# Boost small densities (nonlinear scaling)
heat_boosted = np.sqrt(heat_clipped / heat_clipped.max())  # values 0–1

# --- Cap the alpha so map is always visible ---
alpha_max = 0.8  # maximum opacity
heat_alpha = heat_boosted * alpha_max  # scale alpha

# Create RGBA heatmap
cmap = plt.cm.hot
heat_rgba = cmap(heat_boosted)
heat_rgba[..., -1] = heat_alpha  # apply capped alpha

# Overlay heatmap
ax.imshow(heat_rgba, origin="upper", interpolation="gaussian")

ax.axis("off")
plt.savefig("heatmap_glow_capped.png", dpi=300, bbox_inches="tight", pad_inches=0)
plt.show()