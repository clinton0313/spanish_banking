#%%
import geopandas as gpd
import matplotlib.pyplot as plt
import os
import pandas as pd
import regex as re
import rasterio as rio

from rasterio.plot import show
from shapely.geometry import Point, mapping

os.chdir(os.path.dirname(os.path.realpath(__file__)))


ADM_PATH = "ESP_adm"
DATAPATH = "data"
EPSG = 32630
mainland_bounds = (0.3e6, 1.5e6, 3.7e6, 5.1e6)

#%%

adm_data = gpd.read_file(os.path.join(ADM_PATH, "ESP_adm3.shp"))
bank_data = pd.read_csv(os.path.join(DATAPATH, "municipalities_data.csv"))

bank_data["geometry"] = bank_data.apply(lambda x: Point(x["LONGITUD_ETRS89"], x["LATITUD_ETRS89"]), axis=1)
bank_data = gpd.GeoDataFrame(data=bank_data, geometry="geometry", crs="ETRS89")
bank_data = bank_data.to_crs(epsg=EPSG).cx[mainland_bounds[0]:mainland_bounds[1], mainland_bounds[2]:mainland_bounds[3]]
adm_data = adm_data.to_crs(epsg=EPSG).cx[mainland_bounds[0]:mainland_bounds[1], mainland_bounds[2]:mainland_bounds[3]]
bank_map = rio.open(os.path.join(DATAPATH, "spanish_map_georeferenced_epsg32630.tif"))

# %%
fig, ax = plt.subplots(figsize=(14,14))
show(bank_map, ax=ax)
adm_data.plot(ax=ax)
bank_data.plot(ax=ax, column="treatment", markersize=10, legend=True)

# %%
