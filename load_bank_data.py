#%%
import geopandas as gpd
import matplotlib.pyplot as plt
import os
import pandas as pd
import regex as re

from shapely.geometry import Point

os.chdir(os.path.dirname(os.path.realpath(__file__)))


ADM_PATH = "ESP_adm"
DATAPATH = "data"
#%%

adm_data = gpd.read_file(os.path.join(ADM_PATH, "ESP_adm3.shp"))

bank_data = pd.read_csv(os.path.join(DATAPATH, "municipalities_data.csv"))
# %%

bank_data["geometry"] = bank_data.apply(lambda x: Point(x["LONGITUD_ETRS89"], x["LATITUD_ETRS89"]), axis=1)
bank_data = gpd.GeoDataFrame(data=bank_data, geometry="geometry", crs="ETRS89")
bank_data.set_crs(bank_data.crs)
# %%
fig, ax = plt.subplots(figsize=(14,14))
adm_data.plot(ax=ax)
bank_data.plot(ax=ax, column="treatment", markersize=10, legend=True)
# %%
