#%%
import geopandas as gpd
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

bank_data.apply(lambda x: Point(x["LATITUD_ETRS89"], x["LONGITUD_ETRS89"]), axis=1)
# %%
