# %%
# File: dbf data appender - Project
# Class: Analysis of Spatial Data and Images, Winter 2022
# Author: David Ampudia Vicente  
"""
This script reads historical telephone data on Spanish municipalities and merges it on a dbf.
"""
# %%
#%%
import geopandas as gpd
import numpy as np
import pandas as pd
import os
from pathlib import Path

pd.options.display.max_columns = 300

basedir = Path(os.path.dirname(os.path.realpath(__file__))) # Paste directory otherwise
datadir = basedir / "data"
# %% Spanish historical data
mun_data = pd.read_csv(datadir / "municipalities_w_dist.csv", index_col=0)
# %%
# minor data adjustment for merge
mun_data = (
    mun_data
    .assign(
        Codigo = lambda df: (df["Codigo"]).astype(int).astype(str).str.pad(width=5, side="left", fillchar="0")
    )
)

# load town data and merge commercial banks
entities = pd.read_csv(datadir / "ENTIDADES.csv", sep=";", encoding="latin1")
entities = entities.loc[entities["TIPO"] == "Capital de municipio"]
entities["INEMUNI"] = (
    entities["INEMUNI"]
    .astype(int)
    .astype(str)
    .str
    .pad(width=5, side="left", fillchar="0")
)
# %%
entities = pd.merge(
    entities, 
    mun_data[["Codigo", "num_commercial_banks", "num_savings_banks", "year"]], 
    left_on="INEMUNI", right_on="Codigo", 
    how="right", indicator=True
)
# %%

# Create final data with geographical bank office information
# entities.fillna({"num_commercial_banks": 0, "num_savings_banks": 0}, inplace=True)
entities = entities[["CODIGOINE", "NOMBRE", "POBLACION", "INEMUNI", "LONGITUD_ETRS89", "LATITUD_ETRS89", "ALTITUD", "num_commercial_banks", "num_savings_banks", "year"]]

entities["LONGITUD_ETRS89"] = entities["LONGITUD_ETRS89"].str.replace(",",".").astype(float)
entities["LATITUD_ETRS89"] = entities["LATITUD_ETRS89"].str.replace(",",".").astype(float)
entities = gpd.GeoDataFrame(
    entities, 
    geometry=gpd.points_from_xy(entities.LONGITUD_ETRS89, entities.LATITUD_ETRS89)
)

xmin, ymin, xmax, ymax = entities.total_bounds

entities = entities.cx[-10:xmax, 35:ymax]
# %%

savings = entities.dropna(subset=["num_savings_banks"]).loc[lambda df: df.num_savings_banks!=0]
commercial = entities.dropna(subset=["num_commercial_banks"]).loc[lambda df: df.num_commercial_banks!=0]

savings.to_file(hworkdir / "xy_points" / "savings.shp")
commercial.to_file(hworkdir / "xy_points" / "commercial.shp")

# and so now I have XY files with commercial and savings banks. Let's get roads and topography. 
# %% DBF data (using geopandas) for output least cost distance data
mun_dbf = gpd.read_file(hworkdir / "SHP_municipios" / "PENINSULA89.shp")
#%%
costdist = pd.read_csv(hworkdir / "output" / "savings_cost_municipalities.txt", sep=",")
costdist["Codigo"] = (
    costdist["Codigo"]
    .astype(int)
    .astype(str)
    .str
    .pad(width=5, side="left", fillchar="0")
)

mun_dbf = pd.merge(
    mun_dbf,
    costdist[["Codigo", "MEAN"]],
    on="Codigo",
    how="left"
)

# %%
mun_dbf.to_file(hworkdir / "SHP_municipios" / "COSTOUTCOME.shp")
# Recall: spain's latitude z-order correction is 0.00001171
# %%
