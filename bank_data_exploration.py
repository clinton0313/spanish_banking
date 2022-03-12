#%%
import geopandas as gpd
import numpy as np
import os
import pandas as pd


from libpysal.weights.util import fill_diagonal
from pysal.model import spreg
from pysal.lib import weights
from shapely.geometry import Point
from typing import Union

DATAPATH="data"
EPSG = 32630
mainland_bounds = (-0.2e6, 1.5e6, 3.7e6, 5.1e6)

#%%
bank_data = pd.read_csv(os.path.join(DATAPATH, "municipalities_w_dist.csv"))

bank_data["geometry"] = bank_data.apply(lambda x: Point(x["longitude"], x["latitude"]), axis=1)
bank_data = gpd.GeoDataFrame(data=bank_data, geometry="geometry", crs="ETRS89")
bank_data = bank_data.to_crs(epsg=EPSG).cx[mainland_bounds[0]:mainland_bounds[1], mainland_bounds[2]:mainland_bounds[3]]

#%%
def lag_year(data:pd.DataFrame, kernels:list, bandwidths:list, year:Union[float, int], x_cols:list, year_col:str = "year"):
    yearly_data = data[data[year_col] == year].reset_index(drop=True)
    combined_data = yearly_data.copy()
    for kernel in kernels:
        for bandwidth in bandwidths:
            w = weights.Kernel.from_dataframe(yearly_data, function=kernel, fixed=True, bandwidth=bandwidth, k=50)
            w = fill_diagonal(w, 0)
            w.transform = 'r'
            wX = weights.spatial_lag.lag_spatial(w, yearly_data[x_cols].values.astype(float))
            lagged_data = pd.DataFrame(wX, columns = [f"lag_{kernel}_{bandwidth//1000}_{name}" for name in x_cols])
            combined_data = pd.concat((combined_data, lagged_data), axis=1)
    return combined_data

def add_lags(data: pd.DataFrame, kernels:list, bandwidths:list, x_cols:list, year_col:str = "year"):
    lagged_bank_data = [lag_year(data, kernels, bandwidths, year, x_cols, year_col) for year in data[year_col].unique()]
    return pd.concat(lagged_bank_data, axis=0)
#%%
kernels = ["triangular", "gaussian"]
bandwidths = range(5000, 40000, 5000)
lagged_bank_data = add_lags(
    bank_data, 
    kernels, 
    bandwidths, 
    ["num_commercial_banks", "num_savings_banks", "num_commercial_banks_p1000c", "comm_lic_1", "comm_lic_1_p1000c"]
)
lagged_bank_data.to_csv(os.path.join(DATAPATH, "lagged_municipalities_w_dist.csv"))

#%%


