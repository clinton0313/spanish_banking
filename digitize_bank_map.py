#%%
import geopandas as gpd
import json
import matplotlib.pyplot as plt
import numpy as np
import os
import osgeo.gdal as gdal
import rasterio


from affine import Affine
from rasterio.mask import mask
from shapely.geometry import mapping, MultiPolygon
from vectorize import gdf_to_graph, array2shp, raster2array, pixelOffset2coord, path_to_edges

ADM_PATH = "ESP_adm"
DATAPATH = "maps"
PROCESSEDPATH = os.path.join(DATAPATH, "processed")
EPSG = 32630
MAP = "trafico1975"
CLIPPED_FN = f"{MAP}_clipped.tif"
DIGITIZED_FN = f"{MAP}_digitized.tif"
SHAPE_FILE = f"{MAP}_vectorized.shp"
PROJECTED_MAP = f"{MAP}_reprojected.tif"
mainland_bounds = (-0.2e6, 1.5e6, 3.7e6, 5.1e6)

#%%
#Digitizing the map

# thresholds = [220., 210., 160.] 1960 thresholds
# thresholds = [200., 200., 170.] 1965 and 1970 thresholds
thresholds = [200., 200., 160.]
bins = [np.array([0., threshold]) for threshold in thresholds]

bank_tif = gdal.Open(os.path.join(DATAPATH, PROJECTED_MAP), gdal.GA_ReadOnly)
bank_transform = bank_tif.GetGeoTransform()
rasterbands = [bank_tif.GetRasterBand(i).ReadAsArray() for i in range(1,4)]


digitized_bands = [(np.digitize(band, bins =bin ) - 1) for band, bin in zip(rasterbands, bins)]
digitized = ((digitized_bands[0] + digitized_bands[1] + digitized_bands[2]) > 2)
digitized = (1 - np.float32(digitized)) * 255

fig, ax = plt.subplots(figsize=(26,26))
ax.imshow(digitized, cmap="Greys")

#%%
#Save raster to file

with rasterio.open(
    os.path.join(PROCESSEDPATH, DIGITIZED_FN), 'w',
    driver="GTiff",
    height=digitized.shape[0],
    width=digitized.shape[1],
    count=1,
    dtype=np.float32,
    crs=f"EPSG:{EPSG}",
    transform=Affine.from_gdal(*bank_transform)
) as outfile:
    outfile.write(digitized, 1)
outfile.close()

#%% Clipping the map
bank_map = rasterio.open(os.path.join(PROCESSEDPATH, DIGITIZED_FN), "r")
adm_data = gpd.read_file(os.path.join(ADM_PATH, "ESP_adm4.shp"))
adm_data = adm_data.to_crs(epsg=EPSG).cx[mainland_bounds[0]:mainland_bounds[1], mainland_bounds[2]:mainland_bounds[3]]
spain = adm_data.unary_union
shape = [mapping(spain)]

out_img, out_transform = mask(bank_map, shapes=shape)
out_meta = bank_map.meta.copy()
out_meta["nodata"] = np.nan

with rasterio.open(os.path.join(PROCESSEDPATH, CLIPPED_FN), "w", **out_meta) as outfile:
    outfile.write(out_img)
outfile.close()

# %%
# array = raster2array(os.path.join(PROCESSEDPATH, CLIPPED_FN))
# array2shp(
#     array=array, 
#     outSHPfn=os.path.join(PROCESSEDPATH, SHAPE_FILE), 
#     rasterfn=os.path.join(PROCESSEDPATH, CLIPPED_FN), 
#     pixelValue=255.
#     )

# %%
