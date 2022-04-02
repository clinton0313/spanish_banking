# %%
import arcpy
import xlrd
import fnmatch
from os import listdir
from xlrd import open_workbook, cellname
from arcpy import env
from arcpy.sa import *
arcpy.env.overwriteOutput=True
arcpy.SetLogHistory(False)
arcpy.CheckOutExtension("spatial")

# %% Compute slope
arcpy.gp.Slope_sa("Elevation", "Slope", "DEGREE", "0.00001171") # Adjusting for Spain's lat 40

# %% Savings and commercial banks to raster

for i in np.arange(1963,1975,1):
    arcpy.AddXY_management("commercial"+str(i))
    arcpy.PointToRaster_conversion(
        in_features="commercial"+str(i), 
        value_field="year", 
        out_rasterdataset="commercial"+str(i)+"_r",
        cell_assignment="MAXIMUM", 
        priority_field="NONE", 
        cellsize="0.02"
    )

for i in np.arange(1963,1975,1):
    arcpy.AddXY_management("savings"+str(i))
    arcpy.PointToRaster_conversion(
        in_features="savings"+str(i), 
        value_field="year", 
        out_rasterdataset="savings"+str(i)+"_r",
        cell_assignment="MAXIMUM", 
        priority_field="NONE", 
        cellsize="0.02"
    )

# %% Compute cost, heuristics formula (several specifications used in practice)
arcpy.gp.RasterCalculator_sa(
    """Con(IsNull("roads1960"),5+"Slope",2)""", 
    "cost_raster"
)

lst = [
    {"year": 1963, "map": 1960},
    {"year": 1964, "map": 1960},
    {"year": 1965, "map": 1965},
    {"year": 1966, "map": 1965},
    {"year": 1967, "map": 1965},
    {"year": 1968, "map": 1965},
    {"year": 1969, "map": 1965},
    {"year": 1970, "map": 1970},
    {"year": 1971, "map": 1970},
    {"year": 1972, "map": 1970},
    {"year": 1973, "map": 1970},
    {"year": 1974, "map": 1970}
]
# %% # Cost distance with respect to savings banks
for item in lst:
    arcpy.gp.CostDistance_sa(
        "savings"+str(item["year"]) + "_r",
        "costraster" + str(item["map"]) + "ras",
        "savings_distance" + str(item["year"]),
        "", "", "", "", "", ""
    )

# %% Draw municipality-level statistics of distance to closest savings branch
arcpy.gp.ZonalStatisticsAsTable_sa(
    "PENINSULA89", 
    "Codigo", "distance", "distance_municipality", "DATA", "ALL"
)