import os
import numpy as np
import itertools
from math import sqrt,ceil
import osgeo.gdal as gdal
import osgeo.ogr as ogr
import networkx as nx
from shapely.geometry import Point
import pandas as pd
import geopandas as gpd
from tqdm import tqdm



def gdf_to_graph(gdf):

    # Find all unique start & end points and assign them an id
    gdf["start_node_coords"] = gdf["geometry"].apply(lambda x: x.coords[0])
    gdf["end_node_coords"] = gdf["geometry"].apply(lambda x: x.coords[-1])
    node_ids = {}
    i = 0
    for index, row in gdf.iterrows():
        node_1 = row["start_node_coords"]
        node_2 = row["end_node_coords"]
        if node_1 not in node_ids:
            node_ids[node_1] = i
            i += 1
        if node_2 not in node_ids:
            node_ids[node_2] = i
            i += 1

    # Assign the unique id to each
    gdf["source"] = gdf["start_node_coords"].apply(lambda x: node_ids[x])
    gdf["target"] = gdf["end_node_coords"].apply(lambda x: node_ids[x])

    gdf["length"] = gdf["geometry"].apply(lambda x: x.length)

    node_ids = {node_ids[k]: Point(k) for k in node_ids}
    node_df = pd.DataFrame.from_dict(node_ids, orient="index", columns=["geometry"])
    node_gdf = gpd.GeoDataFrame(node_df, geometry="geometry")
    # node_gdf.to_file("data/out/rivers_nodes.gpkg", driver="GPKG")

    # Drop these columns because they cannot be exported to GeoJSON
    gdf.drop("start_node_coords", axis=1, inplace=True)
    gdf.drop("end_node_coords", axis=1, inplace=True)

    # gdf.to_file("data/out/rivers_unbraided.gpkg", driver="GPKG")

    graph = nx.from_pandas_edgelist(gdf, edge_attr=["length", "geometry"])
    for n in node_gdf.index:
        graph.nodes[n]["geometry"] = node_gdf.iloc[n]

    return graph

def pixelOffset2coord(rasterfn,xOffset,yOffset):
    raster = gdal.Open(rasterfn)
    geotransform = raster.GetGeoTransform()
    originX = geotransform[0]
    originY = geotransform[3]
    pixelWidth = geotransform[1]
    pixelHeight = geotransform[5]
    coordX = originX+pixelWidth*xOffset
    coordY = originY+pixelHeight*yOffset
    return coordX, coordY

def raster2array(rasterfn):
    raster = gdal.Open(rasterfn)
    band = raster.GetRasterBand(1)
    array = band.ReadAsArray()
    return array

def array2shp(array,outSHPfn,rasterfn,pixelValue):

    # max distance between points
    raster = gdal.Open(rasterfn)
    geotransform = raster.GetGeoTransform()
    pixelWidth = geotransform[1]
    maxDistance = ceil(sqrt(2*pixelWidth*pixelWidth))
    print(maxDistance)

    # array2dict
    count = 0
    roadList = np.where(array == pixelValue)
    multipoint = ogr.Geometry(ogr.wkbMultiLineString)
    pointDict = {}
    for indexY in tqdm(roadList[0]):
        indexX = roadList[1][count]
        Xcoord, Ycoord = pixelOffset2coord(rasterfn,indexX,indexY)
        pointDict[count] = (Xcoord, Ycoord)
        count += 1

    # dict2wkbMultiLineString
    shpDriver = ogr.GetDriverByName("ESRI Shapefile")
    if os.path.exists(outSHPfn):
        shpDriver.DeleteDataSource(outSHPfn)
    outDataSource = shpDriver.CreateDataSource(outSHPfn)
    outLayer = outDataSource.CreateLayer(outSHPfn, geom_type=ogr.wkbLineString)
    featureDefn = outLayer.GetLayerDefn()
    outFeature = ogr.Feature(featureDefn)
    for i in tqdm(itertools.combinations(pointDict.values(), 2)):
        point1 = ogr.Geometry(ogr.wkbPoint)
        point1.AddPoint(i[0][0],i[0][1])
        point2 = ogr.Geometry(ogr.wkbPoint)
        point2.AddPoint(i[1][0],i[1][1])

        distance = point1.Distance(point2)

        if distance < maxDistance:
            line = ogr.Geometry(ogr.wkbLineString)
            line.AddPoint(i[0][0],i[0][1])
            line.AddPoint(i[1][0],i[1][1])

            line.AddGeometry(line)
            outFeature.SetGeometry(line)
            outLayer.CreateFeature(outFeature)


def main(rasterfn,outSHPfn,pixelValue):
    array = raster2array(rasterfn)
    array2shp(array,outSHPfn,rasterfn,pixelValue)


def path_to_edges(path, cycle=False):
    edges = []
    for i in range(len(path)-1):
        edges.append((path[i], path[i+1]))
    if cycle:
        edges.append((path[-1], path[0]))
    return edges




# if __name__ == "__main__":
#     rasterfn = 'data/pathfinder_output.tiff'
#     outSHPfn = 'data/test.shp'
#     pixelValue = 3
#     main(rasterfn,outSHPfn,pixelValue)

#     graph = gdf_to_graph(gpd.GeoDataFrame.from_file("data/test.shp"))
#     terminal_nodes = [n for n in graph.nodes if nx.degree(graph, n) == 1]

#     cycle_paths = [path_to_edges(path, cycle=True) for path in nx.cycle_basis(graph, terminal_nodes[0])]
#     cycle_edges = set([item for sublist in cycle_paths for item in sublist])

#     selected_edges = set()
#     for terminal_node in terminal_nodes[1:]:
#         edge_path = path_to_edges(nx.shortest_path(graph, terminal_nodes[0], terminal_node, weight="length"))
#         selected_edges.update(set(edge_path))

#     subgraph = graph.edge_subgraph(selected_edges)
#     out_df = nx.to_pandas_edgelist(subgraph)
#     out_gdf = gpd.GeoDataFrame(out_df)
#     out_gdf.set_geometry("geometry")
#     out_gdf.to_file("data/route_test.shp", driver="ESRI Shapefile")