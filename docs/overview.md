
## Welcome to the BEACONs Hydrology Explorer

 **BEACONs Hydrology Explorer** identifies areas upstream and downstream of an area of interest (AOI; e.g., conservation area, mine site, etc.) and associated hydrologic metrics, using the BEACONs catchment dataset as building blocks. A built-in **User Guide** tab provides step-by-step instructions and function descriptions, while the **Dataset Requirements** tab details data formats and spatial layers needed to run the app.
  
## Input data

**BEACONs Hydrology Explorer** requires key spatial layers such as a study area, catchments, and stream layers. Details on the necessary layers, and their attributes and format can be found under the **Dataset Requirements** tab. A **demo dataset** is included with the app for the Dawson area in central Yukon, Canada. Users can upload their own Shapefiles or GeoPackage, provided the spatial layers follow the required structure. Please refer to the **Dataset Requirements** tab for details on the required spatial layers and associated attributes and formatting.

## Functionality
    
The app consists of the following sections:
    
#### Set input parameters

  - Use the demo dataset or upload the required spatial layers. Layers can be uploaded either as Shapefiles, a GeoPackage or by 
  providing a csv with file pathways for each Shapefile layer. If a custom GeoPackageis uploaded, spatial layer names must match the expected names. 

  - Preview the spatial layers (e.g., study area, catchments and streams)

📌 Note: All layers must have the same projection. Additionally, the catchments and stream segments must capture the full extent of the study area to ensure accurate analysis.

   
#### Add display elements (OPTIONAL)

This section allows users to upload additional spatial layers for visualization. These layers must be vector data (points, lines, or polygons) and 
cannot be rasters. A maximum of three additional spatial layers can be added. The file or layer names are automatically used as display names on 
the map. Colors are assigned by the app and cannot be modified.


#### Select AOI

Define an area of interest (AOI) by either uploading a spatial layer or selecting a set of catchments found within the study area.


#### Generate upstream and downstream

This section identifies the areas upstream and downstream of the AOI and displays intactness, hydrology, and wildfire statistics such as total area upstream and % area burned. 

#### Download results

Download a GeoPackage of the upstream and downstream areas created by the app, as well as the input spatial layers (e.g., study area, AOI, catchments, streams).


## BEACONs Hydrology Explorer workflow diagram

The worflow diagram below provides an overview of the process.

<br><br>
<center><img src="pics/workflow.png" width="600"></center>
<br><br>
