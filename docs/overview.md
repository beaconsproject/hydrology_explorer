
## Welcome to the BEACONs Hydrology Explorer

 **BEACONs Hydrology Explorer** uses the BEACONs catchment dataset as building blocks to identify areas upstream and downstream 
 of a user-defined Area of Interest (AOI) (e.g., conservation area, mine site, etc.) within a predefined study area, as well as associated hydrologic metrics. 
 From here, you can access:

  - The **User Guide** tab provides step-by-step instructions for using the app.

  - The **Dataset Requirements** tab describes the spatial layers and associated attributes needed to run the analyses.
  

## Input data

**BEACONs Hydrology Explorer** requires several key spatial layers such as a study area, catchments, and stream layers. Details on the necessary layers, their attributes, and formatting can 
be found under the **Dataset Requirements** tab. There are twooptions for input data: 

1. A demo dataset for the Dawson area in central Yukon, Canada, is included with the app.
2. Users can upload their own data using one of the available upload options, as long as the data follows the required structure. 


## Functionality
    
The main functionality of the app consists of the following sections:
    

#### - Set input parameters

  - Use the demo dataset or upload the required spatial layers. Layers can be uploaded either as Shapefiles, a GeoPackage or by 
  providing a csv where access path are defined for each Shapefile layers. If a custom GeoPackage, layer names must match the expected names. 

  - Preview the spatial layers (studya rea, catchments and streams segments)

📌 Note: All layers must have the same projection. Additionally, the catchments and stream segments must capture the full extent of the study areas to ensure accurate analysis.

<br>
   
#### - Add display elements (OPTIONAL)

This section allows users to add additional features for visualization. These features must be vector data (points, lines, or polygons) and 
cannot be rasters. A maximum of three additional features can be added. The file or layer names are automatically used as display names on 
the map. Colors are assigned by the app and cannot be modified.

<br>

#### - Select AOI

Define an Area of Interest (AOI) by either uploading a spatial layer or selecting a set of catchments found within the study area

<br>

#### - Generate upstream and downstream

This section launch the calculation of statistics such as total areas and mean of catchment area weighted intactness along the upstream, 
downstream and downstream stem related to the chosen AOI. Hydrological metrics are provided in the adjacent table. 

<br>

#### - Download results

Download a GeoPackage of the upstream, downstream stem and overall downstream areas created by the app, as well as the input spatial layers (e.g., study area, AOI, catchments, streams).

<br>

### BEACONs Hydrology Explorer workflow diagram

The worflow diagram below provides an overview of the process.

<br><br>
<center><img src="pics/workflow.png" width="600"></center>
<br><br>
