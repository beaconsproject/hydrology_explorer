
## Welcome to the BEACONs Hydrology Explorer

The ecological condition of an **area of interest (AOI)** - such as a conservation area, mine site, or management zone- depends not only on what 
happens within its boundaries, but also on activities in the surrounding landscape. Rivers and streams connect the AOI to upstream and downstream 
areas, allowing activities upstream to affect the AOI and the AOI to influence conditions downstream.

**BEACONs Hydrology Explorer** enables users to identify upstream and downstream areas associated with an area of interest (AOI) and explore the 
hydrologic metrics, using the BEACONs catchment dataset as building blocks. A built-in **User Guide** tab provides step-by-step instructions and 
function descriptions, while the **Dataset Requirements** tab details data formats and spatial layers needed to run the app.
  
<br>

### Input data

The app needs a **study area**, **catchments** and **streams**. You can get started in three ways:

- **Demo dataset**: a ready-to-use dataset for the Dawson area in central Yukon, Canada.
- **Study area only**: upload a study area, and the app extracts catchments and streams from the BEACONs boreal catchment dataset.
- **Advanced upload**: upload Shapefiles, a GeoPackage or a CSV of file paths that follow the structure described in **Dataset Requirements**.

Layers from a **Disturbance Explorer** GeoPackage (e.g., undisturbed areas, fires, protected areas, mining claims) can also be loaded for display and intactness calculations.

&#x1F4CC; **Note:**  All layers must use the same projected coordinate system, and the catchments and streams must cover the full study area.

<br>

### Functionality and Workflow
    
The app consists of the following sections:

#### **1. Set input parameters**

  - Use the demo dataset or upload the required spatial layers. Layers can be uploaded either as Shapefiles, a GeoPackage or by 
  providing a csv with file pathways for each Shapefile layer. If a custom GeoPackage is uploaded, spatial layer names must match the expected names. 

  - Preview the spatial layers (e.g., study area, catchments and streams)

&#x1F4CC; **Note:**  All layers must have the same projection. Additionally, the catchments and stream segments must capture the full extent of the study area to ensure accurate analysis.

#### **2. Set intactness**

 Choose how catchment intactness is measured: an intactness attribute in the catchments, the undisturbed layer from Disturbance Explorer, 
 or an uploaded intactness layer.
   
#### **3. Add display elements** (OPTIONAL)

This section allows users to add up to three vector layers (points, lines, or polygons) for visualization only. These layers 
cannot be rasters. The file or layer names are automatically used as display names on the map. Colours are assigned by the app and cannot 
be modified.

#### **4. Set feature to track** (OPTIONAL)

Pick a polygon layer (e.g., fires) whose area is reported within the AOI and the upstream and downstream areas.

#### **5. Select AOI**

Define an area of interest (AOI) by either uploading a spatial layer or selecting a set of catchments found within the study area.

#### **6. Generate upstream and downstream**

This section identifies the areas upstream and downstream of the AOI and displays intactness, hydrology, and wildfire statistics such as total area upstream and % area burned. 

#### **7. Download results**

Export a GeoPackage with the upstream, downstream stem and downstream areas plus the input layers (e.g., study area, AOI, catchments, streams), ready to open in a GIS such as QGIS. 

<br>

### BEACONs Hydrology Explorer workflow diagram

The workflow diagram below provides an overview of the process.

<br><br>
<center><img src="pics/workflow.png" width="800"></center>
<br><br>
