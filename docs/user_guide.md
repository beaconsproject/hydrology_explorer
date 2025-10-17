---
format: md
---

# User guide
When studying or managing landscapes, it is important to recognize that the condition and function of **an area of interest (AOI) such as a conservation area or management zone** can be influenced not only by activities within its boundaries, but also by factors beyond them. Areas are vulnerable to external pressures through edge effects as well as the movement of surface and groundwater. Because rivers and other aquatic systems are highly connected, an AOI may be affected by human activities both upstream and downstream, including water diversion, pollution, or the loss of natural inputs such as organic matter from riparian zones (Pringle 2001). **An AOI such as a mine site or dam** may be a source of pollutants or changes to water flow. To better understand and manage these influences, it is valuable to consider the hydrologic connectivity of the AOI and to identify the extent to which it is connected to upstream and downstream areas. This perspective helps quantify human impact and supports more informed decisions for stewardship, monitoring, and long-term sustainability.

**BEACONs Hydrology Explorer** identifies areas upstream and downstream of an AOI and associated hydrologic metrics. This page provides step-by-step instructions.

The **Welcome** section includes the **Overview** landing page that provides a concise introduction to the app and its functionality. It also includes this guide and **Dataset Requirements** that outlines the description, naming convention, and data structure of the required spatial layers used by the **BEACONs Hydology Explorer** app. 
<br><br>

To get started, click 'Set input parameters' on the left-side panel. 


### Set input parameters

In this step, the user uploads the required spatial data into the BEACONs Hydrology Explorer.

**Select source dataset** offers two options:

1. **Use demo dataset** - This dataset is embedded in the app and is located in the Dawson area in central Yukon, Canada. It includes all spatial layers required to run the app.
   
2. **Upload spatial dataset** - If selected, the panel options will expand:

   -- **Upload individual Shapefile layers**: User need to point on a study area, streams segments  and catchments dataset. Make sure the layers fullfil the datset requirements. 

   -- **Upload CSV with file paths**: Provide a CSV that defines the access paths for each shapefile layer. The CSV must have exactly two columns:

   - Layers – must contain exactly the following three names: studyarea, catchments, streams

    - Access path – the full path to the corresponding shapefile for each layer.

   -- **Upload a Geopackage**: Browse to the GeoPackage. Users are responsible for ensuring that all three required spatial layers (and associated attributes) are in
  the Geopackage and that all layers share a consistent coordinate reference system.

Press the **Preview study area** button to load the three spatial layers. Once loaded, the layers will appear on a map and can be turned on and off. From here, move on to **Set intactness**.

**Set intactness** defines how the intactness of each catchment will be determined. The app offers two options:

1. **Value in catchments dataset** - The catchments dataset includes a column containing the intactness values, ranging from 0 to 1.
 
2. **Upload intactness layer** -  Select a GeoPackage or Shapefile. If you choose a GeoPackage, you must also specify which layer contains the intactness data.


Press the **Confirm** button. If an intactness layer is provided, it will be displayed on the map.
<br><br>


### Add display elements (OPTIONAL)

This section allows users to add a maximum of three layers to the map for visualization purposes only. Additional layers can be 
uploaded as Shapefiles or as layers from a GeoPackage. These layers will not be included in the analysis. Layers will appear on
the map using their original names, and colors are assigned as indicated in the side panel.


### Select AOI

Select your area of interest (AOI) on which you want to evaluated the upstream and downstream area. The app offers two options:

1. **Upload an AOI**  - Can be either a ShapeFile or a GeoPackage. In order to generate upstream and downstream area, the uploaded polygon must completely overlay the study area. 
Once uploaded, users can optionally edit the AOI boundary using the catchments layer. Add how the app assigns catchments to the AOI. If the user uploads an AOI can they modify the boundary by adding catchments?

2. **Select a set of catchments on the map** - This option allows users to interactively select catchments by clicking directly on the map. 
All selected catchments are then combined to form a new area of interest (AOI) for analysis.

Note: The upstream and downstream area won't be computed beyond the extent of the provided catchments layer. 
  

### Generate upstream and downstream

Click on ***View upstream and downstream intactness*** in the left sidebar to display the upstream and downstream area and compute the statistics on intactness.


Once it is done, the map and legend will add three layers: 
- an ***upstream area*** layer that represent upstream catchments of the area of interest, 
- a ***downstream area*** layer for catchment that are downstream the area of interest and,
- a ***downstream stem*** layer that represent the downstream flow. 

The ***upstream and downstream statistics*** tab on the right sidebar will be updated in order to provide the numbers related to the area of interest selected.  

### Download results

Results can then be downloaded as a GeoPackage which will include:

      - Extent of the study area (studyarea)
      - Extent of the area of interest with related statistics (aoi) 
      - Catchments (catchments)
      - Streams segments layer (streams)
      - Intactness layer  (intactness) - if provided
      - Upstream area (upstream)
      - Downstream stem area (downstream stem)
      - Downstream area (downstream)

  

The GeoPackage can be further viewed and analysed in a GIS e.g., QGIS.  
