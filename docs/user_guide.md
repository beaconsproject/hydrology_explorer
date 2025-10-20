---
format: md
---

# User guide
When studying or managing landscapes, it is important to recognize that the condition and function of **an area of interest (AOI) such as a conservation area or management zone** can be influenced not only by activities within its boundaries, but also by factors beyond them. Areas are vulnerable to external pressures through edge effects as well as the movement of surface and groundwater. Because rivers and other aquatic systems are highly connected, an AOI may be affected by human activities both upstream and downstream, including water diversion, pollution, or the loss of natural inputs such as organic matter from riparian zones (Pringle 2001). **An AOI such as a mine site or dam** may be a source of pollutants or changes to water flow. To better understand and manage these influences, it is valuable to consider the hydrologic connectivity of the AOI and to identify the extent to which it is connected to upstream and downstream areas. This perspective helps quantify human impact and supports more informed decisions for stewardship, monitoring, and long-term sustainability.

**BEACONs Hydrology Explorer** identifies areas upstream and downstream of an AOI and associated hydrologic metrics. This page provides step-by-step instructions.

The **Welcome** section includes the **Overview** landing page that provides a concise introduction to the app and its functionality. It also includes this guide and the **Dataset Requirements** tab that describes the required spatial layers used by the **BEACONs Hydology Explorer** app.
<br><br>

To get started, click **Set input parameters** on the left-side panel. 


### Set input parameters

In this step, the user uploads the required spatial data into Hydrology Explorer: study area, streams, catchments, and if required undisturbed (or intact) areas. When uploaded, the layers will appear on the map and can be turned on and off using the legend in the top-right corner. 

For the map, there are two background options: ESRI World Topo Map and ESRI World Imagery. 

**Select source dataset** offers two options:

1. **Use demo dataset** - This dataset is embedded in the app and is located in the Dawson area in central Yukon, Canada. It includes all spatial layers required to run the app. If selected, study area, streams, and catchments will be added to the Mapview.

    📌 **ADD THIS TO THE APP** - While not required, the App will also add the following spatial layers to the map legend: undisturbed areas, fire, linear disturbance, areal disturbance, protected areas, Quartz claims, and Placer claims.
   
2. **Upload spatial dataset** - If selected, the panel options will expand and provide three options for uploading data. For all options, the spatial data must satisfy the requirements described on the **Data Requirements** tab, including a consistent coordinate reference system. 

   i) **Upload individual Shapefile layers**: Browse to each shapefile, select all files associated with the shapefile, and click "Open". A shapefile consists of multiple files with the same name but different extensions (e.g., .shp, .shx, .dbf, .prj). All files associated with the shapefile must be uploaded. 

   ii) **Upload CSV with file paths**: The spatial datasets can be uploaded using a csv file created in a text editor (e.g., Notepad). The csv file must have the following structure:

      Layer,Path <br>
      study area,C:/data/study_area.shp <br>
      stream,C:/data/streams.shp<br>
      catchments,C:/data/catchments.shp

   iii) **Upload a Geopackage**: Browse to the GeoPackage containing study area, streams, and catchment spatial layers, select, and click "Open". Point to the spatial layers in the GeoPackage that correspond to Study Area, Catchments, and Streams.

   📌 **[ADD THIS TO THE APP]** If included in the GeoPackage, the App will recognize the following spatial layer names and add to the map legend: undisturbed, fire, linear_disturbance, areal_disturbance, protected_areas, Quartz_claims, Placer_claims, and mining_claims. These are spatial layers that may appear in the output GeoPackage from BEACONs Disturbance Explorer.

Press the **Preview study area** button to load the three spatial layers into the map. Once loaded, the layers will appear on a map and can be turned on and off using the legend in the top-right corner. From here, move on to **Set intactness**.

**Set intactness** defines how the intactness of each catchment will be determined. The app offers two options:

1. **Value in catchment dataset** - If it exists, select the attribute in the catchment dataset that contains values for the proportion of the catchment undisturbed or intact. Values will range from 0 to 1, with 1 = 100% intact. 

   📌 Select this option when using the embedded Demo dataset. The intactness attribute is called "intact".

2. **Upload intactness layer** -  The intactness layer is a polygonal feature and can be uploaded via a GeoPackage or Shapefile. If a GeoPackage is used, the user must specify which layer contains the intactness data. The app will use this spatial layer to calculate the proportion of the catchment intact. Note: This value will be added to the catchment dataset contained in the Download GeoPackage **[confirm with Melina]**.

Press the **Confirm** button. If an intactness layer is provided, it will be displayed on the map.
<br><br>


### Add display elements (OPTIONAL)

This section allows users to add a maximum of three layers to the map for visualization purposes only. Additional layers can be uploaded as Shapefiles or as layers from a GeoPackage. Layers will appear on the map using their original names (max 25 characters).

### Select AOI

Select your area of interest (AOI) on which you want to evaluated the upstream and downstream area. The app offers two options:

1. **Upload an AOI**  - Can be either a ShapeFile or a GeoPackage. In order to generate upstream and downstream area, the uploaded polygon must fall fully within the study area. **[Question - would it be worth adding a clip function here? clip AOI to the study area?]** 
Once uploaded, users can edit the AOI boundary by selecting catchments. **Add how the app assigns catchments to the AOI.** 

2. **Select a set of catchments on the map** - This option allows users to interactively select catchments by clicking directly on the map. 
All selected catchments are then combined to form a new area of interest (AOI) for analysis.

Note: The upstream and downstream area will not be computed beyond the extent of the provided catchments layer. 
  

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
