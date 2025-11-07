---
format: md
---

# User guide
When studying or managing landscapes, it is important to recognize that the condition and function of **an area of interest (AOI) such as a conservation area or management zone** can be influenced not only by activities within its boundaries, but also by factors beyond them. Areas are vulnerable to external pressures through edge effects as well as the movement of surface and groundwater. Because rivers and other aquatic systems are highly connected, an AOI may be affected by human activities both upstream and downstream, including water diversion, pollution, or the loss of natural inputs such as organic matter from riparian zones (Pringle 2001). **An AOI such as a mine site or dam** may be a source of pollutants or changes to water flow. To better understand and manage these influences, it is valuable to consider the hydrologic connectivity of the AOI and to identify the extent to which it is connected to upstream and downstream areas. This perspective helps quantify human impact and supports more informed decisions for stewardship, monitoring, and long-term sustainability.

**BEACONs Hydrology Explorer** identifies areas upstream and downstream of an AOI and associated hydrologic metrics. This page provides step-by-step instructions.

The **Welcome** section includes the **Overview** landing page that provides a concise introduction to the app and its functionality. It also includes this guide and the **Dataset Requirements** tab that describes the required spatial layers used by the **BEACONs Hydrology Explorer** app.
<br><br>

To get started, click **Set input parameters** on the left-side panel. 


### Set input parameters

In this step, the user uploads the required spatial data into Hydrology Explorer: study area, streams, catchments, and if required undisturbed (or intact) areas. When uploaded, the layers will appear on the map and can be turned on and off using the legend in the top-right corner. 

For the map, there are two background options: ESRI World Topo Map and ESRI World Imagery. 

**Select source dataset** offers two options:

1. **Use demo dataset** - This dataset is embedded in the app and is located in the Dawson area in central Yukon, Canada. It includes all spatial layers required to run the app. If selected, study area, streams, and catchments will be added to the Mapview.

2. **Upload spatial dataset** - If selected, the panel options will expand and provide three options for uploading data. For all options, the spatial data must satisfy the requirements described on the **Data Requirements** tab, including a consistent coordinate reference system. 

   i) **Upload individual Shapefile layers**: Browse to each shapefile, select all files associated with the shapefile, and click "Open". A shapefile consists of multiple files with the same name but different extensions (e.g., .shp, .shx, .dbf, .prj). All files associated with the shapefile must be uploaded. 

   ii) **Upload CSV with file paths**: The spatial datasets can be uploaded using a csv file created in a text editor (e.g., Notepad). The csv file must have the following structure:

      Layer,Path <br>
      study area,C:/data/study_area.shp <br>
      stream,C:/data/streams.shp<br>
      catchments,C:/data/catchments.shp

   iii) **Upload a Geopackage**: Browse to the GeoPackage containing study area, streams, and catchment spatial layers, select, and click "Open". Point to the spatial layers in the GeoPackage that correspond to Study Area, Catchments, and Streams.

**Upload GeoPackage from Disturbance Explorer (optional)**: This geopackage will include linear and/or areal disturbances and disturbed and undisturbed areas, and other spatial layers such as protected areas, wildfire, and mining claims. These layers will be added to the mapview and can be selected in the next step **Set Fires and Intactness**.

Press the **Preview study area** button to load the three spatial layers into the map. Once loaded, the layers will appear on a map and can be turned on and off using the legend in the top-right corner, and the statistics tables on the right will start to populate. 

### Set fires and intactness

In this step, the user specifies the source for (1) fire spatial layer used to calculate the fire statistics and (2) intactness value or spatial layer used to specify or calculate catchment intactness (0-1 or 0-100% intact). 

**Select source for fires** offers two options: 

1. **Use existing fire layer** - If a Disturbance Explorer GeoPackage was uploaded in the previous step, it may include a fire layer. 
   
3. **Upload fire layer** - If selected, the user can upload a shapefile or GeoPackage of the fire spatial layer.

If a fire layer is not uploaded, the **Fire statistics** table will not populate.

**Select source for intactness** offers three options: 

1. **Use existing undisturbed layer** - If a Disturbance Explorer GeoPackage was uploaded in the previous step, it will include an undisturbed layer. The app will use this spatial layer to calculate the proportion of the catchment intact. Note: This value will be added to the catchment dataset contained in the Download GeoPackage.
   
2. **Value in catchment dataset** - If it exists, select the attribute in the catchment dataset that contains values for the proportion of the catchment undisturbed or intact. Values will range from 0 to 1, with 1 = 100% intact. 

   📌 Select this option when using the embedded Demo dataset. The intactness attribute is called "intact".

3. **Upload intactness layer** -  The intactness layer is a polygonal feature and can be uploaded via a GeoPackage or Shapefile. If a GeoPackage is used, the user must specify which layer contains the intactness data. The app will use this spatial layer to calculate the proportion of the catchment intact. Note: This value will be added to the catchment dataset contained in the Download GeoPackage.

Press the **Confirm** button. If an intactness layer is provided, it will be displayed on the map.


### Add display elements (OPTIONAL)

This section allows users to add a maximum of three layers to the map for visualization purposes only (e.g., salmon spawning sites, critical mineral potential). These layers can be uploaded as Shapefiles or as layers from a GeoPackage. Layers will appear on the map using their original names (maximum 25 characters).

### Select AOI

The app offers two options for specifying the AOI (area of interest) for which upstream and downstream areas will be identified: 

1. **Upload an AOI**  - The AOI can be uploaded as a ShapeFile or a GeoPackage. The AOI will be clipped to the study area boundary. Users have the option to edit the AOI boundary by selecting catchments. 

2. **Select a set of catchments on the map** - This option allows users to idenity an AOI by selecting catchments on the map. The AOI may be comprised of non-neighbouring catchments such as geographically dispersed salmon spawning sites or mine sites. All selected catchments are combined to form a new area of interest (AOI) for analysis.

Note: The upstream and downstream area will not be computed beyond the extent of the provided catchments layer. 

The statistics table will update with additional statistics, including Dendritic Connectivity Index (DCI) for the AOI. DCI is a measure of longitudinal connectivity within the AOI, ranging from 0 (low connectivity) to 1 (high connectivity) (Cote et al. 2009).
 

### Generate upstream and downstream

Here, the user launches the App to idenitify the areas upstream and downstream of the AOI. To initiate, click on the **View upstream and downstream intactness** button on the left sidebar. When done, the following upstreamm and downstream areas will be added to the Mapview legend, and the statistics tables will update.  

- **upstream area**: area upstream of the AOI as defined by catchments 
- **downstream stem**: area downstream of the AOI as defined by the main stream exiting the AOI and associated catchments
- **downstream area**: area downstream of the AOI as defined by the downstream stem as well as all streams flowing into the stem

### Download results

Results can be downloaded as a GeoPackage that includes the following spatial layers: 

- **studyarea**: polygon of the study area
- **aoi**: polygon of the area of interest
- **catchments**: catchments dataset for the study area 
- **streams**: stream network associated with the catchment dataset
- **upstream**: area upstream of the AOI identified by the App
- **downstream stem**: downstream stem identified for the AOI by the App
- **downstream**: downstream area identified for the AOI by the App
      
If provided, the following layers will also be added to the GeoPackage: 
- **intactness**: spatial layer uploaded to calculate catchment intactness 
- **fire**: spatial layer of wildfires used to calculate fire statistics

The GeoPackage can be further viewed and analysed in a GIS e.g., QGIS.  


### References

Cote, D., Kehler, D.G., Bourne, C. et al. A new measure of longitudinal connectivity for stream networks. Landscape Ecol 24, 101–113 (2009). https://doi.org/10.1007/s10980-008-9283-y
