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
