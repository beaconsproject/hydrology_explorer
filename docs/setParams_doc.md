### Set input parameters

This step loads the spatial layers required for the analysis: study area, catchments,  streams if required undisturbed (or intact) areas. Once loaded, the layers appear on the map 
and can be turned on and off using the legend in the top-right corner. 

For the map, there are two background options: ESRI World Topo Map and ESRI World Imagery. 

###  Step 1 : Select source dataset

Start with **Select source dataset**. You have two options:

1. **Use demo dataset** - This dataset is embedded in the app and is located in the Dawson area in central Yukon, Canada. It includes all 
spatial layers required to run the app. If selected, study area, streams, and catchments will be added to the Mapview.

2. **Upload spatial dataset** - Selecting this option expands additional settings. Choose how you want to provide your data:

   i) **Study area only** - Upload a study area polygon as a Shapefile or a GeoPackage. Catchments and streams are automatically
   extracted from the BEACONs reference dataset.
      - **Shapefile**: browse to the shapefile and select all associated files (e.g., .shp, .shx, .dbf, .prj), then click "Open".
      - **GeoPackage**: browse to the GeoPackage, click "Open", and select the study area layer. If the GeoPackage was created by 
      **Disturbance Explorer**, check **Load Disturbance Explorer layers** to also load layers such as undisturbed areas, fires, protected areas, 
      and mining claims.

   ii) **Advanced: Upload custom datasets** - Provide your own study area, catchments,  and streams, and !!analysis area!! in one of three ways:
      - **Upload individual layers as shapefiles**: browse to each shapefile and select all associated files.
      - **Provide a CSV containing file paths to the layers**: the CSV must have the following structure:

        Layer,Path <br>
        studyarea,C:/data/study_area.shp <br>
        streams,C:/data/streams.shp <br>
        catchments,C:/data/catchments.shp <br>
        analysis studyarea,C:/data/analysis_area.shp

      - **Upload a GeoPackage containing all layers**: browse to the GeoPackage, click "Open", and select the layers that correspond to Study area, Catchments, Streams, and Analysis area.

&#x1F4CC; **Note:** All layers must use the same projected coordinate reference system (CRS), and the catchments and streams must capture the
full extent of the study area. Refer to the **Dataset Requirements** tab (in the **Welcome** section from the main menu) for detailed 
specifications on required attributes and formatting.

Press the **Preview study area** button to load the three spatial layers into the map. Once loaded, the layers will appear on a map and 
can be turned on and off using the legend in the top-right corner, and the statistics tables on the right will start to populate. 

<br>

### Step 2. Set analysis area (OPTIONAL)

This option is only available with **Study area only**. Once the study area is previewed, the upstream watershed of the study area is displayed on the map. Select the analysis area:

  - **Use uploaded study area only**, or
  - **Use uploaded study area and all upstream watershed** - catchments and streams are extended to include the full upstream watershed.

Click **Set Analysis area** to apply the selection.

<br>

From here, proceed to **Set intactness** in the left-side panel.

