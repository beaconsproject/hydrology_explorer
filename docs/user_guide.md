---
format: md
---

# User guide
When studying or managing landscapes, it is important to recognize that the condition and function of **an area of interest (AOI) such as a conservation area or management zone** can be influenced not only by activities within its boundaries, but also by factors beyond them. Areas are vulnerable to external pressures through edge effects as well as the movement of surface and groundwater. Because rivers and other aquatic systems are highly connected, an AOI may be affected by human activities both upstream and downstream, including water diversion, pollution, or the loss of natural inputs such as organic matter from riparian zones (Pringle 2001). **An AOI such as a mine site or dam** may be a source of pollutants or changes to water flow. To better understand and manage these influences, it is valuable to consider the hydrologic connectivity of the AOI and to identify the extent to which it is connected to upstream and downstream areas. This perspective helps quantify human impact and supports more informed decisions for stewardship, monitoring, and long-term sustainability.

**BEACONs Hydrology Explorer** identifies areas upstream and downstream of an AOI and associated hydrologic metrics. This page provides step-by-step instructions.

The **Welcome** section includes the **Overview** landing page that provides a concise introduction to the app and its functionality. It also includes this guide and the **Dataset Requirements** tab that describes the required spatial layers used by the **BEACONs Hydrology Explorer** app.
<br><br>

To get started, click **Set input parameters** on the left-side panel. 


###  Set input parameters

This step loads the spatial layers required for the analysis: study area, catchments,  streams if required undisturbed (or intact) areas. Once loaded, the layers appear on the map 
and can be turned on and off using the legend in the top-right corner. 

For the map, there are two background options: ESRI World Topo Map and ESRI World Imagery. 

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

&nbsp;

**Set analysis area** is optional and only available with **Study area only**. Once the study area is previewed, the upstream watershed of the study area is displayed on the map. Select the analysis area:

  - **Use uploaded study area only**, or
  - **Use uploaded study area and all upstream watershed** - catchments and streams are extended to include the full upstream watershed.

Click **Set Analysis area** to apply the selection.

<br>

###  Set intactness

This step defines how catchment intactness is measured. Intactness identifies areas without a visible human footprint (e.g., roads, mine sites) 
and is used as a proxy for the ecological integrity of a catchment, ranging from 0 (fully disturbed) to 1 (100% undisturbed). Catchment intactness 
is used to calculate the area-weighted intactness (AWI) of the upstream and downstream areas.

Start with **Select source of intactness**. Up to three options are available:

1. **Value in catchment dataset** - Select the attribute in the catchment dataset that contains the proportion of each catchment that is intact. Values must be numeric and range from 0 (fully disturbed) to 1 (100% undisturbed).

 &#x1F4CC; Note: Select this option when using the demo dataset. The intactness attribute is called "intactKBA".

2. **Use existing undisturbed layer** - This option is only available if **Load Disturbance Explorer layers** was checked in **Set input parameters**. The app uses the undisturbed layer to calculate the proportion of each catchment that is intact.

3. **Upload intactness layer** - Upload a polygon layer of intact (undisturbed) areas as a Shapefile or a GeoPackage. 
   - **Shapefile**: browse to the shapefile and select all associated files (e.g., .shp, .shx, .dbf, .prj), then click "Open".
   - **GeoPackage**: browse to the GeoPackage, click "Open", and select the intactness layer.

   The app uses this layer to calculate the proportion of each catchment that is intact.

&#x1F4CC; Note: When intactness is calculated from a spatial layer (options 2 and 3), the resulting values are added to the catchment dataset included in the downloaded GeoPackage.

Click **Confirm** to apply the selection. If a spatial layer is used, it is displayed on the map as **Undisturbed**. The statistics table on the right is updated with the **Analysis area intactness**.

<br>

### Add display elements (OPTIONAL)

Add up to **three vector layers** to the map as visual references, e.g., salmon spawning sites or critical mineral potential. These layers are **for display only**: they are not used in any analysis and are not saved to the project.

1. Under **Select source for extra layers**, choose **Shapefile** or **GeoPackage**.
2. Select up to three layers:
   - **Shapefile**: for each layer, browse to the shapefile and select all of its files (`.shp`, `.shx`, `.dbf`, `.prj`) before clicking **Open**.
   - **GeoPackage**: upload one GeoPackage, then pick up to three of its layers from the drop-down lists.
3. Click **Confirm**. The layers are drawn on the map and added to the layer control (top right), where they can be turned on or off.

<br>

### Set feature to track (OPTIONAL)

This step allows users to select a polygon feature (e.g., fires, mining claims, disturbances) and calculate its area and proportion within the 
study area, the AOI, and the upstream and downstream areas.


**Select source of the feature to track** offers three options: 

1. **Upload feature layer** - Upload a polygon layer as a Shapefile or a GeoPackage.
   - **Shapefile**: browse to the shapefile and select all associated files (e.g., .shp, .shx, .dbf, .prj), then click "Open".
   - **GeoPackage**: browse to the GeoPackage, click "Open", and select the feature layer.

2. **No feature** - No feature is tracked (default). The **Feature statistics** table will not populate.

📌 Note: Overlapping polygons are merged before statistics are calculated, so shared areas are counted only once.

Click **Confirm** to apply the selection. The feature layer is displayed on the map using the file or layer name. The **Feature statistics** table 
on the right is updated with the area (km²) and percent of the feature **Within study area**. Statistics for the AOI and the upstream and 
downstream areas are added once **Generate upstream and downstream** is run.

<br>

### Select AOI

This step defines the area of interest (AOI), such as a conservation area or a mine site, for which upstream and downstream areas will be 
identified.

Start with **Set an Area of Interest (AOI)**. Two options are available:

1. **Upload an AOI** - Upload the AOI as a Shapefile or a GeoPackage.
   - **Shapefile**: browse to the shapefile and select all associated files (e.g., .shp, .shx, .dbf, .prj), then click "Open".
   - **GeoPackage**: browse to the GeoPackage, click "Open", and select the AOI layer.

   If the AOI contains several polygons, they are combined into a single AOI. The AOI must overlap the analysis area, and any part extending beyond it is clipped.

   (Optional) Check **Enable AOI boundary editing using catchment** to adjust the AOI to catchment boundaries. Catchments intersecting the AOI are highlighted on the map. Click catchments to select or unselect them.

2. **Select a set of catchments on the map** - Click catchments on the map to select or unselect them. The AOI may include non-neighbouring catchments, such as geographically dispersed salmon spawning sites or mine sites. All selected catchments are combined to form the AOI. Use **Clear selection** (top-left of the map) to start over.

📌 Note: Upstream and downstream areas are not computed beyond the extent of the catchments layer.

&nbsp;

Click **Confirm AOI boundary (Analysis AOI)**. The **Analysis AOI** used in the analysis is displayed in red on the map, and the uploaded **AOI** 
in black. When an uploaded AOI is not edited, both are the same.

The statistics table on the right is updated with the area and intactness of the **Analysis AOI**. If a feature to track was set, the 
**Feature statistics** table is updated with the feature area **Within Analysis AOI**.

<br>

### Generate upstream and downstream

This step identifies the areas upstream and downstream of the Analysis AOI and calculates their associated intactness and hydrology statistics.

Click **View upstream and downstream areas** in the left-side panel. Depending on the size of the study area, this may take several minutes. When done, the following areas are added to the map:

- **Upstream area**: catchments draining into the Analysis AOI.
- **Downstream stem areas**: catchments along the main stream exiting the Analysis AOI.
- **Downstream area**: the downstream stem and all catchments draining into it, excluding the upstream area.

The Analysis AOI is excluded from all three areas. Catchments are coloured by their intactness (see the **Percent intactness** legend). Layers can 
be turned on and off using the legend in the top-right corner. The **Downstream area** is hidden by default.

&#x1F4CC; Note:  Upstream and downstream areas are not computed beyond the extent of the catchments layer.

The tables on the right are updated with the following statistics:

- **Area Intactness and Hydrology statistics**: area (km²) and mean area-weighted intactness (AWI, %) of the upstream, downstream stem, 
and downstream areas.
- **Feature statistics** (if a feature to track was set): area (km²) and percent of the feature within the Analysis AOI, the upstream, downstream 
stem, and overall downstream areas.
- **Dendritic Connectivity Index (DCI)**: longitudinal connectivity of the stream network within the Analysis AOI, ranging from 0 (fragmented) 
to 1 (connected) (Cote et al. 2009):

| DCI | Rating |
|---|---|
| ≥ 0.9 | Very High |
| 0.8 - 0.9 | High |
| 0.7 - 0.8 | Moderate |
| < 0.7 | Low |

All statistics are combined in the **Summary statistics** tab, located across the top, and can be downloaded as a CSV file using
**Download summary statistics table (.csv)**.

<br>

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
- **feature to track**: A spatial layer representing a user-defined feature of interest (e.g., wildfires, mining claims, or other disturbances) used to calculate feature-specific statistics. The GeoPackage layer name will be preserved from the uploaded feature.

The GeoPackage can be further viewed and analysed in a GIS e.g., QGIS.  

<br>

### References

Cote, D., Kehler, D.G., Bourne, C. et al. A new measure of longitudinal connectivity for stream networks. Landscape Ecol 24, 101–113 (2009). https://doi.org/10.1007/s10980-008-9283-y
