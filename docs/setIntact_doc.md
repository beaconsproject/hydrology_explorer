### Set intactness

This step defines how catchment intactness is measured. Intactness identifies areas without a visible human footprint (e.g., roads, mine sites) 
and is used as a proxy for the ecological integrity of a catchment, ranging from 0 (fully disturbed) to 1 (100% undisturbed). Catchment intactness 
is used to calculate the area-weighted intactness (AWI) of the upstream and downstream areas.

Start with **Select source of intactness**. Up to three options are available:

1. **Value in catchment dataset** - Select the attribute in the catchment dataset that contains the proportion of each catchment that is intact. Values must be numeric and range from 0 (fully disturbed) to 1 (100% undisturbed).

 &#x1F4CC; Note: Select this option when using the demo dataset. The intactness attribute is called "intact".

2. **Use existing undisturbed layer** - This option is only available if **Load Disturbance Explorer layers** was checked in **Set input parameters**. The app uses the undisturbed layer to calculate the proportion of each catchment that is intact.

3. **Upload intactness layer** - Upload a polygon layer of intact (undisturbed) areas as a Shapefile or a GeoPackage. 
   - **Shapefile**: browse to the shapefile and select all associated files (e.g., .shp, .shx, .dbf, .prj), then click "Open".
   - **GeoPackage**: browse to the GeoPackage, click "Open", and select the intactness layer.

   The app uses this layer to calculate the proportion of each catchment that is intact.

&#x1F4CC; Note: When intactness is calculated from a spatial layer (options 2 and 3), the resulting values are added to the catchment dataset included in the downloaded GeoPackage.

Click **Confirm** to apply the selection. If a spatial layer is used, it is displayed on the map as **Undisturbed**. The statistics table on the right is updated with the **Analysis area intactness**.

<br>

From here, proceed to **Add display elements (OPTIONAL)** or **Set feature to track (OPTIONAL)**, or move directly to **Select AOI** in the left-side panel.
