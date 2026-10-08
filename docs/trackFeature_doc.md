### Set feature to track

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

From here, proceed to **Select AOI** in the left-side panel.
