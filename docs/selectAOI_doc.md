. **Select a set of catchments on the map** - This option allows users to idenity an AOI by selecting catchments on the map. The AOI may be comprised of non-neighbouring catchments such as geographically dispersed salmon spawning sites or mine sites. All selected catchments are combined to form a new area of interest (AOI) for analysis.

Note: The upstream and downstream area will not be computed beyond the extent of the provided catchments layer. 

The statistics table will update with additional statistics, including Dendritic Connectivity Index (DCI) for the AOI. DCI is a measure of longitudinal connectivity within the AOI, ranging from 0 (low connectivity) to 1 (high connectivity) (Cote et al. 2009).


## Select AOI

This step defines the area of interest (AOI), such as a conservation area or a mine site, for which upstream and downstream areas will be 
identified.

### Step 1. Set an Area of Interest (AOI)

Start with **Set an Area of Interest (AOI)**. Two options are available:

1. **Upload an AOI** - Upload the AOI as a Shapefile or a GeoPackage.
   - **Shapefile**: browse to the shapefile and select all associated files (e.g., .shp, .shx, .dbf, .prj), then click "Open".
   - **GeoPackage**: browse to the GeoPackage, click "Open", and select the AOI layer.

   If the AOI contains several polygons, they are combined into a single AOI. The AOI must overlap the analysis area, and any part extending beyond it is clipped.

   (Optional) Check **Enable AOI boundary editing using catchment** to adjust the AOI to catchment boundaries. Catchments intersecting the AOI are highlighted on the map. Click catchments to select or unselect them.

2. **Select a set of catchments on the map** - Click catchments on the map to select or unselect them. The AOI may include non-neighbouring catchments, such as geographically dispersed salmon spawning sites or mine sites. All selected catchments are combined to form the AOI. Use **Clear selection** (top-left of the map) to start over.

📌 Note: Upstream and downstream areas are not computed beyond the extent of the catchments layer.

<br>

### Step 2. Confirm AOI boundary

Click **Confirm AOI boundary (Analysis AOI)**. The **Analysis AOI** used in the analysis is displayed in red on the map, and the uploaded **AOI** 
in black. When an uploaded AOI is not edited, both are the same.

The statistics table on the right is updated with the area and intactness of the **Analysis AOI**. If a feature to track was set, the 
**Feature statistics** table is updated with the feature area **Within Analysis AOI**.

<br>

From here, proceed to **Generate upstream and downstream** in the left-side panel.

#### References

Cote, D., Kehler, D.G., Bourne, C. et al. A new measure of longitudinal connectivity for stream networks. Landscape Ecol 24, 101–113 (2009). https://doi.org/10.1007/s10980-008-9283-y



