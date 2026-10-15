## Datasets

This page outlines the required and optional spatial layers used by **BEACONs Hydrology Explorer**. All layers must use a projected coordinate 
system (not geographic/lat-long) and must share the same coordinate reference system (CRS). The CRS must be a standard, recognized coordinate 
system (e.g., identified by an EPSG or ESRI code). A projected CRS is required to ensure accurate area calculations and spatial statistics. 
For accurate analysis, the catchments and streams must capture the full extent of the study area.

&nbsp;

### Map layers

#### Required

The required layers depend on how data are provided in **Set input parameters**:

- **Study area only**: only the **studyarea** layer is required. Catchments and streams are automatically extracted from the BEACONs reference 
dataset.
- **Advanced: Upload custom datasets**: all layers below are required.

&nbsp;

| Layer | Geometry | Description | Required attributes |
|---|---|---|---|
| **studyarea** | Polygon | Boundary of the study area e.g., watershed, ecoregion or other user-defined area. Displayed as "Study area" on the map. | – |
| **analysis studyarea** | Polygon | Boundary of the area used for the analysis. Displayed as "Analysis area" on the map. Can be the same as the study area. | – |
| **catchments** | Polygon | Catchments covering the study area. | CATCHNUM, SKELUID, BASIN, ORDER1, ORDER2, ORDER3, Area_total, Area_land, Area_water |
| **streams** | Polyline | Stream network associated with the catchments. | SKELUID (must match SKELUID in the catchments layer) |

&nbsp;

📌 **Note:** The catchment attributes come with the catchment dataset provided by BEACONs, or can be created using the BEACONs QGIS plugin 
**BEACONs Processing Provider**.

When layers are uploaded with a CSV file, the **Layer** column must use the names in bold above (e.g., `studyarea`, `analysis studyarea`, 
`catchments`, `streams`). When layers are uploaded as a GeoPackage, the user selects the layer corresponding to each input, so layer names are 
not restricted.

Catchment intactness is required for the analysis and can be provided in one of three ways (see **Set intactness**):

- **Catchment attribute**: a numeric attribute in the catchments layer giving the proportion of each catchment that is undisturbed, ranging from 
0 (fully disturbed) to 1 (100% undisturbed). There is no restriction on the attribute name.
- **undisturbed** layer from a Disturbance Explorer GeoPackage (see below).
- **Intactness layer**: a polygon layer of undisturbed (intact) areas, uploaded as a Shapefile or GeoPackage. There is no restriction on the 
layer name.

<br>

#### Optional 

##### **Disturbance Explorer GeoPackage**

When the study area is uploaded from a GeoPackage created by **BEACONs Disturbance Explorer**, the following layers are recognized and can be 
loaded by checking **Load Disturbance Explorer layers**. Layer names must exactly match the names in bold below:

- **undisturbed** (or **undisturbed_areas_500m**): undisturbed (intact) areas.
- **disturbed** (or **footprint_500m**): disturbed areas (human footprint).
- **fires**: wildfire polygons.
- **protected_areas**: protected and conserved areas.
- **placer_claims**, **quartz_claims**, **mining_claims**: mining claims.
- **intact_fl_2000**, **intact_fl_2025**: intact forest landscapes (Potapov et al. 2017).

<br>

##### **Display elements**

Up to three vector layers (points, lines, or polygons) can be added under **Add display elements (OPTIONAL)** as Shapefiles or from a GeoPackage, 
for display purposes only. The file or layer name is used in the map legend, so a short name (≤ 25 characters) is recommended.

<br>

##### **Feature to track**

A polygon layer representing features of interest (e.g., fire perimeters, mining claims, other disturbances), uploaded as a Shapefile or 
GeoPackage under **Set feature to track (OPTIONAL)**. It is used to calculate the area and percent of the feature within the analysis area, the AOI, and the upstream and downstream areas. Overlapping polygons are merged so that shared areas are counted only once. There is no restriction on the layer name.

<br>

### Demo Dataset

The app comes with a built-in demo dataset for users who want to explore the functionality without uploading their own data. The demo is located 
in the Dawson area in central Yukon, Canada, and includes all the spatial layers required to run the analysis:

- studyarea: boundary of the study area
- catchments: BEACONs catchments for the study area
- streams: stream network associated with the catchments
- undisturbed: undisturbed (or intact) areas identified using [BEACONs Disturbance Explorer](https://github.com/beaconsproject/disturbance_explorer)
- disturbed: disturbed (or human footprint) areas identified using [BEACONs Disturbance Explorer](https://github.com/beaconsproject/disturbance_explorer)
- fires: wildfire polygons from the [Canadian Wildland Fire Information System](https://cwfis.cfs.nrcan.gc.ca/datamart)
- placer_claims: placer claims 1:50,000 from [GeoYukon](https://yukon.maps.arcgis.com/sharing/rest/content/items/a49e93e6430d4773b169ad1450e72bea/info/metadata/metadata.xml?format=default&output=html)
- quartz_claims: quartz claims 1:50,000 from [GeoYukon](https://yukon.maps.arcgis.com/sharing/rest/content/items/871f84d5f97942bba49f9f8e66af76b8/info/metadata/metadata.xml?format=default&output=html)
- protected_areas: protected areas from the [Canadian Protected and Conserved Areas Database](https://www.canada.ca/en/environment-climate-change/services/national-wildlife-areas/protected-conserved-areas-database.html)
- intact_fl_2000: intact forest landscapes in 2000 from [intactforests.org](https://intactforests.org)

A GeoPackage of the demo dataset can be downloaded from the BEACONs Hydrology Explorer [GitHub site](https://github.com/beaconsproject/beaconshydro_explorer).

#### References

Potapov, P., Hansen, M.C., Laestadius, L., et al. The last frontiers of wilderness: Tracking loss of intact forest landscapes from 2000 to 2013. Science Advances 3, e1600821 (2017). https://doi.org/10.1126/sciadv.1600821
