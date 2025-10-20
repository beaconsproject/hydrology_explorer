## Datasets
  
**BEACONs Hydrology Explorer** app depends on several key data layers. A demo dataset located the Dawson area from central Yukon, Canada, comes with the app. 
The user can upload their own data as long as the layer names, attributes and structure follow the specifications below. 

📌 For the app to recognize the spatial layers, the layer names must exactly match the expected names shown below.

📌 All spatial layers must have the same projection.

📌 For accurate analysis, the study area must contain the full extent of disturbance layers.
  
### Data format

In all cases, spatial layers can be uploaded from a GeoPackage or as individual Shapefiles.

### Required data layers

The following spatial layers are required:
    
  - **studyarea**: A single polygon outlining the boundary of the study area e.g., watershed, ecoregion or other user-defined area. The polygon is displayed as "Study area" in the map legend.
    
  - **catchments**: A polygon layer of catchments for the study area. The catchments layer must include the following attributes. These attributes come with the catchment dataset provided by BEACONs or created using BEACONs QGIS plugin BEACONs Processing Provider: CATCHNUM, SKELUID, STHRALER, ORDER1, ORDER2, ORDER3, BASIN, Area_land, Area_water, Area_total, STRMLEN, ZONE, and Isolated. If the dataset includes an attribute describing the proportion of the catchment undisturbed (0-1), the following **undisturbed areas** layer is not required. This attribute does not have a name restriction.
    
  - **undisturbed areas (or intactness)**: This polygon layer is only required if the catchment dataset does not include an attribute that describes the proportion of the catchment undisturbed OR a different set of undisturbed values is desired. This layer could be the output from BEACONs Disturbance Explorer, for example. The app uses this layer to calculate the proportion of the catchment undisturbed.
    
  - **streams**: A polyline layer of streams. There are no attribute requirements for this layer. 
  
### Optional display elements
 
These spatial layers are added to the App under **Add display elements (OPTIONAL)**. Up to three additional spatial layers can be uploaded for display purposes only in the Mapview tab. The name of each layer will be used to identify the layer in the map legend, so a short name (≤25 characters) is recommended. 

## Demo Dataset

The App includes an embedded Demo dataset with the following spatial layers:

- studyarea: boundary of the study area for the demo dataset located in Dawson region, Yukon, Canada
- catchments: catchment dataset for the study area
- streams: streams associated with the catchment dataset
  
A GeoPackage of the Demo Dataset is available for download on the GitHub site, where the following spatial layers are available for upload under **Add display elements (OPTIONAL)**: 

- areal_disturbance: areal (or polygonal) disturbances such as mine sites, urban areas, cutblocks, etc.
- linear_disturbance: linear disturbances such as highways and resource access roads
- undisturbed: undisturbed (or intact) areas identified using Disturbance Explorer
- disturbed: disturbed (or human footprint) areas identified using Disturbance Explorer
- fires: polygons of wildfire
- Placers_Claims: placer claims
- Quartz_Claims: quartz claims
- protected_areas: protected areas
