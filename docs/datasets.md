## Datasets
  
**BEACONs Hydrology Explorer** app depends on several key data layers. A demo dataset located the Dawson area from central Yukon, Canada, comes with the app. 
The user can upload their own data as long as the layer names, attributes and structure follow the specifications below. 

📌 For the app to recognize the spatial layers, the layer names must exactly match the expected names shown below.

📌 All spatial layers must have the same projection.

📌 For accurate analysis, the study area must contain the full extent of disturbance layers.
  
### Data format

In all cases, spatial layers can be uploaded from a GeoPackage or as individual Shapefiles.

📌 The App recognizes the spatial layers in the GeoPackage from Disturbance Explorer which can include the following spatial layers: intact, fire, protected areas, and mining claims. These layers will be automatically added to the map legend.

### Required data layers

The following spatial layers are required:
    
  - **studyarea**: A single polygon outlining the boundary of the study area e.g., watershed, ecoregion or other user-defined area. The polygon is displayed as "Study area" in the map legend.
    
  - **catchments**: A polygon layer of catchments for the study area. The catchments layer must include the following attributes. These attributes come with the catchment dataset provided by BEACONs or created using BEACONs QGIS plugin BEACONs Processing Provider: CATCHNUM, SKELUID, STHRALER, ORDER1, ORDER2, ORDER3, BASIN, Area_land, Area_water, Area_total, STRMLEN, ZONE, and Isolated. If the dataset includes an attribute describing the proportion of the catchment undisturbed (0-1), the following **undisturbed areas** layer is not required. This attribute does not have a name restriction.
    
  - **undisturbed areas (or intactness)**: This polygon layer is only required if the catchment dataset does not include an attribute that describes the proportion of the catchment undisturbed OR a different set of undisturbed values is desired. This layer could be the output from BEACONs Disturbance Explorer, for example. The app uses this layer to calculate the proportion of the catchment undisturbed.
    
  - **streams**: A polyline layer of streams. There are no attribute requirements for this layer. 
  
## Optional display elements
 
Up to three additional spatial layers can be uploaded for display purposes only in the Mapview tab. The name of each layer will be used to identify the layer in the map legend, so a short name (≤25 characters) is recommended.
