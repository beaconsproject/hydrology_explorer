## Datasets
  
**BEACONs Hydrology Explorer** app depends on several key data layers. A demo dataset located the Dawson area from central Yukon, Canada, comes with the app. 
The user can upload their own data as long as the layer names, attributes and structure follow the specifications below. 

📌 For the app to recognize the spatial layers, the layer names must exactly match the expected names shown below.

📌 All spatial layers must have the same projection.

📌 For accurate analysis, the study area must contain the full extent of disturbance layers.
  
### Data format

Spatial layers can be uploaded from a GeoPackage or as individual Shapefiles.

### Required data layers

The following spatial layers are required:
    
  - **studyarea**: A single polygon outlining the boundary of the study area e.g., watershed, ecoregion or other user-defined area. The polygon is displayed as "Study area" in the map legend.
    
  - **catchments**: A polygon layer of catchments for the study area. The catchments layer must include the following attributes. These attributes come with the catchment dataset provided by BEACONs or created using BEACONs QGIS plugin BEACONs Processing Provider: BASIN, ORDER1, ORDER2, ORDER3, SKELUID, STHRALER,  ... THIS LIST IS INCOMPLETE. In the **catchments** layer, catchment intactness is provided using decimal (0-1) and the identification of each catchment to either upstream, downstream and downstream stem is provided using binomial value. 
    
  - **undisturbed areas (or intactness)**: This polygon layer is required if the catchment dataset (1) does not include an attribute that describes the proportion of the catchment undisturbed or (2) if a different set of undisturbed values is desired. This layer could be the output from BEACONs Disturbance Explorer, for example. The app uses this layer to calculate the proportion of the catchment undisturbed.
    
  - **streams**: A polyline layer of streams. There are no attribute requirements for this layer. 
  
## Optional display elements
 
Up to three additional spatial layers can be uploaded for display purposes only in the Mapview tab. The name:
