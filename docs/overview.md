
> ***To run the App, click on Select study area in the Explorer section on the left panel.



## Introduction

The Hydrology Explorer app uses the BEACONs catchments dataset as building blocks for identifying upstream and downstream areas of an area of interest and provide hydrological metrics within a predefine study area. The user need first to identify a study area by either using a predefined FDAs or by uploading a geopackage output produced by the [Disturbance Explorer app](https://beaconsproject.shinyapps.io/disturbance_explorer/). The user can then specify the area of interest for which the upstream and downstream area will be computed by uploading a geopackage that only contain the layer representing the AOI or by interactively selecting catchments on the Mapviewer. Currently, the app can only be used in the southeast Yukon.

<center>
  <img src="app.jpg" width="80%">
    <br>
    Figure 1. Shiny-based disturbance explorer app.
  </center>
    
## Functionality
    
The app consists of two sections:
    
**Overview**:
    
The overview provides a description of the app, its functionality, and contain 2 tabs that give on how to use the app and information on underlying datasets.

**Explorer**:

The Explorer section is where the analysis is happening. The analysis require 4 steps:

  - Select a study area by either :
     -Select a fundamental drainage area found in the dropdown menu.
     -Upload a geopackage created by the [Disturbance Area Explorer](https://beaconsproject.shinyapps.io/disturbance_explorer/) or,
      
Predefined fundamental drainage areas provided in the dropdown menu have been pre-processed by the Disturbance Area Explorer by applying a 500m buffer around areal and linear disturbances. 


  - Choose an Area of Interest (AOI) from one of the two options:
     - Upload a polygon saved as single later geopackage. The projection must be NAD83(CSRS) / Yukon Albers (EPSG:3578). Moreover, the polygon must overlay the study area. An AOI that exceed the extent of the study area will prevent the tools to calculate the hydrological connectivity and statistics. An error message will appear on the interface. 
     - Generate AOI by interactively selecting catchments within your study area. 
    

  - Generate upstream and downstream
     - This section launch the calculation of statistics such as total areas and mean of catchment area weighted intactness along the upstream, downstream and downstream stem related to the chosen AOI. Hydrological metrics  are provided in the adjacent table. 

  - Download the results. 
     - The output is provided as .gpkg and it includes the following:
       - Extent of the area of interest (aoi)
       - Catchments overlaying the study area (catchments)
       - Fires overlaying the study area (fires)
       - Footprint map of the study area (footprint)
       - Intactness map of the study area (intactness)
       - Protected areas overlaying the study area (protected_areas)
       - Extent of the study area (studyarea)
  
In the **catchments** layer, catchment intactness is provided using decimal (0-1) and the identification of each catchment to either upstream, downstream and downstream stem is provided using binomial value. 
