## Datasets
  
The **BEACONs Hydrology Explorer** app depends on several key data layers. A demo dataset located the Dawson area from central Yukon, Canada, comes with the app. 
The user can upload his or her own dataset as long as the layer name, attributes and structure follow the requirement. Below we describe the required datasets along with their attributes (if any).
  
### Required data layers
The following layers are required:
    
  - studyarea : A single polygon outlining the boundary of the study area e.g., a watershed or ecoregion or any other user-defined area; displayed as "Study area" in the map legend.
  - catchments : A polygon layers representing catchments overlapping the study area. Inside the catchments layers, attributes suche as BASIN, ORDER1, ORDER2, ORDER3, SKELUID and STHRALER must be present. distribution of intact forest landscapes; displayed as "Intactness" in the map legend.
  - streams : 
  
User-defined datasets require the following layers within a file geopackage. All layers within the geopackage should be using the coordinate reference system NAD83(CSRS) / Yukon Albers (EPSG:3578).
 
An additional layer, which is not a user-defined layer, is available in the Mapview tab:

  - FDAs : location of FDAs that comprise the demo study regions available with the app.
  - Fire : Distribution of wildfire polygons for the past 70 years. Available from: https://cwfis.cfs.nrcan.gc.ca/datamart
  - Protected areas : Distribution of protected areas from the Canadian Protected and Conserved Areas Database (CPCAD). Available from: https://open.canada.ca/data/en/dataset/6c343726-1e92-451a-876a-76e17d398a1c


Additional attributes may be added in the near future.


In the **catchments** layer, catchment intactness is provided using decimal (0-1) and the identification of each catchment to either upstream, downstream and downstream stem is provided using binomial value. 
