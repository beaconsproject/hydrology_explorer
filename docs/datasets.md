## Datasets
  
For each study area (planning region), the **Hydrology Explorer** app depends on several key data layers contained within one GeoPackage ("gpkg" extension). Several example datasets come with the app - they consist of fundamental drainage areas (FDAs or watersheds) from southeast Yukon and northeast BC. In addition, the user can upload his or her own dataset provided that it contains the same layers (and attributes) and is in the same format as the demo dataset. Below we describe the required datasets along with their attributes (if any).
  
### Required data layers

The demo datasets or user-defined datasets require the following layers within a file geopackage. All layers within the geopackage should be using the coordinate reference system NAD83(CSRS) / Yukon Albers (EPSG:3578).
    
  - footprint : distribution of footprint outlining the boundary of the study area; displayed as "Footprint" in the map legend.
  - intactness : distribution of intact forest landscapes; displayed as "Intactness" in the map legend.
  - studyarea : A single polygon outlining the boundary of the study area e.g., a watershed or ecoregion or any other user-defined area; displayed as "Study area" in the map legend.
  
 
An additional layer, which is not a user-defined layer, is available in the Mapview tab:

  - FDAs : location of FDAs that comprise the demo study regions available with the app.
  - Fire : Distribution of wildfire polygons for the past 70 years. Available from: https://cwfis.cfs.nrcan.gc.ca/datamart
  - Protected areas : Distribution of protected areas from the Canadian Protected and Conserved Areas Database (CPCAD). Available from: https://open.canada.ca/data/en/dataset/6c343726-1e92-451a-876a-76e17d398a1c


Additional attributes may be added in the near future.
