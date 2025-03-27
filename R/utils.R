###### Cluster adjacent catchments into group
## sfpolys : catchment sf_object
## group_name : character name prefix used to identify the group
clusterSF <- function(sfpolys, group_name){
  dmat = st_distance(sfpolys)
  if(nrow(sfpolys)>1){
    hc = hclust(as.dist(dmat), method="single")
    groups = cutree(hc, h=0.5)
    d = st_sf(
      geom = do.call(c,
                     lapply(1:max(groups), function(g){
                       st_union(sfpolys[groups==g,])
                     })
      )
    )
    d$group = paste(group_name, 1:nrow(d), sep= "_")
  }else{
    d<-sfpolys
    d$group <- "AOI_1"
  }
  return(d)
}

# Load gpkg with test on extension
## path : system path to file
## name `: Layer name of the object to load withing the .gpkg`
load_gpkg <- function(path, name) {
  ext <- tools::file_ext(path)
  if(ext == "gpkg"){
      layer <- st_read(path, name, quiet = TRUE) 
    } else{
      # show pop-up ...
      showModal(modalDialog(
        title = "Wrong file type, must be geopackage (.gpkg).",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  return(layer)
}

# Prepare raster for visualization
## var_rast: representation variable object raster 
## planreg: sf object representing the planning region
prep_rast <- function(var_rast, planreg) {
  req(planreg)
  var <- crop(var_rast, planreg, snap = "near")
  varmask <- mask(var, planreg)
  return(varmask)
}