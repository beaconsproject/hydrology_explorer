#########################################################
#########################################################
#         ADDON FUNCTIONS
#########################################################
#########################################################
# Function to check if all required shapefile components exist
check_shp <- function(shapefile_path) {
  req(shapefile_path)
  folder_path <- dirname(shapefile_path)
  source_ext <- tools::file_ext(shapefile_path)
  base_name <- tools::file_path_sans_ext(basename(shapefile_path))
  
  required_extensions <- c("shp", "shx", "dbf", "prj")
  missing_extensions <- setdiff(required_extensions, source_ext)
  if (length(missing_extensions) > 0) {
    showModal(modalDialog(
      title = "Extension File Missing",
      paste(paste(missing_extensions, collapse = ", "), " extension is missing from ", base_name, 
            ". Make sure all required extension (.shp, .shx, .dbf, .prj) files exist prior to upload the shapefile."),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    showNotification("Shapefile is incomplete. Please provide all required files.", type = "error")
    req(FALSE)  # Stop further execution
  }
  
  return(TRUE)  # Return TRUE if all files exist
}

read_gpkg_from_upload <- function(path, name) {
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


# read_shp_from_csv: read layer from path found in csv uploaded with fileInput
read_shp_from_csv <- function(csv_file, layer_name) {
  req(csv_file)
  
  csv_data <- read.csv(csv_file$datapath)
  if (layer_name %in% csv_data$Layer) {
    path <- csv_data$Path[csv_data$Layer == layer_name]
    if (file.exists(path)) {
      shp <- sf::st_read(path, quiet = TRUE)
      # Remove fid column if it exists
      if ("fid" %in% colnames(shp)) {
        shp <- shp %>% dplyr::select(-fid)
      }
      return(shp)
    } else {
      stop(paste("The path for", layer_name, "in the CSV does not exist."))
    }
  } else {
    stop(paste(layer_name, "layer not found in CSV."))
  }
}

# read_shp_from_upload: read a shapefile from fileInput
read_shp_from_upload <- function(upload_input) {
  req(upload_input)
  infile <- upload_input
  if (length(infile$datapath) > 1) {
    dir <- unique(dirname(infile$datapath))
    outfiles <- file.path(dir, infile$name)
    name <- tools::file_path_sans_ext(infile$name[1])
    purrr::walk2(infile$datapath, outfiles, ~file.rename(.x, .y))
    shp_path <- file.path(dir, paste0(name, ".shp"))
    if (file.exists(shp_path)) {
      shp <- sf::st_read(shp_path, quiet = TRUE)
      # Remove fid column if it exists
      if ("fid" %in% colnames(shp)) {
        shp <- shp %>% dplyr::select(-fid)
        return(shp)
      }else{
        return(shp) 
      }
    } else {
      stop("Shapefile (.shp) is missing.")
    }
  } else {
    stop("Upload all necessary files for the shapefile (.shp, .shx, .dbf, etc.).")
  }
}

# function line flow start end point
get_start_end <- function(geom){
  # geom = sfc_LINESTRING or sfc_MULTILINESTRING
  coords_list <- st_geometry(geom) %>% map(st_coordinates)
  
  # extract start and end for each feature
  start_end <- map_dfr(coords_list, function(x){
    # x is a matrix of coordinates, possibly multiple parts
    x <- as.data.frame(x)
    # remove rows with NA
    x <- x[complete.cases(x), ]
    
    if(nrow(x) == 0){
      tibble(start_x = NA, start_y = NA, end_x = NA, end_y = NA)
    } else {
      tibble(
        start_x = x$X[1],
        start_y = x$Y[1],
        end_x   = x$X[nrow(x)],
        end_y   = x$Y[nrow(x)]
      )
    }
  })
  
  return(start_end)
}

check_colnames <- function(x, cols){
   i <- setdiff(cols, colnames(x))
   if(length(i)>0){
     return(i)
   } else{
     return(NA)
   }
}

extractCatchments <- function(sa_sf) {
  #-------------------------
  # Read MDA polygons
  #-------------------------
  mda <- mda_data()
  sa_sf <- st_transform(sa_sf, st_crs(mda))
  #-------------------------
  # Find intersecting MDAs
  #-------------------------
  mda_ids <- mda |>
    dplyr::filter(
      lengths(sf::st_intersects(geometry, sa_sf)) > 0
    ) |>
    dplyr::pull(.data[["MDA"]]) |>
    unique()
  
  if (length(mda_ids) == 0) {
    return(NULL)
  }
  
  #-------------------------
  # Filter catchments
  #-------------------------
  catchments_tbl <- catch_data()
  
  catchments_subset <- catchments_tbl |>
    dplyr::filter(.data[["MDA"]] %in% mda_ids) |>
    dplyr::collect()
  
  #-------------------------
  # Exact spatial filter
  #-------------------------
  catchments <- st_intersection(catchments_subset, sa_sf)
  
  return(catchments)
}

extractStreams <- function(catchments, sa_sf) {
  streams_tbl <- streams_data()
  ids <- unique(catchments$SKELUID)
  
  streams <- streams_tbl |>
    dplyr::filter(.data[["SKELUID"]] %in% ids) |>
    dplyr::collect()
  
  sa_sf <- st_transform(sa_sf, st_crs(streams))
  streams_subset <- st_intersection(streams, sa_sf)
  streams_ids <- streams_subset$SKELUID
  
  streams <- streams |>
    dplyr::filter(.data[["SKELUID"]] %in% streams_ids) |>
    dplyr::collect()
}

