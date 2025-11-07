library(shiny)
library(shinydashboard)
library(dplyr)
library(sf)
library(terra)
library(raster)
library(leaflet)
library(shinyjs)
library(shinyWidgets)
library(leafem)
library(shinycssloaders)
library(rhandsontable)
library(tibble)
library(markdown)
library(purrr)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

MB <- 1024^2

UPLOAD_SIZE_MB <- 5000
options(shiny.maxRequestSize = UPLOAD_SIZE_MB*MB)


# test prior to map that layer is not null, sf and has rows
isMappable <- function(x) {
  !is.null(x) && inherits(x, "sf") && nrow(x) > 0
}

# Function to add a new group to group_names

# read_shp_from_csv: read layer from path found in csv uploaded with fileInput
#read_shp_from_csv <- function(csv_file, layer_name) {
#  req(csv_file)
  
#  csv_data <- read.csv(csv_file$datapath, stringsAsFactors = FALSE)
#  if (!(layer_name %in% csv_data$Layer)) {
#    showModal(modalDialog(
#      title = "Layer Not Found",
#      paste("The layer", layer_name, "was not found in the CSV."),
#      easyClose = TRUE,
#      footer = modalButton("OK")
#    ))
#    showNotification("Layer not found in CSV. Check your file.", type = "error")
#    req(FALSE)  # Stop further execution
#  }
  
#  path <- csv_data$Path[csv_data$Layer == layer_name]
#  if (!file.exists(path)) {
#    showModal(modalDialog(
#      title = "Invalid Path",
#      paste("The path for", layer_name, "does not exist."),
#      easyClose = TRUE,
#      footer = modalButton("OK")
#    ))
#    showNotification("Invalid file path in CSV. Check your file.", type = "error")
#    req(FALSE)  # Stop further execution
#  }
  
  # Check if all required shapefile components are present
#  check_shp(path)
  # If everything is okay, read the shapefile
#  return(sf::st_read(path))
#}


