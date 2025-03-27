library(sf)
library(dplyr)
library(terra)
library(raster)
library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyjs)
library(shinyWidgets)
library(leafem)
library(shinycssloaders)
library(rhandsontable)
library(tibble)
library(markdown)
source("./R/beaconshydro.R")
source("./R/utils.R")

MB <- 1024^2

UPLOAD_SIZE_MB <- 5000
options(shiny.maxRequestSize = UPLOAD_SIZE_MB*MB)

hydrounit_list <- c("Full extent", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")

#Root folder
indir <-"www/102001"

# Define PROJ based on folder name
if(basename(indir) =="102001"){
  textpj<- 'PROJCS["Canada_Albers_Equal_Area_Conic",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Albers"],PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-96.0],PARAMETER["Standard_Parallel_1",50.0],PARAMETER["Standard_Parallel_2",70.0],PARAMETER["Latitude_Of_Origin",40.0],UNIT["Meter",1.0]]'
} else{
  textpj <- as.numeric(basename(indir))
}


## BaSe LAYERS
gpkg <-file.path(indir, "dataset.gpkg")

basewtsh <- reactive({
  st_read(gpkg, 'watersheds', quiet=T)
})
basebnd <- reactive({
  st_read(gpkg, 'bnd', quiet=T)
})
basePAs <- reactive({
  st_read(gpkg, 'pas', quiet=T) 
})

basemines <- reactive({
  st_read(gpkg, 'mineclaims', quiet=T) 
})

ifl2020 <- reactive({
  st_read(gpkg, 'ifl2020', quiet=T) 
})

int2010 <- reactive({
  st_read(gpkg, 'intactness', quiet=T) 
})