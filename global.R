library(shiny)
library(shinydashboard)
library(dplyr)
library(sf)
library(sfarrow)
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
library(readr)


for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

MB <- 1024^2

UPLOAD_SIZE_MB <- 5000
options(shiny.maxRequestSize = UPLOAD_SIZE_MB*MB)
options(timeout = max(300, getOption("timeout")))

# test prior to map that layer is not null, sf and has rows
isMappable <- function(x) {
  !is.null(x) && inherits(x, "sf") && nrow(x) > 0
}

# Define the last update date (git last commit)
app_version_date <- system("git log -1 --format=%ci", intern = TRUE)
date_only <- substr(app_version_date, 1, 10)

# Read the Markdown file
overview_md <- readLines("docs/overview.md")

# Replace placeholder in the Markdown
overview_md <- c(
  paste0('<div style="text-align: right; font-size:0.9em; color: gray;">Last update: ', date_only, '</div>'),
  overview_md
)

# Convert to a single string for rendering
overview_md_text <- paste(overview_md, collapse = "\n")

access_cloud <- "https://data.beaconsproject.ca/app-data/catchments"

catch_att <- readr::read_csv(file.path(access_cloud, "boreal_vPB25_attributes.csv"))

cloud_mda <- file.path(access_cloud, "boreal_vPB25_MDA.parquet")
mda_data <- local({
  x <- NULL
  function() {
    if (is.null(x)) {
      message("Loading MDA dataset...")
      x <<- sfarrow::st_read_parquet(cloud_mda)
    }
    x
  }
})

cloud_catch <- file.path(access_cloud, "boreal_vPB25.parquet")
catch_data <- local({
  x <- NULL
  function() {
    if (is.null(x)) {
      message("Loading catchments dataset...")
      x <<- sfarrow::st_read_parquet(cloud_catch)
    }
    x
  }
})

cloud_streams <- file.path(access_cloud, "borealC_v1_network.parquet")
streams_data <- local({
  x <- NULL
  function() {
    if (is.null(x)) {
      message("Loading streams dataset...")
      x <<- sfarrow::st_read_parquet(cloud_streams)
    }
    x
  }
})

distexplo_lyr <- c("fires", 
                    "intact_fl_2000",
                    "intact_fl_2020",
                    "footprint_500m",
                    "undisturbed_areas_500m",
                    "protected_areas", 
                    "placer_claims", 
                    "quartz_claims", 
                    "mining_claims", 
                    "undisturbed",
                    "disturbed")