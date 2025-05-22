
# beaconshydro (under-development) <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->
<!-- badges: end -->

`beaconshydro` provides a set of functions for extracting catchments
that are upstream or downstream of a reference areas, and for
calculating hydrological metrics. `beaconshydro` can be used with
existing polygons outlining conservation areas, or with conservation
areas created with the partner package `beaconsbuilder`.

`beaconshydro` uses the BEACONs catchments dataset as building blocks
for identifying upstream and downstream areas. Each catchment contains a
description of the associated stream flow attributes as well as
additional attributes describing catchment area and intactness.

You can visit the package website here:
<https://beaconsproject.github.io/beaconshydro>.

## Installation

You can install the development version of `beaconshydro` from
[GitHub](https://github.com/) with:

``` r
#install.packages("devtools")
token <- "putYourToken"  #"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"
devtools::install_github("beaconsproject/beaconshydro", ref="main", auth_token = token)
```

## Citation

Please cite the *beaconshydro* package when using it in publications. To
cite the latest official version, please use:

> Edwards M, Houle M, Lisgo K, Vernier P, Schmiegelow F (2023).
> beaconshydro: Evaluating upstream and downstream catchments of
> conservation area in R. R package version 0.0.0.1000. Available at
> <https://github.com/beaconsproject/beaconshydro>.

## Example workflow to calculate upstream and downstream areas

The following workflow demonstrates the identification of upstream and
downstream areas using the package `beaconshydro`. The case study uses a
hypothetical conservation area within the Fundamental Drainage Area
(FDA) 10AB as well as the BEACONs catchments dataset. The BEACONs
catchments dataset includes watershed (catchment) polygons with
associated stream flow attributes that allow the relative upstream and
downstream positions to be determined within the dataset. Stream order
attributes were calculated using a coastline and stream network where
waterbodies were represented as a skeleton.

## Data

In order to generate the upstream and downstream catchments, the
following data are necessary:

- `catchments`: a catchments dataset that uses CATCHNUM as unique
  identifier and a set of stream-ordering attributes (ORDER1, ORDER2,
  ORDER3, BASIN) from which the upstream and downstream catchments is
  derived.

- `conservation_area`: A conservation area from which the upstream and
  downstream catchments is calculated.

Others data are used in the workflow for visualization purposes:

- `fda`: A layer representing the outline of an FDA within which the
  analysis will operate. In this case study, we selected FDA 10AB.

- `bnd` : A layer representing the extent for visualization that will be
  extracted. In the case study below, we used the FDAs surrounding FDA
  10AB.

- `streams` : The stream network from which the catchments dataset was
  created.

Sample data are provided within the R package as a GeoPackage (/data
folder). Layers from the GeoPackage can also be viewed outside of R
environment using open source software such as QGIS.

``` r
# Load library
library(sf)
library(terra)
library(tmap)
library(tibble)
library(beaconshydro)
library(dplyr)

# set path to dsn
gpk <-  paste0('app/www/fda_10ab.gpkg')

# Initialize R object
catchments <- st_read(gpk, 'catchments', quiet=T)
conservation_area <- st_read(gpk, 'conservation_area', quiet=T)
fda_10ab <- st_read(gpk, 'fda', quiet=T)
streams <- st_read(gpk, 'streams', quiet=T)
bnd <- st_read(gpk, 'bnd', quiet=T)
```

## Map the study region

First, let’s visualize the study region

``` r
tmap_mode("plot")

tm_shape(catchments, bbox = st_bbox(fda_10ab)) +
tm_polygons(col = "white", border.col = "black") +
tm_add_legend(type = "fill", col = "white", border.col = "black", labels = "Catchments 1:1M") +
tm_shape(conservation_area) +
tm_polygons(col = "red" , legend.show = TRUE) +
tm_add_legend(type = "fill", col = "red", border.col = NA, labels = "Conservation area") +
tm_shape(fda_10ab) +
tm_borders(col = "black", lwd = 2, alpha = 1) +
tm_add_legend(type = "fill", col = "white", border.col = "black", labels = "FDA 10ab") +
tm_shape(streams) +
tm_lines(col = "blue" , legend.show = TRUE) +
tm_add_legend(type = "line", col = "blue", labels = "Stream network") +
tm_layout(main.title ="Study region", main.title.position =  "center", legend.outside = TRUE)
```

![](README_files/figure-gfm/plots%20study%20region-1.png)<!-- -->

## Generate upstream area of a conservation area

To generate the upstream area of a conservation area, we need to run the
following functions:

- `get_upstream_catchments()`: calculates the area upstream of a given
  polygon using catchments and stream flow attributes. It needs an sf
  conservation area, a string matching the unique identifier column in
  the conservation area and an sf set of catchments with a unique
  identifier column: CATCHNUM and stream flow attributes (ORDER1,
  ORDER2, ORDER3, BASIN).

- `dissolve_catchments_from_table()`: converts catchment lists
  (e.g. from `get_upstream_catchments()`) to extracted catchments and
  dissolves them into one polygon. `dissolve_catchments_from_table()`
  uses an sf set of catchments with unique identifier column: CATCHNUM,
  a list of catchment created by `get_upstream_catchments()` and a
  string representing the output column name holding the conservation
  area unique identifiers.

``` r
# First generate the list of catchment found upstream of the conservation area based on the catchments stream flow attributes 
upstream_list <- get_upstream_catchments(conservation_area, "RA_ID", catchments)
upstream_catch <- dissolve_catchments_from_table(catchments, upstream_list, "RA_ID")

# Map the resulting upstream area
tmap_mode("plot")

tm_shape(upstream_catch, bbox = st_bbox(fda_10ab)) +
tm_polygons(col = "cadetblue", border.col = "cadetblue") +
tm_add_legend(type = "fill", col = "cadetblue", border.col = "cadetblue", labels = "Upstream area") +
tm_shape(conservation_area) +
tm_polygons(col = "red" , legend.show = TRUE) +
tm_add_legend(type = "fill", col = "red", border.col = NA, labels = "Conservation area") +
tm_shape(fda_10ab) +
tm_borders(col = "black", lwd = 2, alpha = 1) +
tm_add_legend(type = "fill", col = "white", border.col = "black", labels = "FDA 10ab") +
tm_shape(streams) +
tm_lines(col = "blue" , legend.show = TRUE) +
tm_add_legend(type = "line", col = "blue", labels = "Stream network") +
tm_layout(main.title ="Upstream area", main.title.position =  "center", legend.outside = TRUE)
```

![](README_files/figure-gfm/plots%20upstream-1.png)<!-- -->

## Generate downstream catchment

**Generate downstream catchment stem**

The identification of the downstream area of a conservation_area can
either comprise only the main stem or the entire downstream area. The
analysis relies on the following `beaconshydro` functions:

- `get_downstream_catchments()`: calculates the area downstream along
  the stem of a given conservation area using catchments and stream flow
  attributes. The function grabs the next downstream catchments
  following the main stem by selecting catchments that are within the
  same BASIN and ORDER 1, but where ORDER 2 is smaller than the
  catchment upstream. The function needs an sf conservation area, a
  string matching the unique identifier column in the conservation area,
  and an sf catchments layer using CATCHNUM as unique identifier as well
  as the stream flow attributes ORDER1, ORDER2, ORDER3 and BASIN.

- `extract_catchments_from_table()`: converts a catchment lists
  (e.g. from `get_upstream_catchments()`) to extracted catchments and
  dissolved into one polygon. `dissolve_catchments_from_table()` uses an
  sf set of catchments with unique identifier column: CATCHNUM, a list
  of catchment created by `get_upstream_catchments()` and a string
  representing the output column name holding the conservation area
  unique identifiers.

``` r
# Let start by generating the list of catchment composing the downstream main stem of the conservation area. 

downstream_list <- get_downstream_catchments(conservation_area, "RA_ID", catchments)
downstream_catch <- dissolve_catchments_from_table(catchments, downstream_list, "RA_ID", TRUE)

# Map the resulting downstream main stem area
tmap_mode("plot")

tm_shape(downstream_catch, bbox = st_bbox(fda_10ab)) +
tm_polygons(col = "lightsalmon", border.col = "lightsalmon") +
tm_add_legend(type = "fill", col = "lightsalmon", border.col = "lightsalmon", labels = "Downstream main stem area") +
tm_shape(conservation_area) +
tm_polygons(col = "red" , legend.show = TRUE) +
tm_add_legend(type = "fill", col = "red", border.col = NA, labels = "Conservation area") +
tm_shape(fda_10ab) +
tm_borders(col = "black", lwd = 2, alpha = 1) +
tm_add_legend(type = "fill", col = "white", border.col = "black", labels = "FDA 10ab") +
tm_shape(streams) +
tm_lines(col = "blue" , legend.show = TRUE) +
tm_add_legend(type = "line", col = "blue", labels = "Stream network") +
tm_layout(main.title ="Downstream main stem area", main.title.position =  "center", legend.outside = TRUE)
```

![](README_files/figure-gfm/plots%20downstream%20stem-1.png)<!-- -->

**Generate the complete downstream catchment area**

After identifying the catchments composing the downstream main stem, we
can generate the full downstream area using the
`extract_catchments_from_table()` from which we will run the function
`get_upstream_catchments()`.

``` r
# Extract upstream catchment along the downstream main stem
catch_stem <- extract_catchments_from_table(catchments, downstream_list, as.character(colnames(downstream_list)), "RA_ID")
upstem_list <- get_upstream_catchments(catch_stem, "CATCHNUM", catchments)

#Merge stem catchment with their related upstream catchments  
catchList <- c()
catchList <- c(unlist(downstream_list), unlist(upstem_list))
 
catchList <- catchList %>%
  unique() %>%
  subset(!. %in% unlist(upstream_list)) %>%
  enframe(., name = NULL, value = as.character(colnames(downstream_list)))

# Generate downstream polygon by dissolving all downstream catchments
downstream_all <- dissolve_catchments_from_table(catchments, catchList, as.character(colnames(downstream_list)), TRUE)

# Map the resulting overall downstream area
tmap_mode("plot")

tm_shape(downstream_all, bbox = st_bbox(fda_10ab)) +
tm_polygons(col = "lightsalmon", border.col = "lightsalmon") +
tm_add_legend(type = "fill", col = "lightsalmon", border.col = "lightsalmon", labels = "Downstream area") +
tm_shape(conservation_area) +
tm_polygons(col = "red" , legend.show = TRUE) +
tm_add_legend(type = "fill", col = "red", border.col = NA, labels = "Conservation area") +
tm_shape(fda_10ab) +
tm_borders(col = "black", lwd = 2, alpha = 1) +
tm_add_legend(type = "fill", col = "white", border.col = "black", labels = "FDA 10ab") +
tm_shape(streams) +
tm_lines(col = "blue" , legend.show = TRUE) +
tm_add_legend(type = "line", col = "blue", labels = "Stream network") +
tm_layout(main.title ="Overall downstream area", main.title.position =  "center", legend.outside = TRUE)
```

![](README_files/figure-gfm/plots%20downstream-1.png)<!-- -->

## Case scenario: Upstream impacts related to disturbances

Upstream catchments are a source of vulnerability to the ecological
integrity of a conservation area via anthropogenic disturbances such as
the input of pollutants and water diversions (e.g., dams). The user can
get detailed information on the area, the length and the area-weighted
intactness of catchments that occur upstream of each potential
conservation area by calculating several hydrology metrics provided with
the `beaconshydro` package.

``` r
# Provide a disturbance layer. In this example, we decided to buffer areal and linear #disturbance with 500m buffer.
# set path to dsn
gpk_dist <-  paste0('app/www/fda_10ab_dist.gpkg')

# Initialize R object
footprint_sf <- st_read(gpk_dist, 'footprint_buff500m', quiet=T)

dist <- st_union(footprint_sf)
i <- st_intersection(upstream_catch, dist)
distArea <- i %>%
  mutate(area_dist = st_area(.) %>% as.numeric()) %>%
  st_drop_geometry()

# Tabulate dist area per catchment within the upstream area
upstream_catch <- subset(catchments, catchments$CATCHNUM %in% upstream_list$RA_01)
j <- st_intersection(upstream_catch, dist)
distpercatch <- j %>%
  mutate(area_dist = st_area(.) %>% as.numeric()) %>%
  st_drop_geometry()

catchs <- st_drop_geometry(upstream_catch)
catchs <-merge(catchs, distpercatch[,c("CATCHNUM", "area_dist")], by= "CATCHNUM", all.x = TRUE)
catchs$area_dist[is.na(catchs$area_dist)] <- 0
catchs$area_dist <- as.numeric(catchs$area_dist)
catchs$intact <- round((catchs$Area_total-catchs$area_dist)/catchs$Area_total, 3)

catch_out <- merge(upstream_catch, catchs[,c("CATCHNUM", "intact", "area_dist")], by = "CATCHNUM", all.x = TRUE)

catch_out <- catch_out %>% 
  mutate(intact_class = cut(intact, 
                      breaks = c(0, 0.8, 0.85, 0.9, 0.95, 1),
                      labels = c("<80%", "80-85%", "85-90%", "90-95%", "95-100%")))

# Map the resulting overall downstream area
tmap_mode("plot")
        
tm_shape(catch_out, bbox = st_bbox(fda_10ab)) +
tm_polygons("intact_class", palette = "PRGn", title = "Catchment intactness", legend.reverse = TRUE) +
tm_shape(conservation_area) +
tm_polygons(col = "red" , legend.show = TRUE, title = "Conservation area") +
tm_add_legend(type = "fill", col = "red", border.col = NA, labels = "Conservation area") +
tm_shape(footprint_sf, bbox = st_bbox(fda_10ab)) +
tm_polygons(col = "grey40") +
tm_add_legend(type = "fill", col = "grey40", border.col = "white", labels = "Disturbance footprint") +
tm_shape(fda_10ab) +
tm_borders(col = "black", lwd = 2, alpha = 1) +
tm_add_legend(type = "fill", col = "white", border.col = "black", labels = "FDA 10ab") +
tm_shape(streams) +
tm_lines(col = "blue" , legend.show = TRUE) +
tm_add_legend(type = "line", col = "blue", labels = "Stream network") +
tm_layout(main.title ="Upstream disturbance effects", main.title.size = 1.5, main.title.position =  "center", legend.outside = TRUE)
```

![](README_files/figure-gfm/plots%20Upstream%20impacts-1.png)<!-- -->
