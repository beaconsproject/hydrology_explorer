library(sf)
library(dplyr)
library(terra)
library(raster)
library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyjs)
library(shinyWidgets)
library(rgdal)
library(leafem)
library(shinycssloaders)
library(rhandsontable)
library(tibble)
library(rgeos)
source("./beaconshydro.R")

fda_list <- c("10aa", "10ab", "10ac", "10ad", "10ba", "10bb", "10bc", "10bd", "10be")

ui = dashboardPage(skin="blue",
    dashboardHeader(title = "BEACONS HYDRO (DEMO TOOL)"),
    dashboardSidebar(
     sidebarMenu(id = "tabs",
       menuItem("Overview", tabName = "overview", icon = icon("th")),
       menuItem("Select AOI", tabName = "select", icon = icon("th")),
       menuItem("Generate upstream", tabName = "upstream", icon = icon("th")),
       hr()
     ),
       #selectInput("fda", label="Select FDA:", choices=c("10AB")),
     conditionalPanel(
       h4(" Choose AOI from one of the three options:"),
       condition="input.tabs=='select'",
       selectInput("select_fda", label="Select an FDA:", choices=c("",fda_list), selected=""),
       fileInput(inputId = "upload_poly", label = "Upload a polygon:", multiple = FALSE, accept = ".gpkg"),
       actionButton(inputId = "gen_aoi_button", label = "Select using catchments", icon = icon(name = "circle-plus", lib = "font-awesome"))
      ),
     conditionalPanel(
       condition="input.tabs=='upstream'",
       actionButton("goButtonDown", label  =  HTML("View upstream and
                    <br /> downstream disturbances")),
       #hr(),
       div(style="position:relative; left:calc(6%);", downloadButton("downloadCatchment", "Download results"))
     )
    ),
    dashboardBody(
     useShinyjs(),
     tags$head(tags$style(".skin-blue .sidebar a { color: #444; }")),
     tabItems(
       tabItem(tabName="overview",
               fluidRow(
                 tabBox(
                   id = "one", width="8",
                   tabPanel("Mapview", leafletOutput("map", height=750) %>% withSpinner()),
                 ),
                 tabBox(
                   id = "two", width="4",
                   tabPanel(HTML("<b>Upstream statistics</b>"), tableOutput("tab1"),
                            "*Area Weigthed Intactness",)
                 ),
               )
       )
     )
    ))


server = function(input, output) {

  output$help <- renderText({
    includeMarkdown("upstream.md")
  })

  ####################################################################################################
  # READ SPATIAL DATA
  ####################################################################################################
  ################################################################################################
  # Select FDA and zoom in to its extent
  ################################################################################################

  # data objects - set up as reactiveValues
  selected_catchments <- reactiveValues( # this is the list of currently selected catchments
    catchnum = c()
  )

  selected_fda <- reactive({
    if (input$select_fda >0) {
      paste0('www/fda',tolower(input$select_fda),'.gpkg')
    } else {
      "www/fda10.gpkg"
    }
  })

  bnd <- reactive({
    st_read('www/fda10.gpkg', 'bnd', quiet=T)
  })

  catchments <- reactive({
    catchments <- st_read('www/fda10.gpkg', 'catchments', quiet=T)
  })

  fdas <- reactive({
    catchments <- st_read('www/fda10.gpkg', 'fda', quiet=T)
  })

  fda <- reactive({
    st_read(selected_fda(), 'fda', quiet=T)
  })

  footprint_sf <- reactive({
    st_read(selected_fda(), 'footprint', quiet=T)
  })

  intactness_sf <- reactive({
    st_read(selected_fda(), 'intactness', quiet=T)
  })

  ################################################################################################
  # Upload AOI and zoom in to its extent
  ################################################################################################
  aoi_sf <- eventReactive(input$upload_poly, {
    file <- input$upload_poly$datapath
    ext <- tools::file_ext(file)
    if(ext == "gpkg"){
      aoi <- st_read(file) #%>% st_transform(4326)
    } else{
      showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
    }
  })

  ################################################################################################
  # Set footprint and intactness for uploaded polygon
  ################################################################################################
  foot_sf <- reactive({
    if(!is.null(input$upload_poly)){
      aoi <-  aoi_sf()
      aoi_catch <- st_intersection(catchments(), aoi)
      catch_list <- unique(aoi_catch$CATCHNUM)
      footprint <- catchments() %>%
        filter(CATCHNUM %in% catch_list) %>%
        st_union() %>%
        st_intersection(footprint_sf())
    }else if(input$gen_aoi_button>0){
      req(length(selected_catchments$catchnum) > 0)
      data_selected <- catchments()[catchments()$CATCHNUM %in% selected_catchments$catchnum,]
      footprint <- data_selected %>%
        st_union() %>%
        st_intersection(footprint_sf())
    }else{
      footprint <- footprint_sf()
    }
    return(footprint)
  })

  intact_sf <- reactive({
    if(!is.null(input$upload_poly)){
      aoi <-  aoi_sf()
      aoi_catch <- st_intersection(catchments(), aoi)
      catch_list <- unique(aoi_catch$CATCHNUM)
      intactness <- catchments() %>%
        filter(CATCHNUM %in% catch_list) %>%
        st_union() %>%
        st_intersection(intactness_sf())
    }else if(input$gen_aoi_button>0){
      req(length(selected_catchments$catchnum) > 0)
      data_selected <- catchments()[catchments()$CATCHNUM %in% selected_catchments$catchnum,]
      intactness <- data_selected %>%
        st_union() %>%
        st_intersection(intactness_sf())
    }else{
      intactness <- intactness_sf()
    }
      return(intactness)
  })
  ####################################################################################################
  # UPLOAD CUSTOM DISTURBANCE TABLE from GPKG
  ####################################################################################################

  # Upload geopackage and add polygon to map
  #options(shiny.maxRequestSize = 9*1024^2)
  #observe({
  #  req(input$upload_poly)
  #  file <- input$upload_poly$datapath
  #  ext <- tools::file_ext(file)
  #  if(ext == "gpkg"){
  #    intact_sf <- st_read(file, 'intactness', quiet=T) %>% st_transform(4326)
  #    footprint_sf <- st_read(file, 'footprint', quiet=T) %>% st_transform(4326)
  #  } else{
  #    showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
  #  }
 # })

  # Add intersecting catchments to selection when button pressed
  #observeEvent(input$select_poly_button, {

  #  add_catchments <- catchnums_in_polygon(st_transform(selection_poly$poly, 3579), st_transform(catchments, 3579))

  #  selected_catchments$catchnum <- c(selected_catchments$catchnum, add_catchments)
  #})

  ####################################################################################################
  # UPSTREAM SECTION
  ####################################################################################################
  catch_up <- eventReactive(input$goButtonDown, {
    # Generate upstream catchments
    upstream_list <- get_upstream_catchments(consarea(), "RA_ID", catchments())
    upstream_area <- dissolve_catchments_from_table(catchments(), upstream_list, "RA_ID")

    # Tabulate dist area within the FDA
    dist <- st_union(footprint_sf())
    i <- st_intersection(catchments(), dist)
    distArea <- i %>%
      mutate(area_dist = st_area(.) %>% as.numeric()) %>%
      st_drop_geometry()
    catchments <- merge(catchments(), distArea[,c("CATCHNUM", "area_dist")], by = "CATCHNUM", all.x = TRUE)
    catchments$area_dist <- as.numeric(catchments$area_dist)
    catchments$area_dist[is.na(catchments$area_dist)] <- 0
    catchments$intact <- round((catchments$Area_total-catchments$area_dist)/catchments$Area_total, 3)
    catchments$consarea <- NA
    catchments$consarea[which(catchments$CATCHNUM %in% consarea()$CATCHNUM)] <- 1

    # Tabulate dist area per catchment within the upstream area
    upstream_catch <- subset(catchments, catchments$CATCHNUM %in% upstream_list$RA_01)
    upstream_catch <- st_drop_geometry(upstream_catch)
    upstream_catch$up <- 1
    merge(catchments, upstream_catch[,c("CATCHNUM", "up")], by = "CATCHNUM", all.x = TRUE)
  })

  ####################################################################################################
  # DOWNSTREAM STEM SECTION
  ####################################################################################################
  catch_stem <- eventReactive(input$goButtonDown, {
    # downstream stem polygon
    downstream_stem_list <- get_downstream_catchments(consarea(), "RA_ID", catchments())
    catch_stem <- catchments()[catchments()$CATCHNUM %in% downstream_stem_list$RA_01, drop = TRUE]
    catch_stem$stem <- 1
    merge(catch_up(), catch_stem[,c("CATCHNUM", "stem")], by = "CATCHNUM", all.x = TRUE)
  })

  ####################################################################################################
  # DOWNSTREAM SECTION
  ####################################################################################################
  catch_att <- eventReactive(input$goButtonDown, {
    # downstream  polygon
    # Extract upstream catchment along the downstream stem
    downstream_stem_list <- get_downstream_catchments(consarea(), "RA_ID", catchments())
    catch_stem <- extract_catchments_from_table(catchments(), downstream_stem_list, as.character(colnames(downstream_stem_list)), "RA_ID")
    upstem_list <- get_upstream_catchments(catch_stem, "CATCHNUM", catchments())

    # CReate upstream catchment along the downstream stem
    upstream_list <- get_upstream_catchments(consarea(), "RA_ID", catchments())

    #Merge stem catchment with their related upstream catchments
    catchList <- c()
    catchList <- c(unlist(downstream_stem_list), unlist(upstem_list))
    catchList <- catchList %>%
      unique() %>%
      subset(!. %in% unlist(upstream_list)) %>% #drop catchments within the upstream
      subset(!. %in% consarea()$CATCHNUM) %>% #drop catchments within the reference_area
      enframe(., name = NULL, value = as.character(colnames(downstream_stem_list)))

    # Generate downstream polygon by dissolving all downstream catchments
    catch_down<- catchments()[catchments()$CATCHNUM %in% catchList$RA_01, drop = TRUE]
    catch_down$down <- 1
    merge(catch_stem(), catch_down[,c("CATCHNUM", "down")], by = "CATCHNUM", all.x = TRUE)
  })

  ####################################################################################################
  # Map viewer
  ####################################################################################################
  # Render the initial map
  output$map <- renderLeaflet({
    # Re-project
    bnd <- st_transform(bnd(), 4326)
    fdas <- st_transform(fdas(), 4326)
    catch_4326 <- st_transform(catchments(), 4326)
    intact <- st_transform(intact_sf(), 4326)
    footprint <- st_transform(foot_sf(), 4326)
    map_bounds <- bnd %>% st_bbox() %>% as.character()

    # Render initial map
    leaflet() %>%

      addProviderTiles("Esri.NatGeoWorldMap", group="Esri.NatGeoWorldMap") %>%
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
      addPolygons(data=bnd, color='black', fill=F, weight=2, group="Data extent") %>%
      addPolygons(data=fdas, color='black', fill=F, weight=3, fillOpacity=1, group="FDA") %>%
      addStaticLabels(data = fdas, label = fdas$FDA, style = list("color" = "black", "font-weight" = "bold", "font-size" = "13px"), group='label') %>%
      addPolygons(data=catch_4326, color='black', fillColor = "grey", fillOpacity = 0, weight=1, layerId = catch_4326$CATCHNUM, group="Catchments") %>%
      addPolygons(data=intact, color='darkgreen', stroke=F, fillOpacity=0.5, group='Intactness') %>%
      addPolygons(data=footprint, fill=T, stroke=F, fillColor='black', fillOpacity=0.5, group="Footprint") %>%

      fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) %>% # set view to the selected FDA
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.NatGeoWorldMap", "Esri.WorldImagery"),
                       overlayGroups = c("Data extent", "FDA", "Catchments", "Intactness", "Footprint"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Catchments", "Intactness", "Footprint"))
  })


  # Render selected FDA
  observe({
    req(input$select_fda >0)

    #Display fda of interest and respectives disturbances
    aoi <- st_transform(fda(), 4326)
    intact <- st_transform(intactness_sf(), 4326)
    footprint <- st_transform(footprint_sf(), 4326)
    map_bounds1 <- aoi %>% st_bbox() %>% as.character()

    leafletProxy("map") %>%
      clearGroup('Intactness') %>%
      clearGroup('Footprint') %>%
      clearGroup('label') %>%
      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
      addPolygons(data=aoi, fill=F, color="red", weight=4, group="AOI") %>%
      addPolygons(data=intact, color='darkgreen', stroke=F, fillOpacity=0.5, group='Intactness') %>%
      addPolygons(data=footprint, fill=T, stroke=F, fillColor='black', fillOpacity=0.5, group="Footprint")
  })

  # Render AOI by uploading gpkg
  observe({
    req(input$upload_poly)

    # Clip to area of interest and display those features
    aoi <- st_transform(aoi_sf(), 4326)
    intact <- st_transform(intact_sf(), 4326)
    footprint <- st_transform(foot_sf(), 4326)
    map_bounds1 <- aoi %>% st_bbox() %>% as.character()

    leafletProxy("map") %>%
        clearGroup('Intactness') %>%
        clearGroup('Footprint') %>%
        clearGroup('label') %>%
        fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
        addPolygons(data=aoi, fill=F, color="red", weight=4, group="AOI") %>%
        addPolygons(data=intact, color='darkgreen', stroke=F, fillOpacity=0.5, group='Intactness') %>%
        addPolygons(data=footprint, fill=T, stroke=F, fillColor='black', fillOpacity=0.5, group="Footprint")
    })

  # Render AOI by selecting catchment
  # Track a list of which catchnums have been selected
  observeEvent(input$map_shape_click,{

    clickId <- input$map_shape_click$id # id is the layerId assigned to the polygon layer in Leaflet

    if(clickId %in% selected_catchments$catchnum){
      selected_catchments$catchnum <- selected_catchments$catchnum[!selected_catchments$catchnum %in% clickId]
    } else{
      selected_catchments$catchnum <- c(selected_catchments$catchnum, clickId)
    }
  })

  # If clear selection button is pressed, remove all catchnums from list
  observeEvent(input$clear_button, {
    selected_catchments$catchnum <- c()
  })

  # Highlight the selected catchments on the map - using proxy prevents map re drawing every time
  observe({
    data_selected <- catchments()[catchments()$CATCHNUM %in% selected_catchments$catchnum,]
    data_select <- st_transform(data_selected, 4326)

    leafletProxy("map") %>%
      clearGroup("Selected") %>%
      addPolygons(data = data_select,
                  color='black', weight = 1, fillColor = "grey", fillOpacity = 0.7,
                  layerId = data_select$CATCHNUM,
                  group = "Selected")
  })

  # hide selected catchments when catchments not selected
  observe({
    if(!"Catchments" %in% input$map_groups){
      leafletProxy("map") %>%
        hideGroup("Selected")
    } else{
      leafletProxy("map") %>%
        showGroup("Selected")
    }
  })
  # When gen_aoi_button is pressed...
  observeEvent(input$gen_aoi_button, {
    browser()
    req(length(selected_catchments$catchnum) > 0)
    data_selected <- catchments()[catchments()$CATCHNUM %in% selected_catchments$catchnum,]
    aoi <- st_transform(st_union(data_selected), 4326)
    intact <- st_transform(intact_sf(), 4326)
    footprint <- st_transform(foot_sf(), 4326)
    map_bounds1 <- aoi %>% st_bbox() %>% as.character()

    leafletProxy("map") %>%
      clearGroup('AOI')
      clearGroup('Intactness') %>%
      clearGroup('Footprint') %>%
      clearGroup('label') %>%
      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
      addPolygons(data=aoi, fill=F, color="red", weight=4, group="AOI") %>%
      addPolygons(data=intact, color='darkgreen', stroke=F, fillOpacity=0.5, group='Intactness') %>%
      addPolygons(data=footprint, fill=T, stroke=F, fillColor='black', fillOpacity=0.5, group="Footprint")
  })
}

shinyApp(ui, server)
