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

options(shiny.maxRequestSize=30*1024^2) 

fda_list <- c("Full extent", "10aa", "10ab", "10ac", "10ad", "10ba", "10bb", "10bc", "10bd", "10be")

ui = dashboardPage(skin="blue",
    dashboardHeader(title = "BEACONS HYDRO (DEMO TOOL)"),
    dashboardSidebar(
      width = 250,
     sidebarMenu(id = "tabs",
       menuItem("Overview", tabName = "overview", icon = icon("th")),
       menuItem("Select disturbances & AOI", tabName = "select", icon = icon("th")),
       menuItem("Generate upstream", tabName = "upstream", icon = icon("th")),
       hr()
     ),
     conditionalPanel(
       h4(" Choose disturbance from one of the two option:"),
       condition="input.tabs=='select'",
       selectInput("select_fda", label="Use default disturbance", choices=c("Select an FDA", fda_list), selected="Select an FDA"),
       #actionButton("distButton", "Use default disturbance"),
       fileInput(inputId = "upload_dist", label = "Upload disturbances:", multiple = FALSE, accept = ".gpkg")
     ),
    conditionalPanel(
      h4(" Choose AOI from one of the two options:"),
      condition="input.tabs=='select'",
      fileInput(inputId = "upload_poly", label = "Upload a polygon:", multiple = FALSE, accept = ".gpkg"),
      actionButton(inputId = "gen_aoi_button", label = "Generate AOI using catchments", icon = icon(name = "circle-plus", lib = "font-awesome")),
      tags$br(),
      tags$br()
    ),
     conditionalPanel(
       condition="input.tabs=='upstream'",
       actionButton("goButtonDown", label  =  HTML("View upstream and
                    <br /> downstream disturbances")),
       #hr(),
       div(style="position:relative; left:calc(6%);", downloadButton("downloadData", "Download results"))
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
                            "*Catchment Area Weighted Intactness",)
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

  # data objects - set up as reactiveValues
  selected_catchments <- reactiveValues( # this is the list of currently selected catchments
    catchnum = c()
  )

  selected_fda <- reactive({
    if (input$select_fda == "Full extent") {
      "www/fda10.gpkg"
    } else {
      paste0('www/fda',tolower(input$select_fda),'.gpkg')
    }
  })

  bnd <- reactive({
    st_read('www/fda10.gpkg', 'bnd', quiet=T)
  })

  catchments <- reactive({
    st_read('www/fda10.gpkg', 'catchments', quiet=T)
  })

  fdas <- reactive({
    st_read('www/fda10.gpkg', 'fda', quiet=T)
  })

  streams <- reactive({
    st_read('www/fda10.gpkg', 'stream', quiet=T)
  })
  
  fda <- reactive({
    st_read(selected_fda(), 'fda', quiet=T)
  })

  stream <- reactive({
    st_read(selected_fda(), 'stream', quiet=T)
  })
  
  footprint_sf <- reactive({
    st_read(selected_fda(), 'footprint', quiet=T)
  })

  intactness_sf <- reactive({
    st_read(selected_fda(), 'intactness', quiet=T)
  })


  ################################################################################################
  # Set disturbances
  ################################################################################################
  #Footprint
  foot_sf <- reactive({
    if(!is.null(input$upload_dist)){
      file <- input$upload_dist$datapath
      ext <- tools::file_ext(file)
      if(ext == "gpkg"){
        footprint <- st_read(file, 'footprint', quiet=T)
      }else{
        showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
      }
    }else{   
      footprint <- footprint_sf()
    }  
    return(footprint)
  })
  
  #Intactness
  intact_sf <- reactive({
    if(!is.null(input$upload_dist)){
      file <- input$upload_dist$datapath
      ext <- tools::file_ext(file)
      if(ext == "gpkg"){
        intactness <- st_read(file, 'intactness', quiet=T) 
      }else{
        showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
      }
    }else{   
      intactness <- intactness_sf()
    }  
    return(intactness)
  })
  
  #Planning region
  planreg_sf <- reactive({
    if(!is.null(input$upload_dist)){
      file <- input$upload_dist$datapath
      ext <- tools::file_ext(file)
      if(ext == "gpkg"){
        planreg <- st_read(file, 'aoi', quiet = TRUE) 
      } else{
        showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error") 
      }
    }else if(input$select_fda != "Select an FDA"){
      if(input$select_fda != "Full extent"){
        planreg <- fda()
      }else{
        planreg <- bnd()
      }
    }else{
      planreg <- NULL
    }
    return(planreg)
  })

  #stream
  stream_sf <- reactive({
    if(!is.null(input$upload_dist)){
      file <- input$upload_dist$datapath
      ext <- tools::file_ext(file)
      if(ext == "gpkg"){
        aoi <- st_read(file, 'aoi', quiet = TRUE) 
        stream <- st_intersection(streams(), aoi)
      } else{
        showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error") 
      }
    }else if(input$select_fda != "Select an FDA"){
      if(input$select_fda != "Full extent"){
        stream <- streams()
      }else{
        stream <- stream()
      }
    }else{
      stream <- streams()
    }
    return(stream)
  })
  
  #Catchments within the planning region
  catchment <- reactive({
    req(planreg_sf())
    catchments <- st_intersection(catchments(), planreg_sf())
    intact <- st_intersection(foot_sf(), catchments)
    distArea <- intact %>%
      mutate(area_dist = st_area(.) %>% as.numeric()) %>%
      st_drop_geometry()
    catchments <- merge(catchments, distArea[,c("CATCHNUM", "area_dist")], by = "CATCHNUM", all.x = TRUE)
    catchments$area_dist <- as.numeric(catchments$area_dist)
    catchments$area_dist[is.na(catchments$area_dist)] <- 0
    catchments$intact <- round((catchments$Area_total-catchments$area_dist)/catchments$Area_total, 3)
    return(catchments)
  })
  
  ################################################################################################
  # Upload AOI and zoom in to its extent
  ################################################################################################
  #aoi_sf <- eventReactive(input$upload_poly, {
  #  file <- input$upload_poly$datapath
  #  ext <- tools::file_ext(file)
  #  if(ext == "gpkg"){
  #    aoi <- st_read(file, 'aoi') #%>% st_transform(4326)
  #  }else{
  #    showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
   # }
  #})
  aoi_sf <- reactive({
    if(!is.null(input$upload_poly)){
      file <- input$upload_poly$datapath
      ext <- tools::file_ext(file)
      if(ext == "gpkg"){
        aoi <- st_read(file, 'aoi') #%>% st_transform(4326)
      }else{
        showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
      }
    }else if(input$gen_aoi_button>0){
      req(length(selected_catchments$catchnum) > 0)
      catch_sf <- catchment()[catchment()$CATCHNUM %in% selected_catchments$catchnum,]
      aoi <- st_union(catch_sf)
    }else{
      aoi <- NULL
    }
    return(aoi)
  })
  
  
  catch_aoi <- reactive({
    if(!is.null(input$upload_poly)){
      aoi <-  aoi_sf()
      aoi_catch <- st_intersection(st_centroid(catchment()), aoi)
      catch_list <- unique(aoi_catch$CATCHNUM)
      catch_sf <- catchments() %>%
        filter(CATCHNUM %in% catch_list)
    }else if(input$gen_aoi_button>0){
      req(length(selected_catchments$catchnum) > 0)
      catch_sf <- catchment()[catchment()$CATCHNUM %in% selected_catchments$catchnum,]
    }else{
      catch_sf <- catchment()
    }
    return(catch_sf)
  })


  ####################################################################################################
  # UPSTREAM SECTION
  ####################################################################################################
  catch_up <- eventReactive(input$goButtonDown, {
    # Generate upstream catchments
    aoi_sf <- aoi_sf() %>%
      st_as_sf() %>%
      mutate(AOI_ID = "AOI_01")
    upstream_list <- get_upstream_catchments(aoi_sf, "AOI_ID", catchment())
    upstream_area <- dissolve_catchments_from_table(catchments(), upstream_list, "AOI_ID")

    # Tabulate dist area per catchment within the upstream area
    if(nrow(upstream_list)==0)
    {
      # show pop-up ...
      showModal(modalDialog(
        title = "No upstream catchments found!",
        easyClose = TRUE,
        footer = NULL
      ))
      catchment <- catchment() %>%
        mutate(up = 0)
    }else{
      catch_up <- catchment()[catchment()$CATCHNUM %in% upstream_list$AOI_01, drop = TRUE]
      catch_up$up <- 1
      catchment <- merge(catchment(), catch_up[,c("CATCHNUM", "up")], by = "CATCHNUM", all.x = TRUE)
      catchment$up[is.na(catchment$up)] <- 0
    }
    return(catchment)
  })

  ####################################################################################################
  # DOWNSTREAM STEM SECTION
  ####################################################################################################
  catch_stem <- eventReactive(input$goButtonDown, {
    aoi_sf <- aoi_sf() %>%
      st_as_sf() %>%
      mutate(AOI_ID = "AOI_01")
    # downstream stem polygon
    downstream_stem_list <- get_downstream_catchments(aoi_sf, "AOI_ID", catchment())
    if(nrow(downstream_stem_list)==0)
    {
      # show pop-up ...
      showModal(modalDialog(
        title = "No downstream stem catchments found!",
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
      catch_stem <- catchment()[catchment()$CATCHNUM %in% downstream_stem_list$AOI_01, drop = TRUE]
      catch_stem$stem <- 1
      merge(catchment(), catch_stem[,c("CATCHNUM", "stem")], by = "CATCHNUM", all.x = TRUE)
    }
  })

  ####################################################################################################
  # DOWNSTREAM SECTION
  ####################################################################################################
  catch_att <- eventReactive(input$goButtonDown, {
    # downstream  polygon
    # Extract upstream catchment along the downstream stem
    aoi_sf <- aoi_sf() %>%
      st_as_sf() %>%
      mutate(AOI_ID = "AOI_01")
    downstream_stem_list <- get_downstream_catchments(aoi_sf, "AOI_ID", catchment())
    catch_stem <- extract_catchments_from_table(catchment(), downstream_stem_list, as.character(colnames(downstream_stem_list)), "AOI_ID")
    # Create upstream catchment along the downstream stem
    upstem_list <- get_upstream_catchments(catch_stem, "CATCHNUM", catchment())

    # List upstream catchments
    upstream_list <- get_upstream_catchments(aoi_sf, "AOI_ID", catchment())
    #Merge stem catchment with their related upstream catchments
    catchList <- c()
    catchList <- c(unlist(downstream_stem_list), unlist(upstem_list))
    catchList <- catchList %>%
      unique() %>%
      subset(!. %in% unlist(upstream_list)) %>% #drop catchments within the upstream
      subset(!. %in% catch_aoi()$CATCHNUM) %>% #drop catchments within the reference_area
      enframe(., name = NULL, value = as.character(colnames(downstream_stem_list)))

    # Generate downstream polygon by dissolving all downstream catchments
    if(nrow(downstream_stem_list)==0)
    {
      # show pop-up ...
      showModal(modalDialog(
        title = "No downstream catchments found!",
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
      catch_down<- catchment()[catchment()$CATCHNUM %in% catchList$AOI_01, drop = TRUE]
      catch_down$down <- 1
      
      catch_down <- catch_down %>%
        st_drop_geometry() %>%
        dplyr::select(CATCHNUM, down) 
               
      catch_stem <- catch_stem() %>%
        st_drop_geometry() %>%
        dplyr::select(CATCHNUM, stem)       
      
      catch_up <- catch_up() %>%
        st_drop_geometry() %>%
        dplyr::select(CATCHNUM, up)
      
      catch_att <- catchment() %>%
        left_join(catch_down, by = "CATCHNUM") %>%
        left_join(catch_stem, by = "CATCHNUM") %>%
        left_join(catch_up, by = "CATCHNUM") 
    }
  })
  
  ####################################################################################################
  ####################################################################################################
  # Map viewer
  ####################################################################################################
  ####################################################################################################
  # Render the initial map
  output$map <- renderLeaflet({
    # Re-project
    bnd <- st_transform(bnd(), 4326)
    fdas <- st_transform(fdas(), 4326)
    catch_4326 <- st_transform(catchments(), 4326)
    stream_4326 <- st_transform(stream_sf(), 4326)
    map_bounds <- bnd %>% st_bbox() %>% as.character()

    # Render initial map
    leaflet() %>%
      addMapPane(name = "layer1", zIndex=380) %>%
      addMapPane(name = "layer2", zIndex=420) %>%
      addProviderTiles("Esri.NatGeoWorldMap", group="Esri.NatGeoWorldMap") %>%
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
      addPolygons(data=bnd, color='black', fill=F, weight=2, group="Data extent", options = leafletOptions(pane = "layer1")) %>%
      addPolygons(data=fdas, color='black', fill=F, weight=3, fillOpacity=1, group="FDAs", options = leafletOptions(pane = "layer1")) %>%
      addStaticLabels(data = fdas, label = fdas$FDA, style = list("color" = "black", "font-weight" = "bold", "font-size" = "13px"), group='label', options = leafletOptions(pane = "layer1")) %>%
      addPolygons(data=catch_4326, color='black', fillColor = "grey", fillOpacity = 0, weight=1, layerId = catch_4326$CATCHNUM, group="Catchments", options = leafletOptions(pane = "layer2")) %>%
      addPolylines(data=stream_4326, color='#0066FF', weight=0.8, group="Streams") %>%
      
      fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) %>% # set view to the selected FDA
      addControl(actionButton(inputId = "clear_button", label = "Clear selection"), position="topleft", className = "class_clear_button") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.NatGeoWorldMap", "Esri.WorldImagery"),
                       overlayGroups = c("Data extent", "FDAs", "Catchments", "Streams"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Catchments", "Streams"))
  })

  # Render default disturbance
  observe({
    req(input$select_fda != "Select an FDA")
    #Display fda of interest and respectives disturbances
    planreg_sf <- st_transform(planreg_sf(), 4326)
    intact <- st_transform(intact_sf(), 4326)
    footprint <- st_transform(foot_sf(), 4326)
    planreg <- st_transform(planreg_sf(), 4326)
    catch_4326 <- st_transform(catchment(), 4326)
    stream_4326 <- st_transform(stream_sf(), 4326)
    map_bounds1 <- footprint %>% st_bbox() %>% as.character()
    
    leafletProxy("map") %>%
    clearGroup('Catchments') %>%  
    clearGroup('Streams') %>%  
    fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
    addPolygons(data=planreg, color='red', fill=F, weight=4, group='Planning region', options = leafletOptions(pane = "layer1")) %>%
    addPolygons(data=intact, color='darkgreen', stroke=F, fillOpacity=0.5, group='Intactness', options = leafletOptions(pane = "layer1")) %>%
    addPolygons(data=footprint, fill=T, stroke=F, fillColor='black', fillOpacity=0.5, group="Footprint", options = leafletOptions(pane = "layer2")) %>%
    addPolygons(data=catch_4326, color='black', fillColor = "grey", fillOpacity = 0, weight=1, layerId = catch_4326$CATCHNUM, group="Catchments", options = leafletOptions(pane = "layer2")) %>%
    addPolylines(data=stream_4326, color='#0066FF', weight=1, group="Streams", options = leafletOptions(pane = "layer1")) %>%
    addLayersControl(position = "topright",
                     baseGroups=c("Esri.NatGeoWorldMap", "Esri.WorldImagery"),
                     overlayGroups = c("Data extent", "FDAs", "Planning region", "Catchments", "Streams", "Intactness", "Footprint"),
                     options = layersControlOptions(collapsed = FALSE))  %>%
      hideGroup(c("Intactness"))
  })

  # Render custom disturbance
  observe({
    req(input$upload_dist >0)
    #Display fda of interest and respectives disturbances
    intact <- st_transform(intact_sf(), 4326)
    footprint <- st_transform(foot_sf(), 4326)
    planreg <- st_transform(planreg_sf(), 4326)
    catch_4326 <- st_transform(catchments(), 4326)
    stream_4326 <- st_transform(stream_sf(), 4326)
    map_bounds1 <- planreg %>% st_bbox() %>% as.character()
    
    leafletProxy("map") %>%
    clearGroup('Catchments') %>%
    clearGroup('Streams') %>%
    fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
    addPolygons(data=planreg, color='red', fill=F, weight=4, group='Planning region') %>%
    addPolygons(data=intact, color='darkgreen', stroke=F, fillOpacity=0.5, group='Intactness', options = leafletOptions(pane = "layer1")) %>%
    addPolygons(data=footprint, fill=T, stroke=F, fillColor='black', fillOpacity=0.5, group="Footprint", options = leafletOptions(pane = "layer2")) %>%
    addPolygons(data=catch_4326, color='black', fillColor = "grey", fillOpacity = 0, weight=1, layerId = catch_4326$CATCHNUM, group="Catchments", options = leafletOptions(pane = "layer1")) %>%
    addPolylines(data=stream_4326, color='#0066FF', weight=1, group="Streams", options = leafletOptions(pane = "layer1")) %>%
    addLayersControl(position = "topright",
                       baseGroups=c("Esri.NatGeoWorldMap", "Esri.WorldImagery"),
                       overlayGroups = c("Data extent", "FDAs", "Planning region","Streams", "Catchments", "Intactness", "Footprint"),
                       options = layersControlOptions(collapsed = FALSE))%>%
      hideGroup(c("Intactness"))
  })  
  
  # Render AOI by uploading gpkg
  observe({
    req(input$upload_poly)
    # Clip to area of interest and display those features
    intact <- intact_sf() %>% st_transform(4326)
    footprint <- foot_sf() %>% st_transform(4326)
    aoi <- st_transform(aoi_sf(), 4326)
    map_bounds1 <- aoi %>% st_bbox() %>% as.character()

    leafletProxy("map") %>%
        clearGroup('Intactness') %>%
        clearGroup('Footprint') %>%
        clearGroup('label') %>%
        fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
        addPolygons(data=aoi, fill=F, color="blue", weight=4, group="AOI", options = leafletOptions(pane = "layer1")) %>%
        addPolygons(data=intact, color='darkgreen', stroke=F, fillOpacity=0.5, group='Intactness', options = leafletOptions(pane = "layer1")) %>%
        addPolygons(data=footprint, fill=T, stroke=F, fillColor='black', fillOpacity=0.5, group="Footprint", options = leafletOptions(pane = "layer2")) %>%
        addLayersControl(overlayGroups = c("Data extent", "FDAs", "Planning region", "Catchments", "Streams",  "Intactness", "Footprint", "AOI"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("Intactness"))
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
  
  # Render final AOI based on selected catchments
  observeEvent(input$gen_aoi_button, {
    req(length(selected_catchments$catchnum) > 0)
    data_selected <- catchment()[catchment()$CATCHNUM %in% selected_catchments$catchnum,]
    aoi <- st_transform(st_union(data_selected), 4326)
    map_bounds1 <- aoi %>% st_bbox() %>% as.character()

    leafletProxy("map") %>%
      clearGroup('AOI') %>%
      clearGroup('label') %>%
      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
      addPolygons(data=aoi, fill=F, color="blue", weight=4, group="AOI", options = leafletOptions(pane = "layer1")) %>%
      addLayersControl(overlayGroups = c("Data extent", "FDAs", "Planning region", "Catchments", "Streams","Intactness", "Footprint", "AOI"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Intactness"))
  })
  
  # Render Upstream / downstream
  observe({
    req(input$goButtonDown >0)
    catch_up <- catch_up() %>% 
      filter(up ==1) %>%
      st_transform(4326)
    catch_att <- catch_att() %>% 
      filter(down==1) %>%
      st_transform(4326)
    catch_stem <- catch_stem() %>% 
      filter(stem ==1) %>%
      st_transform(4326)

    ## Create bin palette function for percent
    catchintact = colorBin(
      palette = 'PRGn',
      domain = catch_att$intact,
      bins = 10)
    
    map_bounds1 <- catch_att %>% st_bbox() %>% as.character()
    leafletProxy("map") %>%
    clearGroup('Intactness') %>%
    clearGroup('FDAs') %>%
    
    #clearGroup('Footprint') %>%
    fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
    addPolygons(data=catch_att, color=~catchintact(intact), stroke=F, fillOpacity=0.8, group='Downstream', options = leafletOptions(pane = "layer1")) %>%
    #addPolygons(data=catch_stem, fill=F, color= 'black', weight = 3, group="Downstream stem") %>%
    addPolygons(data=catch_stem, color=~catchintact(intact), stroke=F, fillOpacity=0.8, group="Downstream stem", options = leafletOptions(pane = "layer1")) %>%
    addLegend(position = "bottomleft", pal = catchintact, values = catch_att$intact, opacity = 1,
              title = "Percent intactness", labFormat = labelFormat(
                suffix = "%",
                transform = function(x) 100 * x),
              group = "Downstream catchment intactness") %>%
    addLayersControl(position = "topright",
                     baseGroups=c("Esri.NatGeoWorldMap", "Esri.WorldImagery"),
                     overlayGroups = c("Data extent", "Planning region", "AOI", "Catchments", "Streams","Footprint", "Downstream", "Downstream stem"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Downstream stem"))
    if(nrow(catch_up)>0){
      leafletProxy("map") %>%
      addPolygons(data=catch_up, fill=T, color="#CC9966", fillOpacity=0.5, stroke=F, group="Upstream", options = leafletOptions(pane = "layer1")) %>%
      addLayersControl(position = "topright",
                         baseGroups=c("Esri.NatGeoWorldMap", "Esri.WorldImagery"),
                         overlayGroups = c("Data extent", "Planning region", "AOI", "Catchments", "Streams","Footprint", "Upstream", "Downstream", "Downstream stem"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("Downstream stem"))
    } 
  })

  ################################################################################################
  # Generate statistics
  ################################################################################################
  outputtab1 <- reactive({
    # If button has been pressed
    if(input$goButtonDown > 0) {
      # upstream catchment intactness
      catch_map <- catch_att() %>%
        filter(up ==1) %>%
        st_transform(4326)
      
      # upstream area
      upstream_area <- catch_map %>%
        st_union(.)
      
      #downstream stem intact
      downstream_stem_int <- catch_att() %>%
        filter(stem ==1) %>%
        st_transform(4326)
      
      #downstream stem area
      downstream_stem <- catch_att() %>%
        filter(stem ==1) %>%
        st_union(.) %>%
        st_transform(4326)
      
      #downstream area
      downstream_all <- catch_att() %>%
        filter(down ==1) %>%
        st_union(.) %>%
        st_transform(4326) %>%
        st_make_valid(.)
      
      x <- tibble(Variables=c("Planning region",
                              "AOI",
                              "Percent AOI in planning region",
                              "Upstream area",
                              "Downstream stem area",
                              "Downstream area",
                              "Upstream mean AWI*",
                              "Downstream stem mean AWI*"
      ), Area_km2=NA, Percent = NA)
      x$Area_km2[x$Variables=="Planning region"] <- round(st_area(planreg_sf())/1000000,0)
      x$Area_km2[x$Variables=="AOI"] <- round(st_area(st_union(aoi_sf()))/1000000,0)
      x$Percent[x$Variables=="Percent AOI in planning region"] <- round(st_area(st_union(aoi_sf()))/st_area(planreg_sf())*100,2)
      x$Area_km2[x$Variables=="Upstream area"] <- round(st_area(upstream_area)/1000000,0)
      x$Area_km2[x$Variables=="Downstream stem area"] <- round(st_area(downstream_stem)/1000000,0)
      x$Area_km2[x$Variables=="Downstream area"] <- round(st_area(downstream_all)/1000000,0)
      x$Percent[x$Variables=="Upstream mean AWI*"] <- round(((sum(catch_map$Area_total)-sum(catch_map$area_dist))/sum(catch_map$Area_total))*100,1)
      x$Percent[x$Variables=="Downstream stem mean AWI*"] <- round(((sum(downstream_stem_int$Area_total)-sum(downstream_stem_int$area_dist))/sum(downstream_stem_int$Area_total))*100,1)
    }else{
      req(aoi_sf())
      x <- tibble(Variables=c("Planning region",
                              "AOI",
                              "Percent AOI in planning region" ,
                              "Upstream area",
                              "Downstream stem area",
                              "Downstream area",
                              "Upstream mean AWI*",
                              "Downstream stem mean AWI*"
      ), Area_km2=NA, Percent = NA)
      x$Area_km2[x$Variables=="Planning region"] <- round(st_area(planreg_sf())/1000000,0)
      x$Area_km2[x$Variables=="AOI"] <- round(st_area(st_union(aoi_sf()))/1000000,0)
      x$Percent[x$Variables=="Percent AOI in planning region"] <- round(st_area(st_union(aoi_sf()))/st_area(planreg_sf())*100,2)
    }
    x
  })
  
  output$tab1 <- renderTable({
    outputtab1()
  }, digits = 1)
  
  ################################################################################################
  # Save features to a geopackage
  ################################################################################################
  output$downloadData <- downloadHandler(
    filename = function() { paste("hydro_explorer-", Sys.Date(), ".gpkg", sep="") },
    content = function(file) {
        st_write(planreg_sf(), dsn=file, layer='planning_region')
        st_write(aoi_sf(), dsn=file, layer='aoi', append=TRUE)
        st_write(foot_sf(), dsn=file, layer='footprint', append=TRUE)
        st_write(intact_sf(), dsn=file, layer='intactness', append=TRUE)
        st_write(catch_att(), dsn=file, layer='catchments', append=TRUE)
      }
  )
  
}

shinyApp(ui, server)
