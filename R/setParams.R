setParamsServer <- function(input, output, session, project, map, rv){
  
  preview_ready <- reactiveVal(FALSE)
  
  observeEvent(input$selectsource, {
    preview_ready(FALSE)
  })
  
  observeEvent(input$upload_sa, {
    preview_ready(FALSE)
  })
  observeEvent(input$previewLayers, {
    preview_ready(TRUE)
  })
  ################################################################################################
  # Required layers
  required_layers <- c("catchments", "streams", "studyarea", "analysis studyarea")
  
  ####################################################################################################
  # RENDER UI
  output$include_layers_ui <- renderUI({
    req(input$sa_gpkg)
    
    file <- input$sa_gpkg$datapath
    layers <- tryCatch(
      st_layers(file)$name,
      error = function(e) NULL
    )
    req(layers)
    
    has_distexplo <- any(layers %in% distexplo_lyr)
    
    if (has_distexplo) {
      div(style = "margin-top: -30px;", checkboxInput( "include_layers",  "Load Disturbance Explorer layers", value = FALSE))
    }
  })
  
  output$sa_upstream_ui <- renderUI({
    req(upstream_extent())
    
    tagList(radioButtons("upsa_included", "Select analysis area:",
                 choices = list("Use uploaded study area only" = "sa_only", 
                                "Use uploaded study area and all upstream watershed" = "sa_up"),
                 selected = "sa_only", 
                 inline = FALSE),
    actionButton("apply_changes", "Set analysis study area", icon = icon(name = "map-location-dot", lib = "font-awesome"), class = "btn-warning", style="width:250px"),
    )
  }) 
  ####################################################################################################
  # READ SPATIAL DATA
  # Reactive function to validate the input file
  validate_csv <- reactive({
    req(input$csv_paths)  # Ensure the file input is not NULL
    # Read the uploaded CSV file
    csv_data <- read.csv(input$csv_paths$datapath)
    
    # Find missing layers
    missing_layers <- setdiff(required_layers, csv_data$Layer)
    if (length(missing_layers) > 0) {
      showModal(modalDialog(
        title = "Missing Layers",
        paste("The uploaded CSV is missing the following layers:",
              paste(missing_layers, collapse = ", "),
              ". Please fix and re-upload."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(FALSE)  # Stop further execution
    } else {
      # Return validated data if all checks pass
      return(TRUE)
    }
  })
  
  ##############################################################################
  # Observe on layers names in gpkg
  lyr_names <- reactive({
    
    if (input$selectsource == "usedemo"){
      file <- 'www/demo.gpkg'
    } else if (isTRUE(input$include_layers)){
        file <- input$sa_gpkg$datapath
        ext <- tools::file_ext(file)
        if (ext == "gpkg") {
          layers <- st_layers(file)$name
          return(layers)
        }
    } else{
      return(NULL)
    }
    layers <- st_layers(file)$name
  })
  
  ################################################################################################
  # Observe on selectInput
  output$gpkgReady <- reactive({
    !is.null(input$advanced_gpkg)
  })
  outputOptions(output, "gpkgReady", suspendWhenHidden = FALSE)
  
  observe({
    req(!is.null(input$sa_gpkg))
    layers <- st_layers(input$sa_gpkg$datapath)$name
    updateSelectInput(session = getDefaultReactiveDomain(), "sa_layer", choices = layers, selected= if ("studyarea" %in% layers) "studyarea" else layers[1])
  })
  
  observe({
    req(!is.null(input$advanced_gpkg))
    layers <- st_layers(input$advanced_gpkg$datapath)$name
    updateSelectInput(session = getDefaultReactiveDomain(), "advanced_salyr", choices = layers, selected= if ("studyarea" %in% layers) "studyarea" else layers[1])
    updateSelectInput(session = getDefaultReactiveDomain(), "advanced_catchlyr", choices = layers, selected= if ("catchments" %in% layers) "catchments" else layers[1])
    updateSelectInput(session = getDefaultReactiveDomain(), "advanced_streamslyr", choices = layers, selected= if ("streams" %in% layers) "streams" else layers[1])
  })
  
  ################################################################################################
  # Set studyarea
  sa_sf <- reactive({
    req(input$selectsource)
    req(input$previewLayers)

    if(input$selectsource == "usedemo"){
      i<- st_read("www/demo.gpkg", 'studyarea', quiet=T) %>% st_zm(drop = TRUE, what = "ZM")
    } else if (!is.null(input$csv_paths)) {
      req(validate_csv())
      i <- read_shp_from_csv(input$csv_paths, "studyarea")
    } else if (!is.null(input$advanced_sa)) {
      i <- read_shp_from_upload(input$advanced_sa)  %>% st_zm(drop = TRUE, what = "ZM")
    }else if (!is.null(input$advanced_gpkg) && !is.null(input$advanced_salyr)){
      req(input$advanced_salyr != "")
      i <- st_read(input$advanced_gpkg$datapath, input$advanced_salyr, quiet = TRUE)  %>% st_zm(drop = TRUE, what = "ZM")
    } else if (!is.null(input$sa_gpkg) && !is.null(input$sa_layer)){
      req(input$sa_gpkg != "")
      i <- st_read(input$sa_gpkg$datapath, input$sa_layer, quiet = TRUE)  %>% st_zm(drop = TRUE, what = "ZM")
    }else if (!is.null(input$upload_sashp)){
      req(input$upload_sashp != "")
      i <- read_shp_from_upload(input$upload_sashp)  %>% st_zm(drop = TRUE, what = "ZM")
    }else {
      i <- NULL
    }
    
    rv$layers_rv$sa_sf <- i
    preview_ready(TRUE)
    
    return(i) 
  })  
  
  # Set analysis area
  planreg_sf <- reactive({
    req(input$selectsource)
    req(input$previewLayers)
    
    if(input$selectsource == "usedemo"){
      i<- st_read("www/demo.gpkg", 'studyarea', quiet=T) %>% st_zm(drop = TRUE, what = "ZM")
    # Provided by user
    }else if (!is.null(input$csv_paths)) {
      req(validate_csv())
      i <- read_shp_from_csv(input$csv_paths, "analysis studyarea")
    }else if (!is.null(input$advanced_planreg)) {
      i <- read_shp_from_upload(input$advanced_planreg)  %>% st_zm(drop = TRUE, what = "ZM")
    }else if (!is.null(input$advanced_gpkg) && !is.null(input$advanced_planreglyr)){
      req(input$advanced_planreglyr != "")
      i <- st_read(input$advanced_gpkg$datapath, input$advanced_planreglyr, quiet = TRUE)  %>% st_zm(drop = TRUE, what = "ZM")
    }else if(input$upload_sa == "sa"){
      req(input$upsa_included)
      if(isTRUE(input$upsa_included == "sa_up")){
        i <- st_union(rv$layers_rv$sa_sf, rv$upstream_extent())
      } else{
      #}else if (isTRUE(input$upsa_included == "sa_only")){
        i <-  rv$layers_rv$sa_sf
      } #else{
        #i <- NULL
      #}
    }else{
      i <- NULL
    }
    if(!is.null(i)){
      if (st_crs(i) != st_crs(catchments())) {
        i <- st_transform(i, st_crs(catchments()))
      }
    }
    rv$layers_rv$planreg_sf <- i
    return(i) 
  })
  
  # Set streams
  stream_sf <- reactive({
    req(input$selectsource)
    req(input$previewLayers)
    req(catchments())
    
    if(input$selectsource == "usedemo"){
      stream <- st_read("www/demo.gpkg", 'streams', quiet=T)
    } else if (!is.null(input$csv_paths)) {
      req(validate_csv())
      stream <- read_shp_from_csv(input$csv_paths, "streams")
    } else if (!is.null(input$upload_stream)) {
      stream <- read_shp_from_upload(input$upload_stream)
    }else if (!is.null(input$upload_sashp)){
      req(sa_sf())
      req(rv$layers_rv$catchments)
      showModal(modalDialog(
        title = "Extracting streams",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      stream <- extractStreams(rv$layers_rv$catchments, rv$layers_rv$sa_sf)
      removeModal()
    }else if (!is.null(input$advanced_gpkg)  && !is.null(input$streams_layer)){
      req(input$streams_layer != "")
      stream <- st_read(input$advanced_gpkg$datapath, input$streams_layer, quiet = TRUE)
    }else if (!is.null(input$sa_layer)){
      req(sa_sf())
      showModal(modalDialog(
        title = "Extracting streams",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      stream <- extractStreams(rv$layers_rv$catchments, rv$layers_rv$sa_sf)
      removeModal()
    }else {
      stream <- NULL
    }
    if(!is.null(stream)){
      if(is.null(stream$geometry)){stream$geometry <- stream$geom}
      coords_df <- get_start_end(stream$geometry)
      stream <- bind_cols(stream, coords_df)
    }
    req(stream)
    required_col <- check_colnames(stream, c("SKELUID"))
    if(!is.na(required_col)){
      showModal(modalDialog(
        title = "Missing required column",
        paste0("In the stream layers, column ", required_col, " is missing."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(FALSE)
    }
    req(is.na(required_col))
    
    rv$layers_rv$streams_sf <- stream
    return(stream)
  })
  # Set catchments
  catchments <- reactive({
    req(input$selectsource)
    req(input$previewLayers)

    if(input$selectsource == "usedemo"){
      i <- st_read("www/demo.gpkg", 'catchments', quiet=T)
    } else if (!is.null(input$csv_paths)) {
      req(validate_csv())
      i <- read_shp_from_csv(input$csv_paths, "catchments")
    } else if (!is.null(input$advanced_catchshp)) {
      i <- read_shp_from_upload(input$advanced_catchshp)
    }else if (!is.null(input$advanced_gpkg) && !is.null(input$advanced_catchlyr)){
      req(input$advanced_catchlyr != "" && input$advanced_catchlyr)
      i <- st_read(input$adances_gpkg$datapath, input$advanced_catchlyr, quiet = TRUE)
    }else if (!is.null(input$upload_sashp)){
      req(sa_sf())
      showModal(modalDialog(
        title = "Extracting catchments",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      i <- extractCatchments(sa_sf()) %>%
        sf::st_collection_extract("POLYGON") %>%
        sf::st_cast("MULTIPOLYGON")
      removeModal()
    }else if (!is.null(input$sa_layer)){
      req(sa_sf())
      showModal(modalDialog(
        title = "Extracting catchments",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      i <- extractCatchments(sa_sf()) %>%
        sf::st_collection_extract("POLYGON") %>%
        sf::st_cast("MULTIPOLYGON")
      removeModal()
    }else {
      i <- NULL
    }

    required_col <- check_colnames(i, c("Area_land", "Area_water","Area_total", "CATCHNUM", "ORDER1", "ORDER2", "ORDER3", "BASIN", "SKELUID"))
    if(!is.na(required_col)){
      showModal(modalDialog(
        title = "Missing required column",
        paste0("In the catchments layers, column(s) ", required_col, " is/are missing."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(FALSE)
    }
    req(is.na(required_col))
    geom_idx <- which(names(i) == attr(i, "sf_column"))
    names(i)[geom_idx] <- "geom"
    st_geometry(i) <- "geom"
  
    rv$layers_rv$catchments <- i
    return(i)
  })
  
  upstream_extent <- reactive({
    req(catchments())
    req(stream_sf())
    req(input$upload_sa == "sa")
    
    if (!is.null(input$upload_sashp) || !is.null(input$sa_layer)){
      
      catchnums <- catchments() |>
        dplyr::filter(SKELUID %in% unique(stream_sf()$SKELUID)) |>
        dplyr::pull(CATCHNUM)
      
      upList <- getAggregationUpstreamCatchments_R(catch_att, catchnums)
      upList <- c(upList, catchments()$CATCHNUM)
      if(length(upList>0)){
        cloudcatch <-  catch_data()
        catch_up <- cloudcatch %>%
          dplyr::filter(CATCHNUM %in% upList) 
          
        catch_up <- sf::st_difference(
          catch_up,
          sf::st_union(catchments())
        )
      }else{
        catch_up <- NULL
      }
      
      rv$upstream_catch(catch_up)
      
      if(!is.null(catch_up)){
        dslv <- catch_up %>%
          dplyr::summarise(geometry = sf::st_union(geometry)) %>%
          st_transform(st_crs(rv$layers_rv$sa_sf)) %>%
          dplyr::mutate(up_sa_area_km2 = round(as.numeric(sf::st_area(geometry) / 1000000), 2))
        
      }else{
        dslv <- NULL
      }
      rv$upstream_extent(dslv)
      return(dslv)
    }else {
      rv$upstream_extent(NULL)
      return(NULL)
    }
  })
  
  include_saup <- reactive(input$upsa_included == "sa_up")
  
  ################################################################################################
  ## distExplo output
  observeEvent(input$previewLayers,{
    req(planreg_sf())
    req(lyr_names())
    
    # show pop-up ...
    showModal(modalDialog(
      title = "Uploading geopackage layers. Please wait...",
      easyClose = TRUE,
      footer = NULL)
    )
    
    if(input$selectsource == 'usedemo'){
      gpkg_path <- 'www/demo.gpkg'
    }else{
      gpkg_path <- input$sa_gpkg$datapath
    }
    
    if ("fires" %in% lyr_names()) {
      fi <-st_read(gpkg_path, 'fires', quiet = TRUE) %>% 
        st_transform(st_crs(planreg_sf())) %>%  
        st_intersection(st_make_valid(planreg_sf())) %>%
        dplyr::select(all_of(names(st_read(gpkg_path, "fires", quiet = TRUE)))) %>%
        suppressWarnings() %>%
        st_cast('MULTIPOLYGON') %>% 
        st_zm(drop = TRUE, what = "ZM")  %>%
        mutate(area_ha = as.numeric(st_area(geom)/10000))
      rv$layers_rv$fires <- fi
    }
    
    undist_layer <- if ("undisturbed" %in% lyr_names()) {
      "undisturbed"
    } else if ("undisturbed_areas_500m" %in% lyr_names()) {
      "undisturbed_areas_500m"
    } else {
      NULL
    }
    if (!is.null(undist_layer)) {
      la <-st_read(gpkg_path, undist_layer, quiet = TRUE) %>% 
        st_transform(st_crs(planreg_sf())) %>%  
        st_intersection(st_make_valid(planreg_sf())) #%>%
       # dplyr::select(all_of(names(st_read(gpkg_path, undist_layer, quiet = TRUE))))
      rv$layers_rv$undisturbed <- la
    }
    dist_layer <- if ("disturbed" %in% lyr_names()) {
      "disturbed"
    } else if ("footprint_500m" %in% lyr_names()) {
      "footprint_500m"
    } else {
      NULL
    }
    if (!is.null(dist_layer)) {
      la <-st_read(gpkg_path, dist_layer, quiet = TRUE) %>% 
        st_transform(st_crs(planreg_sf())) %>%  
        st_intersection(st_make_valid(planreg_sf())) #%>%
      # dplyr::select(all_of(names(st_read(gpkg_path, undist_layer, quiet = TRUE))))
      rv$layers_rv$disturbed <- la
    }
    if ("intact_fl_2000" %in% lyr_names()) {
      la <-st_read(gpkg_path, 'intact_fl_2000', quiet = TRUE) %>% 
        st_transform(st_crs(planreg_sf())) %>%  
        st_intersection(st_make_valid(planreg_sf())) #%>%
        #dplyr::select(all_of(names(st_read(gpkg_path, "intact_fl_2000", quiet = TRUE))))      
      rv$layers_rv$ifl2000 <- la
    }
    if ("intact_fl_2020" %in% lyr_names()) {
      la <-st_read(gpkg_path, 'intact_fl_2020', quiet = TRUE) %>% 
        st_transform(st_crs(planreg_sf())) %>%  
        st_intersection(st_make_valid(planreg_sf())) #%>%
        #dplyr::select(all_of(names(st_read(gpkg_path, "intact_fl_2020", quiet = TRUE)))) 
      rv$layers_rv$ifl2020 <- la
    }
    if ("protected_areas" %in% lyr_names()) {
      la <-st_read(gpkg_path, 'protected_areas', quiet = TRUE) %>% 
        st_transform(st_crs(planreg_sf())) %>%  
        st_intersection(st_make_valid(planreg_sf())) #%>%
        #dplyr::select(all_of(names(st_read(gpkg_path, "protected_areas", quiet = TRUE)))) 
      rv$layers_rv$pa2021 <- la
    }
    if ("placer_claims" %in% lyr_names()) {
      la <-st_read(gpkg_path, 'placer_claims', quiet = TRUE) %>% 
        st_transform(st_crs(planreg_sf())) %>%  
        st_intersection(st_make_valid(planreg_sf())) #%>%
        #dplyr::select(all_of(names(st_read(gpkg_path, "placer_claims", quiet = TRUE)))) 
      rv$layers_rv$placers <- la
    }
    if ("quartz_claims" %in% lyr_names()) {
      la <-st_read(gpkg_path, 'quartz_claims', quiet = TRUE) %>% 
        st_transform(st_crs(planreg_sf())) %>%  
        st_intersection(st_make_valid(planreg_sf())) #%>%
        #dplyr::select(all_of(names(st_read(gpkg_path, "quartz_claims", quiet = TRUE)))) 
      rv$layers_rv$quartz <- la
    }
    if ("mining_claims" %in% lyr_names()) {
      la <-st_read(gpkg_path, 'mining_claims', quiet = TRUE) %>% 
        st_transform(st_crs(planreg_sf())) %>%  
        st_intersection(st_make_valid(planreg_sf())) #%>%
        #dplyr::select(all_of(names(st_read(gpkg_path, "mining_claims", quiet = TRUE))))
      rv$layers_rv$mines <- la
    } 
    if ("disturbed" %in% lyr_names()) {
      la <-st_read(gpkg_path, 'disturbed', quiet = TRUE) %>% 
        st_transform(st_crs(planreg_sf())) %>%  
        suppressWarnings(st_cast('MULTIPOLYGON')) %>% 
        st_zm(drop = TRUE, what = "ZM")  %>%
        st_make_valid() %>%
        mutate(area_ha = as.numeric(st_area(geom)/10000))
      rv$layers_rv$disturbed <- la
    }
  }, ignoreInit = TRUE)
  
  ####################################################################################################
  # Map viewer - Set input parameters
  ####################################################################################################
  # Render planning region
  observeEvent(input$previewLayers, {
    # show pop-up ...
    showModal(modalDialog(
      title = "Please wait.", " Layers are being uploaded.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
    req(sa_sf())
    req(catchments())
    req(stream_sf())
    
    grps <- rv$grps
    group_names_new <- c()
    
    sa_sf <- st_transform(rv$layers_rv$sa_sf, 4326)
    stream_4326 <- st_transform(rv$layers_rv$streams_sf, 4326)
    catch_4326 <- st_transform(rv$layers_rv$catchments, 4326)
    mda_4326 <- mda_data() %>% st_transform(4326)
    legend <- c("Study area", "Streams", "Catchments", "MDA")
    rv$overlayBase(legend)
    map_bounds <- sa_sf %>% st_bbox() %>% as.character()
    
    leafletProxy("map") %>% 
      clearGroup('Study area') %>%
      clearGroup('Analysis study area') %>%
      clearGroup('Catchments') %>%
      clearGroup('Streams') %>%
      clearGroup('Undisturbed') %>%
      clearGroup("Intact FL 2000") %>%
      clearGroup("Intact FL 2020") %>%
      clearGroup('Fires') %>%
      clearGroup('Placer claims') %>%
      clearGroup('Quartz claims') %>%
      clearGroup('Protected areas') %>%
      clearGroup('Disturbed') %>%
      clearGroup('Mining claims') %>%
      clearGroup('Study area - upstream area') %>%
      clearGroup(rv$display1_name) %>%
      clearGroup(rv$display2_name) %>%
      clearGroup(rv$display3_name) %>%
      fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) %>% # set view to the selected FDA
      addPolylines(data=stream_4326, color='#0066FF', weight=1.2, group="Streams", options = leafletOptions(pane = "ground")) %>%
      addPolygons(data=mda_4326, color='black', fillColor = "", fillOpacity = 0, weight=2, group="MDA", options = leafletOptions(pane = "ground")) %>%
      addPolygons(data=sa_sf, color='#663399', fillColor = "", fillOpacity = 0, weight=3, group="Study area", options = leafletOptions(pane = "ground")) %>%
      addPolygons(data=catch_4326, color='black', fillColor = "grey", fillOpacity = 0.4, weight=1, group="Catchments", options = leafletOptions(pane = "over")) 

    if(!is.null(planreg_sf())){
      planreg_sf <- st_transform(rv$layers_rv$planreg_sf, 4326)
      leafletProxy("map") %>% addPolygons(data=planreg_sf, color='purple', fillColor = "", fillOpacity = 0, weight=3, group="Analysis study area", options = leafletOptions(pane = "ground"))
      legend <- c("Study area", "Analysis study area", "Streams", "Catchments", "MDA")
    }    

    # Optional
    disturbed <- isolate(rv$layers_rv$disturbed)
    if(!is.null(disturbed)){
      disturbed <- st_transform(disturbed, 4326)
      leafletProxy("map") %>% addPolygons(data=disturbed, color='black', stroke=F, fillOpacity=0.5, group="Disturbed", options = leafletOptions(pane = "ground")) 
      group_names_new <- c(group_names_new, "Disturbed")
    }
    undisturbed <- isolate(rv$layers_rv$undisturbed)
    if(!is.null(undisturbed)){
      undisturbed <- st_transform(undisturbed, 4326)
      leafletProxy("map") %>% addPolygons(data=undisturbed, color='#336633', stroke=F, fillOpacity=0.5, group="Undisturbed", options = leafletOptions(pane = "ground")) 
      group_names_new <- c(group_names_new, "Undisturbed")
    }
    fires <- isolate(rv$layers_rv$fires)
    if(!is.null(fires)){
      fires <- st_transform(fires, 4326)
      leafletProxy("map") %>% addPolygons(data=fires, fill=T, stroke=F, fillColor="#996633", fillOpacity=0.8, group="Fires", options = leafletOptions(pane = "ground")) 
      group_names_new <- c(group_names_new, "Fires")
    }
    ifl2000 <- isolate(rv$layers_rv$ifl2000)
    if(!is.null(ifl2000)){
      ifl2000 <- st_transform(ifl2000, 4326)
      leafletProxy("map") %>% addPolygons(data=ifl2000, fill=T, stroke=F, fillColor='#3366FF', fillOpacity=0.5, group="Intact FL 2000", options = leafletOptions(pane = "ground")) 
      group_names_new <- c(group_names_new, "Intact FL 2000")
    }
    ifl2020 <- isolate(rv$layers_rv$ifl2020)
    if(!is.null(ifl2020)){
      ifl2020 <- st_transform(ifl2020, 4326)
      leafletProxy("map") %>% addPolygons(data=ifl2020, fill=T, stroke=F, fillColor='#000066', fillOpacity=0.5, group="Intact FL 2020", options = leafletOptions(pane = "ground")) 
      group_names_new <- c(group_names_new, "Intact FL 2020")
    }
    pa2021 <- isolate(rv$layers_rv$pa2021)
    if(!is.null(pa2021)){
      pa2021 <- st_transform(pa2021, 4326)
      leafletProxy("map") %>% addPolygons(data=pa2021, fill=T, stroke=F, fillColor='#699999', fillOpacity=1,  group="Protected areas", options = leafletOptions(pane = "ground")) 
      group_names_new <- c(group_names_new, "Protected areas")
    }
    placers <- isolate(rv$layers_rv$placers)
    if(!is.null(placers)){
      placers <- st_transform(placers, 4326)
      leafletProxy("map") %>% addPolygons(data=placers, color= '#666666', fill=T, fillColor='#666666', weight=1, fillOpacity = 1, group="Placer claims", options = leafletOptions(pane = "ground")) 
      group_names_new <- c(group_names_new, "Placer claims")
    }
    quartz <- isolate(rv$layers_rv$quartz)
    if(!is.null(quartz)){
      quartz <- st_transform(quartz, 4326)
      leafletProxy("map") %>% addPolygons(data=quartz, color = '#CCCCCC', fill=T, fillColor='#CCCCCC', weight=1, fillOpacity = 1, group="Quartz claims", options = leafletOptions(pane = "ground")) 
      group_names_new <- c(group_names_new, "Quartz claims")
    }
    mines <- isolate(rv$layers_rv$mines)
    if(!is.null(mines)){
      mines <- st_transform(mines, 4326)
      leafletProxy("map") %>% addPolygons(data=mines, color='#666666', fill=T, fillColor='#666666', weight=1, fillOpacity = 1, group="Mining claims", options = leafletOptions(pane = "ground")) 
      group_names_new <- c(group_names_new, "Mining claims")
    } 
    
    if(isTRUE(input$upload_sa == "sa")){
      up_sa <- isolate(upstream_extent())
      if(!is.null(up_sa)){
        up_sa <- st_transform(up_sa, 4326)
        leafletProxy("map") %>% addPolygons(data=up_sa, color='#330066', stroke=F, fillOpacity=0.5, group="Study area - upstream area", options = leafletOptions(pane = "ground")) 
        group_names_new <- c(group_names_new, "Study area - upstream area")
      }
    }
    leafletProxy("map") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                       overlayGroups = c(legend, group_names_new),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Streams", "Catchments", group_names_new))
    
    # Close the modal once processing is done
    rv$group_names(group_names_new)
    removeModal()
    
  })
  
  observeEvent(input$apply_changes, {
    # show pop-up ...
    showModal(modalDialog(
      title = "Please wait.", " Layers are being uploaded.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
    if(!is.null(rv$layers_rv$planreg_sf)){
      planreg <- rv$layers_rv$planreg_sf
    }else if(input$upsa_included == "sa_up"){
      sf1 <- st_as_sf(rv$upstream_catch())
      sf2 <- st_as_sf(rv$layers_rv$catchments)
      
      if("geom" %in% names(sf2)){
        sf2 <- sf2 %>%
          dplyr::rename(geometry = geom)
      }
      if("geom" %in% names(sf1)){
        sf1 <- sf1 %>%
          dplyr::rename(geometry = geom)
      }
      
      catch <- dplyr::bind_rows(sf1, sf2)
      rv$layers_rv$catchments <- catch
      
      planreg <- catch %>%
        dplyr::summarise(geometry = sf::st_union(geometry)) %>%
        st_buffer(dist = 20) %>% 
        st_buffer(dist = -20)
      rv$layers_rv$planreg_sf <- planreg
      
      stream <- extractStreams(catch, planreg)
      rv$layers_rv$streams_sf <- stream
    } else{
      planreg <- rv$layers_rv$sa_sf
      rv$layers_rv$planreg_sf <- planreg
    }
    
    planreg_sf <- planreg %>% st_transform(4326)
    stream_4326 <- st_transform(rv$layers_rv$streams_sf, 4326)
    catch_4326 <- st_transform(rv$layers_rv$catchments, 4326)
    
    legend <- c("Study area", "Analysis study area", "Streams", "Catchments", "MDA")
    rv$group_names(setdiff(rv$group_names(), "Study area - upstream area"))
    
    leafletProxy("map") %>% 
      clearGroup('Catchments') %>%
      clearGroup('Streams') %>%
      clearGroup("Study area - upstream area") %>%
      addPolylines(data=stream_4326, color='#0066FF', weight=1.2, group="Streams", options = leafletOptions(pane = "ground")) %>%
      addPolygons(data=planreg_sf, color='purple', fillColor = "", fillOpacity = 0, weight=3, group="Analysis study area", options = leafletOptions(pane = "ground")) %>%
      addPolygons(data=catch_4326, color='black', fillColor = "grey", fillOpacity = 0, weight=1, group="Catchments", options = leafletOptions(pane = "over")) %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                       overlayGroups = c(legend, rv$group_names()),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Streams", "Catchments", rv$group_names()))
    
    removeModal()
  })
  ##############################################
  ##  Stats
  ##############################################
  
  #Update with Study area
  observeEvent(input$previewLayers, {
    
    req(rv$layers_rv$sa_sf)
    x <- tibble(Variables=c("Study area", "Analysis study area"), 
                Area_km2= NA_real_,
                Percent = NA_real_)
    
    x <- x %>% 
      mutate(Area_km2 = case_when(Variables == "Study area" ~  round(as.numeric(st_area(rv$layers_rv$sa_sf)/1000000,0))),
             Percent= case_when(Variables == "Study area" ~  100))
    
    if(input$selectsource == "usedemo"){
      x <- x %>% 
        mutate(Area_km2 = case_when(Variables == "Analysis study area" ~  round(as.numeric(st_area(rv$layers_rv$sa_sf)/1000000,0)),
                                    TRUE ~ Area_km2),
               Percent= case_when(Variables == "Analysis study area" ~  100,
                                  TRUE ~ Percent))
    }else if(input$selectsource == "usedata" && input$upload_sa == 'sa_advanced'){
      req(!is.null(rv$layers_rv$planreg_sf))
      x <- x %>% 
        mutate(Area_km2 = case_when(Variables == "Analysis study area" ~  round(as.numeric(st_area(rv$layers_rv$planreg_sf)/1000000,0)),
                                    TRUE ~ Area_km2),
               Percent= case_when(Variables == "Analysis study area" ~  100,
                                  TRUE ~ Percent))
    }
    
    rv$outAOI(x)
    rv$outtab1(x)
    
    rv$outputsumStats(x)
    
    y <- tibble(
      Variables = "No feature",
      Area_km2 = NA_real_,
      Percent = NA_real_
    )
    
    rv$outfeaturetab(y)
  },
  ignoreInit = TRUE)

  observeEvent(input$apply_changes, {
    req(planreg_sf())
    x <- rv$outAOI()
    
    new_rows  <- tibble(Variables="Analysis study area", 
                        Area_km2= NA_real_,
                        Percent = NA_real_)
    
    x <- x %>% dplyr::filter(!Variables %in% new_rows$Variables)
    x <- dplyr::bind_rows(x, new_rows)
    
    x <- x %>% 
      mutate(Area_km2 = case_when(Variables == "Analysis study area" ~  round(as.numeric(st_area(rv$layers_rv$planreg_sf)/1000000,0)), 
                                  TRUE ~ Area_km2),
             Percent= case_when(Variables == "Analysis study area" ~  100,
                                TRUE ~ Percent))
    rv$outAOI(x)
    rv$outtab1(x)
    
    rv$outputsumStats(x)
  },   ignoreInit = TRUE)  
  
  
  output$dynamicTabs <- renderUI({
    tabs <- list()
    # Feature tab
    tabs[[1]] <- tabPanel(HTML("<h4>Feature statistics</h4>"), textOutput("featureNameTxt"), tableOutput("tabFeature"))
    # DCI tab 
    if (!is.null(input$confAOI) && input$confAOI > 0) {
      tabs[[length(tabs) + 1]] <- tabPanel(HTML("<h4>Dendritic Connectivity Index (DCI)</h4>"), tableOutput("tabDCI"))
    }
    do.call(tabBox, c(list(id = "metric", width = NULL), tabs))  
  })
}