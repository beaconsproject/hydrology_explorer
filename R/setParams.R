setParamsServer <- function(input, output, session, project, map, rv){
  
  
  ################################################################################################
  # Function to add a new group to group_names
  addGroup <- function(new_group) {
    current_groups <- rv$group_names()  # Retrieve the current value
    if (!(new_group %in% current_groups)) {  # Check if the group is not already in the vector
      updated_groups <- c(current_groups, new_group)  # Append the new group
      rv$group_names(updated_groups)  # Update the reactiveVal
    }
  }
  
  removeGroup <- function(group_to_remove) {
    current_groups <- rv$group_names()  # Retrieve the current value
    updated_groups <- current_groups[current_groups != group_to_remove]  # Filter out the specified group
    rv$group_names(updated_groups)  # Update the reactiveVal
  }
  
  # Required layers
  required_layers <- c("catchments", "streams", "studyarea")
  
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
  lyr_names <- eventReactive(input$selectsource, {
    if (input$selectsource == "usedemo"){
      file <- 'www/demo.gpkg'
    }else if (!is.null(input$upload_gpkg)) {
      file <- input$upload_gpkg$datapath
      ext <- tools::file_ext(file)
      if (ext == "gpkg") {
        layers <- st_layers(file)$name
        return(layers)
      }
    }else{
      return(NULL)
    }
    layers <- st_layers(file)$name
  })
  
  ################################################################################################
  # Observe on selectInput
  output$gpkgReady <- reactive({
    !is.null(input$gpkg_file)
  })
  outputOptions(output, "gpkgReady", suspendWhenHidden = FALSE)
  
  observe({
    req(!is.null(input$gpkg_file))
    layers <- st_layers(input$gpkg_file$datapath)$name
    updateSelectInput(session = getDefaultReactiveDomain(), "sa_layer", choices = layers, selected= if ("studyarea" %in% layers) "studyarea" else layers[1])
    updateSelectInput(session = getDefaultReactiveDomain(), "catch_layer", choices = layers, selected= if ("catchments" %in% layers) "catchments" else layers[1])
    updateSelectInput(session = getDefaultReactiveDomain(), "streams_layer", choices = layers, selected= if ("streams" %in% layers) "streams" else layers[1])
  })
  
 
  observe({
    req(input$display4)
    file <- input$display4$datapath
    layers <- st_layers(file)$name
    updateSelectInput(session = getDefaultReactiveDomain(), "display4a", choices = c("Select a layer", layers))
  })
  observe({
    req(input$display4)
    file <- input$display4$datapath
    layers <- st_layers(file)$name
    updateSelectInput(session = getDefaultReactiveDomain(), "display4b", choices = c("Select a layer", layers))
  })
  observe({
    req(input$display4)
    file <- input$display4$datapath
    layers <- st_layers(file)$name
    updateSelectInput(session = getDefaultReactiveDomain(), "display4c", choices = c("Select a layer", layers))
  })
  observe({
    req(input$upload_aoi)
    req(input$sourceAOI == 'gpkgAOI')
    file <- input$upload_aoi$datapath
    layers <- st_layers(file)$name
    updateSelectInput(session = getDefaultReactiveDomain(), "aoiLayer", choices = c("Select AOI layer", layers))
  })
  
  
  ################################################################################################
  # Observe on tabs
  observe({
    req(input$tabs == "tabIntact")  # Trigger when "Set intactness" 
    
    # Check if input is unset or NULL
    if (input$previewLayers == 0) {
      showModal(modalDialog(
        title = "Missing input parameters",
        "Please set input parameters prior to set intactness and fires.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  
  observe({
    req(input$tabs == "addLayers")  # Trigger when "Select AOI" is active
    
    # Check if intactSource is unset or NULL
    if (input$previewLayers == 0) {
      showModal(modalDialog(
        title = "Missing input parameters",
        "Please set input parameters prior to upload additional elements.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  
  observe({
    req(input$tabs == "selectAOI")  # Trigger when "Select AOI" is active
    
    # Check if intactSource is unset or NULL
    if (is.null(input$intactSource) || input$intactSource == "") {
      showModal(modalDialog(
        title = "Missing input parameters",
        "Please set the Intactness Source before selecting an Area of Interest (AOI).",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  
  observe({
    req(input$tabs == "upstream")  # Trigger when "Select AOI" is active
    
    # Check if intactSource is unset or NULL
    if (input$confAOI ==0 || input$confIntact ==0 || input$previewLayers == 0) {
      showModal(modalDialog(
        title = "Missing input parameters",
        "Please confirm input parameters and Area of Interest (AOI) in previous step.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  
  ####################################################################################################
  # READ SPATIAL DATA
  # Set studyarea
  planreg_sf <- reactive({
    req(input$selectsource)
    req(input$previewLayers)
    
    if(input$selectsource == "usedemo"){
      i<- st_read("www/demo.gpkg", 'studyarea', quiet=T) %>% st_zm(drop = TRUE, what = "ZM")
    } else if (!is.null(input$csv_paths)) {
      req(validate_csv())
      i <- read_shp_from_csv(input$csv_paths, "studyarea")
    } else if (!is.null(input$upload_sa)) {
      i <- read_shp_from_upload(input$upload_sa)  %>% st_zm(drop = TRUE, what = "ZM")
    }else if (!is.null(input$gpkg_file) && !is.null(input$sa_layer)){
      req(input$sa_layer != "")
      i <- st_read(input$gpkg_file$datapath, input$sa_layer, quiet = TRUE)  %>% st_zm(drop = TRUE, what = "ZM")
    } else {
      i <- NULL
    }
    rv$layers_rv$planreg_sf <- i
    return(i) 
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
    } else if (!is.null(input$upload_catch)) {
      i <- read_shp_from_upload(input$upload_catch)
    }else if (!is.null(input$gpkg_file) && !is.null(input$catch_layer)){
      req(input$catch_layer != "")
      i <- st_read(input$gpkg_file$datapath, input$catch_layer, quiet = TRUE)
    }else {
      i <- NULL
    }
    rv$layers_rv$catchments <- i
    return(i)
  })
  
  # Set streams
  stream_sf <- reactive({
    req(input$selectsource)
    req(input$previewLayers)
    
    if(input$selectsource == "usedemo"){
      stream <- st_read("www/demo.gpkg", 'streams', quiet=T)
    } else if (!is.null(input$csv_paths)) {
      req(validate_csv())
      stream <- read_shp_from_csv(input$csv_paths, "streams")
    } else if (!is.null(input$upload_stream)) {
      stream <- read_shp_from_upload(input$upload_stream)
    }else if (!is.null(input$gpkg_file)  && !is.null(input$streams_layer)){
      req(input$streams_layer != "")
      stream <- st_read(input$gpkg_file$datapath, input$streams_layer, quiet = TRUE)
    }else {
      stream <- NULL
    }
    if(!is.null(stream)){
      if(is.null(stream$geometry)){stream$geometry <- stream$geom}
      coords_df <- get_start_end(stream$geometry)
      stream <- bind_cols(stream, coords_df)
    }
    rv$layers_rv$stream_sf <- stream
    return(stream)
  })
  
  ################################################################################################
  ## distExplo output
  observeEvent(input$previewLayers,{
    req(planreg_sf())
    req(lyr_names)
    
    # show pop-up ...
    showModal(modalDialog(
      title = "Uploading geopackage layers. Please wait...",
      easyClose = TRUE,
      footer = NULL)
    )
    
    if(input$selectsource == 'usedemo'){
      gpkg_path <- 'www/demo.gpkg'
    }else{
      req(input$upload_gpkg)
      gpkg_path <- file.path(tempdir(), paste0("uploaded_", input$upload_gpkg$name))
      file.copy(input$upload_gpkg$datapath, gpkg_path, overwrite = TRUE)
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
      addGroup("Fires")
      rv$layers_rv$fires <- fi
    }
    if ("undisturbed" %in% lyr_names()) {
      la <-st_read(gpkg_path, 'undisturbed', quiet = TRUE) %>% 
        st_transform(st_crs(planreg_sf())) %>%  
        st_intersection(st_make_valid(planreg_sf())) %>%
        dplyr::select(all_of(names(st_read(gpkg_path, "undisturbed", quiet = TRUE))))
      addGroup("Undisturbed areas")
      rv$layers_rv$undisturbed <- la
    }
    if ("Intact_FL_2000" %in% lyr_names()) {
      la <-st_read(gpkg_path, 'Intact_FL_2000', quiet = TRUE) %>% 
        st_transform(st_crs(planreg_sf())) %>%  
        st_intersection(st_make_valid(planreg_sf())) %>%
        dplyr::select(all_of(names(st_read(gpkg_path, "Intact_FL_2000", quiet = TRUE))))      
      addGroup("Intact FL 2000")
      rv$layers_rv$ifl2000 <- la
    }
    if ("Intact_FL_2020" %in% lyr_names()) {
      la <-st_read(gpkg_path, 'Intact_FL_2020', quiet = TRUE) %>% 
        st_transform(st_crs(planreg_sf())) %>%  
        st_intersection(st_make_valid(planreg_sf())) %>%
        dplyr::select(all_of(names(st_read(gpkg_path, "Intact_FL_2020", quiet = TRUE)))) 
      addGroup("Intact FL 2020")
      rv$layers_rv$ifl2020 <- la
    }
    if ("protected_areas" %in% lyr_names()) {
      la <-st_read(gpkg_path, 'protected_areas', quiet = TRUE) %>% 
        st_transform(st_crs(planreg_sf())) %>%  
        st_intersection(st_make_valid(planreg_sf())) %>%
        dplyr::select(all_of(names(st_read(gpkg_path, "protected_areas", quiet = TRUE)))) 
      addGroup("Protected areas")
      rv$layers_rv$pa2021 <- la
    }
    if ("Caribou_Herds" %in% lyr_names()) {
      la <-st_read(gpkg_path, 'Caribou_Herds', quiet = TRUE) %>%
        st_transform(st_crs(planreg_sf()))   
      addGroup("Caribou Herds")
      rv$layers_rv$herds <- la
    }
    if ("Placer_Claims" %in% lyr_names()) {
      la <-st_read(gpkg_path, 'Placer_Claims', quiet = TRUE) %>% 
        st_transform(st_crs(planreg_sf())) %>%  
        st_intersection(st_make_valid(planreg_sf())) %>%
        dplyr::select(all_of(names(st_read(gpkg_path, "Placer_Claims", quiet = TRUE)))) 
      addGroup("Placer claims")
      rv$layers_rv$placers <- la
    }
    if ("Quartz_Claims" %in% lyr_names()) {
      la <-st_read(gpkg_path, 'Quartz_Claims', quiet = TRUE) %>% 
        st_transform(st_crs(planreg_sf())) %>%  
        st_intersection(st_make_valid(planreg_sf())) %>%
        dplyr::select(all_of(names(st_read(gpkg_path, "Quartz_Claims", quiet = TRUE)))) 
      addGroup("Quartz claims")
      rv$layers_rv$quartz <- la
    }
    if ("Mining_Claims" %in% lyr_names()) {
      la <-st_read(gpkg_path, 'Mining_Claims', quiet = TRUE) %>% 
        st_transform(st_crs(planreg_sf())) %>%  
        st_intersection(st_make_valid(planreg_sf())) %>%
        dplyr::select(all_of(names(st_read(gpkg_path, "Mining_Claims", quiet = TRUE))))
      addGroup("Mining claims")
      rv$layers_rv$mines <- la
    } 
    if ("disturbed" %in% lyr_names()) {
      la <-st_read(gpkg_path, 'disturbed', quiet = TRUE) %>% 
        st_transform(st_crs(planreg_sf())) %>%  
        suppressWarnings(st_cast('MULTIPOLYGON')) %>% 
        st_zm(drop = TRUE, what = "ZM")  %>%
        st_make_valid() %>%
        mutate(area_ha = as.numeric(st_area(geom)/10000))
      addGroup("Disturbed areas")
      rv$layers_rv$disturbed <- la
    }
  }, ignoreInit = TRUE)
  
  ####################################################################################################
  # Map viewer - Set input parameters
  ####################################################################################################
  # Render planning region
  observeEvent(input$previewLayers, {
    req(planreg_sf())
    # show pop-up ...
    showModal(modalDialog(
      title = "Please wait.", " Layers are being uploaded.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    
    grps <- rv$grps
    group_names_new <- c()
    
    planreg_sf <- planreg_sf() %>% st_transform(4326)
    stream_4326 <- st_transform(stream_sf(), 4326)
    catch_4326 <- st_transform(catchments(), 4326)
    legend <- c("Study area", "Streams", "Catchments")
    rv$overlayBase(legend)
    map_bounds <- planreg_sf %>% st_bbox() %>% as.character()

    leafletProxy("map") %>% 
      clearGroup('Study area') %>%
      clearGroup('Catchments') %>%
      clearGroup('Streams') %>%
      clearGroup('Intactness') %>%
      clearGroup("Intact FL 2000") %>%
      clearGroup("Intact FL 2020") %>%
      clearGroup('Fires') %>%
      clearGroup('Placer claims') %>%
      clearGroup('Quartz claims') %>%
      clearGroup('Protected areas') %>%
      clearGroup('Disturbed areas') %>%
      clearGroup('Herds') %>%
      clearGroup(rv$display1_name) %>%
      clearGroup(rv$display2_name) %>%
      clearGroup(rv$display3_name) %>%
      fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) %>% # set view to the selected FDA
      addPolygons(data=planreg_sf, color='black', fillColor = "", fillOpacity = 0, weight=3, group="Study area", options = leafletOptions(pane = "ground")) %>%
      addPolylines(data=stream_4326, color='#0066FF', weight=1.2, group="Streams", options = leafletOptions(pane = "ground")) %>%
      addPolygons(data=catch_4326, color='black', fillColor = "grey", fillOpacity = 0, weight=1, group="Catchments", options = leafletOptions(pane = "over")) 
    
    # Optional
    
    disturbed <- isolate(rv$layers_rv$disturbed)
    if(!is.null(disturbed)){
      disturbed <- st_transform(disturbed, 4326)
      leafletProxy("map") %>% addPolygons(data=disturbed, color='black', stroke=F, fillOpacity=0.5, group="Disturbed areas", options = leafletOptions(pane = "ground")) 
      group_names_new <- c(group_names_new, "Disturbed areas")
    }
    undisturbed <- isolate(rv$layers_rv$undisturbed)
    if(!is.null(disturbed)){
      undisturbed <- st_transform(undisturbed, 4326)
      leafletProxy("map") %>% addPolygons(data=undisturbed, color='#336633', stroke=F, fillOpacity=0.5, group="Undisturbed areas", options = leafletOptions(pane = "ground")) 
      group_names_new <- c(group_names_new, "Undisturbed areas")
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
    herds <- isolate(rv$layers_rv$herds)
    if(!is.null(herds)){
      herds <- st_transform(herds, 4326)
      leafletProxy("map") %>% addPolygons(data=herds, color= '#666666', fill=T, fillColor='#666666', weight=1, fillOpacity = 1, group="Caribou Herds", options = leafletOptions(pane = "ground")) 
      group_names_new <- c(group_names_new, "Caribou Herds")
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
      leafletProxy("map") %>% addPolygons(data=mines, color='#666666', fill=T, fillColor='#666666', weight=1, fillOpacity = 1, group="Mining Claims", options = leafletOptions(pane = "ground")) 
      group_names_new <- c(group_names_new, "Mining claims")
    } 
    
    leafletProxy("map") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c(legend, group_names_new),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Streams", "Catchments", group_names_new))
    
    # Close the modal once processing is done
    rv$group_names(group_names_new)
    removeModal()
    
  })
  
  
  mines_all <- reactive({
    geoms <- list(quartz(), placers(), mines()) |>
      purrr::compact() |>                # removes NULLs
      purrr::map(sf::st_geometry)       # extract just the geometries
    
    # If no layers, return NULL
    if (length(geoms) == 0) return(NULL)
    
    # Combine geometries into one sf object
    sf::st_as_sf(sf::st_union(do.call(c, geoms)))
  })
  
  ##############################################
  ##  Stats
  ##############################################
  
  #Update with Study area
  observeEvent(input$previewLayers, {
    x <- tibble(Variables=c("Study area"), 
                Area_km2= NA_real_,
                Percent = NA_real_)
    x <- x %>% 
      mutate(Area_km2 = case_when(Variables == "Study area" ~  round(as.numeric(st_area(rv$layers_rv$planreg_sf)/1000000,0))),
             Percent= case_when(Variables == "Study area" ~  100))
    rv$outtab1(x)
    
    #Fire stat
    if(!is.null(rv$layers_rv$fires)){
      y <- tibble(Variables=c("Within study area"), 
                  Area_Burned_km2= NA_real_, 
                  'Area_Burned_%' = NA_real_)
      
      y <- y %>% 
        mutate(Area_Burned_km2 = case_when(Variables == "Within study area" ~  round(as.numeric(sum(st_area(rv$layers_rv$fires))/1000000,2))),
               'Area_Burned_%'= case_when(Variables == "Within study area" ~  round(as.numeric(sum(st_area(rv$layers_rv$fires))/st_area(rv$layers_rv$planreg_sf))*100))
        )
    }else{
      y <- tibble(
        Variables = "No fire",
        Area_Burned_km2 = NA_real_,
        `Area_Burned_%` = NA_real_
      )
    }
    rv$outfiretab(y)
    
    
    #Summary stat
    z <- tibble(Variables=c("Fires within the study area"), 
                Area_km2= y$Area_Burned_km2, 
                Percent = y$`Area_Burned_%`)
    
    out <-rbind(x,z)
    rv$outputsumStats(out)
  })
  
  output$dynamicTabs <- renderUI({
    # Fires
    tabs <- list(
      tabPanel(HTML("<h4>Fire statistics</h4>"), tableOutput("tabFires"))
    )
    # DCI
    if (!is.null(input$confAOI) && input$confAOI > 0) {
      tabs <- append(
        list(tabPanel(HTML("<h4>Dendritic Connectivity Index (DCI)</h4>"), tableOutput("tabDCI"))),
        tabs,
        after = 0  # Add DCI as the first tab
      )
    }
    do.call(tabBox, c(list(id = "metric", width = 4), tabs))
  })
  
}