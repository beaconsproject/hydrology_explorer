server = function(input, output, session) {
  
  outtab1 <- reactiveVal()
  outfiretab <- reactiveVal()
  overlayBase <- reactiveVal(c())
  grps <- reactiveVal(c())
  group_names <- reactiveVal(c())
  overlayHydro <- reactiveVal(c()) 
  upstream_sf <- reactiveVal()
  downstream_sf <- reactiveVal()
  downstream_stem_sf <- reactiveVal()
  display1_name <- reactiveVal()
  display2_name <- reactiveVal()
  display3_name <- reactiveVal()
  
  output$help <- renderText({
    includeMarkdown("docs/upstream.md")
  })
  
  ################################################################################################
  # RELOAD
  observeEvent(input$reload_btn, {
    session$reload()
  })
  ################################################################################################
  ####################################################################################################
  # Set reactiveValues
  selected_catchments <- reactiveValues( # this is the list of currently selected catchments
    catchnum = c()
  )
  
  # Function to add a new group to group_names
  addGroup <- function(new_group) {
    current_groups <- group_names()  # Retrieve the current value
    if (!(new_group %in% current_groups)) {  # Check if the group is not already in the vector
      updated_groups <- c(current_groups, new_group)  # Append the new group
      group_names(updated_groups)  # Update the reactiveVal
    }
  }
  
  removeGroup <- function(group_to_remove) {
    current_groups <- group_names()  # Retrieve the current value
    updated_groups <- current_groups[current_groups != group_to_remove]  # Filter out the specified group
    group_names(updated_groups)  # Update the reactiveVal
  }
  
  ################################################################################################
  # Validate csv
  ################################################################################################
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
      layers <- st_layers("www/demo.gpkg")$name
      return(layers)
    }else if (!is.null(input$upload_intact)) {
      if(input$intactformat =="intgpkg"){
        file <- input$upload_intact$datapath
        ext <- tools::file_ext(file)
        if (ext == "gpkg") {
          if(input$distExplo){
            layers <- st_layers(file)$name
            return(layers)
          }
        }else{
          return(NULL)
        }
      }else{
        return(NULL)
      }
    } 
  })
  
  ################################################################################################
  # Observe on selectInput
  ################################################################################################
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
    req(catchments())
    updateSelectInput(session = getDefaultReactiveDomain(), "intactColumnName", choices = colnames(catchments()), selected= "IntactPB")
  })
  
  observe({
    req(input$upload_intact)
    req(input$intactformat == 'intgpkg')
    file <- input$upload_intact$datapath
    layers <- st_layers(file)$name
    updateSelectInput(session = getDefaultReactiveDomain(), "intactLayer", choices = c("Select a layer", layers))
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
    updateSelectInput(session = getDefaultReactiveDomain(), "aoiLayer", choices = c("Select a layer", layers))
  })
  
  
  ################################################################################################
  # Observe on tabs
  ################################################################################################
  observe({
    req(input$tabs == "selectAOI")  # Trigger when "Select AOI" is active
    
    # Check if intactSource is unset or NULL
    if (is.null(input$intactSource) || input$intactSource == "") {
      showModal(modalDialog(
        title = "Missing Input",
        "Please set the Intactness Source before selecting an Area of Interest (AOI).",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  ####################################################################################################
  # READ SPATIAL DATA
  ####################################################################################################
  ################################################################################################
  # Set studyarea
  planreg_sf <- reactive({
    req(input$selectsource)
    req(input$previewLayers)
    
    if(input$selectsource == "usedemo"){
      return(st_read("www/demo.gpkg", 'studyarea', quiet=T) %>% st_zm(drop = TRUE, what = "ZM"))
    } else if (!is.null(input$csv_paths)) {
      req(validate_csv())
      return(read_shp_from_csv(input$csv_paths, "studyarea"))
    } else if (!is.null(input$upload_sa)) {
      return(read_shp_from_upload(input$upload_sa)  %>% st_zm(drop = TRUE, what = "ZM"))
    }else if (!is.null(input$gpkg_file) && !is.null(input$sa_layer)){
      req(input$sa_layer != "")
      return(st_read(input$gpkg_file$datapath, input$sa_layer, quiet = TRUE)  %>% st_zm(drop = TRUE, what = "ZM"))
    } else {
      return(NULL)
    }
  })  
  
  ################################################################################################
  # Set catchments
  catchments <- reactive({
    req(input$selectsource)
    req(input$previewLayers)
    
    if(input$selectsource == "usedemo"){
      return(st_read("www/demo.gpkg", 'catchments', quiet=T))
    } else if (!is.null(input$csv_paths)) {
      req(validate_csv())
      return(read_shp_from_csv(input$csv_paths, "catchments"))
    } else if (!is.null(input$upload_catch)) {
      return(read_shp_from_upload(input$upload_catch))
    }else if (!is.null(input$gpkg_file) && !is.null(input$catch_layer)){
      req(input$catch_layer != "")
      return(st_read(input$gpkg_file$datapath, input$catch_layer, quiet = TRUE))
    }else {
      return(NULL)
    }
  })
  
  ################################################################################################
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
     return(stream)
  })
  
  ################################################################################################
  # Set intactness
  intactness_sf <- reactive({
    req(input$selectsource)
    i <- NULL
    
    if(input$selectsource == "usedemo" && input$intactSource == 'intcatch'){
      i <- st_read("www/demo.gpkg", 'undisturbed', quiet=T) %>%
        dplyr::select(any_of(c("geometry", "geom")))
    } else if(input$intactSource == 'intupload'){
      req(input$intactSource)
      req(input$intactformat)
      infile <- input$upload_intact
      if(input$intactformat == 'intgpkg'){
        req(input$intactLayer)
        if(input$intactLayer != "Select intactness layer"){
          i <- read_gpkg_from_upload(infile$datapath, input$intactLayer) %>%
            dplyr::select(any_of(c("geometry", "geom")))
        }
      }else{
        check_shp(infile$datapath)
        i <- read_shp_from_upload(infile) %>%
          dplyr::select(any_of(c("geometry", "geom")))
      }
    } 
    return(i)
  })
  
  ################################################################################################
  ## distExplo input
  ################################################################################################
  # IFL 2020
  ifl2020 <- reactive({
    if (input$selectInput=='usedemo') {
      la <-st_read('www/demo.gpkg', 'Intact_FL_2020', quiet=T)
      addGroup("Intact FL 2020")
      return(la)
    } else if (!is.null(input$upload_gpkg)){
      if ("Intact_FL_2020" %in% lyr_names()) {
        # Read the "intactness_2020" layer from the uploaded file if it exists
        la <-st_read(input$upload_gpkg$datapath, 'Intact_FL_2020', quiet = TRUE)
        addGroup("Intact FL 2020")
        return(la)
      } else {
        # Optionally, handle the case where the "Intact_FL_2020" layer is missing
        removeGroup("Intact FL 2020")
        return(NULL)  
      }
    }else{
      NULL
    }
  })
  
  # Fires
  fire_sf <- reactive({
    if (input$selectsource=='usedemo') {
      fi <-st_read('www/demo.gpkg', 'fires', quiet=T) %>%
        suppressWarnings(st_cast('MULTIPOLYGON')) %>% 
        st_zm(drop = TRUE, what = "ZM")  %>%
        st_make_valid() %>%
        mutate(area_ha = as.numeric(st_area(geom)/10000))
      addGroup("Fires")
      return(fi)
    } else if (input$distExplo){
      if ("fires" %in% lyr_names()) {
        # Read the "fires" layer from the uploaded file if it exists
        fi <-st_read(input$upload_intact$datapath, 'fires', quiet = TRUE) %>%
          suppressWarnings(st_cast('MULTIPOLYGON')) %>% 
          st_zm(drop = TRUE, what = "ZM")  %>%
          st_make_valid() %>%
          mutate(area_ha = as.numeric(st_area(geom)/10000))
        addGroup("Fires")
        if(is.null(fi$geometry)){fi$geometry <- fi$geom}
        fi <- st_set_geometry(fi, "geometry")
        return(fi)
      } else {
        removeGroup("Fires")
        return(NULL)  # or display a message, warning, etc.
      }
    }else{
      return(NULL)
    }
  })
  
  # Set PAs
  pas_sf <- reactive({
    if (input$selectsource=='usedemo') {
      fi <-st_read('www/demo.gpkg', 'protected_areas', quiet=T) %>%
        suppressWarnings(st_cast('MULTIPOLYGON')) %>% 
        st_zm(drop = TRUE, what = "ZM")  %>%
        st_make_valid() %>%
        mutate(area_ha = as.numeric(st_area(geom)/10000))
      addGroup("Protected areas")
      return(fi)
    } else if (input$distExplo){
      if ("protected_areas" %in% lyr_names()) {
        # Read the "fires" layer from the uploaded file if it exists
        fi <-st_read(input$upload_intact$datapath, 'protected_areas', quiet = TRUE) %>%
          suppressWarnings(st_cast('MULTIPOLYGON')) %>% 
          st_zm(drop = TRUE, what = "ZM")  %>%
          st_make_valid() %>%
          mutate(area_ha = as.numeric(st_area(geom)/10000))
        if(is.null(fi$geometry)){fi$geometry <- fi$geom}
        fi <- st_set_geometry(fi, "geometry")
        addGroup("Protected areas")
        return(fi)
      } else {
        removeGroup("Protected areas")
        return(NULL)  # or display a message, warning, etc.
      }
    }else{
      return(NULL)
    }
  })
  
  # Mines
  placers <- reactive({
    if (input$selectsource=='usedemo') {
      if ("Placer_Claims" %in% lyr_names()){
        fi <-st_read('www/demo.gpkg', 'Placer_Claims', quiet=T) %>%
          suppressWarnings(st_cast('MULTIPOLYGON')) %>% 
          st_zm(drop = TRUE, what = "ZM")  %>%
          st_make_valid() %>%
          mutate(area_ha = as.numeric(st_area(geom)/10000))
        return(fi)
      }
    } else if (input$distExplo){
      if ("Placer_Claims" %in% lyr_names()) {
        # Read the "Placer_Claims" layer from the uploaded file if it exists
        la <-st_read(input$upload_intact$datapath, 'Placer_Claims', quiet = TRUE)
        addGroup("Placer Claims")
        return(la)
      } else {
        # Optionally, handle the case where the "Placer_Claims" layer is missing
        removeGroup("Placer Claims")
        return(NULL)  
      }
    }
  })
  
  quartz <- reactive({
    if (input$selectsource=='usedemo') {
      if ("Quartz_Claims" %in% lyr_names()){
        fi <-st_read('www/demo.gpkg', 'Quartz_Claims', quiet=T) %>%
          suppressWarnings(st_cast('MULTIPOLYGON')) %>% 
          st_zm(drop = TRUE, what = "ZM")  %>%
          st_make_valid() %>%
          mutate(area_ha = as.numeric(st_area(geom)/10000))
        return(fi)
      }
    } else if (input$distExplo){
      if ("Quartz_Claims" %in% lyr_names()) {
        # Read the "Quartz_Claims" layer from the uploaded file if it exists
        la <-st_read(input$upload_intact$datapath, 'Quartz_Claims', quiet = TRUE)
        addGroup("Quartz Claims")
        return(la)
      } else {
        # Optionally, handle the case where the "Quartz_Claims" layer is missing
        removeGroup("Quartz Claims")
        return(NULL)  # or display a message, warning, etc.
      }
    }
  })
  
  mines <- reactive({
    if (input$selectsource=='usedemo') {
      if ("Mining_Claims" %in% lyr_names()){
        fi <-st_read('www/demo.gpkg', 'Mining_Claims', quiet=T) %>%
          suppressWarnings(st_cast('MULTIPOLYGON')) %>% 
          st_zm(drop = TRUE, what = "ZM")  %>%
          st_make_valid() %>%
          mutate(area_ha = as.numeric(st_area(geom)/10000))
        return(fi)
      }
    } else if (input$distExplo){
      if ("Mining_Claims" %in% lyr_names()) {
        # Read the "Mining_Claims" layer from the uploaded file if it exists
        la <-st_read(input$upload_intact$datapath, 'Mining_Claims', quiet = TRUE)
        addGroup("Mining Claims")
        return(la)
      } else {
        # Optionally, handle the case where the "Mining_Claims" layer is missing
        removeGroup("Mining Claims")
        return(NULL)  # or display a message, warning, etc.
      }
    }
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
  
  # Footprint
  footprint_sf <- reactive({
    if (input$selectsource=='usedemo') {
      fi <-st_read('www/demo.gpkg', 'disturbed', quiet=T) %>%
        suppressWarnings(st_cast('MULTIPOLYGON')) %>% 
        st_zm(drop = TRUE, what = "ZM")  %>%
        st_make_valid() %>%
        mutate(area_ha = as.numeric(st_area(geom)/10000))
      addGroup("Disturbed areas")
      return(fi)
    } else if (input$distExplo){
      if ("footprint" %in% lyr_names()) {
        # Read the "footprint" layer from the uploaded file if it exists
        fi <-st_read(input$upload_intact$datapath, 'disturbed', quiet = TRUE) %>%
          suppressWarnings(st_cast('MULTIPOLYGON')) %>% 
          st_zm(drop = TRUE, what = "ZM")  %>%
          st_make_valid() %>%
          mutate(area_ha = as.numeric(st_area(geom)/10000))
        if(is.null(fi$geometry)){fi$geometry <- fi$geom}
        fi <- st_set_geometry(fi, "geometry")
        addGroup("Disturbed areas")
        return(fi)
      } else {
        removeGroup("Disturbed areas")
        return(NULL)  # or display a message, warning, etc.
      }
    }else{
      return(NULL)
    }
  })
  
  
  ################################################################################################
  ## extra layers
  ################################################################################################
  # Display1
  display1_sf <- eventReactive(input$confExtra,{
    req(input$confExtra)  
    i <- NULL
    
    if(input$extraupload == "extrashp"){
      if(!is.null(input$display1)){
        req(input$display1)
        i <- read_shp_from_upload(input$display1) %>%
          st_zm(drop = TRUE, what = "ZM")  %>%
          st_make_valid()
        
        name <- substr(attr(i, "name"), 1, 25)
        display1_name(name)
        
        geom_type <- unique(sf::st_geometry_type(i))
        if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
          i <- suppressWarnings(sf::st_cast(i, "MULTIPOLYGON"))
        } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
          i <- suppressWarnings(sf::st_cast(i, "MULTILINESTRING"))
        } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
          i <- suppressWarnings(sf::st_cast(i, "POINT"))
        }
      }
    } else if (input$extraupload == "extragpkg"){
      req(input$display4)
      req(input$display4a)
      if(input$display4a != "Select a layer"){
        i <- st_read(input$display4$datapath, layer = input$display4a, quiet = TRUE) 
        name <- substr(input$display4a, 1, 25)
        display1_name(name)
      }
    }
    return(i)
  })
  
  display2_sf <- eventReactive(input$confExtra,{
    req(input$confExtra)  
    i <- NULL
    
    if(input$extraupload == "extrashp"){
      if(!is.null(input$display2)){
        req(input$display2)
        i <- read_shp_from_upload(input$display2) %>%
          st_zm(drop = TRUE, what = "ZM")  %>%
          st_make_valid()
        
        name <- substr(attr(i, "name"), 1, 25)
        display2_name(name)
        
        geom_type <- unique(sf::st_geometry_type(i))
        if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
          i <- suppressWarnings(sf::st_cast(i, "MULTIPOLYGON"))
        } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
          i <- suppressWarnings(sf::st_cast(i, "MULTILINESTRING"))
        } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
          i <- suppressWarnings(sf::st_cast(i, "POINT"))
        }
      }
    } else if (input$extraupload == "extragpkg"){
      req(input$display4)
      req(input$display4b)
      if(input$display4b != "Select a layer"){
        i <- st_read(input$display4$datapath, layer = input$display4b, quiet = TRUE)
        name <- substr(input$display4b, 1, 25)
        display2_name(name)
      }
    }
    return(i)
  })  
  
  display3_sf <- eventReactive(input$confExtra,{
    req(input$confExtra)  
    i <- NULL
    
    if(input$extraupload == "extrashp"){
      if(!is.null(input$display3)){
        req(input$display3)
        i <- read_shp_from_upload(input$display3) %>%
          st_zm(drop = TRUE, what = "ZM")  %>%
          st_make_valid()
        
        name <- substr(attr(i, "name"), 1, 25)
        display3_name(name)
        
        geom_type <- unique(sf::st_geometry_type(i))
        if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
          i <- suppressWarnings(sf::st_cast(i, "MULTIPOLYGON"))
        } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
          i <- suppressWarnings(sf::st_cast(i, "MULTILINESTRING"))
        } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
          i <- suppressWarnings(sf::st_cast(i, "POINT"))
        }
      }
    } else if (input$extraupload == "extragpkg"){
      req(input$display4)
      req(input$display4c)
      if(input$display4c != "Select a layer"){
        i <- st_read(input$display4$datapath, layer = input$display4c, quiet = TRUE)
        name <- substr(input$display4c, 1, 25)
        display3_name(name)
      }
    }
    return(i)
  })  
  
  ################################################################################################
  # Select AOI and zoom in to its extent
  ################################################################################################
  #trigger
  toListen <- reactive({
    req(!is.null(input$upload_aoi) || input$confAOI>0)
  })
  
  aoi_sf <- eventReactive(toListen(), {
    # upload AOI
    if(!is.null(input$upload_aoi)){ 
      if(is.null(planreg_sf())){
        # show pop-up ...
        showModal(modalDialog(
          title = "Study region is missing. Please select a study region and reload your 
                 area of interest.",
          easyClose = TRUE,
          footer = modalButton("OK")))
      }
      
      req(planreg_sf())
      infile <- input$upload_aoi
      if(input$sourceAOI == 'gpkgAOI'){
        req(input$aoiLayer)
        if(input$aoiLayer != "Select AOI layer"){
          i <- read_gpkg_from_upload(infile$datapath, input$aoiLayer) %>%
            dplyr::select(any_of(c("geometry", "geom")))
        }
      }else{
        check_shp(infile$datapath)
        i <- read_shp_from_upload(infile) %>%
          dplyr::select(any_of(c("geometry", "geom")))
      }
      aoi <- st_transform(i, st_crs(planreg_sf()))
      aoi <- st_union(aoi)
      aoi <- st_sf(geometry = aoi)
      
      if(!isTRUE(st_within(aoi, st_buffer(planreg_sf(), dist = 100), sparse = FALSE))){
        # show pop-up ...
        showModal(modalDialog(
          title = "Your Area of Interest is not within the planning region selected. 
                     Please upload an area of interest found within the planning region
                     or generate it by selecting catchments.",
          easyClose = TRUE,
          footer = modalButton("OK")))
      } 
      # Generate using catchment  
    }else{
      if(is.null(planreg_sf())){ 
        aoi <- NULL
        
        selected_catchments$catchnum <- c()
        # show pop-up ...
        showModal(modalDialog(
          title = "Study region is missing. Please select a study region prior to select catchment that 
                 composed your Area of Interest.",
          easyClose = TRUE,
          footer = modalButton("OK")))
      }
      req(length(selected_catchments$catchnum) > 0)
      
      catch_sf <- catchments()[catchments()$CATCHNUM %in% selected_catchments$catchnum,]
      aoi <- st_union(catch_sf) %>%
        st_buffer(dist = 20) %>% 
        st_buffer(dist = -20)
      aoi <- st_sf(geometry = aoi) %>% st_make_valid()
    }
    if(is.null(aoi$geometry)){aoi$geometry <- aoi$geom} 
    aoi <- st_set_geometry(aoi, "geometry")
    aoi$AOI_ID <- paste("AOI",as.character(1:nrow(aoi)) ,sep ="_")
    return(aoi)
  }
  )
  
  analysis_aoi <- reactive({
    req(input$confAOI)
    
    if(input$typeAOI == "catchAOI" || input$typeAOI == "uploadAOI" && isTRUE(input$editAOI)){
      analysis_aoi <- catchment_pr()[catchment_pr()$CATCHNUM %in% selected_catchments$catchnum,]
      if(input$intactSource=="intcatch"){
        intact_col <- input$intactColumnName
        # Weighted intact area = area * percentage (assuming % is 0-100)
        weighted_intact <- sum(as.numeric(st_area(analysis_aoi)) * (analysis_aoi[[intact_col]] / 100), na.rm = TRUE)
        # Total area
        total_area <- sum(as.numeric(st_area(analysis_aoi)), na.rm = TRUE)
        intactness <- weighted_intact / total_area * 100
        merged_sf <- st_union(analysis_aoi)
        
        # Assign intactness to merged feature
        merged_aoi <- st_sf(
          intact = intactness,
          geometry = merged_sf,
          crs = st_crs(analysis_aoi)
        )
      }else {
        merged_sf <- st_union(analysis_aoi)
        intact_intersection <- st_intersection(merged_sf, intactness_sf())
        
        total_area <- as.numeric(st_area(merged_sf))
        intact_area <- as.numeric(sum(st_area(intact_intersection), na.rm = TRUE))
        intactness <- (intact_area / total_area) * 100
        
        merged_aoi <- sf::st_sf(
          intact = intactness,
          geometry = merged_sf,
          crs = st_crs(analysis_aoi)
        )
      }
      merged_aoi$AOI_ID <- "AOI_1"
    } else { #input$typeAOI == "uploadAOI" && isFALSE(input$editAOI)
      if(input$intactSource=="intcatch"){
        merged_aoi <- aoi_sf()
        intact_col <- input$intactColumnName
        aoi_catch_inter <- st_intersection(merged_aoi, catchment_pr())
        aoi_catch_inter$area <- as.numeric(st_area(aoi_catch_inter))
        weighted_intact_area <- sum(aoi_catch_inter$area * (aoi_catch_inter[[intact_col]] / 100), na.rm = TRUE)
        total_aoi_area <- as.numeric(sf::st_area(merged_aoi))
        intactness_value <- (weighted_intact_area / total_aoi_area) * 100
        merged_aoi[[intact_col]] <- intactness_value
      }else { #intupload
        analysis_aoi <- aoi_sf()
        intact_intersection <- st_intersection(analysis_aoi, intactness_sf())
        
        total_area <- as.numeric(st_area(analysis_aoi))
        intact_area <- as.numeric(sum(st_area(intact_intersection), na.rm = TRUE))
        intactness <- (intact_area / total_area) * 100
        
        merged_aoi <- sf::st_sf(
          intact = intactness,
          geometry = analysis_aoi$geometry,
          crs = st_crs(analysis_aoi)
        )
      }
    }
    return(merged_aoi)
  })
  ################################################################################################
  # Set catchments
  ################################################################################################
  #Catchments within planning region
  catchment_pr <- reactive({
    req(planreg_sf())
    req(catchments())
    req(input$intactSource)
    if(input$intactSource =='intcatch'){
      req(input$intactColumnName)  # Ensure the textInput value is available
      intact_column <- input$intactColumnName  # Get the column name from the text input
      catchment <- catchments()
      catchment$intact <- catchment[[intact_column]]  # Dynamically access the specified column
    }else{
      catch_int <- st_intersects(st_centroid(catchments()), planreg_sf(), sparse = FALSE)
      catchments <- catchments()[catch_int,]
      intact <- st_intersection(intactness_sf(), catchments)
      intactArea <- intact %>%
        mutate(area_intact = st_area(.) %>% as.numeric()) %>%
        st_drop_geometry() %>%
        group_by(CATCHNUM) %>%
        summarise(area_intact = sum(area_intact, na.rm = TRUE)) %>%
        ungroup()
      
      catchments <- merge(catchments, intactArea[,c("CATCHNUM", "area_intact")], by = "CATCHNUM", all.x = TRUE)
      catchments$area_intact <- as.numeric(catchments$area_intact)
      catchments$area_intact[is.na(catchments$area_intact)] <- 0
      catchments$intact_per <- round(catchments$area_intact/st_area(catchments), 3)
      catchment <- st_cast(catchments, "MULTIPOLYGON")
    }
    return(catchment)
  }) 
  
  #Catchments within AOI
  catch_aoi <- reactive({
    #req(input$editAOI)
    catch_pr <- st_make_valid(catchment_pr())
    if(!is.null(input$upload_aoi)){
      catch_sf <- st_filter(catch_pr, aoi_sf(), .predicate = st_intersects)
      catch_sf$Area_total <- as.numeric(st_area(catch_sf))
      catch_sf$AOI_ID <- "AOI_1"
      selected_catchments$catchnum <- catch_sf$CATCHNUM
    }else if(is.null(input$upload_aoi) && input$confAOI>0){
      catch_sf <- catch_pr[catch_pr$CATCHNUM %in% selected_catchments$catchnum,]
      catch_sf$AOI_ID <- "AOI_1"
    }
    return(catch_sf)
  })
  
  ####################################################################################################
  # TRACK CATCHMENTS
  ####################################################################################################
  # Track a list of which catchnums have been selected
  observeEvent(input$map_shape_click,{
    req(input$previewLayers)
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
  
  ####################################################################################################
  # UPSTREAM SECTION
  ####################################################################################################
  catch_up <- eventReactive(input$confAnalysis, {
    req(toListen())
    req(analysis_aoi())
    
    if(input$typeAOI == "uploadAOI" || isFALSE(input$editAOI)){
      aoi_stream <- st_filter(stream_sf(), analysis_aoi(), .predicate = st_intersects)
      catch_stream <- catchment_pr()[catchment_pr()$SKELUID %in% aoi_stream$SKELUID,]
      
      start_points <- st_as_sf(
        st_drop_geometry(aoi_stream), 
        coords = c("start_x", "start_y"), 
        crs = st_crs(catch_stream)
      )
      end_points   <- st_as_sf(
        st_drop_geometry(aoi_stream), 
        coords = c("end_x", "end_y"), 
        crs = st_crs(catch_stream)
      )
      start_inside <- lengths(st_intersects(start_points, analysis_aoi())) > 0
      end_inside   <- lengths(st_intersects(end_points, analysis_aoi())) > 0
      streams_end_only <- aoi_stream[end_inside & !start_inside, ]
      catch_up_bits <- catchment_pr()[catchment_pr()$SKELUID %in% streams_end_only$SKELUID,]
            #catch_up_bits <- st_difference(catch_stream, st_union(catch_aoi()))
      
      #split_lines <- st_difference(catch_stream, st_union(analysis_aoi()))
      #catch_up_bits2 <- split_lines[ lengths(st_intersects(split_lines, start_points)) > 0, ]

      
      

      
      catch_stream$AOI_ID <- "AOI_1"
      upstream_list <- get_upstream_catchments(catch_stream, "AOI_ID", catchment_pr())
    }else{
      upstream_list <- get_upstream_catchments(analysis_aoi(), "AOI_ID", catchment_pr())
      catch_up_bits <- NULL
      #catch_up_bits2 <-  NULL
    } 
    # Tabulate dist area per catchment within the upstream area
    if(nrow(upstream_list)==0)
    {# show pop-up ...
      showModal(modalDialog(
        title = "No upstream catchments found!",
        easyClose = TRUE,
        footer = modalButton("OK")))
      data$catch_up(NULL)
    }else if (input$intactSource =='intcatch'){
      catch_up <- catchment_pr()[catchment_pr()$CATCHNUM %in% upstream_list$AOI_1,]
      catch_up <- rbind(catch_up_bits, catch_up)
      catch_up <- catch_up %>%
        st_difference(st_union(analysis_aoi())) %>%
        mutate(up =1,
               Area_total = st_area(geom))
    }else if(input$intactSource =='intupload'){
      intact <- st_intersection(intactness_sf(), catch_up)
      intArea <- intact %>%
        mutate(area_int = st_area(.) %>% as.numeric()) %>%
        st_drop_geometry()
      catch_up <- merge(catch_up, intArea [,c("CATCHNUM", "area_int")], by = "CATCHNUM", all.x = TRUE)
      catch_up$area_int <- as.numeric(catchments$area_int)
      catch_up$area_int[is.na(catchments$area_int)] <- 0
      catch_up$intact <- round((catch_up$area_int)/catch_up$Area_total, 3)
      catch_up <- st_cast(catch_up, "MULTIPOLYGON")
    }
    return(catch_up)
  })
  
  catch_up2 <- eventReactive(input$confAnalysis, {
    req(toListen())
    upstream_list <- get_upstream_catchments(analysis_aoi(), "AOI_ID", catchment_pr())
    
    # Tabulate dist area per catchment within the upstream area
    if(nrow(upstream_list)==0)
    {# show pop-up ...
      showModal(modalDialog(
        title = "No upstream catchments found!",
        easyClose = TRUE,
        footer = modalButton("OK")))
      catchment <- catchment_pr() %>%
        mutate(up = 0)
    }else{
      catch_up <- catchment_pr()[catchment_pr()$CATCHNUM %in% upstream_list$AOI_1,]
      catch_up <- catch_up %>%
        mutate(up =1) %>%
        st_drop_geometry()
      catchment <- merge(catchment_pr(), catch_up[,c("CATCHNUM", "up")], by = "CATCHNUM", all.x = TRUE)
      catchment$up[is.na(catchment$up)] <- 0
    }
    return(catchment)
  })
  
  ####################################################################################################
  # DOWNSTREAM STEM SECTION
  ####################################################################################################
  catch_stem <- eventReactive(input$confAnalysis, {
    req(toListen())
    browser()
    # downstream stem polygon
    downstream_stem_list <- get_downstream_catchments(analysis_aoi(), "AOI_ID", catchment_pr())
    if(nrow(downstream_stem_list)==0)
    {# show pop-up ...
      showModal(modalDialog(
        title = "No downstream stem catchments found!",
        easyClose = TRUE,
        footer = modalButton("OK")))
    }else{
      catch_stem <- catchment_pr()[catchment_pr()$CATCHNUM %in% downstream_stem_list$AOI_1, drop = FALSE]
      #catch_stem <- merge(catchment_pr(), catch_stem[,c("CATCHNUM")], by = "CATCHNUM", all.y = TRUE)
      
      aoi_stream <- st_filter(stream_sf(), analysis_aoi(), .predicate = st_intersects)
      catch_stream <- catchment_pr()[catchment_pr()$SKELUID %in% aoi_stream$SKELUID,]
      start_points <- st_as_sf(
        st_drop_geometry(aoi_stream), 
        coords = c("start_x", "start_y"), 
        crs = st_crs(catch_stream)
      )
      end_points   <- st_as_sf(
        st_drop_geometry(aoi_stream), 
        coords = c("end_x", "end_y"), 
        crs = st_crs(catch_stream)
      )
      start_inside <- lengths(st_intersects(start_points, analysis_aoi())) > 0
      end_inside   <- lengths(st_intersects(end_points, analysis_aoi())) > 0

      streams_start_only <- aoi_stream[start_inside & !end_inside, ]
      catch_stem_bits <- catchment_pr()[catchment_pr()$SKELUID %in% streams_start_only$SKELUID,]
      
      catch_stem_all <- rbind(catch_stem_bits, catch_stem)
      catch_stem <- catch_stem_all %>%
        st_difference(st_union(analysis_aoi())) %>%
        mutate(stem =1,
               Area_total = st_area(geom))
    }
    return(catch_stem)
  })
  
  ####################################################################################################
  # DOWNSTREAM SECTION
  ####################################################################################################
  catch_down <- eventReactive(input$confAnalysis, {
    
    if(is.null(aoi_sf()))
    { # show pop-up ...
      showModal(modalDialog(
        title = "Area of Interest is missing. Please select an Area of Interest.",
        easyClose = TRUE,
        footer =modalButton("OK")))
    }
    
    # downstream  polygon
    # Extract downstream stem and upstream from downstream stem
    req(toListen())
    req(catch_up())
    downstream_stem_list <- get_downstream_catchments(analysis_aoi(), "AOI_ID", catchment_pr())
    catch_stem <- extract_catchments_from_table(catchment_pr(), downstream_stem_list, as.character(colnames(downstream_stem_list)), "AOI_ID")
    upstem_list <- get_upstream_catchments(catch_stem, "AOI_ID", catchment_pr())
    catch_upstem <- extract_catchments_from_table(catchment_pr(), upstem_list, as.character(colnames(upstem_list)), "AOI_ID")
    
    #Merge stem catchment with their related upstream catchments
    catchList <- c()
    catchList <- rbind(downstream_stem_list,upstem_list)
    catchList <- pull(catchList, "AOI_1")
    catchList <- catchList %>%
      unique() %>%
      subset(!. %in% catch_up()$CATCHNUM) %>% #drop catchments within the upstream
      enframe(., name = NULL, value = as.character(colnames(downstream_stem_list)))
    
    # Generate downstream polygon by dissolving all downstream catchments
    if(nrow(downstream_stem_list)==0)
    {# show pop-up ...
      showModal(modalDialog(
        title = "No downstream catchments found!",
        easyClose = TRUE,
        footer = NULL))
    }else{
      catch_down<- catchment_pr()[catchment_pr()$CATCHNUM %in% catchList$AOI_1, drop = FALSE]
      catch_down$down <- 1
      # Final dataframe
      #catch_down <- catch_down %>%
      #  st_drop_geometry() %>%
      #  dplyr::select(CATCHNUM, down) 
      
      #catch_stem <- catch_stem() %>%
      #  st_drop_geometry() %>%
      #  dplyr::select(CATCHNUM, stem)       
      
      #catch_up <- catch_up() %>%
      #  st_drop_geometry() %>%
      #  dplyr::select(CATCHNUM, up)
      
      #catch_att <- catchment_pr() %>%
      #  left_join(catch_down, by = "CATCHNUM") %>%
      #  left_join(catch_stem, by = "CATCHNUM") %>%
      #  left_join(catch_up, by = "CATCHNUM") %>%
      #  filter(!CATCHNUM %in% analysis_aoi()$CATCHNUM)
      #catch_att$up[is.na(catch_att$up)] <- 0
      #catch_att$down[is.na(catch_att$down)] <- 0
      #catch_att$stem[is.na(catch_att$stem)] <- 0
      return(catch_down)
    }
  })
  
  ####################################################################################################
  ####################################################################################################
  # Map viewer
  ####################################################################################################
  ####################################################################################################
  # Render the initial map
  output$map <- renderLeaflet({
    # Render initial map
    
    map <- leaflet(options = leafletOptions(attributionControl=FALSE)) %>%
      fitBounds(lng1 = -121, lat1 = 44, lng2 = -65, lat2 = 78) %>%
      addMapPane(name = "ground", zIndex=380) %>%
      addMapPane(name = "over", zIndex=420) %>%
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>% 
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       options = layersControlOptions(collapsed = FALSE)) 
    
  })
  
  
  # Render planning region
  observeEvent(input$previewLayers, {
    req(planreg_sf())
    grps <- grps()
    planreg_sf <- planreg_sf() %>% st_transform(4326)
    stream_4326 <- st_transform(stream_sf(), 4326)
    catch_4326 <- st_transform(catchments(), 4326)
    legend <- c("Study area", "Streams", "Catchments")
    overlayBase(legend)
    map_bounds <- planreg_sf %>% st_bbox() %>% as.character()
    
    # show pop-up ...
    showModal(modalDialog(
      title = "Please wait.", " Layers are being uploaded.",
      easyClose = TRUE,
      footer = modalButton("OK")))
    
    map <- leafletProxy("map") %>% 
      clearGroup('Study area') %>%
      clearGroup('Catchments') %>%
      clearGroup('Streams') %>%
      clearGroup('Intactness') %>%
      clearGroup('Fires') %>%
      clearGroup('Mining claims') %>%
      clearGroup('Protected areas (PAs)') %>%
      clearGroup(display1_name()) %>%
      clearGroup(display2_name()) %>%
      clearGroup(display3_name()) %>%
      fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) %>% # set view to the selected FDA
      addPolygons(data=planreg_sf, color='black', fillColor = "", fillOpacity = 0, weight=3, group="Study area", options = leafletOptions(pane = "ground")) %>%
      addPolylines(data=stream_4326, color='#0066FF', weight=1.2, group="Streams", options = leafletOptions(pane = "ground")) %>%
      addPolygons(data=catch_4326, color='black', fillColor = "grey", fillOpacity = 0, weight=1, group="Catchments", options = leafletOptions(pane = "over")) %>%
      addControl(actionButton(inputId = "clear_button", label = "Clear selection"), position="topleft", className = "class_clear_button") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = legend,
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Streams", "Catchments"))
    removeModal()
  })
  
  # Map Intactness
  observeEvent(input$confIntact,{ 
    map <- leafletProxy("map") %>%
      clearGroup('Catchments') %>%
      clearGroup('Intactness') %>%
      clearGroup('Fires') %>%
      clearGroup('Protected areas (PAs)') %>%
      clearGroup('Mining claims') %>%
      clearGroup(display1_name()) %>%
      clearGroup(display2_name()) %>%
      clearGroup(display3_name())
    
    grps <- grps()
    overlayBase <- overlayBase()
    if (isMappable(intactness_sf())) { 
      catch <- catchment_pr() %>% st_transform(4326)
      intact <- st_transform(intactness_sf(), 4326)
      pop = ~paste("CATCHNUM:", CATCHNUM, "<br>Area (km²):", round(Area_Total/1000000,1), "<br>Intactness (%):", intact*100 )
      map <- map %>% addPolygons(data=intact, color='blue', fill = T, fillOpacity = 0.2, weight=0, group='Intactness', options = leafletOptions(pane = "ground"))
      map <- map %>% addPolygons(data=catch, color='black', fillColor = "grey", fillOpacity = 0, weight=1, layerId = ~CATCHNUM, popup = pop, group="Catchments", options = leafletOptions(pane = "over"))
      overlayBase <- c(overlayBase, "Intactness")
    } else {
      catch <- catchment_pr() %>% st_transform(4326) %>%
        mutate(
          area_km2 = round(as.numeric(st_area(.)) / 1e6, 1),
          intactness = .[[input$intactColumnName]] * 100,
          popup_text = paste0(
            "<b>CATCHNUM:</b> ", CATCHNUM,
            "<br><b>Area (km²):</b> ", area_km2,
            "<br><b>Intactness (%):</b> ", round(intactness, 1)
          )
        )
      map <- map %>% addPolygons(data=catch, color='black', fillColor = "grey", fillOpacity = 0, weight=1, layerId = ~CATCHNUM, popup = ~popup_text, group="Catchments", options = leafletOptions(pane = "over"))
    }
    
    if (isMappable(fire_sf())) { 
      fire_4326 <- st_transform(fire_sf(), 4326)
      map <- map %>% addPolygons(data=fire_4326, fill=T, stroke=F, fillColor='#FF6600', fillOpacity=0.5, group="Fires", options = leafletOptions(pane = "ground"))
      grps <- c(grps,"Fires")
    }
    if (isMappable(pas_sf())) { 
      pas_4326 <- st_transform(pas_sf(), 4326)
      map <- map %>% addPolygons(data=pas_4326, fillColor='#699999', stroke=F, fill = T, fillOpacity = 0.1, weight=1, group="Protected areas (PAs)", options = leafletOptions(pane = "ground"))
      grps <- c(grps,"Protected areas (PAs)") 
    }
    if (isMappable(mines_all())) { 
      mines_4326 <- st_transform(mines_all(), 4326)
      map <- map %>% addPolygons(data=mines_4326, fillColor='#666666', stroke=F, fill = T, fillOpacity = 0.1, weight=1, group="Mining claims", options = leafletOptions(pane = "ground"))
      grps <- c(grps,"Mining claims")
    }
    
    # Update reactiveVal
    grps(grps)
    overlayBase(overlayBase)
    
    # Map
    leafletProxy("map") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c(overlayBase(), grps),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Streams", "Catchments", grps))
  })
  
  # Map extra layers
  observeEvent(input$confExtra,{ 
    map <- leafletProxy("map") %>%
      clearGroup(display1_name()) %>%
      clearGroup(display2_name()) %>%
      clearGroup(display3_name())
    
    grps <- grps()
    
    if (isMappable(display1_sf())) { 
      display1 <- st_transform(display1_sf(), 4326)
      geom_type <- unique(sf::st_geometry_type(display1))
      if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
        map <- map %>% addPolygons(data=display1,  fillColor='#663300', stroke=F, fill = T, fillOpacity = 0.5, group=display1_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
        map <- map %>% addPolylines(data = display1, color = '#663300', weight = 2, group = display1_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
        map <- map %>% addCircleMarkers(data = display1, color = '#663300', radius = 5, fillOpacity = 0.7, group = display1_name(), options = leafletOptions(pane = "ground"))
      } else {
        showNotification("Unsupported geometry type", type = "error")
      }
      #map <- map %>% addPolygons(data=display1,  fillColor='#663300', stroke=F, fill = T, fillOpacity = 0.5, group=display1_name(), options = leafletOptions(pane = "ground"))
      grps <- c(grps,display1_name())
    } 
    if (isMappable(display2_sf())) { 
      display2 <- st_transform(display2_sf(), 4326)
      geom_type <- unique(sf::st_geometry_type(display2))
      if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
        map <- map %>% addPolygons(data=display2,  fillColor='#330066', stroke=F, fill = T, fillOpacity = 0.5, group=display2_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
        map <- map %>% addPolylines(data = display2, color = '#330066', weight = 2, group = display2_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
        map <- map %>% addCircleMarkers(data = display2, color = '#330066', radius = 5, fillOpacity = 0.7, group = display2_name(), options = leafletOptions(pane = "ground"))
      } else {
        showNotification("Unsupported geometry type", type = "error")
      }
      #map <- map %>% addPolygons(data=display2, fillColor='#330066', stroke=F, fill = T, fillOpacity = 0.5, group=display2_name(), options = leafletOptions(pane = "ground"))
      grps <- c(grps,display2_name())
    } 
    if (isMappable(display3_sf())) { 
      display3 <- st_transform(display3_sf(), 4326)
      geom_type <- unique(sf::st_geometry_type(display3))
      if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
        map <- map %>% addPolygons(data=display3,  fillColor='#003333', stroke=F, fill = T, fillOpacity = 0.5, group=display3_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
        map <- map %>% addPolylines(data = display3, color = '#003333', weight = 2, group = display3_name(), options = leafletOptions(pane = "ground"))
      } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
        map <- map %>% addCircleMarkers(data = display3, color = '#003333', radius = 5, fillOpacity = 0.7, group = display3_name(), options = leafletOptions(pane = "ground"))
      } else {
        showNotification("Unsupported geometry type", type = "error")
      }
      grps <- c(grps,display3_name())
    } 
    grps(grps)
    
    leafletProxy("map") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c(overlayBase(), grps),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Streams", "Catchments", grps))
  })  
  
  # Render selected catchments
  observe({
    req(catchments())
    
    # skip if upstream/downstream analysis is running
    if (input$confAnalysis > 0) return(NULL)
    
    data_selected <- catchments()[catchments()$CATCHNUM %in% selected_catchments$catchnum,]
    data_select <- st_transform(data_selected, 4326)

    if(nrow(data_select)>0){
      leafletProxy("map") %>%
        clearGroup("Catchments AOI") %>%
        addPolygons(data = data_select, color='black', weight = 1, fillColor = "grey", fillOpacity = 0.7,
                    layerId = data_select$CATCHNUM, group = "Catchments AOI") %>%
        addLayersControl(overlayGroups = c(overlayBase(), "Catchments AOI", grps()),
                         options = layersControlOptions(collapsed = FALSE))
    }
  })
  
  # Render AOI uploaded
  observeEvent(input$upload_aoi,{
    req(input$upload_aoi)
    req(aoi_sf())
    
    aoi_sf <- st_transform(aoi_sf(), 4326)
    legend <- c(overlayBase(), "AOI")
    overlayBase(legend)
    
    map_bounds1 <- aoi_sf %>% st_bbox() %>% as.character()
    leafletProxy("map") %>%
      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
      addPolygons(data=aoi_sf, fill=F, color="red", weight=3, group="AOI", options = leafletOptions(pane = "ground")) %>%
      addLayersControl(overlayGroups = c(overlayBase(), grps()),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Fires", "Protected areas (PAs)", "Intactness"))
  })
  
  # Render catchments to select AOI
  observe({
    req(catchments())
    req(input$typeAOI)
    
    if(input$typeAOI == "catchAOI"){
      leafletProxy("map") %>%
        showGroup("Catchments") %>%
        hideGroup(c("Streams", grps()))
    }else{
      leafletProxy("map") %>%
        hideGroup(c("Streams", "Catchments", grps()))
    }
  })
  
  # Render AOI editing
  observeEvent(input$editAOI,{
    req(input$editAOI)
    req(aoi_sf())
    
    aoi <- st_transform(aoi_sf(), 4326)
    catch_aoi <- st_transform(catch_aoi(), 4326)
    
    map_bounds1 <- aoi %>% st_bbox() %>% as.character()
    map <- leafletProxy("map") %>%
      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
      addPolygons(data=aoi, fill=F, color="black", weight=3, group="AOI", options = leafletOptions(pane = "ground")) %>%
      addLayersControl(overlayGroups = c(overlayBase(), grps()),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Fires", "Protected areas (PAs)", "Intactness"))
  })
  
  # Render AOI confirmed
  observeEvent(input$confAOI, {
    req(aoi_sf())
    
    showModal(modalDialog(
      title = "Generating AOI. Please wait.",
      easyClose = TRUE,
      footer = modalButton("OK")))

    aoi <- st_transform(aoi_sf(), 4326)
    analysis_aoi <- analysis_aoi() %>% 
      #st_union() %>% 
      st_buffer(dist = 20) %>% 
      st_buffer(dist = -20) %>%
      st_transform(4326)
    map_bounds1 <- analysis_aoi %>% st_bbox() %>% as.character()
    
    legend <- c(overlayBase(),  "AOI", "Analysis AOI")
    overlayBase(legend)
    
    map <- leafletProxy("map") %>%
      clearGroup('Catchments AOI') %>%
      clearGroup('AOI') %>%
      clearGroup('Selected') %>%
      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
      addPolygons(data=analysis_aoi, fill = F, color="red", weight=4, group="Analysis AOI", options = leafletOptions(pane = "ground")) %>%
      addPolygons(data=aoi, fill=F, color="black", weight=3, group="AOI", options = leafletOptions(pane = "ground")) %>%
      addLayersControl(overlayGroups = c(overlayBase(), grps()),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Fires", "Protected areas (PAs)", "Intactness"))
    showModal(modalDialog(
      title = "Analysis AOI displayed.",
      easyClose = TRUE,
      footer = modalButton("OK")))
  })
  
  # Render Upstream / downstream
  observe({
    req(input$confAnalysis >0)
    # show pop-up ...
    showModal(modalDialog(
      title = "Generating upstream and downstream layers. This may take several minutes to display.",
      easyClose = TRUE,
      footer = modalButton("OK")))
    
    #catch_att <- st_transform(catch_att(), 4326)
    
    #catch_up <- catch_att %>% 
    #  filter(up ==1) 
    #upstream_sf(catch_up)
    catch_up <- st_transform(catch_up(), 4326)
    
    catch_down <- st_transform(catch_down(), 4326) 

    catch_stem <- st_transform(catch_stem(), 4326)  

    all_values <- c(catch_stem$intact, catch_down$intact, catch_up$intact)
    all_values <- all_values[!is.na(all_values)]
    
    ## Create bin palette function for percent
    bins <-c(1.0, 0.99, 0.9, 0.8, 0.7, 0)
    pal <- colorBin(
      palette = "Greens",
      domain = all_values,
      bins = bins,
      na.color = "transparent"
    )
    legend <- c("Downstream area", "Downstream stem")
    overlayHydro(legend)
    
    map_bounds1 <- planreg_sf() %>% st_transform(4326) %>% st_bbox() %>% as.character()
    map <- leafletProxy("map") %>%
      clearGroup('Downstream area') %>%
      clearGroup('Downstream stem') %>%
      clearGroup('Upstream area') %>%
      clearGroup('Selected') %>%
      clearControls() %>%
      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
      addPolygons(data=catch_down, color=~pal(intact), stroke=F, fillOpacity=0.8, group='Downstream area', options = leafletOptions(pane = "ground")) %>%
      addPolygons(data=catch_stem, color=~pal(intact), stroke=F, fillOpacity=0.8, group="Downstream stem", options = leafletOptions(pane = "ground")) %>%
      addLegend(position = "bottomleft", pal = pal, values = all_values, opacity = 1,
                title = "Percent intactness", labFormat = labelFormat(
                  suffix = "%",
                  transform = function(x) 100 * x),
                group = "Downstream catchment intactness") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c(overlayBase(), grps(), overlayHydro()),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(c("Fires", "Intactness", "Protected areas (PAs)", "Catchemnts","Downstream area"))
    if(nrow(catch_up)>0){
      map <- leafletProxy("map") %>%
        addPolygons(data=catch_up, color=~pal(intact), stroke=F, fillOpacity=0.8, group="Upstream area", options = leafletOptions(pane = "ground")) %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c(overlayBase(), grps(), overlayHydro(), "Upstream area"),
                         options = layersControlOptions(collapsed = TRUE)) %>%
        hideGroup(c("Fires", "Intactness", "Protected areas (PAs)", "Catchemnts", "Downstream area"))
    } 
  })
  
  ################################################################################################
  ################################################################################################
  # -Render statistics table
  ################################################################################################
  ################################################################################################
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
  
  #Update with Study area
  observeEvent(input$previewLayers, {
    x <- tibble(Variables=c("Study area"), 
                Area_km2= NA_real_,
                Percent = NA_real_)
    
    x <- x %>% 
      mutate(Area_km2 = case_when(Variables == "Study area" ~  round(as.numeric(st_area(planreg_sf())/1000000,0))),
             Percent= case_when(Variables == "Study area" ~  100))
    outtab1(x)
    
    #Fire stat
    if(!is.null(fire_sf())){
      y <- tibble(Variables=c("Within study area"), 
                  Area_Burned_km2= NA_real_, 
                  'Area_Burned_%' = NA_real_)
      
      y <- y %>% 
        mutate(Area_Burned_km2 = case_when(Variables == "Within study area" ~  round(as.numeric(sum(st_area(fire_sf()))/1000000,2))),
               'Area_Burned_%'= case_when(Variables == "Within study area" ~  round(as.numeric(sum(st_area(fire_sf()))/st_area(planreg_sf()))*100))
        )
    }else{
      y <- tibble(
        Variables = "No fire",
        Area_Burned_km2 = NA_real_,
        `Area_Burned_%` = NA_real_
      )
    }
    outfiretab(y)
  })
  
  observeEvent(input$confIntact, {
    #Fire stat
    if(!is.null(fire_sf())){
      y <- tibble(Variables=c("Within study area"), 
                  Area_Burned_km2= NA_real_, 
                  'Area_Burned_%' = NA_real_)
      
      y <- y %>% 
        mutate(Area_Burned_km2 = case_when(Variables == "Within study area" ~  round(as.numeric(sum(st_area(fire_sf()))/1000000,2))),
               'Area_Burned_%'= case_when(Variables == "Within study area" ~  round(as.numeric(sum(st_area(fire_sf()))/st_area(planreg_sf()))*100))
        )
    }else{
      y <- tibble(
        Variables = "No fire",
        Area_Burned_km2 = NA_real_,
        `Area_Burned_%` = NA_real_
      )
    }
    outfiretab(y)
  })
  
  observeEvent(input$confAOI, {
    if(input$confAOI==0 && is.null(input$upload_aoi)){
      return()
    }
    
    x <- tibble(Variables=c("Study area", 
                            "Analysis AOI", 
                            "Analysis AOI intactness"), 
                Area_km2= NA_real_,
                Percent = NA_real_)
    
    x <- x %>% 
      mutate(Area_km2 = case_when(Variables == "Study area" ~  round(as.numeric(st_area(planreg_sf())/1000000,0))),
             Percent= case_when(Variables == "Study area" ~  100),
             Area_km2 = case_when(Variables == "Analysis AOI" ~  round(as.numeric(st_area(st_union(analysis_aoi()))/1000000,0)), 
                                  TRUE ~ Area_km2),
             Percent= case_when(Variables == "Analysis AOI" ~ round(as.numeric(st_area(st_union(analysis_aoi()))/st_area(planreg_sf())*100,2)),
                                Variables == "Analysis AOI intactness" ~ round(as.numeric(sum(analysis_aoi()$intact*st_area(analysis_aoi())))/as.numeric(st_area(st_union(analysis_aoi())))*100,2),
                                TRUE ~ Percent)
      )
    outtab1(x)
    
    #Fire
    if(!is.null(fire_sf())){
      fire_aoi <-st_intersection(analysis_aoi(), fire_sf())
      y <- outfiretab()
      
      new_firerows <- tibble(Variables=c("Within Analysis AOI"), 
                             Area_Burned_km2= NA_real_,
                             `Area_Burned_%` = NA_real_)
      
      y <- dplyr::bind_rows(y, new_firerows)
      
      if(nrow(fire_aoi)>0){
        y <- y %>% 
          mutate(Area_Burned_km2 = case_when(Variables == "Within Analysis AOI" ~  round(as.numeric(sum(st_area(fire_aoi))/1000000,2)), 
                                             TRUE ~ Area_Burned_km2),
                 `Area_Burned_%`= case_when(Variables == "Within Analysis AOI" ~ round(as.numeric(sum(st_area(fire_aoi))/sum(st_area(analysis_aoi())))*100,2),
                                            TRUE ~ `Area_Burned_%`))
      }else{
        y <- y %>% 
          mutate(Area_Burned_km2 = case_when(Variables == "Within Analysis AOI" ~  0, 
                                             TRUE ~ Area_Burned_km2),
                 `Area_Burned_%`= case_when(Variables == "Within Analysis AOI" ~  0, 
                                            TRUE ~ `Area_Burned_%`)
          )
      }
      outfiretab(y)
    }
  })
  
  #Update with Upstream/Downstream stats
  observeEvent(input$confAnalysis, {
    #upstream_area <- catch_att() %>%
    #  filter(up ==1) 
    upstream_area <- catch_up()
    #downstream_stem_int <- catch_att() %>%
    #  filter(stem ==1) 
    downstream_stem_int  <- catch_stem()
   # downstream_int <- catch_att() %>%
   #   filter(down ==1) 
    downstream_int <- catch_down()
    x <- outtab1()
    new_rows  <- tibble(Variables=c("Upstream area",
                                    "Downstream stem area",
                                    "Downstream area",
                                    "Upstream mean AWI*",
                                    "Downstream stem mean AWI*",
                                    "Downstream mean AWI*" ), 
                        Area_km2= NA_real_,
                        Percent = NA_real_)
    
    x <- dplyr::bind_rows(x, new_rows)
    
    if(nrow(upstream_area)>0){
      x <- x %>% 
        mutate(Area_km2 = case_when(Variables == "Upstream area" ~  round(as.numeric(sum(st_area(st_make_valid(upstream_area)))/1000000,0)), 
                                    TRUE ~ Area_km2),
               Percent= case_when(Variables == "Upstream mean AWI*" ~ round(as.numeric(sum(upstream_area$intact*st_area(upstream_area))/sum(st_area(upstream_area)))*100,1),
                                  TRUE ~ Percent))
    }else{
      x <- x %>% 
        mutate(Area_km2 = case_when(Variables == "Upstream area" ~  0, 
                                    TRUE ~ Area_km2),
               Percent= case_when(Variables == "Upstream mean AWI*" ~ 0,
                                  TRUE ~ Percent))
    }
    if(nrow(downstream_stem_int)>0){
      x <- x %>% 
        mutate(Area_km2 = case_when(Variables == "Downstream stem area" ~ round(as.numeric(sum(st_area(st_make_valid(downstream_stem_int)))/1000000,0)),
                                    TRUE ~ Area_km2),
               Percent= case_when(Variables == "Downstream stem mean AWI*" ~ round(as.numeric(sum(downstream_stem_int$intact*st_area(downstream_stem_int))/sum(st_area(downstream_stem_int)))*100,1),
                                  TRUE ~ Percent))
    }else{
      x <- x %>% 
        mutate(Area_km2 = case_when(Variables == "Downstream stem area" ~  0, 
                                    TRUE ~ Area_km2),
               Percent= case_when(Variables == "Downstream stem mean AWI*" ~ 0,
                                  TRUE ~ Percent))
    }
    
    if(nrow(downstream_int)>0){
      x <- x %>% 
        mutate(Area_km2 = case_when(Variables == "Downstream area" ~ round(as.numeric(sum(st_area(st_make_valid(downstream_int)))/1000000,0)),
                                    TRUE ~ Area_km2),
               Percent= case_when(Variables == "Downstream mean AWI*" ~ round(as.numeric(sum(downstream_int$intact*st_area(downstream_int))/sum(st_area(downstream_int)))*100,1),
                                  TRUE ~ Percent))
    }else{
      x <- x %>% 
        mutate(Area_km2 = case_when(Variables == "Downstream area" ~  0, 
                                    TRUE ~ Area_km2),
               Percent= case_when(Variables == "Downstream mean AWI*" ~ 0,
                                  TRUE ~ Percent))
    }  
    outtab1(x)
    
    #FIRE
    if(!is.null(fire_sf())){
      upstream_fire <- upstream_area %>%
        st_union() %>%
        st_intersection(fire_sf())
      
      downstream_stem_fire <- downstream_stem_int %>%
        st_union() %>%
        st_intersection(fire_sf())
      
      downstream_fire <- downstream_int %>%
        st_union() %>%
        st_intersection(fire_sf())
      
      y <- outfiretab()
      new_rows  <- tibble(Variables=c("Within upstream area",
                                      "Within downstream stem area",
                                      "Within overall downstream area"), 
                          Area_Burned_km2= NA_real_,
                          `Area_Burned_%` = NA_real_)
      y <- dplyr::bind_rows(y, new_rows)
      
      if(length(upstream_fire)>0){
        y <- y %>%
          mutate(Area_Burned_km2 = case_when(Variables == "Within upstream area" ~ round(as.numeric(sum(st_area(st_make_valid(upstream_fire)))/1000000,2)),
                                             TRUE ~ Area_Burned_km2),
                 `Area_Burned_%`= case_when(Variables == "Within upstream area" ~   round(as.numeric(sum(st_area(st_make_valid(upstream_fire)))/sum(st_area(upstream_area)))*100,2),
                                            TRUE ~ `Area_Burned_%`))
      }else{
        y <- y %>%
          mutate(Area_Burned_km2 = case_when(Variables == "Within upstream area" ~ 0,
                                             TRUE ~ Area_Burned_km2),
                 `Area_Burned_%`= case_when(Variables == "Within upstream area" ~   0,
                                            TRUE ~ `Area_Burned_%`))
      }
      if(length(downstream_stem_fire)>0){
        y <- y %>%
          mutate(Area_Burned_km2 = case_when(Variables == "Within downstream stem area" ~ round(as.numeric(sum(st_area(st_make_valid(downstream_stem_fire)))/1000000,2)),
                                             TRUE ~ Area_Burned_km2),
                 `Area_Burned_%`= case_when(Variables == "Within downstream stem area" ~ round(as.numeric(sum(st_area(st_make_valid(downstream_stem_fire)))/sum(st_area(downstream_stem_int)))*100,2),
                                            TRUE ~ `Area_Burned_%`))
      }else{
        y <- y %>%
          mutate(Area_Burned_km2 = case_when(Variables == "Within downstream stem area" ~ 0,
                                             TRUE ~ Area_Burned_km2),
                 `Area_Burned_%`= case_when(Variables == "Within downstream stem area" ~   0,
                                            TRUE ~ `Area_Burned_%`))
      }
      
      if(length(downstream_fire)>0){
        y <- y %>%
          mutate(Area_Burned_km2 = case_when(Variables == "Within overall downstream area" ~ round(as.numeric(sum(st_area(st_make_valid(downstream_fire)))/1000000,2)),
                                             TRUE ~ Area_Burned_km2),
                 `Area_Burned_%`= case_when(Variables == "Within overall downstream area" ~ round(as.numeric(sum(st_area(st_make_valid(downstream_fire)))/sum(st_area(downstream_int)))*100,2),
                                            TRUE ~ `Area_Burned_%`))
      }else{
        y <- y %>%
          mutate(Area_Burned_km2 = case_when(Variables == "Within overall downstream area" ~ 0,
                                             TRUE ~ Area_Burned_km2),
                 `Area_Burned_%`= case_when(Variables == "Within overall downstream area" ~   0,
                                            TRUE ~ `Area_Burned_%`))
      }  
      outfiretab(y)
    } 
  })
  
  output$tab1 <- renderTable({
    outtab1()
  }, digits = 1)
  
  output$tabFires <- renderTable({
    outfiretab()
  }, digits = 2)
  
  
  ################################################################################################
  # DCI
  ################################################################################################
  dci_att <- reactive({ 
    req(aoi_sf())
    
    aoi_sf <- analysis_aoi() %>%
      st_union() %>%
      st_as_sf("POLYGON") %>%
      mutate(network = "AOI_1") %>%
      rename(geometry = x)
    
    aoi_sf$DCI <- round(calc_dci(aoi_sf, stream_sf()),2)
    aoi_var <- aoi_sf %>%
      st_drop_geometry() %>%
      dplyr::select(network, DCI)
    return(aoi_var)
  })
  
  outputDCI <- reactive({
    tbl <- dci_att()
    DCI_score <- case_when(tbl[,"DCI"]>=0.9 ~ "Very High",
                           tbl[,"DCI"]>=0.8 & tbl[,"DCI"]<0.9 ~ "High",
                           tbl[,"DCI"]>=0.7 & tbl[,"DCI"]<0.8 ~ "Moderate",
                           tbl[,"DCI"]<0.7 ~ "Low"
    )
    x <- tibble(Variable=c("DCI"), Metric=NA, Rating = NA)
    x$Metric[x$Variable=="DCI"] <- tbl[,"DCI"]
    x$Rating[x$Variable=="DCI"] <- DCI_score
    colnames(x) <- c("", "Metric","Rating")
    return(x)
  })
  
  output$tabDCI <- renderTable({
    req(input$confAOI)
    outputDCI()
  })
  
  
  ################################################################################################
  # Save features to a geopackage
  ################################################################################################
  output$downloadData <- downloadHandler(
    filename = function() { paste("Hydro_explorer_output-", Sys.Date(), ".gpkg", sep="") },
    content = function(file) {
      catch_att <- catch_att()[,c("CATCHNUM", "Area_land", "Area_water", "Area_total", "intact", "down", "stem", "up")]
      
      x <- data.frame(AOI_area = outtab1()[2,2],
                      AOI_intact  = outtab1()[3,3],
                      PAs_AOI_per = outtab1()[4,3],
                      Upstream_area = outtab1()[5,2],
                      Upstream_mean_AWI= outtab1()[8,3],
                      Downstream_area = outtab1()[7,2],
                      Downstream_mean_AWI = outtab1()[10,3],
                      Downstream_stem_area = outtab1()[6,2],
                      Downstream_stem_mean_AWI = outtab1()[9,3],
                      Fire_within_study_area =outfiretab()[1,2],
                      Fire_within_aoi =outfiretab()[2,2],
                      Fire_within_upstream_area =outfiretab()[4,2],
                      Fire_within_downstream_stem_area =outfiretab()[4,2],
                      Fire_within_downstream_area =outfiretab()[5,2])
      colnames(x) <-c("area_km2","AOI_intact_per","PAs_AOI_per","Upstream_area", "Upstream_mean_AWI","Downstream_area","Downstream_mean_AWI",
                      "Downstream_stem_area","Downstream_stem_mean_AWI","Fire_areastudy_area","Fire_area_aoi",  "Fire_area_upstream",
                      "Fire_area_downstream_stem", "Fire_area_downstream")
      y <- analysis_aoi() %>% st_union() 
      analysis_aoi <- st_as_sf(data.frame(
        AOI_ID = "AOI_1",
        geometry = y
      ))
      
      aoi <- cbind(analysis_aoi, x)
      st_write(basebnd(), dsn=file, layer='kdtt', append=TRUE)
      st_write(basewtsh(), dsn=file, layer='watersheds', append=TRUE)
      st_write(baseplanR(), dsn=file, layer='LFN_IPCA_planning_area', append=TRUE)
      st_write(Ross(), dsn=file, layer='Ross_River', append=TRUE)
      st_write(Dene(), dsn=file, layer='Dene_Keh_Kusan', append=TRUE)
      st_write(TuCho(), dsn=file, layer='Tu_Cho_Frances_Lakes', append=TRUE)
      st_write(TuBall(), dsn=file, layer='Tu_Ball', append=TRUE)
      st_write(LR(), dsn=file, layer='Little_Rancheria', append=TRUE)
      st_write(activeMines(), dsn=file, layer='YT_mining_claims', append=TRUE)
      st_write(caribou(), dsn=file, layer='YT_caribou_herds', append=TRUE)
      st_write(rivers(), dsn=file, layer='river_corridors', append=TRUE)
      st_write(streams(), dsn=file, layer='streams', append=TRUE)
      st_write(planreg_sf(), dsn=file, layer='Liard_river_bassin', append=TRUE)
      st_write(aoi, dsn=file, layer='aoi', append=TRUE) 
      st_write(foot_sf(), dsn=file, layer='footprint', append=TRUE)
      st_write(intact_sf(), dsn=file, layer='intactness', append=TRUE)
      st_write(catch_att(), dsn=file, layer='catchments', append=TRUE)
      st_write(fires(), dsn=file, layer='fires', append=TRUE)
      st_write(pas_sf(), dsn=file, layer='protected_areas', append=TRUE)
      st_write(upstream_sf(), dsn=file, layer='upstream', append=TRUE)
      st_write(downstream_sf(), dsn=file, layer='downstream', append=TRUE)
      st_write(downstream_stem_sf(), dsn=file, layer='downstream_stem', append=TRUE)
    }
  )
  
}