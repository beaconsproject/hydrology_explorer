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
  
  selected_wsd <- reactive({
    if (input$select_wsd == "Please select") {
      file.path(indir, "studyarea.gpkg")
    } else if(input$select_wsd == "Full extent"){
      file.path(indir, "studyarea.gpkg")
    }else{
      file.path(indir, paste0('watershed',tolower(input$select_wsd),'.gpkg'))  
    }
  })
  
  bnd <- reactive({
    st_read(selected_wsd(), 'studyarea', quiet=T)
  })
  
  catch <- reactive({
    st_read(selected_wsd(), 'catchments', quiet=T)
  })
  
  streams <- reactive({
    st_read(selected_wsd(), 'streams', quiet=T)
  })
  
  
  footprint <- reactive({
    st_read(selected_wsd(), 'footprint', quiet=T)
  })
  
  intactness <- reactive({
    st_read(selected_wsd(), 'intactness', quiet=T)
  })
  
  fires <- reactive({
    st_read(selected_wsd(), 'fires', quiet=T) 
  })
  
  PAs <- reactive({
    st_read(selected_wsd(), 'pas', quiet=T) 
  })
  
  mines <- reactive({
    st_read(selected_wsd(), 'mineclaims', quiet=T) 
  })
  
  data <- reactiveValues(aoi_sf=reactiveVal(NULL),
                         analysis_aoi=reactiveVal(NULL),
                         catch_aoi=reactiveVal(NULL),
                         catch_up=reactiveVal(NULL),
                         catch_stem=reactiveVal(NULL),
                         catch_down=reactiveVal(NULL))
  ###############################################################################################
  # Set Planning Region
  ################################################################################################
  planreg_sf <- reactive({
    planreg <- NULL
    if(!is.null(input$upload_dist)){
      planreg <- load_gpkg(input$upload_dist$datapath, "studyarea") %>%
        st_transform(textpj) %>%
        st_make_valid()
    } else if (input$select_wsd == "Please select") {
      planreg <- NULL
    } else {
      planreg <- bnd()
    }
    
    if(!is.null(planreg)){
      if(is.null(planreg$geometry)){planreg$geometry <- planreg$geom}
      planreg <- st_set_geometry(planreg, "geometry")
    }
    return(planreg)
  })
  
  ################################################################################################
  # Set mines
  ################################################################################################
  mines_sf <- reactive({
    req(planreg_sf())
    if(!is.null(input$upload_dist)){
      mine <- load_gpkg(input$upload_dist$datapath, "mineclaims") %>%
        st_transform(textpj)
    } else {
      mine <- mines() %>%
        st_cast('MULTIPOLYGON') %>% 
        st_zm(drop = TRUE, what = "ZM")  %>%
        st_make_valid()
    }
    return(mine)
  })
  
  ################################################################################################
  # Set footprint
  ################################################################################################
  footprint_sf <- reactive({
    req(planreg_sf())
    if(!is.null(input$upload_dist)){
      footprints <- load_gpkg(input$upload_dist$datapath, "footprint") %>%
        st_transform(textpj)
    } else {
      footprints <- footprint()
    }
    return(footprints)
  })
  
  ################################################################################################
  # Set intactness
  ################################################################################################
  intactness_sf <- reactive({
    req(planreg_sf())
    if(!is.null(input$upload_dist)){
      intact <- load_gpkg(input$upload_dist$datapath, "intactness") %>%
        st_transform(textpj)
    } else {
      intact <- intactness()
    }
    return(intact)
  })
  
  ################################################################################################
  # Set stream
  ################################################################################################
  stream_sf <- reactive({
    req(planreg_sf())
    if(!is.null(input$upload_dist)){
      stream <- load_gpkg(input$upload_dist$datapath, "streams") %>%
        st_transform(textpj)
    } else {
      stream <- streams()
    }
    if(is.null(stream$geometry)){stream$geometry <- stream$geom}
    streams <- st_set_geometry(stream, "geometry")
    return(streams)
  })
  
  ################################################################################################
  # Set fire
  ################################################################################################
  fire_sf <- reactive({
    req(planreg_sf())
    if(!is.null(input$upload_dist)){
      fire <- load_gpkg(input$upload_dist$datapath, "fires") %>%
        st_cast('MULTIPOLYGON') %>% 
        st_zm(drop = TRUE, what = "ZM")  %>%
        st_make_valid() %>%
        st_transform(textpj) 
    } else {
      fire <- fires() %>%
        st_cast('MULTIPOLYGON') %>% 
        st_zm(drop = TRUE, what = "ZM")  %>%
        st_make_valid()
    }
    if(is.null(fire$geometry)){fire$geometry <- fire$geom}
    fires <- st_set_geometry(fire, "geometry")
    return(fires)
  })
  
  ################################################################################################
  # Set catchments
  ################################################################################################
  catchments <- reactive({
    req(planreg_sf())
    if(!is.null(input$upload_dist)){
      catchment <- load_gpkg(input$upload_dist$datapath, "catchments") %>%
        st_transform(textpj)  %>%
        st_make_valid()
    } else {
      catchment <- catch()
    }
    return(catchment)
  })  
  ################################################################################################
  # Set PAs
  ################################################################################################
  pas_sf <- reactive({
    req(planreg_sf())
    if(!is.null(input$upload_dist)){
      pas <- load_gpkg(input$upload_dist$datapath, "pas") %>%
        st_transform(textpj)
      pas <- st_cast(pas, "MULTIPOLYGON")
    } else {
      pas <- PAs() %>%
        #dplyr::select(NAME_E, TYPE_E, OWNER_E, STATUS_E, PROTDATE, Shape_Leng, Shape_Area)  %>%
        st_cast("MULTIPOLYGON")
    }
    return(pas)
  })
  
  ################################################################################################
  # Set layers choices
  ################################################################################################
  observe({
    req(catchments())
    updateSelectInput(session = getDefaultReactiveDomain(), "intactColumnName", choices = colnames(catchments()))
  })
  
  ################################################################################################
  # Set disturbances
  ################################################################################################
  #Footprint
  # Reactive value to store the computed footprint
  footprint_cached <- reactiveVal(NULL)
  
  # Observer to compute and store the footprint once
  observe({
    #req(input$upload_dist)
    req(planreg_sf())
    req(input$intactSource)
    if (is.null(footprint_cached())) {
      if (input$intactSource == 'intgpkg') {
        footprint <-footprint_sf()
      } else if (input$intactSource == 'intupload') {
        req(input$intacttype)
        if (input$intacttype == 'footmap') {
          req(input$shp_intact)
          infile <- input$shp_intact
          if (length(input$shp_intact) > 1) {
            dir <- unique(dirname(infile$datapath)) # get the directory
            outfiles <- file.path(dir, infile$name) # create new path names
            name <- strsplit(infile$name[1], "\\.")[[1]][1] # strip name 
            purrr::walk2(infile$datapath, outfiles, ~file.rename(.x, .y)) # rename files
            footprint<-st_read(file.path(dir, paste0(name, ".shp"))) %>%
              st_transform(textpj)
          }
        } else if (input$intacttype == 'intmap') {
          footprint <- st_difference(planreg_sf(), intact_sf())
        } else{
          footprint <- footprint_sf()
        }
      } else {
        footprint <- footprint_sf()
      }
      footprint_cached(footprint)
    }
  })
  
  # Reactive expression to access the cached footprint
  foot_sf <- reactive({
    req(footprint_cached())
    
    footprint_cached()
  })
  
  #Intactness
  # Reactive value to store the computed footprint
  intactness_cached <- reactiveVal(NULL)
  
  # Observer to compute and store the footprint once
  observe({
    req(planreg_sf())
    req(input$intactSource)
    if (is.null(intactness_cached())) {
      if (input$intactSource == 'intgpkg') {
        intactness <-intactness_sf() 
      } else if (input$intactSource == 'intupload') {
        req(input$intacttype)
        if (input$intacttype == 'intmap') {
          req(input$shp_intact)
          infile <- input$shp_intact
          if (length(input$shp_intact) > 1) {
            dir <- unique(dirname(infile$datapath)) # get the directory
            outfiles <- file.path(dir, infile$name) # create new path names
            name <- strsplit(infile$name[1], "\\.")[[1]][1] # strip name 
            purrr::walk2(infile$datapath, outfiles, ~file.rename(.x, .y)) # rename files
            intactness <- st_read(file.path(dir, paste0(name, ".shp"))) %>%
              st_transform(textpj)
          }
        } else if (input$intacttype == 'footmap') {
          intactness <- st_difference(planreg_sf(), foot_sf())
        } else{
          intactness <- intactness_sf()
        }
      } else {
        intactness <- intactness_sf()
      }
      intactness_cached(intactness)
    }
  })
  
  # Reactive expression to access the cached footprint
  intact_sf <- reactive({
    
    req(intactness_cached())
    intactness_cached()
  })
  
  ################################################################################################
  # AOI - Add radioButton based on existence of fileInput
  ################################################################################################
  observe({
    if (!is.null(input$upload_poly)) {
      output$conditional_ui <- renderUI({
        radioButtons(inputId = "sel_aoi", label = "Select Analysis AOI", 
                     choices = list("Using intersecting catchments" = "aoi_catch", 
                                    "Using provided AOI boundaries" = "aoi_bnd"),
                     selected = "aoi_catch")
      })
    } else {
      output$conditional_ui <- renderUI({
        NULL
      })
    }
  })
  ################################################################################################
  # AOI - Select AOI and zoom in to its extent
  ################################################################################################
  #trigger
  toListen <- reactive({
    req(!is.null(input$upload_poly) || input$gen_aoi_button>0)
  })
  
  observeEvent(toListen(), {
    # upload AOI
    if(!is.null(input$upload_poly)){ 
      if(is.null(planreg_sf())){
        # show pop-up ...
        showModal(modalDialog(
          title = "Study region is missing. Please select a study region and reload your 
                 area of interest.",
          easyClose = TRUE,
          footer = modalButton("OK")))
      }
      
      req(planreg_sf())
      aoi <- load_gpkg(input$upload_poly$datapath)
      aoi <- st_transform(aoi, st_crs(planreg_sf()))
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
    }else if(input$gen_aoi_button>0){
      if(is.null(planreg_sf())){ 
        data$aoi_sf(NULL)
        
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
      #aoi <- clusterSF(catch_sf, "AOI")
      aoi <- st_union(catch_sf)
      aoi <- st_sf(geometry = aoi) %>% st_make_valid()
    }
    if(is.null(aoi$geometry)){aoi$geometry <- aoi$geom} 
    aoi <- st_set_geometry(aoi, "geometry")
    aoi$AOI_ID <- paste("AOI",as.character(1:nrow(aoi)) ,sep ="_")
    data$aoi_sf(aoi)
  })
  
  
  observe({
    req(input$gen_aoi_button>0)
    req(length(selected_catchments$catchnum)>0)
    if(input$sel_aoi == "aoi_catch" || is.null(input$sel_aoi)){
      analysis_aoi <- catchment_pr()[catchment_pr()$CATCHNUM %in% selected_catchments$catchnum,]
      analysis_aoi$AOI_ID <- "AOI_1"
    }else{
      req(data$aoi_sf())
      analysis_aoi <- data$aoi_sf() %>%
        mutate(AOI_ID ="AOI_1",
               Area_total = st_area(data$aoi_sf()))
      intact <- st_intersection(intact_sf(), analysis_aoi)
      intact <- intact %>%
        st_area(intact)
      analysis_aoi$intact <- as.numeric(round(intact/analysis_aoi$Area_total, 3))
    }
    data$analysis_aoi(analysis_aoi)
  })
  ################################################################################################
  # Set catchments
  ################################################################################################
  #Catchments within planning region
  catchment_pr <- reactive({
    req(planreg_sf())
    if((!is.null(input$upload_dist) && input$intactSource =='intcatch') | (input$select_wsd != "Please select" && input$intactSource =='intcatch')) {
      req(input$intactColumnName)  # Ensure the textInput value is available
      intact_column <- input$intactColumnName  # Get the column name from the text input
      catchment <- catchments()
      catchment$intact <- catchment[[intact_column]]  # Dynamically access the specified column
    }else{
      catch_int <- st_intersects(st_centroid(catchments()), planreg_sf(), sparse = FALSE)
      catchments <- catchments()[catch_int,]
      intact <- st_intersection(foot_sf(), catchments)
      distArea <- intact %>%
        mutate(area_dist = st_area(.) %>% as.numeric()) %>%
        st_drop_geometry()
      catchments <- merge(catchments, distArea[,c("CATCHNUM", "area_dist")], by = "CATCHNUM", all.x = TRUE)
      catchments$area_dist <- as.numeric(catchments$area_dist)
      catchments$area_dist[is.na(catchments$area_dist)] <- 0
      catchments$intact <- round((catchments$Area_total-catchments$area_dist)/catchments$Area_total, 3)
      catchment <- st_cast(catchments, "MULTIPOLYGON")
    }
    return(catchment)
  }) 
  
  #Catchments within AOI
  observe({
    if(!is.null(input$upload_poly)){
      req(input$sel_aoi)
      if(input$sel_aoi == "aoi_bnd" || is.null(input$sel_aoi)){
        aoi <-  data$aoi_sf()
        catch_sf <- st_intersection(catchment_pr(), aoi)
        catch_sf <-  st_cast(catch_sf)[which(st_is(st_cast(catch_sf), c("POLYGON", "MULTIPOLYGON"))),]
        catch_sf$Area_total <- as.numeric(st_area(catch_sf))
        selected_catchments$catchnum <- NULL
      }else{
        aoi <-  data$aoi_sf()
        catch_sparse <- st_intersects(catchment_pr(), aoi)
        catch_int <- which(lengths(catch_sparse) > 0)
        catch_sf <- catchment_pr()[catch_int, ]
        catch_sf$Area_total <- as.numeric(st_area(catch_sf))
        catch_sf$AOI_ID <- "AOI_1"
        selected_catchments$catchnum <- catch_sf$CATCHNUM
      }
    }else if(input$gen_aoi_button>0){
      if (length(selected_catchments$catchnum)>0){
        catch_sf <- catchment_pr()[catchment_pr()$CATCHNUM %in% selected_catchments$catchnum,]
        catch_sf$AOI_ID <- "AOI_1"
      }else{
        catch_sf <- NULL
      }
      
    }else {
      catch_sf <- NULL
    }
    data$catch_aoi(catch_sf)
  })

  ####################################################################################################
  # TRACK CATCHMENTS
  ####################################################################################################
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
  
  ####################################################################################################
  # UPSTREAM SECTION
  ####################################################################################################
  observeEvent(input$goButtonDown, {
    req(toListen())
    req(data$analysis_aoi())
    if(input$sel_aoi == "aoi_bnd" || is.null(input$sel_aoi)){
      aoi_stream <- st_intersection(st_centroid(stream_sf()), data$analysis_aoi()) 
      catch_stream <- catchment_pr()[catchment_pr()$SKELUID %in% aoi_stream$SKELUID,]
      catch_up_bits <- st_difference(catch_stream, st_union(data$catch_aoi()))
      catch_stream$AOI_ID <- "AOI_1"
      upstream_list <- get_upstream_catchments(catch_stream, "AOI_ID", catchment_pr())
    }else{
      upstream_list <- get_upstream_catchments(data$analysis_aoi(), "AOI_ID", catchment_pr())
      catch_up_bits <- NULL
    } 
    # Tabulate dist area per catchment within the upstream area
    if(nrow(upstream_list)==0)
    {# show pop-up ...
      showModal(modalDialog(
        title = "No upstream catchments found!",
        easyClose = TRUE,
        footer = modalButton("OK")))
      data$catch_up(NULL)
    }else{
      catch_up <- catchment_pr()[catchment_pr()$CATCHNUM %in% upstream_list$AOI_1,]
      catch_up <- rbind(catch_up_bits, catch_up)
      catch_up <- catch_up %>%
        st_difference(st_union(data$analysis_aoi())) %>%
        mutate(up =1,
               Area_total = st_area(geom))
    }
    
    if(input$intactSource !='intcatch'){
      intact <- st_intersection(foot_sf(), catch_up)
      distArea <- intact %>%
        mutate(area_dist = st_area(.) %>% as.numeric()) %>%
        st_drop_geometry()
      catch_up <- merge(catch_up, distArea[,c("CATCHNUM", "area_dist")], by = "CATCHNUM", all.x = TRUE)
      catch_up$area_dist <- as.numeric(catchments$area_dist)
      catch_up$area_dist[is.na(catchments$area_dist)] <- 0
      catch_up$intact <- round((catch_up$Area_total-catch_up$area_dist)/catch_up$Area_total, 3)
      catch_up <- st_cast(catch_up, "MULTIPOLYGON")
    }
    data$catch_up(catch_up)
  })
  
  ####################################################################################################
  # DOWNSTREAM STEM SECTION
  ####################################################################################################
  observeEvent(input$goButtonDown, {
    req(toListen())
    req(data$analysis_aoi())
    # downstream stem polygon
    downstream_stem_list <- get_downstream_catchments(data$analysis_aoi(), "AOI_ID", catchment_pr())
    if(nrow(downstream_stem_list)==0)
    {# show pop-up ...
      showModal(modalDialog(
        title = "No downstream stem catchments found!",
        easyClose = TRUE,
        footer = modalButton("OK")))
      data$catch_stem(NULL)
    }else{
      catch_stem <- catchment_pr()[catchment_pr()$CATCHNUM %in% downstream_stem_list$AOI_1,]
      if (!is.null(data$catch_up())) {
        catch_stem <- catch_stem %>%
          st_difference(st_union(data$analysis_aoi())) %>%
          subset(!CATCHNUM %in% data$catch_up()$CATCHNUM) %>% #drop catchments within the upstream
          mutate(stem =1,
               Area_total = st_area(geom))
      } else {
        catch_stem <- catch_stem %>%
          st_difference(st_union(data$analysis_aoi())) %>%
          mutate(stem =1,
                 Area_total = st_area(geom))
      }  
    }
    if(input$intactSource !='intcatch'){
      intact <- st_intersection(foot_sf(), catch_stem)
      distArea <- intact %>%
        mutate(area_dist = st_area(.) %>% as.numeric()) %>%
        st_drop_geometry()
      catch_stem <- merge(catch_stem, distArea[,c("CATCHNUM", "area_dist")], by = "CATCHNUM", all.x = TRUE)
      catch_stem$area_dist <- as.numeric(catchments$area_dist)
      catch_stem$area_dist[is.na(catchments$area_dist)] <- 0
      catch_stem$intact <- round((catch_stem$Area_total-catch_stem$area_dist)/catch_stem$Area_total, 3)
      catch_stem <- st_cast(catch_stem, "MULTIPOLYGON")
    }
    data$catch_stem(catch_stem)
  })
  
  ####################################################################################################
  # DOWNSTREAM SECTION
  ####################################################################################################
  observeEvent(input$goButtonDown, {
    req(data$analysis_aoi())
    if(is.null(data$aoi_sf()))
    { # show pop-up ...
      showModal(modalDialog(
        title = "Area of Interest is missing. Please select an Area of Interest.",
        easyClose = TRUE,
        footer =modalButton("OK")))
    }
    # downstream  polygon
    # Extract downstream stem and upstream from downstream stem
    req(toListen())
    downstream_stem_list <- get_downstream_catchments(data$analysis_aoi(), "AOI_ID", catchment_pr())
    catch_stem <- extract_catchments_from_table(catchment_pr(), downstream_stem_list, as.character(colnames(downstream_stem_list)), "AOI_ID")
    upstem_list <- get_upstream_catchments(catch_stem, "AOI_ID", catchment_pr())
    catch_upstem <- extract_catchments_from_table(catchment_pr(), upstem_list, as.character(colnames(upstem_list)), "AOI_ID")
    
    #Merge stem catchment with their related upstream catchments
    catchList <- c()
    catchList <- rbind(downstream_stem_list,upstem_list)
    catchList <- pull(catchList, "AOI_1")

    if (!is.null(data$catch_up())) {
      catchList <- catchList %>%
        unique() %>%
        subset(!. %in% data$catch_up()$CATCHNUM) %>% #drop catchments within the upstream
        enframe(., name = NULL, value = as.character(colnames(downstream_stem_list)))
    } else {
      catchList <- catchList %>%
        unique() %>%
        enframe(., name = NULL, value = as.character(colnames(downstream_stem_list)))
    }
    
    # Generate downstream polygon by dissolving all downstream catchments
    if(nrow(downstream_stem_list)==0)
    {# show pop-up ...
      showModal(modalDialog(
        title = "No downstream catchments found!",
        easyClose = TRUE,
        footer = NULL))
    }else{
      catch_down <- catchment_pr()[catchment_pr()$CATCHNUM %in% catchList$AOI_1, ]
      catch_down <- catch_down %>%
        st_difference(st_union(data$analysis_aoi())) %>%
        mutate(down =1,
               Area_total = st_area(geom))
    }
    if(input$intactSource !='intcatch'){
      intact <- st_intersection(foot_sf(), catch_up)
      distArea <- intact %>%
        mutate(area_dist = st_area(.) %>% as.numeric()) %>%
        st_drop_geometry()
      catch_down <- merge(catch_down, distArea[,c("CATCHNUM", "area_dist")], by = "CATCHNUM", all.x = TRUE)
      catch_down$area_dist <- as.numeric(catchments$area_dist)
      catch_down$area_dist[is.na(catchments$area_dist)] <- 0
      catch_down$intact <- round((catch_down$Area_total-catch_down$area_dist)/catch_down$Area_total, 3)
      catch_down <- st_cast(catch_down, "MULTIPOLYGON")
    }
    data$catch_down(catch_down)
  })
  
  ####################################################################################################
  # Validate
  ####################################################################################################
  observe({
    if((input$previewLayers>0) && is.null(input$intactSource)){   
      # show pop-up ...
      showModal(modalDialog(
        title = "You need to select the way intactness will be provided.",
        easyClose = TRUE,
        footer = modalButton("OK")))
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
    bnd <- st_transform(basebnd(), 4326)
    wtsh <- st_transform(basewtsh(), 4326)
    pas_4326 <- st_transform(basePAs(), 4326)
    mines_4326 <- st_transform(basemines(), 4326)
    ifl2020_4326 <- st_transform(ifl2020(), 4326)
    int2010_4326 <- st_transform(int2010(), 4326)
    
    map_bounds <- bnd %>% st_bbox() %>% as.character()

    # Render initial map
    map <- leaflet(options = leafletOptions(attributionControl=FALSE)) %>%
      addMapPane(name = "layer1", zIndex=380) %>%
      addMapPane(name = "layer2", zIndex=420) %>%
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>% 
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
      addTiles(urlTemplate = "", group = "Blank Background") %>%
      addPolygons(data=bnd, color='black', fill=F, weight=3, group="Data extent", options = leafletOptions(pane = "layer1")) %>%
      addPolygons(data=wtsh, color='black', fill=F, weight=2, fillOpacity=1, label =~watershed, 
                  labelOptions = labelOptions(noHide = TRUE, direction = 'auto', style = list("color" = "black", "font-weight" = "bold", "font-size" = "15px"), textOnly = TRUE), 
                  group="Watersheds", options = leafletOptions(pane = "layer1")) %>%
      #addStaticLabels(data = wtsh, label = wtsh$watershed, style = list("color" = "black", "font-weight" = "bold", "font-size" = "13px"), group='label', options = leafletOptions(pane = "layer1")) %>%
      addPolygons(data=pas_4326, color='purple', fill = T, fillOpacity = 0.1, weight=1, group="Protected areas (PAs)", options = leafletOptions(pane = "layer1")) %>%
      addPolygons(data=mines_4326, color='black', fill = T, fillOpacity = 0.2, weight=1, group="Active mining claims", options = leafletOptions(pane = "layer1")) %>%
      addPolygons(data=ifl2020_4326, color='turquoise', fill = T, fillOpacity = 0.2, weight=0, group="IFL 2020", options = leafletOptions(pane = "layer1")) %>%
      addPolygons(data=int2010_4326, color='blue', fill = T, fillOpacity = 0.2, weight=0, group="Intactness", options = leafletOptions(pane = "layer1")) %>%
      fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) %>% # set view to the selected FDA
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                       overlayGroups = c("Data extent", "Watersheds", "Intactness", "IFL 2020", "Protected areas (PAs)", "Active mining claims"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("IFL 2020", "Protected areas (PAs)", "Active mining claims"))
    
    # Render planning region
    if (input$previewLayers>0 && (!(input$intactColumnName=="CATCHNUM") || !is.null(input$intactgpkg) || !is.null(input$intupload))) {
      req(planreg_sf())  
      grps <- NULL
        
      # show pop-up ...
      showModal(modalDialog(
        title = "Layers are uploading. Depending on study area size and source for catchment intactness,
        this may take several minutes to display.",
          easyClose = TRUE,
          footer = modalButton("OK")))
      planreg_sf <- st_transform(planreg_sf(), 4326)
      catch_4326 <- st_transform(catchment_pr(), 4326)
      stream_4326 <- st_transform(stream_sf(), 4326)
      map_bounds1 <- planreg_sf %>% st_bbox() %>% as.character()
      pop = ~paste("CATCHNUM:", CATCHNUM, "<br>Area (km²):", round(Area_total/1000000,1), "<br>Intactness (%):", intact*100 )
        
      map <- map %>%
          clearGroup('Protected areas (PAs)') %>%  
          clearGroup('Active mining claims') %>% 
          clearGroup('Intactness') %>%
          clearGroup('IFL 2020') %>%
          fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
          addPolygons(data=planreg_sf, color='blue', fill=F, weight=4, group='Study area', options = leafletOptions(pane = "layer1")) %>%
          addPolygons(data=catch_4326, color='black', fillColor = "grey", fillOpacity = 0, weight=1, layerId = catch_4326$CATCHNUM, popup = pop, group="Catchments", options = leafletOptions(pane = "layer2")) %>%
          addPolylines(data=stream_4326, color='#0066FF', weight=1.2, group="Streams", options = leafletOptions(pane = "layer1")) %>%
          addControl(actionButton(inputId = "clear_button", label = "Clear selection"), position="topleft", className = "class_clear_button") %>%
          addLayersControl(position = "topright",
                           overlayGroups = c("Data extent", "Watersheds", "Study area", "Catchments", "Streams"),
                           options = layersControlOptions(collapsed = FALSE))  %>%
          hideGroup(c("Streams"))
        
        if (length(foot_sf())>0) { 
          footprint <- st_transform(foot_sf(), 4326)
          map <- map %>% addPolygons(data=footprint, fill=T, stroke=F, fillColor='black', fillOpacity=0.5, group="Footprint", options = leafletOptions(pane = "layer2"))
          grps <- c(grps,"Footprint")
        }
        if (length(intact_sf())>0) { 
          intact <- st_transform(intact_sf(), 4326)
          map <- map %>% addPolygons(data=intact, color='darkgreen', stroke=F, fillOpacity=0.5, group='Intactness', options = leafletOptions(pane = "layer1"))
          grps <- c(grps,"Intactness")
        }
        if (length(fire_sf())>0) { 
          fire_4326 <- st_transform(fire_sf(), 4326)
          map <- map %>% addPolygons(data=fire_4326, fill=T, stroke=F, fillColor='brown', fillOpacity=0.5, group="Fires", options = leafletOptions(pane = "layer2"))
          grps <- c(grps,"Fires")
        }
        if (length(pas_sf())>0) { 
          pas_4326 <- st_transform(pas_sf(), 4326)
          map <- map %>% addPolygons(data=pas_4326, color='purple', fill = T, fillOpacity = 0.1, weight=1, group="Protected areas (PAs)", options = leafletOptions(pane = "layer1"))
          grps <- c(grps,"Protected areas (PAs)")
        }
        if (length(mines_sf())>0) { 
          mines_4326 <- st_transform(mines_sf(), 4326)
          map <- map %>% addPolygons(data=mines_4326, color='black', fill = T, fillOpacity = 0.2, weight=1, group="Active mining claims", options = leafletOptions(pane = "layer1"))
          grps <- c(grps,"Active mining claims")
        }
        map <- map %>% 
          addLayersControl(position = "topright",
                           baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                           overlayGroups = c("Data extent", "Watersheds", "Study area", "Catchments", "Streams", grps),
                           options = layersControlOptions(collapsed = FALSE)) %>%
          hideGroup(c("Streams", grps))
      }
    #})
    map
      
  })
  
  # Render selected catchments
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
  
  # Render AOI uploaded
  observe({
    req(data$aoi_sf())
    data$catch_aoi()
    aoi_sf <- st_transform(data$aoi_sf(), 4326)
    map_bounds1 <- aoi_sf %>% st_bbox() %>% as.character()
    leafletProxy("map") %>%
      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
      addPolygons(data=aoi_sf, fill=F, color="red", weight=3, group="AOI", options = leafletOptions(pane = "layer1")) %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                       overlayGroups = c("Study area", "AOI", "Catchments", "Streams", "Protected areas (PAs)", "Active mining claims", "Fires", "Intactness", "Footprint"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Fires", "Protected areas (PAs)", "Intactness", "Active mining claims"))
  })
  
  # Render AOI confirmed
  observeEvent(input$gen_aoi_button, {
    req(data$aoi_sf())
    showModal(modalDialog(
      title = "Generating AOI. Please wait.",
      easyClose = TRUE,
      footer = modalButton("OK")))
    
    aoi <- st_transform(data$aoi_sf(), 4326)
    analysis_aoi <- data$analysis_aoi() %>% st_union() %>% st_transform(4326)
    map_bounds1 <- analysis_aoi %>% st_bbox() %>% as.character()
    map <- leafletProxy("map") %>%
      clearGroup('AOI') %>%
      clearGroup('Selected') %>%
      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
      addPolygons(data=analysis_aoi, fill = F, color="red", weight=4, group="Analysis AOI", options = leafletOptions(pane = "layer1")) %>%
      addPolygons(data=aoi, fill=F, color="black", weight=3, group="AOI", options = leafletOptions(pane = "layer1")) %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                       overlayGroups = c("Study area", "Analysis AOI", "AOI", "Catchments", "Streams", "Protected areas (PAs)", "Active mining claims", "Fires", "Intactness", "Footprint"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Fires", "Protected areas (PAs)", "Intactness", "Active mining claims"))
    showModal(modalDialog(
      title = "Analysis AOI displayed.",
      easyClose = TRUE,
      footer = modalButton("OK")))
  })
  
  # Render Upstream / downstream
  observe({
    req(data$catch_down(), data$catch_up(), data$catch_stem())
    req(input$goButtonDown >0)
    # show pop-up ...
    showModal(modalDialog(
      title = "Generating upstream and downstream layers. This may take several minutes to display.",
      easyClose = TRUE,
      footer = modalButton("OK")))
    catch_down <- st_transform(data$catch_down(), 4326)
    catch_att <- catch_down[,"intact"]
    
    if (!is.null(data$catch_up())){
      catch_up <- st_transform(data$catch_up(), 4326)
      catch_att <- rbind(catch_att,catch_up[,"intact"] )
    } 
    if (!is.null(data$catch_stem())){
      catch_stem <- st_transform(data$catch_stem(), 4326)
      catch_att <- rbind(catch_att,catch_stem[,"intact"])
    }

    ## Create bin palette function for percent
    bins <-c(1.0, 0.99, 0.9, 0.8, 0.7, 0)
    catchintact = colorBin(
      palette = 'Greens',
      domain = catch_att$intact,
      bins = bins)
    
    map_bounds1 <- catch_down %>% st_bbox() %>% as.character()
    map <- leafletProxy("map") %>%
      clearGroup('Selected') %>%
      clearControls() %>%
      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
      addPolygons(data=catch_down, color=~catchintact(intact), stroke=F, fillOpacity=0.8, group='Downstream area', options = leafletOptions(pane = "layer1")) %>%
      addLegend(position = "bottomleft", pal = catchintact, values = catch_down$intact, opacity = 1,
                title = "Percent intactness", labFormat = labelFormat(
                  suffix = "%",
                  transform = function(x) 100 * x),
                group = "Downstream catchment intactness") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                       overlayGroups = c("Study area", "Analysis AOI","AOI", "Catchments", "Streams", "Protected areas (PAs)", "Active mining claims", "Fires","Footprint", "Downstream area", "Downstream stem"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Fires", "Active mining claims", "Protected areas (PAs)", "Downstream stem"))
    if (!is.null(data$catch_up())){
      map <-leafletProxy("map") %>%
        addPolygons(data=catch_up, color=~catchintact(intact), stroke=F, fillOpacity=0.8, group="Upstream area", options = leafletOptions(pane = "layer1")) %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                         overlayGroups = c("Study area", "Analysis AOI", "AOI", "Catchments", "Streams", "Protected areas (PAs)", "Active mining claims", "Fires", "Intactness", "Footprint", "Upstream area", "Downstream area", "Downstream stem"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("Fires", "Intactness", "Active mining claims", "Protected areas (PAs)", "Downstream stem"))
    } 
    if (!is.null(data$catch_stem())){
      map <-leafletProxy("map") %>%
        addPolygons(data=catch_stem, color=~catchintact(intact), stroke=F, fillOpacity=0.8, group="Downstream stem", options = leafletOptions(pane = "layer1")) %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                         overlayGroups = c("Study area", "Analysis AOI", "AOI", "Catchments", "Streams", "Protected areas (PAs)", "Active mining claims", "Fires", "Intactness", "Footprint", "Upstream area", "Downstream area", "Downstream stem"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("Fires", "Intactness", "Active mining claims", "Protected areas (PAs)", "Downstream stem"))
    } 
    map
  })
  
  ################################################################################################
  # Generate statistics
  ################################################################################################
  outtab1 <- reactiveVal(
    tibble(Variables = character(), Area_km2 = numeric(), Percent = numeric())
  )
  #Update with Study area
  observeEvent(input$previewLayers, {
    
    x <- tibble(Variables=c("Study area"), 
                Area_km2=round(st_area(planreg_sf())/1000000,0), 
                Percent = 100
    )
    x <- x %>% 
      mutate_at(c('Area_km2', 'Percent'), as.numeric)
    t <- rbind(outtab1(), x)
    t <- t %>% distinct()
    outtab1(t)
  })
  
  #Update with AOI
  observeEvent(input$gen_aoi_button, {
    #if(input$gen_aoi_button==0 && is.null(input$upload_poly)){
    if(is.null(data$aoi_sf())){
      return()
    }
    pas_sf <- st_make_valid(pas_sf())
    pas_aoi <-st_intersection(data$analysis_aoi(), pas_sf)
    x <- tibble(Variables=c("Analysis AOI", "Analysis AOI intactness", "PAs within Analysis AOI"), 
                Area_km2=NA,
                Percent = NA
    )
    x$Area_km2[x$Variables=="Analysis AOI"] <- round(st_area(st_union(data$analysis_aoi()))/1000000,0)
    x$Percent[x$Variables=="Analysis AOI"] <- round(st_area(st_union(data$analysis_aoi()))/st_area(planreg_sf())*100,2)
    x$Area_km2[x$Variables=="Analysis AOI intactness"] <- round((sum(data$analysis_aoi()$intact*data$analysis_aoi()$Area_total))/1000000,0)
    x$Percent[x$Variables=="Analysis AOI intactness"] <- round((sum(data$analysis_aoi()$intact*data$analysis_aoi()$Area_total)/sum(data$analysis_aoi()$Area_total))*100,2)
    if (nrow(pas_aoi)==0){
      x$Area_km2[x$Variables=="PAs within Analysis AOI"] <- 0
      x$Percent[x$Variables=="PAs within Analysis AOI"] <- 0
    }else{
      x$Area_km2[x$Variables=="PAs within Analysis AOI"] <- round(st_area(st_union(pas_aoi))/1000000,0)
      x$Percent[x$Variables=="PAs within Analysis AOI"] <- round(st_area(st_union(pas_aoi))/st_area(st_union(data$analysis_aoi()))*100,2)
    }
    x <- x %>% 
      mutate_at(c('Area_km2', 'Percent'), as.numeric)
    t <- rbind(outtab1(), x)
    outtab1(t)
  })
  
  #Update with Upstream/Downstream stats
  observeEvent(input$goButtonDown, ({
    upstream_area <- data$catch_up()
   
    downstream_stem_int <- data$catch_stem()
    
    downstream_int <- data$catch_down() 
    
    x <- tibble(Variables=c("Upstream area",
                            "Downstream stem area",
                            "Downstream area",
                            "Upstream mean AWI*",
                            "Downstream stem mean AWI*",
                            "Downstream mean AWI*"),
                Area_km2 = NA, 
                Percent = NA)
    
    if (!is.null(upstream_area)){
      x$Area_km2[x$Variables=="Upstream area"] <- round(sum(st_area(st_make_valid(upstream_area)))/1000000,0)
      x$Percent[x$Variables=="Upstream mean AWI*"] <- round((sum(upstream_area$intact*upstream_area$Area_total)/sum(upstream_area$Area_total))*100,1)
    }else{
      x$Area_km2[x$Variables=="Upstream area"] <- 0
      x$Percent[x$Variables=="Upstream mean AWI*"] <- 0
    }
    if (!is.null(downstream_stem_int)){
      x$Area_km2[x$Variables=="Downstream stem area"] <- round(sum(st_area(st_make_valid(downstream_stem_int)))/1000000,0)
      x$Percent[x$Variables=="Downstream stem mean AWI*"] <- round((sum(downstream_stem_int$intact*downstream_stem_int$Area_total)/sum(downstream_stem_int$Area_total))*100,1)
      
    }else{
      x$Area_km2[x$Variables=="Downstream stem area"] <- 0
      x$Percent[x$Variables=="Downstream stem mean AWI*"] <- 0
    }
    if (!is.null(downstream_int)){
      x$Area_km2[x$Variables=="Downstream area"] <- round(sum(st_area(st_make_valid(downstream_int)))/1000000,0)
      x$Percent[x$Variables=="Downstream mean AWI*"] <- round((sum(downstream_int$intact*downstream_int$Area_total)/sum(downstream_int$Area_total))*100,1)
      
    }else{
      x$Area_km2[x$Variables=="Downstream area"] <- 0
      x$Percent[x$Variables=="Downstream mean AWI*"] <- 0
    }  
    x <- x %>% 
      mutate_at(c('Area_km2', 'Percent'), as.numeric)
    t <- rbind(outtab1(), x)
    outtab1(t)
  }))
  
  output$tab1 <- renderTable({
    outtab1()
  }, digits = 1)
  
  ################################################################################################
  # DCI
  ################################################################################################
  dci_att <- reactive({ 
    req(data$aoi_sf())
    aoi_sf <- data$analysis_aoi() %>%
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
    req(input$gen_aoi_button)
    outputDCI()
  })
  
  ################################################################################################
  # Fires
  ################################################################################################
  outfiretab <- reactiveVal(
    tibble(Variables = character(), Area_Burned_km2 = numeric(), 'Area_Burned_%' = numeric())
  )
  
  #Update with Study area
  observeEvent(input$previewLayers, {
    x <- tibble(Variables=c("Within study area"), 
                Area_Burned_km2= 0, 
                'Area_Burned_%' = 0)
    
    fire_pr <- fire_sf()
    if(nrow(fire_pr)>0){
      x$Area_Burned_km2[x$Variables=="Within study area"] <- round(sum(st_area(fire_pr))/1000000,2)
      x$'Area_Burned_%'[x$Variables=="Within study area"] <- round((sum(st_area(fire_pr))/st_area(planreg_sf()))*100,2)
    }
    x <- x %>% 
      mutate_at(c('Area_Burned_km2', 'Area_Burned_%'), as.numeric)
    t <- rbind(outfiretab(), x)
    outfiretab(t)
  })
  
  observeEvent(input$gen_aoi_button, {

    fire_aoi <-st_intersection(data$analysis_aoi(), fire_sf())
    x <- tibble(Variables=c("Within Analysis AOI"), 
                Area_Burned_km2= 0, 
                'Area_Burned_%' = 0)
    
    if(nrow(fire_aoi)>0){
      x$Area_Burned_km2[x$Variables=="Within Analysis AOI"] <- round(sum(st_area(fire_aoi))/1000000,2)
      x$'Area_Burned_%'[x$Variables=="Within Analysis AOI"] <- round((sum(st_area(fire_aoi))/sum(st_area(data$analysis_aoi())))*100,2)
    }
    x <- x %>% 
      mutate_at(c('Area_Burned_km2', 'Area_Burned_%'), as.numeric)
    t <- rbind(outfiretab(), x)
    outfiretab(t)
  })
  
  observeEvent(input$goButtonDown, ({
    req(data$catch_up())
    upstream_area <- data$catch_up() 
    
    downstream_stem_int <- data$catch_stem() 
    
    downstream_int <- data$catch_down() 
    
    if (!is.null(upstream_area)){
      upstream_fire <- upstream_area %>%
        st_union() %>%
        st_intersection(fire_sf())
    } else {
      upstream_fire <- NULL
    }
    if (!is.null(downstream_stem_int)){
      downstream_stem_fire <- downstream_stem_int %>%
        st_union() %>%
        st_intersection(fire_sf())
    } else {
      downstream_stem_fire <- NULL
    }
    if (!is.null(downstream_int)){
      downstream_fire <- downstream_int %>%
        st_union() %>%
        st_intersection(fire_sf())
    } else {
      downstream_fire <- NULL
    }
    x <- tibble(Variables=c("Within upstream area",
                            "Within downstream stem area",
                            "Within overall downstream area"),
                Area_Burned_km2 = 0, 
                'Area_Burned_%' = 0)
    
    if(!is.null(upstream_fire) && length(upstream_fire)>0){
      x$Area_Burned_km2[x$Variables=="Within upstream area"] <- round(sum(st_area(st_make_valid(upstream_fire)))/1000000,2)
      x$'Area_Burned_%'[x$Variables=="Within upstream area"] <- round((sum(st_area(st_make_valid(upstream_fire)))/sum(upstream_area$Area_total))*100,2)
    }
    if(!is.null(downstream_stem_fire) && length(downstream_stem_fire)>0){
      x$Area_Burned_km2[x$Variables=="Within downstream stem area"] <- round(sum(st_area(st_make_valid(downstream_stem_fire)))/1000000,2)
      x$'Area_Burned_%'[x$Variables=="Within downstream stem area"] <- round((sum(st_area(st_make_valid(downstream_stem_fire)))/sum(downstream_stem_int$Area_total))*100,2)
    }    
    if(!is.null(downstream_fire) && length(downstream_fire)>0){
      x$Area_Burned_km2[x$Variables=="Within overall downstream area"] <- round(sum(st_area(st_make_valid(downstream_fire)))/1000000,2)
      x$'Area_Burned_%'[x$Variables=="Within overall downstream area"] <- round((sum(st_area(st_make_valid(downstream_fire)))/sum(downstream_int$Area_total))*100,2)
    }    
    
    x <- x %>% 
      mutate_at(c('Area_Burned_km2', 'Area_Burned_%'), as.numeric)
    t <- rbind(outfiretab(), x)
    outfiretab(t)
  }))
  
  output$tabFires <- renderTable({
    outfiretab()
  }, digits = 2)
  
  
  observeEvent(input$reset_button,{
    selected_catchments$catchnum <- c()
    grps <- NULL
    planreg_sf <- st_transform(planreg_sf(), 4326)
    catch_4326 <- st_transform(catchment_pr(), 4326)
    stream_4326 <- st_transform(stream_sf(), 4326)
    map_bounds1 <- planreg_sf %>% st_bbox() %>% as.character()
    pop = ~paste("CATCHNUM:", CATCHNUM, "<br>Area (km²):", round(Area_total/1000000,1), "<br>Intactness (%):", intact*100 )
    
    data$aoi_sf(NULL)
    data$analysis_aoi(NULL)
    data$catch_aoi(NULL)
    data$catch_up(NULL)
    data$catch_stem(NULL)
    data$catch_down(NULL)
    
    map <- leafletProxy("map") %>%
      clearGroup('AOI') %>% 
      clearGroup("Analysis AOI") %>%
      clearGroup('Upstream area') %>%  
      clearGroup('Downstream area') %>%  
      clearGroup('Downstream stem area') %>%  
      clearGroup('Protected areas (PAs)') %>%  
      clearGroup('Active mining claims') %>% 
      clearGroup('Intactness') %>%
      clearGroup('IFL 2020') %>%
      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
      addPolygons(data=planreg_sf, color='blue', fill=F, weight=4, group='Study area', options = leafletOptions(pane = "layer1")) %>%
      addPolygons(data=catch_4326, color='black', fillColor = "grey", fillOpacity = 0, weight=1, layerId = catch_4326$CATCHNUM, popup = pop, group="Catchments", options = leafletOptions(pane = "layer2")) %>%
      addPolylines(data=stream_4326, color='#0066FF', weight=1.2, group="Streams", options = leafletOptions(pane = "layer1")) %>%
      addControl(actionButton(inputId = "clear_button", label = "Clear selection"), position="topleft", className = "class_clear_button") %>%
      addLayersControl(position = "topright",
                       overlayGroups = c("Data extent", "Watersheds", "Study area", "Catchments", "Streams"),
                       options = layersControlOptions(collapsed = FALSE))  %>%
      hideGroup(c("Streams"))
    
    if (length(foot_sf())>0) { 
      footprint <- st_transform(foot_sf(), 4326)
      map <- map %>% addPolygons(data=footprint, fill=T, stroke=F, fillColor='black', fillOpacity=0.5, group="Footprint", options = leafletOptions(pane = "layer2"))
      grps <- c(grps,"Footprint")
    }
    if (length(intact_sf())>0) { 
      intact <- st_transform(intact_sf(), 4326)
      map <- map %>% addPolygons(data=intact, color='darkgreen', stroke=F, fillOpacity=0.5, group='Intactness', options = leafletOptions(pane = "layer1"))
      grps <- c(grps,"Intactness")
    }
    if (length(fire_sf())>0) { 
      fire_4326 <- st_transform(fire_sf(), 4326)
      map <- map %>% addPolygons(data=fire_4326, fill=T, stroke=F, fillColor='brown', fillOpacity=0.5, group="Fires", options = leafletOptions(pane = "layer2"))
      grps <- c(grps,"Fires")
    }
    if (length(pas_sf())>0) { 
      pas_4326 <- st_transform(pas_sf(), 4326)
      map <- map %>% addPolygons(data=pas_4326, color='purple', fill = T, fillOpacity = 0.1, weight=1, group="Protected areas (PAs)", options = leafletOptions(pane = "layer1"))
      grps <- c(grps,"Protected areas (PAs)")
    }
    if (length(mines_sf())>0) { 
      mines_4326 <- st_transform(mines_sf(), 4326)
      map <- map %>% addPolygons(data=mines_4326, color='black', fill = T, fillOpacity = 0.2, weight=1, group="Active mining claims", options = leafletOptions(pane = "layer1"))
      grps <- c(grps,"Active mining claims")
    }
    map <- map %>% 
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                       overlayGroups = c("Data extent", "Watersheds", "Study area", "Catchments", "Streams", grps),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Streams", grps))
  #})
  map
  
  })
  ################################################################################################
  # Save features to a geopackage
  ################################################################################################
  output$downloadData <- downloadHandler(
    filename = function() { paste("Hydro_explorer_output-", Sys.Date(), ".gpkg", sep="") },
    content = function(file) {
      
      catch_att <- catchment_pr()
      
      if (!is.null(data$catch_down())){
        catch_down <- data$catch_down() %>%
          st_drop_geometry() %>%
          dplyr::select(CATCHNUM, down)
        catch_att<- merge(catch_att, catch_down, by = "CATCHNUM", all.x = TRUE)
        is.na(catch_att$down) <- 0
      }
      if (!is.null(data$catch_stem())){
        catch_stem <- data$catch_stem() %>%
          st_drop_geometry() %>%
          dplyr::select(CATCHNUM, stem) 
        catch_att<- merge(catch_att, catch_stem, by = "CATCHNUM", all.x = TRUE)
        is.na(catch_att$stem) <- 0
      }
      if (!is.null(data$catch_up())){
        catch_up <- data$catch_up() %>%
          st_drop_geometry() %>%
          dplyr::select(CATCHNUM, up) 
        catch_att<- merge(catch_att, catch_up, by = "CATCHNUM", all.x = TRUE)
        is.na(catch_att$up) <- 0
      }
      
      textproj <- as.numeric(input$textproj)
      if(textproj ==102001){
        textproj<- textpj
      }
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
      y <- data$analysis_aoi() %>% st_union() 
      analysis_aoi <- st_as_sf(data.frame(
        AOI_ID = "AOI_1",
        geometry = y
      ))
      aoi <- cbind(analysis_aoi, x)
      aoi <- aoi %>% st_transform(textproj)
      planreg_sf <- planreg_sf() %>% st_transform(textproj) 
      foot_sf <- foot_sf() %>% st_transform(textproj)
      intact_sf <- intact_sf() %>% st_transform(textproj)
      catch_att <-catch_att %>% st_transform(textproj) 
      fire <- fire_sf() %>% st_transform(textproj) 
      pas_sf <- pas_sf() %>% st_transform(textproj) 
      st_write(planreg_sf, dsn=file, layer='studyarea')
      st_write(aoi, dsn=file, layer='aoi', append=TRUE) 
      st_write(foot_sf, dsn=file, layer='footprint', append=TRUE)
      st_write(intact_sf, dsn=file, layer='intactness', append=TRUE)
      st_write(catch_att, dsn=file, layer='catchments', append=TRUE)
      st_write(fire, dsn=file, layer='fires', append=TRUE)
      st_write(pas_sf, dsn=file, layer='protected_areas', append=TRUE)
    }
  )
  
}