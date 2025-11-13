selectAOIServer  <- function(input, output, session, project, map, rv){
  ################################################################################################
  ################################################################################################
  # Set AOI
  ################################################################################################
  ################################################################################################
  aoi_sf <- eventReactive({
    list(
      input$shp_aoi,
      input$gpkg_aoi,
      input$aoiLayer,
      rv$selected_catchments$catchnum,
      input$typeAOI,
      input$sourceAOI,
      input$confAOI
    )
  }, {
    req(input$typeAOI)
    
    # ------------------------------------------------------------------
    # 1. Upload AOI from shapefile
    # ------------------------------------------------------------------
    if (input$typeAOI == "uploadAOI") {
      req(input$sourceAOI)
      if(input$sourceAOI == "shpAOI"){
        req(input$shp_aoi)
        
        if (is.null(rv$layers_rv$planreg_sf)) {
          showModal(modalDialog(
            title = "Study region is missing. Please select a study region and reload your AOI.",
            easyClose = TRUE, footer = modalButton("OK")
          ))
          return(NULL)
        }
        
        check_shp(input$shp_aoi$datapath)
        aoi <- read_shp_from_upload(input$shp_aoi) %>%
          dplyr::select(any_of(c("geometry", "geom"))) %>%
          st_transform(st_crs(rv$layers_rv$planreg_sf)) %>%
          st_union() %>%
          st_sf() %>%
          st_make_valid() 
        
        # Check overlap between AOI and planreg_sf
        overlap <- suppressWarnings(st_intersects(aoi, rv$layers_rv$planreg_sf, sparse = FALSE))
        
        if (!any(overlap)) {
          showModal(modalDialog(
            title = "Warning: AOI outside study area",
            "The AOI you uploaded does not overlap with the planning region. Please upload a new AOI.",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          
          rv$layers_rv$aoi <- NULL
          return(NULL)
        }
        
        # Calculate intersection area
        clipped_aoi <- suppressWarnings(st_intersection(aoi, rv$layers_rv$planreg_sf))
        
        # Check if it was clipped (by comparing areas)
        original_area <- sum(st_area(aoi))
        clipped_area <- sum(st_area(clipped_aoi))
        
        if (clipped_area < original_area * 0.999) {  # allow tiny rounding differences
          showModal(modalDialog(
            title = "Notice: AOI clipped",
            "Part of your AOI extended beyond the planning region and has been clipped.",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          aoi <- clipped_aoi
        }
        
      # ------------------------------------------------------------------
      # 2. Upload AOI from GeoPackage
      # ------------------------------------------------------------------
      } else if (input$sourceAOI == "gpkgAOI") {
        req(input$gpkg_aoi)
        req(input$aoiLayer != "Select AOI layer")
        
        infile <- input$gpkg_aoi
        aoi <- read_gpkg_from_upload(infile$datapath, input$aoiLayer) %>%
          dplyr::select(any_of(c("geometry", "geom"))) %>%
          st_transform(st_crs(rv$layers_rv$planreg_sf)) %>%
          st_union() %>%
          st_sf() %>%
          st_make_valid() %>%
          st_intersection(rv$layers_rv$planreg_sf)
        
        # Check overlap between AOI and planreg_sf
        overlap <- suppressWarnings(st_intersects(aoi, rv$layers_rv$planreg_sf, sparse = FALSE))
        
        if (!any(overlap)) {
          showModal(modalDialog(
            title = "Warning: AOI outside study area",
            "The AOI you uploaded does not overlap with the planning region. Please upload a new AOI.",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          
          rv$layers_rv$aoi <- NULL
          return(NULL)
        }
        
        # Calculate intersection area
        clipped_aoi <- suppressWarnings(st_intersection(aoi, rv$layers_rv$planreg_sf))
        
        # Check if it was clipped (by comparing areas)
        original_area <- sum(st_area(aoi))
        clipped_area <- sum(st_area(clipped_aoi))
        
        if (clipped_area < original_area * 0.999) {  # allow tiny rounding differences
          showModal(modalDialog(
            title = "Notice: AOI clipped",
            "Part of your AOI extended beyond the planning region and has been clipped.",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          aoi <- clipped_aoi
        }
      } 
    # ------------------------------------------------------------------
    # 3. Build AOI from selected catchments
    # ------------------------------------------------------------------
    }  else if (input$typeAOI == "catchAOI") {
      req(length(rv$selected_catchments$catchnum) > 0)
      req(rv$layers_rv$planreg_sf)
      req(input$confAOI)
      
      catch_sf <- rv$layers_rv$catchments[
        rv$layers_rv$catchments$CATCHNUM %in% rv$selected_catchments$catchnum, ]
      
      aoi <- catch_sf |>
        st_union() |>
        st_buffer(dist = 20) |>
        st_buffer(dist = -20) |>
        st_sf() |>
        st_make_valid()
    } else{
      return(NULL)
    }
    
    # ------------------------------------------------------------------
    # Finalize AOI geometry
    # ------------------------------------------------------------------
    req(aoi)
    aoi <- st_set_geometry(aoi, "geometry")
    aoi$AOI_ID <- paste0("AOI_", seq_len(nrow(aoi)))
    
    rv$layers_rv$aoi_sf <- aoi
    return(aoi)
  })
  
  # Render AOI uploaded
  observeEvent(aoi_sf(),{
    req(aoi_sf())
    
    aoi_sf <- st_transform(aoi_sf(), 4326)
    legend <- c(rv$overlayBase(), "AOI")
    rv$overlayBase(legend)
    
    map_bounds1 <- aoi_sf %>% st_bbox() %>% as.character()
    leafletProxy("map") %>%
      clearGroup('Selected') %>%
      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
      addPolygons(data=aoi_sf, fill=F, color="red", weight=3, group="AOI", options = leafletOptions(pane = "ground")) %>%
      addLayersControl(baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                       overlayGroups = c(rv$overlayBase(), rv$group_names(), rv$grps()),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c(rv$group_names()))
  })
  
  observe({
    req(input$confAOI)
    
    if(input$typeAOI == "catchAOI" || input$typeAOI == "uploadAOI" && isTRUE(input$editAOI)){
      analysis_aoi <- rv$layers_rv$catchment_pr[rv$layers_rv$catchment_pr$CATCHNUM %in% rv$selected_catchments$catchnum,]
      if(nrow(analysis_aoi)>0){
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
          intact_intersection <- st_intersection(merged_sf, rv$layers_rv$intactness_sf)
          
          total_area <- as.numeric(st_area(merged_sf))
          intact_area <- as.numeric(sum(st_area(intact_intersection), na.rm = TRUE))
          intactness <- (intact_area / total_area)
          
          merged_aoi <- sf::st_sf(
            intact = intactness,
            geometry = merged_sf,
            crs = st_crs(analysis_aoi)
          )
        }
        merged_aoi$AOI_ID <- "AOI_1"
      }else{
        merged_aoi <- NULL
      }
    } else { #input$typeAOI == "uploadAOI" && isFALSE(input$editAOI)
      if(input$intactSource=="intcatch"){
        merged_aoi <- rv$layers_rv$aoi_sf
        intact_col <- input$intactColumnName
        aoi_catch_inter <- st_intersection(merged_aoi, rv$layers_rv$catchment_pr)
        aoi_catch_inter$area <- as.numeric(st_area(aoi_catch_inter))
        weighted_intact_area <- sum(aoi_catch_inter$area * (aoi_catch_inter[[intact_col]] / 100), na.rm = TRUE)
        total_aoi_area <- as.numeric(sf::st_area(merged_aoi))
        intactness_value <- (weighted_intact_area / total_aoi_area) * 100
        merged_aoi$intact <- intactness_value
      }else { #intupload
        analysis_aoi <- rv$layers_rv$aoi_sf
        intact_intersection <- st_intersection(analysis_aoi, rv$layers_rv$intactness_sf)
        
        total_area <- as.numeric(st_area(analysis_aoi))
        intact_area <- as.numeric(sum(st_area(intact_intersection), na.rm = TRUE))
        intactness <- (intact_area / total_area) * 100
        
        merged_aoi <- sf::st_sf(
          intact = intactness,
          geometry = analysis_aoi$geometry,
          crs = st_crs(analysis_aoi)
        )
      }
      merged_aoi$AOI_ID <- "AOI_1"
    }
    rv$layers_rv$analysis_aoi <- merged_aoi
    return(merged_aoi)
  })
  
  #Catchments within AOI
  catch_aoi <- reactive({
    catch_pr <- st_make_valid(rv$layers_rv$catchment_pr)
    if(input$typeAOI == "uploadAOI"){
      catch_sf <- st_filter(catch_pr, rv$layers_rv$aoi_sf, .predicate = st_intersects)
      catch_sf$Area_total <- as.numeric(st_area(catch_sf))
      catch_sf$AOI_ID <- "AOI_1"
      rv$selected_catchments$catchnum <- catch_sf$CATCHNUM
    }else{
      catch_sf <- catch_pr[catch_pr$CATCHNUM %in% rv$selected_catchments$catchnum,]
      catch_sf$AOI_ID <- "AOI_1"
    }
    return(catch_sf)
  })
  
  ####################################################################################################
  # TRACK CATCHMENTS
  ####################################################################################################
  # Track a list of which catchnums have been selected
  observeEvent(input$map_shape_click,{
    req(input$tabs == "selectAOI")
    
    clickId <- input$map_shape_click$id # id is the layerId assigned to the polygon layer in Leaflet
    if(clickId %in% rv$selected_catchments$catchnum){
      rv$selected_catchments$catchnum <- rv$selected_catchments$catchnum[!rv$selected_catchments$catchnum %in% clickId]
    } else{
      rv$selected_catchments$catchnum <- c(rv$selected_catchments$catchnum, clickId)
    }
  })
  # If clear selection button is pressed, remove all catchnums from list
  observeEvent(input$clear_button, {
    rv$selected_catchments$catchnum <- c()
    leafletProxy("map") %>%
      clearGroup("Catchments AOI")
  })
  
  ####################################################################################################
  # Map viewer - AOI
  ####################################################################################################
  # Render selected catchments
  observe({
    req(rv$layers_rv$catchment_pr)
    
    catchments <- rv$layers_rv$catchment_pr

    data_selected <- catchments[catchments$CATCHNUM %in% rv$selected_catchments$catchnum,]
    data_select <- st_transform(data_selected, 4326)
    pop = ~paste("CATCHNUM:", CATCHNUM, "<br>Area (km²):", round(Area_Total/1000000,1), "<br>Intactness (%):", intact*100 )
    
    if(nrow(data_select)>0){
      leafletProxy("map") %>%
        clearGroup("Catchments AOI") %>%
        addPolygons(data = data_select, color='black', weight = 1, fillColor = "grey", fillOpacity = 0.7,
                    layerId = data_select$CATCHNUM, group = "Catchments AOI") 
    }
  })
  

  # Render catchments to select AOI
  observe({
    req(rv$layers_rv$catchments)
    req(input$typeAOI)
    
    if(input$typeAOI == "catchAOI"){
      leafletProxy("map") %>%
        showGroup("Catchments") %>%
        hideGroup(c("Streams", rv$group_names())) %>%
        clearControls()  %>%
        addControl(actionButton(inputId = "clear_button", label = "Clear selection"), position="topleft", className = "class_clear_button")
    }else{
      leafletProxy("map") %>%
        clearControls()  %>%
        hideGroup(c("Streams", "Catchments", rv$group_names()))
    }
  })
  
  # Render AOI editing
  observeEvent(input$editAOI,{
    req(input$editAOI)
    req(rv$layers_rv$aoi_sf)
    
    aoi <- st_transform(rv$layers_rv$aoi_sf, 4326)
    catch_aoi <- st_transform(catch_aoi(), 4326)
    
    map_bounds1 <- aoi %>% st_bbox() %>% as.character()
    map <- leafletProxy("map") %>%
      clearGroup('AOI') %>%
      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
      addPolygons(data=aoi, fill=F, color="black", weight=3, group="AOI", options = leafletOptions(pane = "ground")) %>%
      addLayersControl(baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                       overlayGroups = c(rv$overlayBase(), rv$group_names(), rv$grps()),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c(rv$group_names()))
  })
  
  # Render AOI confirmed
  observeEvent(input$confAOI, {
    req(rv$layers_rv$analysis_aoi)
    
    showModal(modalDialog(
      title = "Generating AOI. Please wait.",
      easyClose = TRUE,
      footer = modalButton("OK")))
    
    aoi <- st_transform(aoi_sf(), 4326)
    analysis_aoi <- rv$layers_rv$analysis_aoi %>% 
      #st_union() %>% 
      st_buffer(dist = 20) %>% 
      st_buffer(dist = -20) %>%
      st_transform(4326)
    map_bounds1 <- analysis_aoi %>% st_bbox() %>% as.character()
    
    legend <- c(rv$overlayBase(), "Analysis AOI")
    rv$overlayBase(legend)
    hidegroup <- c(rv$group_names(), "AOI")
    rv$group_names(hidegroup)
    
    map <- leafletProxy("map") %>%
      clearGroup('Catchments AOI') %>%
      clearGroup('AOI') %>%
      clearGroup('Analysis AOI') %>%
      clearGroup('Selected') %>%
      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
      addPolygons(data=analysis_aoi, fill = F, color="red", weight=4, group="Analysis AOI", options = leafletOptions(pane = "ground")) %>%
      addPolygons(data=aoi, fill=F, color="black", weight=3, group="AOI", options = leafletOptions(pane = "ground")) %>%
      addLayersControl(baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                       overlayGroups = c(rv$overlayBase(),  rv$group_names(), rv$group_names(), rv$grps()),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup( rv$group_names())
    showModal(modalDialog(
      title = "Analysis AOI displayed.",
      easyClose = TRUE,
      footer = modalButton("OK")))
  })
  
  ################################################################################################
  # Stats
  ################################################################################################
  observeEvent(input$confAOI, {
    req(rv$layers_rv$analysis_aoi)
    if(input$confAOI==0 && is.null(input$upload_aoi)){
      return()
    }
    
    x <- tibble(Variables=c("Study area", 
                            "Study area intactness",
                            "Analysis AOI", 
                            "Analysis AOI intactness"), 
                Area_km2= NA_real_,
                Percent = NA_real_)
    
    x <- x %>% 
      mutate(Area_km2 = case_when(Variables == "Study area" ~  round(as.numeric(st_area(rv$layers_rv$planreg_sf)/1000000,0)),
                                  Variables == "Study area intactness" ~ round(as.numeric(st_area(st_union(rv$layers_rv$intactness_sf)))/1000000,0)),
             Percent= case_when(Variables == "Study area" ~  100,
                                Variables == "Study area intactness" ~  round(as.numeric(st_area(st_union(rv$layers_rv$intactness_sf)))/as.numeric(st_area(rv$layers_rv$planreg_sf))*100,2)),
             Area_km2 = case_when(Variables == "Analysis AOI" ~  round(as.numeric(st_area(st_union(rv$layers_rv$analysis_aoi))/1000000,0)), 
                                  TRUE ~ Area_km2),
             Percent= case_when(Variables == "Analysis AOI" ~ round(as.numeric(st_area(st_union(rv$layers_rv$analysis_aoi))/st_area(rv$layers_rv$planreg_sf)*100,2)),
                                Variables == "Analysis AOI intactness" ~ round(as.numeric(sum(rv$layers_rv$analysis_aoi$intact*st_area(rv$layers_rv$analysis_aoi)))/as.numeric(st_area(st_union(rv$layers_rv$analysis_aoi)))*100,2),
                                TRUE ~ Percent)
      )
    rv$outtab1(x)
    
    #Fire
    if(!is.null(rv$layers_rv$fires)){
      fire_aoi <-st_intersection(rv$layers_rv$analysis_aoi, rv$layers_rv$fires)
      y <- rv$outfiretab()
      
      new_firerows <- tibble(Variables=c("Within Analysis AOI"), 
                             Area_Burned_km2= NA_real_,
                             `Area_Burned_%` = NA_real_)
      
      y <- y %>% dplyr::filter(!Variables %in% new_firerows$Variables)
      y <- dplyr::bind_rows(y, new_firerows)
      
      if(nrow(fire_aoi)>0){
        y <- y %>% 
          mutate(Area_Burned_km2 = case_when(Variables == "Within Analysis AOI" ~  round(as.numeric(sum(st_area(fire_aoi))/1000000,2)), 
                                             TRUE ~ Area_Burned_km2),
                 `Area_Burned_%`= case_when(Variables == "Within Analysis AOI" ~ round(as.numeric(sum(st_area(fire_aoi))/sum(st_area(rv$layers_rv$analysis_aoi)))*100,2),
                                            TRUE ~ `Area_Burned_%`))
      }else{
        y <- y %>% 
          mutate(Area_Burned_km2 = case_when(Variables == "Within Analysis AOI" ~  0, 
                                             TRUE ~ Area_Burned_km2),
                 `Area_Burned_%`= case_when(Variables == "Within Analysis AOI" ~  0, 
                                            TRUE ~ `Area_Burned_%`)
          )
      }
      rv$outfiretab(y)
    }
    
    ## DCI
    aoi_sf <- rv$layers_rv$analysis_aoi %>%
      st_union() %>%
      st_as_sf("POLYGON") %>%
      mutate(network = "AOI_1") %>%
      rename(geometry = x)
      
    aoi_sf$DCI <- round(calc_dci(aoi_sf, rv$layers_rv$stream_sf),2)
    aoi_var <- aoi_sf %>%
      st_drop_geometry() %>%
      dplyr::select(network, DCI)

    DCI_score <- case_when(aoi_var[,"DCI"]>=0.9 ~ "Very High",
                           aoi_var[,"DCI"]>=0.8 & aoi_var[,"DCI"]<0.9 ~ "High",
                           aoi_var[,"DCI"]>=0.7 & aoi_var[,"DCI"]<0.8 ~ "Moderate",
                           aoi_var[,"DCI"]<0.7 ~ "Low"
    )
    
    i <- tibble(Variable=c("DCI"), Metric=NA, Rating = NA)
    i$Metric[i$Variable=="DCI"] <- aoi_var[,"DCI"]
    i$Rating[i$Variable=="DCI"] <- DCI_score
    colnames(i) <- c("", "Metric","Rating")
      
    rv$outputDCI(i)
    
    # Summary stats
    z <- tibble(Variables=c("Fires within the study area", "Fires within the Analysis AOI"), 
                Area_km2= y$Area_Burned_km2, 
                Percent = y$`Area_Burned_%`)
    
    space_out <- tibble(Variables=c(NA,NA),
                      Area_km2= c(NA, "Metric"), 
                      Percent = c(NA,"Rating"))
    dci_out <- tibble(Variables= "DCI score", 
                           Area_km2= i$Metric, 
                           Percent = i$Rating)
    out <-rbind(x, z, space_out, dci_out)
    
    rv$outputsumStats(out)
  })
}
  