runHydroServer  <- function(input, output, session, project, map, rv){
  
  ####################################################################################################
  #UPSTREAM SECTION 
  catch_up <- eventReactive(input$confAnalysis, {
    if(is.null(rv$layers_rv$aoi_sf))
    { # show pop-up ...
      showModal(modalDialog(
        title = "Area of Interest is missing. Please select an Area of Interest.",
        easyClose = TRUE,
        footer =modalButton("OK")))
    }
    req(rv$layers_rv$analysis_aoi)
    
    if(input$typeAOI == "uploadAOI"){
      if(isFALSE(input$editAOI)){
        # Upload AOI as it is
        aoi_stream <- st_filter(rv$layers_rv$stream_sf, rv$layers_rv$analysis_aoi, .predicate = st_intersects)
        catch_stream <- rv$layers_rv$catchment_pr[rv$layers_rv$catchment_pr$SKELUID %in% aoi_stream$SKELUID,]
        #browser()
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
        start_inside <- lengths(st_intersects(start_points, rv$layers_rv$analysis_aoi)) > 0
        end_inside   <- lengths(st_intersects(end_points, rv$layers_rv$analysis_aoi)) > 0
        streams_end_only <- aoi_stream[end_inside & !start_inside, ]
        streams_both_outside <- aoi_stream[!start_inside & !end_inside, ]
        #catch_up_bits <- rv$layers_rv$catchment_pr[rv$layers_rv$catchment_pr$SKELUID %in% streams_end_only$SKELUID,]
        catch_up_bits <- rv$layers_rv$catchment_pr[rv$layers_rv$catchment_pr$SKELUID %in%
                                                     union(streams_end_only$SKELUID, streams_both_outside$SKELUID), ]
        
        catch_stream$AOI_ID <- "AOI_1"
        upstream_list <- get_upstream_catchments(catch_stream, "AOI_ID", rv$layers_rv$catchment_pr)
      } else{
        # Upload AOI , but use intersection catchment
        upstream_list <- get_upstream_catchments(rv$layers_rv$analysis_aoi, "AOI_ID", rv$layers_rv$catchment_pr)
        catch_up_bits <- NULL
      }
    }else{
      # Use intersecting catchments
      upstream_list <- get_upstream_catchments(rv$layers_rv$analysis_aoi, "AOI_ID", rv$layers_rv$catchment_pr)
      catch_up_bits <- NULL
    } 
    # Tabulate dist area per catchment within the upstream area
    if(nrow(upstream_list)==0 && is.null(catch_up_bits) )
    {# show pop-up ...
      showModal(modalDialog(
        title = "No upstream catchments found!",
        easyClose = TRUE,
        footer = modalButton("OK")))
      data$catch_up(NULL)
    }else if (input$intactSource =='intcatch'){
      catch_up <- rv$layers_rv$catchment_pr[rv$layers_rv$catchment_pr$CATCHNUM %in% upstream_list$AOI_1,]
      catch_up <- rbind(catch_up_bits, catch_up)
      catch_up <- catch_up %>%
        st_difference(st_union(rv$layers_rv$analysis_aoi))
    }else{
      # subset catchments
      catch_up <- rv$layers_rv$catchment_pr[rv$layers_rv$catchment_pr$CATCHNUM %in% upstream_list$AOI_1,]
      catch_up_bits <- catch_up_bits[!catch_up_bits$CATCHNUM %in% catch_up$CATCHNUM, ]
      catch_up <- rbind(catch_up_bits, catch_up)
      
      # Clip intactness layer to those catchments
      intact <- st_intersection(rv$layers_rv$intactness_sf, catch_up)
      
      # Compute intact area per catchment
      intArea <- intact %>%
        mutate(area = st_area(.) %>% as.numeric()) %>%
        st_drop_geometry() %>%                           
        dplyr::select(CATCHNUM, area) %>%            
        group_by(CATCHNUM) %>%                           
        summarise(area_int = sum(area, na.rm = TRUE)) %>% 
        ungroup()
        
      # Join results back to catchment polygons
      catch_up <- catch_up |>
        left_join(intArea, by = "CATCHNUM") |>
        mutate(area_int = ifelse(is.na(area_int), 0, area_int))
      
      # Remove AOI overlap and keep valid polygons
      catch_up <- catch_up |>
        st_difference(st_union(rv$layers_rv$analysis_aoi)) |>
        st_cast("MULTIPOLYGON")
      
      # Compute intact ratio
      catch_up <- catch_up |>
        mutate(intact = round(area_int / Area_Total, 3))
    }
    catch_up <- catch_up %>%
      mutate(up =1,
             Area_Total = as.numeric(st_area(geom)))
    # Return
    rv$layers_rv$catch_up <- catch_up
    return(catch_up)
  })
  
  ####################################################################################################
  # DOWNSTREAM STEM SECTION
  catch_stem <- eventReactive(input$confAnalysis, {
    #browser()
    if(input$typeAOI == "uploadAOI"){
      if(isFALSE(input$editAOI)){
        # downstream stem polygon
        downstream_stem_list <- get_downstream_catchments(rv$layers_rv$analysis_aoi, "AOI_ID", rv$layers_rv$catchment_pr)
        
        if(nrow(downstream_stem_list)==0)
        {# show pop-up ...
          showModal(modalDialog(
            title = "No downstream stem catchments found!",
            easyClose = TRUE,
            footer = modalButton("OK")))
        }else{
          #catch_stem <- rv$layers_rv$catchment_pr[rv$layers_rv$catchment_pr$CATCHNUM %in% downstream_stem_list$AOI_1, drop = FALSE]
          aoi_stream <- st_filter(rv$layers_rv$stream_sf, rv$layers_rv$analysis_aoi, .predicate = st_intersects)
          catch_stream <- rv$layers_rv$catchment_pr[rv$layers_rv$catchment_pr$SKELUID %in% aoi_stream$SKELUID,]
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
          start_inside <- lengths(st_intersects(start_points, rv$layers_rv$analysis_aoi)) > 0
          end_inside   <- lengths(st_intersects(end_points, rv$layers_rv$analysis_aoi)) > 0
          
          streams_start_only <- aoi_stream[start_inside & !end_inside, ]
          catch_stem_bits <- rv$layers_rv$catchment_pr[rv$layers_rv$catchment_pr$SKELUID %in% streams_start_only$SKELUID,]
        }
      }else{
        downstream_stem_list <- get_downstream_catchments(rv$layers_rv$analysis_aoi, "AOI_ID", rv$layers_rv$catchment_pr)
        catch_stem_bits <- NULL
      }
    } else{
      downstream_stem_list <- get_downstream_catchments(rv$layers_rv$analysis_aoi, "AOI_ID", rv$layers_rv$catchment_pr)
      catch_stem_bits <- NULL
    } 
    if(nrow(downstream_stem_list)==0 && is.null(catch_stem_bits) )
    {# show pop-up ...
      showModal(modalDialog(
        title = "No downstream catchments found!",
        easyClose = TRUE,
        footer = modalButton("OK")))
      data$catch_stem(NULL)
    }else if (input$intactSource =='intcatch'){
      catch_stem <- rv$layers_rv$catchment_pr[rv$layers_rv$catchment_pr$CATCHNUM %in% downstream_stem_list$AOI_1,]
      catch_stem_all <- rbind(catch_stem_bits, catch_stem)
      catch_stem <- catch_stem_all %>%
        st_difference(st_union(rv$layers_rv$analysis_aoi))
    }else{
      # subset catchments
      catch_stem <- rv$layers_rv$catchment_pr[rv$layers_rv$catchment_pr$CATCHNUM %in% downstream_stem_list$AOI_1,]
      catch_stem_bits <- catch_stem_bits[!catch_stem_bits$CATCHNUM %in% catch_stem$CATCHNUM, ]
      catch_stem <- rbind(catch_stem_bits, catch_stem)
      
      # Remove AOI overlap and keep valid polygons
      catch_stem <- catch_stem |>
        st_difference(st_union(rv$layers_rv$analysis_aoi)) |>
        st_cast("MULTIPOLYGON")

      # Clip intactness layer to those catchments
      intact <- st_intersection(rv$layers_rv$intactness_sf, catch_stem)
      
      # Compute intact area per catchment
      intArea <- intact %>%
        mutate(area = st_area(.) %>% as.numeric()) %>%
        st_drop_geometry() %>%                           
        dplyr::select(CATCHNUM, area) %>%            
        group_by(CATCHNUM) %>%                           
        summarise(area_int = sum(area, na.rm = TRUE)) %>% 
        ungroup()
      
      # Join results back to catchment polygons
      catch_stem <- catch_stem %>%
        left_join(intArea, by = "CATCHNUM") %>%
        mutate(area_int = ifelse(is.na(area_int), 0, area_int))
      
      # Compute intact ratio
      catch_stem <- catch_stem %>%
        mutate(intact = round(area_int / as.numeric(st_area(catch_stem)), 3))
    }
    catch_stem <- catch_stem %>%
      mutate(stem =1,
             Area_Total = as.numeric(st_area(geom)))
    rv$layers_rv$catch_stem <- catch_stem
    return(catch_stem)
  })
  
  # DOWNSTREAM SECTION
  catch_down <- eventReactive(input$confAnalysis, {
    # Extract downstream stem and upstream from downstream stem
    req(catch_stem())
    stem_sf <- rv$layers_rv$catch_stem %>% st_union() %>% st_as_sf() 
    stem_sf$AOI_ID <- "AOI_1"
    upstem_list <- get_upstream_catchments(stem_sf, "AOI_ID", rv$layers_rv$catchment_pr)

    #Merge stem catchment with their related upstream catchments
    catchList <- c()
    catchList <- c(rv$layers_rv$catch_stem$CATCHNUM,upstem_list$AOI_1)
    catchList <- catchList %>%
      unique() %>%
      subset(!. %in% rv$layers_rv$catch_up$CATCHNUM)  #drop catchments within the upstream
    
    if (input$intactSource =='intcatch'){
      catch_down<- rv$layers_rv$catchment_pr[rv$layers_rv$catchment_pr$CATCHNUM %in% catchList, drop = FALSE]
      catch_down <- catch_down %>%
        st_difference(st_union(rv$layers_rv$analysis_aoi))
    }else {
      # subset catchments
      catch_down <- rv$layers_rv$catchment_pr[rv$layers_rv$catchment_pr$CATCHNUM %in% catchList, drop = FALSE]
      # Remove AOI overlap and keep valid polygons
      catch_down <- catch_down %>%
        st_difference(st_union(rv$layers_rv$analysis_aoi)) %>%
        st_cast("MULTIPOLYGON")
      # Clip intactness layer to those catchments
      intact <- st_intersection(rv$layers_rv$intactness_sf, catch_down)
      # Compute intact area per catchment
      intArea <- intact %>%
        mutate(area = st_area(.) %>% as.numeric()) %>%
        st_drop_geometry() %>%                           
        dplyr::select(CATCHNUM, area) %>%            
        group_by(CATCHNUM) %>%                           
        summarise(area_int = sum(area, na.rm = TRUE)) %>% 
        ungroup()
      # Join results back to catchment polygons
      catch_down <- catch_down %>%
        left_join(intArea, by = "CATCHNUM") %>%
        mutate(area_int = ifelse(is.na(area_int), 0, area_int))

      
      # Compute intact ratio
      catch_down <- catch_down %>%
        mutate(intact = round(area_int / Area_Total, 3))
    }
    catch_down <- catch_down %>%
      mutate(down =1,
             Area_Total = as.numeric(st_area(geom)))
    rv$layers_rv$catch_down <- catch_down
    return(catch_down)
  })
  
  ####################################################################################################
  # Map viewer - Upstream downstream analysis
  ####################################################################################################
  # Render Upstream / downstream
  observeEvent(input$confAnalysis, {
    #req(input$confAnalysis)
    
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
    
    catch_stem <- st_transform(catch_stem(), 4326)  
    catch_down <- st_transform(catch_down(), 4326) 
    
    
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
    rv$overlayHydro(legend)
    
    map_bounds1 <- rv$layers_rv$planreg_sf %>% st_transform(4326) %>% st_bbox() %>% as.character()
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
                       overlayGroups = c(rv$overlayBase(), rv$group_names(), rv$grps(), rv$overlayHydro()),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(c("Catchemnts","Downstream area", rv$group_names()))
    if(nrow(catch_up)>0){
      map <- leafletProxy("map") %>%
        addPolygons(data=catch_up, color=~pal(intact), stroke=F, fillOpacity=0.8, group="Upstream area", options = leafletOptions(pane = "ground")) %>%
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c(rv$overlayBase(), rv$group_names() , rv$grps(), rv$overlayHydro(), "Upstream area"),
                         options = layersControlOptions(collapsed = TRUE)) %>%
        hideGroup(c("Catchemnts", "Downstream area", rv$group_names()))
    } 
  })
  
  #Update with Upstream/Downstream stats
  observeEvent(input$confAnalysis, {
    
    upstream_area <- catch_up()
    downstream_stem_int  <- catch_stem()
    downstream_int <- catch_down()
    
    x <- rv$outtab1()
    new_rows  <- tibble(Variables=c("Upstream area",
                                    "Downstream stem area",
                                    "Downstream area",
                                    "Upstream mean AWI*",
                                    "Downstream stem mean AWI*",
                                    "Downstream mean AWI*" ), 
                        Area_km2= NA_real_,
                        Percent = NA_real_)
    
    x <- x %>% dplyr::filter(!Variables %in% new_rows$Variables)
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
    rv$outtab1(x)
    
    #FIRE
    if(!is.null(rv$layers_rv$fires)){
      upstream_fire <- upstream_area %>%
        st_union() %>%
        st_intersection(rv$layers_rv$fires)
      
      downstream_stem_fire <- downstream_stem_int %>%
        st_union() %>%
        st_intersection(rv$layers_rv$fires)
      
      downstream_fire <- downstream_int %>%
        st_union() %>%
        st_intersection(rv$layers_rv$fires)
      
      y <- rv$outfiretab()
      new_rows  <- tibble(Variables=c("Within upstream area",
                                      "Within downstream stem area",
                                      "Within overall downstream area"), 
                          Area_Burned_km2= NA_real_,
                          `Area_Burned_%` = NA_real_)
      y <- y %>% dplyr::filter(!Variables %in% new_rows$Variables)
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
      rv$outfiretab(y)
    } 
  })
}