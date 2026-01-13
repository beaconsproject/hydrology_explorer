setIntactServer <- function(input, output, session, project, map, rv){
  
  intactUI_static <- function() {
    tagList(
      
      # ---- Intactness header ----
      div(style = "margin: 15px; font-size:17px; font-weight:bold",
          "Select source of intactness"),
      
      div(style = "margin-left:12px; font-size:12px;",
          "Intactness identifies areas without a visible human footprint (e.g., road, mine site) and is used as a proxy to assess the overall ecological integrity of a catchment e.g., 0-100% intact or low-high ecological integrity."),
      
      # ---- Intact source (always shown) ----
      radioButtons("intactSource", "",  choices = if(is.null(rv$layers_rv$undisturbed)) {c("Value in catchment dataset" = "intcatch",
                                                                                           "Upload intactness layer" = "intupload")
                                                  } else {c("Value in catchment dataset" = "intcatch",
                                                            "Use existing undisturbed layer" = "intIncluded",
                                                            "Upload intactness layer" = "intupload")
                                                  }, selected = "intcatch"),
      
      conditionalPanel("input.intactSource == 'intcatch'",
        selectInput("intactColumnName",  "Catchment dataset – select intactness attribute", choices = NULL)
      ),
      
      conditionalPanel("input.intactSource == 'intupload'",
        radioButtons("intactformat", "Select intactness file format:", choices = c("Shapefile" = "intshp", 
                                                                                   "GeoPackage" = "intgpkg"), selected = character(0), inline = TRUE),
        fileInput("upload_intact", "Upload undisturbed layer", multiple = TRUE, accept = c(".shp",".dbf",".shx",".prj",".cpg",".gpkg")),
        
        conditionalPanel("input.intactformat == 'intgpkg'",
          selectInput("intactLayer", "Select intactness layer", choices = NULL)
        )
      ),
      tags$hr(),
      
      # ---- Fire section  ----
      radioButtons("firesSource", "Select source for fire (optional):", choices = if(is.null(rv$layers_rv$undisturbed)) {c("Upload fire layer" = "fireupload",
                                                                                                                           "No fire" = "nofire")
                                                                                  } else {c("Upload fire layer" = "fireupload",
                                                                                            "Use existing fire layer" = "fireIncluded")
                                                                                  }, selected = "nofire"),
      
      conditionalPanel("input.firesSource == 'fireupload'",
        radioButtons("fireformat", "Select fire file format:", choices = c("Shapefile" = "fireshp", 
                                                                           "GeoPackage" = "firegpkg"), selected = character(0), inline = TRUE),
        fileInput("upload_fire", "Upload fire layer", multiple = TRUE, accept = c(".shp",".dbf",".shx",".prj",".cpg",".gpkg")),
        
        conditionalPanel("input.fireformat == 'firegpkg'",
          selectInput("fireLayer", "Select fire layer", choices = NULL))
      ),
      br(),
      actionButton("confIntact","Confirm", icon = icon("map-location-dot", lib = "font-awesome"),  class = "btn-warning", style = "width:250px")
    )
  }
  
  output$intactUI <- renderUI({
    intactUI_static()
  })

  observe({
    req(input$intactSource == 'intcatch')
    req(rv$layers_rv$catchments)
    updateSelectInput(session = getDefaultReactiveDomain(), "intactColumnName", choices = colnames(rv$layers_rv$catchments), selected= colnames(rv$layers_rv$catchments)[1])
  })
  
  observe({
    req(input$upload_intact)
    req(input$intactformat == 'intgpkg')
    file <- input$upload_intact$datapath
    layers <- st_layers(file)$name
    updateSelectInput(session = getDefaultReactiveDomain(), "intactLayer", choices = c("Select a layer", layers))
  })
  
  observe({
    req(input$upload_fire)
    req(input$fireformat == 'firegpkg')
    file <- input$upload_fire$datapath
    layers <- st_layers(file)$name
    updateSelectInput(session = getDefaultReactiveDomain(), "fireLayer", choices = c("Select a layer", layers))
  })
  
  # Set intactness
  intactness_sf <- reactive({
    i <- NULL
    if(input$intactSource == 'intIncluded'){
      i <- rv$layers_rv$undisturbed %>%
        dplyr::select(any_of(c("geometry", "geom")))
    }else if (input$intactSource == 'intupload'){
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
    }else{
      return(NULL)
    }
    rv$layers_rv$intactness_sf <- i
    return(i)
  })
  
  # Add intact to catchments
  observe({
    req(rv$layers_rv$planreg_sf)
    req(rv$layers_rv$catchments)
    req(input$intactSource)
    
    if(input$intactSource =='intcatch'){
      req(input$intactColumnName)  # Ensure the textInput value is available
      intact_column <- input$intactColumnName  # Get the column name from the text input
      catch_int <- st_intersects(st_centroid(rv$layers_rv$catchments), rv$layers_rv$planreg_sf, sparse = FALSE)
      catchment <- rv$layers_rv$catchments[catch_int,]
      #catchment <- rv$layers_rv$catchments
      catchment$intact <- catchment[[intact_column]]  # Dynamically access the specified column
      catchment$area_intact <- catchment$Area_total * catchment$intact
    }else{
      catch_int <- st_intersects(st_centroid(rv$layers_rv$catchments), rv$layers_rv$planreg_sf, sparse = FALSE)
      catchments <- rv$layers_rv$catchments[catch_int,]
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
      catchments$intact <- as.numeric(round(catchments$area_intact/st_area(catchments), 3))
      catchment <- st_cast(catchments, "MULTIPOLYGON")
    }
    rv$layers_rv$catchment_pr <- catchment
  }) 
  
  # Set fire
  fire_sf <- eventReactive(input$confIntact,{
    i <- NULL
    
    if(input$firesSource == 'fireIncluded'){
      i <- rv$layers_rv$fires
    }else if (input$firesSource == "fireupload"){
      req(input$fireformat)
      infile <- input$upload_fire
      if(input$fireformat == 'firegpkg'){
        req(input$fireLayer)
        if(input$fireLayer != "Select fire layer"){
          i <- read_gpkg_from_upload(infile$datapath, input$fireLayer) %>%
            dplyr::select(any_of(c("geometry", "geom"))) %>%
            suppressWarnings() %>%
            st_cast('MULTIPOLYGON') %>% 
            st_zm(drop = TRUE, what = "ZM")  %>%
            mutate(area_ha = as.numeric(st_area(geom)/10000))
          current_groups <- rv$group_names()  
          if (!("Fires" %in% current_groups)) {  
            updated_groups <- c(current_groups, "Fires")  
            rv$group_names(updated_groups)  
          }
        }
      }else{
        check_shp(infile$datapath)
        
        i <- read_shp_from_upload(input$upload_fire) %>%
          dplyr::select(any_of(c("geometry", "geom"))) %>%
          suppressWarnings() %>%
          st_cast('MULTIPOLYGON') %>% 
          st_zm(drop = TRUE, what = "ZM")  %>%
          { 
            geom_col <- attr(., "sf_column")   # get current geometry column name
            mutate(., area_ha = as.numeric(st_area(.data[[geom_col]]) / 10000))
          }
        current_groups <- rv$group_names()  
        if (!("Fires" %in% current_groups)) {  
          updated_groups <- c(current_groups, "Fires")  
          rv$group_names(updated_groups)  
        }
      }
    }else{
      return(NULL)
    }
    geom_idx <- which(names(i) == attr(i, "sf_column"))
    names(i)[geom_idx] <- "geom"
    st_geometry(i) <- "geom"
    rv$layers_rv$fires <- i
    return(i)
  })
  
  ####################################################################################################
  # Map viewer - fires and intactness
  ####################################################################################################
  observeEvent(input$confIntact,{ 
    #browser()
    showModal(modalDialog(
      title = "Mapping fires and intactness",
      easyClose = TRUE,
      footer = modalButton("OK")))
    
    leafletProxy("map") %>%
      clearGroup('Catchments') %>%
      clearGroup('Intactness') %>%
      clearGroup('Fires') 
    
    catch <- rv$layers_rv$catchment_pr %>% st_transform(4326)
    pop = ~paste("CATCHNUM:", CATCHNUM, "<br>Area (km²):", round(Area_total/1000000,1), "<br>Intactness (%):", intact*100 )
    if (isMappable(rv$layers_rv$intactness_sf)) { 
      intact <- st_transform(rv$layers_rv$intactness_sf, 4326)
      leafletProxy("map") %>% addPolygons(data=intact, color='blue', fill = T, fillOpacity = 0.2, weight=0, group='Intactness', options = leafletOptions(pane = "ground"))
      leafletProxy("map") %>% addPolygons(data=catch, color='black', fillColor = "grey", fillOpacity = 0, weight=1, layerId = ~CATCHNUM, popup = pop, group="Catchments", options = leafletOptions(pane = "over"))
      overlay <- c(rv$overlayBase(), "Intactness")
      rv$overlayBase(overlay)
    } else {
      leafletProxy("map") %>% addPolygons(data=catch, color='black', fillColor = "grey", fillOpacity = 0, weight=1, layerId = ~CATCHNUM, popup = pop, group="Catchments", options = leafletOptions(pane = "over"))
    }
    
    fires <- isolate(fire_sf())
    if(!is.null(fires)){
      fires <- st_transform(fires, 4326)
      leafletProxy("map") %>% addPolygons(data=fires, fill=T, stroke=F, fillColor="#996633", fillOpacity=0.8, group="Fires", options = leafletOptions(pane = "ground")) 
    }
    
    leafletProxy("map") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                       overlayGroups = c(rv$overlayBase(), rv$group_names()),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Streams", "Catchments", rv$group_names()))
    
    removeModal()
    
    
    # Update stats
    x <- tibble(Variables=c("Study area", 
                            "Study area intactness"), 
                Area_km2= NA_real_,
                Percent = NA_real_)
    
    if(!is.null(rv$layers_rv$intactness_sf)){
      x <- x %>% 
        mutate(Area_km2 = case_when(Variables == "Study area" ~  round(as.numeric(st_area(rv$layers_rv$planreg_sf)/1000000,0)),
                                    Variables == "Study area intactness" ~ round(as.numeric(st_area(st_union(rv$layers_rv$intactness_sf)))/1000000,0)),
               Percent= case_when(Variables == "Study area" ~  100,
                                  Variables == "Study area intactness" ~  round(as.numeric(st_area(st_union(rv$layers_rv$intactness_sf)))/as.numeric(st_area(rv$layers_rv$planreg_sf))*100,2))
        ) 
    } else {
      x <- x %>% 
        mutate(Area_km2 = case_when(Variables == "Study area" ~  round(as.numeric(st_area(rv$layers_rv$planreg_sf)/1000000,0)),
                                    Variables == "Study area intactness" ~ round(as.numeric(sum(rv$layers_rv$catchment_pr$area_int)/1000000,0))),
               Percent= case_when(Variables == "Study area" ~  100,
                                  Variables == "Study area intactness" ~  round(as.numeric(sum(rv$layers_rv$catchment_pr$area_int)/as.numeric(st_area(rv$layers_rv$planreg_sf)))*100,2))
        ) 
    }
    
    
    
    rv$outAOI(x)
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
  })
}
  