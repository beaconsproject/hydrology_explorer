setIntactServer <- function(input, output, session, project, map, rv){
  
  observe({
    req(input$tabs == "tabIntact")  # Trigger when "Set intactness" 
    
    missing_inputs <- any(
      is.null(rv$layers_rv$planreg_sf),
      is.null(rv$layers_rv$streams_sf),
      is.null(rv$layers_rv$catchments)
    )
    
    if (missing_inputs) {# Check if input is unset or NULL
      showModal(modalDialog(
        title = "Missing input parameters",
        "Please set input parameters prior to set intactness.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  
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
                                                  }, selected = "intIncluded"),
      
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
  

  # Set intactness
  intactness_sf <- reactive({
    i <- NULL
    if (input$intactSource == 'intupload'){
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
    }else if(!is.null(rv$layers_rv$undisturbed)){
      i <- rv$layers_rv$undisturbed %>%
        dplyr::select(any_of(c("geometry", "geom")))
    }else {
      return(NULL)
    }
    
    if(!is.null(i)){
      if (st_crs(i) != st_crs(rv$layers_rv$planreg_sf)) {
        i <- st_transform(i, st_crs(rv$layers_rv$planreg_sf))
      }
    }
    i <- st_make_valid(i)
    rv$layers_rv$intactness_sf <- i
    return(i)
  })
  
  # Add intact to catchments
  observe({
    req(input$confIntact)
    req(rv$layers_rv$planreg_sf)
    req(rv$layers_rv$catchments)
    req(input$intactSource)
    req(intactness_sf())
    
    if(input$intactSource =='intcatch'){
      req(input$intactColumnName)  # Ensure the textInput value is available
      intact_column <- input$intactColumnName  # Get the column name from the text input
      #catch_int <- st_intersects(st_centroid(rv$layers_rv$catchments), rv$layers_rv$planreg_sf, sparse = FALSE)
      #catchment <- rv$layers_rv$catchments[catch_int,]
      catchment <- st_intersection(rv$layers_rv$catchments, rv$layers_rv$planreg_sf) |>
        st_make_valid() |>
        st_collection_extract("POLYGON") |>
        st_cast("MULTIPOLYGON")
      
      # Test on Column type 
      if (!is.numeric(catchment[[intact_column]])) {
        showModal(modalDialog(
          title = "Invalid intactness attribute type",
          paste0("The column '", intact_column, "' must be numeric."),
          easyClose = FALSE,
          footer = modalButton("OK")
        ))
        return(NULL)
      }
      
      # Test on Value range 
      if (!all(catchment[[intact_column]] >= 0 & catchment[[intact_column]] <= 1)) {
        showModal(modalDialog(
          title = "Intactness is out of range.",
          paste0("All values in column '", intact_column, "' must be between 0 and 1."),
          easyClose = FALSE,
          footer = modalButton("OK")
        ))
        return(NULL)
      }
      catchment$intact <- catchment[[intact_column]]  # Dynamically access the specified column
      catchment$area_intact <- st_area(catchment) * catchment$intact
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
  
  ####################################################################################################
  # Map viewer - fires and intactness
  ####################################################################################################
  observeEvent(input$confIntact,{ 
    req(rv$layers_rv$catchment_pr)
    
    showModal(modalDialog(
      title = "Mapping undisturbed areas",
      easyClose = TRUE,
      footer = modalButton("OK")))
    
    leafletProxy("map") %>%
      clearGroup('Catchments') %>%
      clearGroup('Undisturbed')
    
    catch <- rv$layers_rv$catchment_pr %>% st_transform(4326)
    pop = ~paste("CATCHNUM:", CATCHNUM, "<br>Area (km²):", round(Area_total/1000000,1), "<br>Undisturbed (%):", intact*100 )
    if (isMappable(rv$layers_rv$intactness_sf)) { 
      intact <- st_transform(rv$layers_rv$intactness_sf, 4326)
      leafletProxy("map") %>% addPolygons(data=intact, color='blue', fill = T, fillOpacity = 0.2, weight=0, group='Undisturbed', options = leafletOptions(pane = "ground"))
      leafletProxy("map") %>% addPolygons(data=catch, color='black', fillColor = "grey", fillOpacity = 0, weight=1, layerId = ~CATCHNUM, popup = pop, group="Catchments", options = leafletOptions(pane = "over"))
      overlay <- c(rv$overlayBase(), "Undisturbed")
      rv$overlayBase(overlay)
    } else {
      leafletProxy("map") %>% addPolygons(data=catch, color='black', fillColor = "grey", fillOpacity = 0, weight=1, layerId = ~CATCHNUM, popup = pop, group="Catchments", options = leafletOptions(pane = "over"))
    }
    leafletProxy("map") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                       overlayGroups = c(rv$overlayBase(), rv$group_names()),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Streams", "Catchments", rv$group_names()))
    
    removeModal()
    
    
    ####################################################################################################
    # Update stats
    ####################################################################################################
    x <- rv$outAOI()
    
    new_rows  <- tibble(Variables="Analysis area intactness", 
                        Area_km2= NA_real_,
                        Percent = NA_real_)
    
    x <- x %>% dplyr::filter(!Variables %in% new_rows$Variables)
    x <- dplyr::bind_rows(x, new_rows)
    
    if(input$intactSource == "intcatch"){
      x <- x %>% 
        mutate(Area_km2 = case_when(Variables == "Analysis area intactness" ~ round(as.numeric(sum(rv$layers_rv$catchment_pr$area_int)/1000000,0)),
                                    TRUE ~ Area_km2),
               Percent= case_when(Variables == "Analysis area intactness" ~  round(as.numeric(sum(rv$layers_rv$catchment_pr$area_int)/as.numeric(st_area(rv$layers_rv$planreg_sf)))*100,2),
                                  TRUE ~ Percent)
        ) 
    } else {
      x <- x %>% 
        mutate(Area_km2 = case_when(Variables == "Analysis area intactness" ~ round(as.numeric(st_area(st_union(rv$layers_rv$intactness_sf)))/1000000,0), 
                                    TRUE ~ Area_km2),
               Percent= case_when(Variables == "Analysis area intactness" ~  round(as.numeric(st_area(st_union(rv$layers_rv$intactness_sf)))/as.numeric(st_area(rv$layers_rv$planreg_sf))*100,2),
                                  TRUE ~ Percent)
        ) 
    }
    
    rv$outAOI(x)
    rv$outtab1(x)
  })
}
  