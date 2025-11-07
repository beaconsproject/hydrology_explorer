setIntactServer <- function(input, output, session, project, map, rv){
  
  output$intactUI <- renderUI({
    req(input$tabs)
    if (input$tabs != "tabIntact") return(NULL)
    
    # Check if layers exist
    fires_exist  <- !is.null(rv$layers_rv$fires) && nrow(rv$layers_rv$fires) > 0
    intact_exist <- !is.null(rv$layers_rv$undisturbed) && nrow(rv$layers_rv$undisturbed) > 0
    
    tagList(
      # --- Fires section ---
      if (fires_exist) {
        # Show choice: use existing or upload
        tagList(
          div(style = "margin-top: -20px;",
              radioButtons("firesSource", "Select source for fires:",
                           choices = list("Use existing fire layer" = "fireIncluded", 
                                          "Upload fires layer" = "fireupload"),
                           selected = "fireIncluded", 
                           inline = FALSE)
          ),
          conditionalPanel(
            condition = "input.firesSource == 'fireupload'",
            div(style = "margin-top: -20px;",
                radioButtons("fireformat", "Select fire file format:",
                             choices = list("Shapefile" = "fireshp", 
                                            "GeoPackage" = "firegpkg"),
                             selected = character(0), 
                             inline = TRUE)),
            div(style = "margin-top: -20px;",
                fileInput("upload_fire", "Upload fire layer", multiple = TRUE,
                          accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg', '.gpkg'))),
            conditionalPanel(
              condition = "input.fireformat == 'firegpkg'",
              div(style = "margin-top: -20px;",
                  selectInput("fireLayer", "Select fire layer", choices = NULL, multiple = FALSE))
            )
          )
        )
      } else {
        # Only show upload (no existing fires)
        tagList(
          div(style = "margin-top: -20px;",
              radioButtons("fireformat", "Select fire file format:",
                           choices = list("Shapefile" = "fireshp", 
                                          "GeoPackage" = "firegpkg"),
                           selected = character(0), 
                           inline = TRUE)),
          div(style = "margin-top: -20px;",
              fileInput("upload_fire", "Upload fire layer", multiple = TRUE,
                        accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg', '.gpkg')) ),
          conditionalPanel(
            condition = "input.fireformat == 'firegpkg'",
            div(style = "margin-top: -20px;",
                selectInput("fireLayer", "Select fire layer", choices = NULL, multiple = FALSE))
          )
        )
      },
      
      # --- Intactness section ---
      div(style = "margin: 15px; margin-top: 20px; font-size:17px; font-weight: bold", "Select source of intactness"),
      div(style = "margin-left: 12px; margin-top: -10px; font-size:12px;",
          "Intactness refers to natural areas that have not been subject to industrial-scale development or disturbance."
      ),
      br(),
      
      if (intact_exist) {
        # Show choice: use existing, value in catchments, or upload
        tagList(
          div(style = "margin-top: -20px;", 
              radioButtons("intactSource", "",
                           choices = list("Use existing undisturbed layer" = "intIncluded", 
                                          "Value in catchment dataset" = "intcatch",
                                          "Upload intactness layer" = "intupload"),
                           selected = "intIncluded", 
                           inline = FALSE)
          ),
          conditionalPanel(
            condition = "input.intactSource == 'intcatch'",
            selectInput("intactColumnName", "Catchment dataset - select intactness attribute",
                        choices = NULL, selected = "IntactPB")
          ),
          conditionalPanel(
            condition = "input.intactSource == 'intupload'",
            div(style = "margin-top: -20px;",
                radioButtons("intactformat", "Select intactness file format:",
                             choices = list("Shapefile" = "intshp", 
                                            "GeoPackage" = "intgpkg"),
                             selected = character(0), inline = TRUE)),
            div(style = "margin-top: -20px;",
                fileInput("upload_intact", "Upload undisturbed layer", multiple = TRUE,
                          accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg', '.gpkg'))
            ),
            conditionalPanel(
              condition = "input.intactformat == 'intgpkg' && input.intactSource == 'intupload'",
              div(style = "margin-top: -20px;",
                  selectInput("intactLayer", "Select intactness layer", choices = NULL, multiple = FALSE))
            )
          )
        )
      } else {
        # Only show upload intactness
        tagList(
          div(style = "margin-top: -20px;", 
              radioButtons("intactSource", "",
                           choices = list("Value in catchment dataset" = "intcatch",
                                          "Upload intactness layer" = "intupload"),
                           selected = "intIncluded", 
                           inline = FALSE)
          ),
          conditionalPanel(
            condition = "input.intactSource == 'intcatch'",
            selectInput("intactColumnName", "Catchment dataset - select intactness attribute",
                        choices = NULL, selected = "IntactPB")
          ),
          conditionalPanel(
            condition = "input.intactSource == 'intupload'",
            div(style = "margin-top: -20px;",
                radioButtons("intactformat", "Select intactness file format:",
                             choices = list("Shapefile" = "intshp", 
                                            "GeoPackage" = "intgpkg"),
                             selected = character(0), inline = TRUE)),
            div(style = "margin-top: -20px;",
                fileInput("upload_intact", "Upload undisturbed layer", multiple = TRUE,
                          accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg', '.gpkg'))
            ),
            conditionalPanel(
              condition = "input.intactformat == 'intgpkg' && input.intactSource == 'intupload'",
              div(style = "margin-top: -20px;",
                  selectInput("intactLayer", "Select intactness layer", choices = NULL, multiple = FALSE))
            )
          )
        )
      },
      
      # Confirm button
      actionButton("confIntact", "Confirm", icon = icon(name = "map-location-dot", lib = "font-awesome"),
                   class = "btn-warning", style="width:250px")
    )
  })
  
  observe({
    req(input$intactSource == 'intcatch')
    #browser()
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
      catchment <- rv$layers_rv$catchments
      catchment$intact <- catchment[[intact_column]]  # Dynamically access the specified column
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
    f <- NULL
    
    if(input$firesSource == 'fireIncluded'){
      f <- rv$layers_rv$fires
    }else if (input$firesSource == "fireupload"){
      req(input$fireformat)
      infile <- input$upload_fire
      if(input$fireformat == 'firegpkg'){
        req(input$fireLayer)
        if(input$fireLayer != "Select fire layer"){
          f <- read_gpkg_from_upload(infile$datapath, input$fireLayer) %>%
            dplyr::select(any_of(c("geometry", "geom")))
        }
      }else{
        check_shp(infile$datapath)
        f <- read_shp_from_upload(infile) %>%
          dplyr::select(any_of(c("geometry", "geom")))
      }
    }else{
      return(NULL)
    }
    rv$layers_rv$fires <- f
    return(f)
  })
  
  ####################################################################################################
  # Map viewer - fires and intactness
  ####################################################################################################
  observeEvent(input$confIntact,{ 
    
    showModal(modalDialog(
      title = "Mapping fires and intactness",
      easyClose = TRUE,
      footer = modalButton("OK")))
    
    leafletProxy("map") %>%
      clearGroup('Catchments') %>%
      clearGroup('Intactness') %>%
      clearGroup('Fires') 
    
    catch <- rv$layers_rv$catchment_pr %>% st_transform(4326)
    pop = ~paste("CATCHNUM:", CATCHNUM, "<br>Area (km²):", round(Area_Total/1000000,1), "<br>Intactness (%):", intact*100 )
    if (isMappable(rv$layers_rv$intactness_sf)) { 
      intact <- st_transform(rv$layers_rv$intactness_sf, 4326)
      leafletProxy("map") %>% addPolygons(data=intact, color='blue', fill = T, fillOpacity = 0.2, weight=0, group='Intactness', options = leafletOptions(pane = "ground"))
      leafletProxy("map") %>% addPolygons(data=catch, color='black', fillColor = "grey", fillOpacity = 0, weight=1, layerId = ~CATCHNUM, popup = pop, group="Catchments", options = leafletOptions(pane = "over"))
      overlay <- c(rv$overlayBase(), "Intactness")
      rv$overlayBase(overlay)
    } else {
      leafletProxy("map") %>% addPolygons(data=catch, color='black', fillColor = "grey", fillOpacity = 0, weight=1, layerId = ~CATCHNUM, popup = ~popup_text, group="Catchments", options = leafletOptions(pane = "over"))
    }
    
    fires <- isolate(rv$layers_rv$fires)
    if(!is.null(fires)){
      fires <- st_transform(fires, 4326)
      leafletProxy("map") %>% addPolygons(data=fires, fill=T, stroke=F, fillColor="#996633", fillOpacity=0.8, group="Fires", options = leafletOptions(pane = "ground")) 
    }
    
    leafletProxy("map") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c(rv$overlayBase(), rv$group_names()),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Streams", "Catchments", rv$group_names()))
    
    removeModal()
  })
}
  