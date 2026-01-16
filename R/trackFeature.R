trackFeatureServer <- function(input, output, session, project, map, rv){
  
  trackUI_static <- function() {
    tagList(
      
      # ---- Fire section  ----
      radioButtons("featSource", "Select source for feature to track (optional):", choices = if(is.null(input$upload_distExplo)) {c("Upload feature layer" = "featupload",
                                                                                                                           "No feature" = "nofeature")
                                                                                  } else {c("Use uploaded feature" = "featureIncluded",
                                                                                            "Upload new feature" = "featupload",
                                                                                            "No feature" = "nofeature")
                                                                                  }, selected = "nofeature"),
      conditionalPanel("input.featSource == 'featureIncluded'",
                       selectInput("distexploLayer", "Select feature layer", choices = NULL)),
      conditionalPanel("input.featSource == 'featupload'",
        radioButtons("featformat", "Select fire file format:", choices = c("Shapefile" = "featshp", 
                                                                           "GeoPackage" = "featgpkg"), selected = character(0), inline = TRUE),
        conditionalPanel("input.featformat == 'featshp'",
                         fileInput("upload_shpfeat", "Upload feature layer", multiple = TRUE, accept = c(".shp",".dbf",".shx",".prj",".cpg"))),
        
        conditionalPanel("input.featformat == 'featgpkg'",
                         fileInput("upload_gpkgfeat", "Upload feature layer", multiple = FALSE, accept = ".gpkg"),
                         selectInput("featLayer", "Select feature layer", choices = NULL))
      ),
      br(),
      actionButton("confFeat","Confirm", icon = icon("map-location-dot", lib = "font-awesome"),  class = "btn-warning", style = "width:250px")
    )
  }
  
  output$trackUI <- renderUI({
    trackUI_static()
  })

  
  observe({
    req(input$upload_gpkgfeat)
    req(input$featformat == 'featgpkg')
    file <- input$upload_gpkgfeat$datapath
    layers <- st_layers(file)$name
    updateSelectInput(session = getDefaultReactiveDomain(), "featLayer", choices = c("Select a layer", layers))
  })
  
  observe({
    req(input$upload_distExplo)
    req(input$featSource == 'featureIncluded')
    file <- input$upload_distExplo$datapath
    layers <- st_layers(file)$name
    updateSelectInput(session = getDefaultReactiveDomain(), "distexploLayer", choices = c("Select a layer", layers))
  })
  
  # Set feature to track
  feat_sf <- eventReactive(input$confFeat,{
    i <- NULL
    if(input$featSource == 'featureIncluded'){
      req(input$upload_distExplo)
      req(input$distexploLayer != "Select a layer")
      infile <- input$upload_distExplo
      i <- read_gpkg_from_upload(infile$datapath, input$distexploLayer) %>%
        dplyr::select(any_of(c("geometry", "geom"))) %>%
        suppressWarnings() %>%
        st_cast('MULTIPOLYGON') %>% 
        st_zm(drop = TRUE, what = "ZM")  %>%
        mutate(area_ha = as.numeric(st_area(geom)/10000))
      rv$trackfeat_name(input$distexploLayer)
    }else if (input$featSource == "featupload"){
      req(input$featformat)
      if(input$featformat == 'featgpkg'){
        if (input$featLayer == "Select a layer") {
          showModal(modalDialog(
            title = "Missing input parameters",
            "Please confirm feature layer.",
            easyClose = FALSE,
            footer = modalButton("OK")
          ))
        }
        
        validate(
          need(input$featLayer != "Select a layer", "")
        )
        infile <- input$upload_gpkgfeat
        i <- read_gpkg_from_upload(infile$datapath, input$featLayer) %>%
          dplyr::select(any_of(c("geometry", "geom"))) %>%
          suppressWarnings() %>%
          st_cast('MULTIPOLYGON') %>% 
          st_zm(drop = TRUE, what = "ZM")  %>%
          mutate(area_ha = as.numeric(st_area(geom)/10000))
        rv$trackfeat_name(input$featLayer)
      }else{
        infile <- input$upload_shpfeat
        check_shp(infile$datapath)
        i <- read_shp_from_upload(input$upload_shpfeat) %>%
          dplyr::select(any_of(c("geometry", "geom"))) %>%
          suppressWarnings() %>%
          st_cast('MULTIPOLYGON') %>% 
          st_zm(drop = TRUE, what = "ZM")  %>%
          { 
            geom_col <- attr(., "sf_column")   # get current geometry column name
            mutate(., area_ha = as.numeric(st_area(.data[[geom_col]]) / 10000))
          }
        rv$trackfeat_name(tools::file_path_sans_ext(infile$name[1]))
      }
    }else{
      return(NULL)
    }
    geom_idx <- which(names(i) == attr(i, "sf_column"))
    names(i)[geom_idx] <- "geom"
    st_geometry(i) <- "geom"
    rv$layers_rv$trackFeat <- i
    return(i)
  })
  
  ####################################################################################################
  # Map viewer - fires and intactness
  ####################################################################################################
  observeEvent(input$confFeat,{ 
    req(rv$layers_rv$catchment_pr)
    
    showModal(modalDialog(
      title = "Mapping feature to track",
      easyClose = TRUE,
      footer = modalButton("OK")))
    
    if(input$featSource == 'featureIncluded'){
      leafletProxy("map") %>%
        hideGroup(rv$oldtrackfeat_name()) 
    } else if (input$featSource == "featupload"){
      leafletProxy("map") %>%
        clearGroup(rv$oldtrackfeat_name()) 
    }
#    catch <- rv$layers_rv$catchment_pr %>% st_transform(4326)
#    pop = ~paste("CATCHNUM:", CATCHNUM, "<br>Area (km²):", round(Area_total/1000000,1), "<br>Intactness (%):", intact*100 )
#    leafletProxy("map") %>% addPolygons(data=catch, color='black', fillColor = "grey", fillOpacity = 0, weight=1, layerId = ~CATCHNUM, popup = pop, group="Catchments", options = leafletOptions(pane = "over"))
    
    track_sf <- isolate(feat_sf())
    if(!is.null(track_sf)){
      track_sf <- st_transform(track_sf, 4326)
      leafletProxy("map") %>% addPolygons(data=track_sf, fill=T, stroke=F, fillColor="#996633", fillOpacity=0.8, group=rv$trackfeat_name(), options = leafletOptions(pane = "ground"))
      rv$oldtrackfeat_name(rv$trackfeat_name())  
    }
    
    leafletProxy("map") %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery", "Blank Background"),
                       overlayGroups = c(rv$overlayBase(), rv$group_names(), rv$grps(), rv$trackfeat_name()),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Streams", "Catchments")) %>%
      showGroup(rv$trackfeat_name())
    
    removeModal()
    
    #Feature stat
    if(!is.null(rv$layers_rv$trackFeat)){
      y <- tibble(Variables=c("Within study area"), 
                  Area_km2= NA_real_, 
                  Percent = NA_real_)
      
      y <- y %>% 
        mutate(Area_km2 = case_when(Variables == "Within study area" ~  round(as.numeric(sum(st_area(rv$layers_rv$trackFeat))/1000000,2))),
               Percent= case_when(Variables == "Within study area" ~  round(as.numeric(sum(st_area(rv$layers_rv$trackFeat))/st_area(rv$layers_rv$planreg_sf))*100))
        )
    }else{
      y <- tibble(
        Variables = "No feature",
        Area_km2 = NA_real_,
        Percent = NA_real_
      )
    }
    rv$outfeaturetab(y)
  })
  
  output$featureNameTxt <- renderText({
    req(rv$trackfeat_name())
    paste("Selected feature:", rv$trackfeat_name())
  })
  
}
  