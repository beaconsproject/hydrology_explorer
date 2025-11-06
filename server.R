server = function(input, output, session) {
  
  reactiveValsList <- list(outtab1 = reactiveVal(),
                           outfiretab = reactiveVal(),
                           overlayBase = reactiveVal(c()),
                           grps = reactiveVal(c()),
                           group_names = reactiveVal(c()),
                           overlayHydro = reactiveVal(c()) ,
                           upstream_sf = reactiveVal(),
                           downstream_sf = reactiveVal(),
                           downstream_stem_sf = reactiveVal(),
                           display1_name = reactiveVal(),
                           display2_name = reactiveVal(),
                           display3_name = reactiveVal(),
                           layers_rv = reactiveValues(streams_sf = NULL, 
                                                      planreg_sf = NULL,
                                                      ifl2000 = NULL,
                                                      ifl2020 = NULL,
                                                      pa2021 = NULL,
                                                      herds = NULL, 
                                                      placers = NULL,
                                                      quartz= NULL,
                                                      mines = NULL,
                                                      fires = NULL,
                                                      disturbed = NULL,
                                                      undisturbed = NULL,
                                                      catchments = NULL,
                                                      catchment_pr = NULL,
                                                      intactness_sf = NULL,
                                                      footprint_sf = NULL,
                                                      display1_sf = NULL,
                                                      display2_sf = NULL,
                                                      display3_sf = NULL, 
                                                      aoi_sf = NULL,
                                                      analysis_aoi = NULL
                           ),
                           outfiretab = reactiveVal(),
                           outtab1 = reactiveVal(),
                           outputDCI = reactiveVal(),
                           selected_catchments = reactiveValues(catchnum = c())
  )
  
  
  output$help <- renderText({
    includeMarkdown("docs/upstream.md")
  })
  
  ################################################################################################
  # RELOAD
  observeEvent(input$reload_btn, {
    session$reload()
  })
  ################################################################################################
  ######################################################
  ##  SERVE USER GUIDE SECTION
  ######################################################
  output$guide_ui <- renderUI({
    req(input$tabs)  # 'sidebar' is the id of your sidebarMenu
    
    guide_file <- switch(input$tabs,
                         "tabUpload" = "docs/upload_doc.md",
                         "addLayers" = "docs/addLayers_doc.md",
                         "selectAOI" = "docs/selectAOI_doc.md",
                         "upstream" = "docs/upstream_doc.md",
                         "download" = "docs/download_doc.md",
                         NULL
    )
    
    if (!is.null(guide_file) && file.exists(guide_file)) {
      includeMarkdown(guide_file)
    } else {
      tags$p("No user guide available for this section.")
    }
  })
  
  ################################################################################################
  ################################################################################################
  # Set input params
  ################################################################################################
  ################################################################################################
  
  # Render the initial map
  output$map <- renderLeaflet({
    
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
  
  myMap <- leafletProxy("map", session)
  
  #Set input parameters
  setParamsServer(input, output, session, project, myMap, reactiveValsList)
  
  # set firest and intactness
  setIntactServer(input, output, session, project, myMap, reactiveValsList)
  
  # Add display layers
  addDisplayServer(input, output, session, project, myMap, reactiveValsList)
  
  # Select AOI
  selectAOIServer(input, output, session, project, myMap, reactiveValsList)
  
  # run Upstream / Downstream
  runHydroServer(input, output, session, project, myMap, reactiveValsList)
  
  # run Upstream / Downstream
  dwdServer(input, output, session, project, myMap, reactiveValsList)
  
  output$tab1 <- renderTable({
    reactiveValsList$outtab1()
  }, digits = 1)
  
  output$tabFires <- renderTable({
    reactiveValsList$outfiretab()
  }, digits = 2)
  
  output$tabDCI <- renderTable({
    reactiveValsList$outputDCI()
  }, digits = 2)
  
}