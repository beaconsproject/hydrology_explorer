library(sf)
library(dplyr)
library(terra)
library(raster)
library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyjs)
library(shinyWidgets)
library(rgdal)
library(shinycssloaders)
library(rhandsontable)
library(tibble)
source("./beaconshydro.R")

ui = dashboardPage(skin="blue",
    dashboardHeader(title = "BEACONS HYDRO (DEMO TOOL)"),
    dashboardSidebar(
     sidebarMenu(id = "tabs",
       menuItem("Controls", tabName = "upstream", icon = icon("th")),
       selectInput("fda", label="Select FDA:", choices=c("10AB")),
       hr(),
       sliderInput("buffer1", label="Linear buffer size (m):", min=0, max=2000, value = 500, step=100, ticks=FALSE),
       sliderInput("buffer2", label="Areal buffer size (m):", min=0, max=2000, value = 500, step=100, ticks=FALSE),
       hr(),
       div(disabled(fileInput(inputId = "upload_poly", label = "Upload disturbances", multiple = FALSE, accept = ".gpkg", placeholder = "Inactive")),
           style = "color: #545450;"),
       #actionButton(inputId = "select_poly_button", label = "Select using polygon", icon = icon(name = "circle-plus", lib = "font-awesome")),
       hr(),
       actionButton("goButton", "Generate footprint map"),
       actionButton("goButtonDown", label  =  HTML("View upstream and
                    <br /> downstream disturbances")),
       hr(),
       div(style="position:relative; left:calc(6%);", downloadButton("downloadCatchment", "Download catchments"))
     )
    ),
    dashboardBody(
     useShinyjs(),
     tags$head(tags$style(".skin-blue .sidebar a { color: #444; }")),
     tabItems(
       tabItem(tabName="upstream",
               fluidRow(
                 tabBox(
                   id = "one", width="8",
                   tabPanel("Mapview", leafletOutput("map", height=750) %>% withSpinner()),
                   tabPanel(HTML("<b>Buffers</b>"),
                            tags$h2("Custom buffers"),
                            tags$p("Buffer widths can specified by disturbance type using the table below. Otherwise buffer width is set using the
                                    sliders on the map view. Once buffer widths are specified by disturbances types, switch to the Mapview tab to
                                    generate the footprint map. "),
                            materialSwitch("custom_buffer_switch",
                                           label = "Use custom buffers",
                                           value = FALSE, status = "primary",
                                           inline = TRUE),
                            rHandsontableOutput('buffer_table'))
                 ),
                 tabBox(
                   id = "two", width="4",
                   tabPanel(HTML("<b>Upstream statistics</b>"), tableOutput("tab1"),
                            "*Area Weigthed Intactness",)
                 ),
               )
       )
     )
    ))


server = function(input, output) {

  output$help <- renderText({
    includeMarkdown("upstream.md")
  })

  ####################################################################################################
  # READ SPATIAL DATA
  ####################################################################################################
  fda <- reactive({
    paste0('www//fda_',tolower(input$fda),'.gpkg')
  })

  bnd <- reactive({
    st_read(fda(), 'fda', quiet=T)
  })

  bnd10k <- reactive({
    x <- st_buffer(bnd(), 10000)
  })

  streams <- reactive({
    st_read(fda(), 'streams', quiet=T) %>% st_union()
  })

  linear <- reactive({
    st_read(fda(), 'Linear_Features+', quiet=T)
  })

  areal <- reactive({
    st_read(fda(), 'Areal_Features+', quiet=T)
  })

  catchments <- reactive({
    catchments <- st_read(fda(), 'catchments', quiet=T)
  })

  consarea <- reactive({
    consarea <- st_read(fda(), 'conservation_area', quiet=T)
  })

  ####################################################################################################
  # UPLOAD CUSTOM DISTURBANCE TABLE from GPKG  (INACTIVE)
  ####################################################################################################

  # Upload geopackage and add polygon to map
  options(shiny.maxRequestSize = 9*1024^2)
  observe({
    req(input$upload_poly)
    file <- input$upload_poly$datapath
    ext <- tools::file_ext(file)
    if(ext == "gpkg"){
      areal <- st_read(file, 'Areal_dist', quiet=T) %>% st_transform(4326)
      linear <- st_read(file, 'Linear_dist', quiet=T) %>% st_transform(4326)
    } else{
      showNotification("Wrong file type, must be geopackage (.gpkg)", type = "error")
    }
  })

  # Add intersecting catchments to selection when button pressed
  observeEvent(input$select_poly_button, {

    add_catchments <- catchnums_in_polygon(st_transform(selection_poly$poly, 3579), st_transform(catchments, 3579))

    selected_catchments$catchnum <- c(selected_catchments$catchnum, add_catchments)
  })
  ####################################################################################################
  # SET UP CUSTOM BUFFER TABLE
  ####################################################################################################

  # Activate grey out sliders if button selected. Note that disable doesn't seem to work on the table. Instead we can set it to read only mode when rendering.
  observe({

    if(input$custom_buffer_switch == TRUE){
      shinyjs::disable("buffer1")
      shinyjs::disable("buffer2")
    } else{
      shinyjs::enable("buffer1")
      shinyjs::enable("buffer2")
    }
  })

  reactive_vals <- reactiveValues() # This sets up a reactive element that will store the buffer width df

  # Make table of unique disturbance types in areal linear attribute tables
  output$buffer_table <- renderRHandsontable({

    linear_types_df <- linear() %>%
      st_drop_geometry() %>%
      group_by(TYPE_INDUSTRY, TYPE_DISTURBANCE) %>%
      summarise(Features = "Linear") %>%
      ungroup() %>%
      rename(Industry = TYPE_INDUSTRY, Disturbance = TYPE_DISTURBANCE) %>%
      relocate(Features, Industry, Disturbance)

    area_types_df <- areal() %>%
      st_drop_geometry() %>%
      group_by(TYPE_INDUSTRY, TYPE_DISTURBANCE) %>%
      summarise(Features = "Areal") %>%
      rename(Industry = TYPE_INDUSTRY, Disturbance = TYPE_DISTURBANCE) %>%
      relocate(Features, Industry, Disturbance)

    types_df <- rbind(linear_types_df, area_types_df) %>%
      arrange(Features, Industry, Disturbance) %>%
      mutate(Buffer = 1000)

    if(input$custom_buffer_switch == TRUE){
      rhandsontable(types_df) %>%
        hot_cols(columnSorting = TRUE) %>%
        hot_col("Features", readOnly = TRUE) %>%
        hot_col("Industry", readOnly = TRUE) %>%
        hot_col("Disturbance", readOnly = TRUE)
    } else{
      # grey out the table and make it read_only (this is a work around because shinyjs::disable doesn't work)
      rhandsontable(types_df, readOnly = TRUE) %>%
        hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
              td.style.background = 'lightgrey';
              td.style.color = 'grey';
           }")
    }
  })

  observe({
    reactive_vals$buffer_tab <- hot_to_r(input$buffer_table)
  })

  ####################################################################################################
  # BUFFER DISTURBANCES AND CALCULATE FOOTPRINT AND INTACTNESS
  ####################################################################################################
  footprint_sf <- eventReactive(input$goButton, {

    if(input$custom_buffer_switch == TRUE){
      # If custom buffer table requested, for each unique buffer width, extract all features and buffer
      # Then union all layers
      unique_buffers_linear <- unique(reactive_vals$buffer_tab$Buffer[reactive_vals$buffer_tab$Features == "Linear"]) # get unique buffers
      counter <- 1
      for(i in unique_buffers_linear){
        buff_sub <- reactive_vals$buffer_tab %>%
          filter(Features == "Linear", Buffer == i)
        linear_join <- right_join(linear(), buff_sub, by = c("TYPE_INDUSTRY" = "Industry", "TYPE_DISTURBANCE" = "Disturbance"))
        linear_buff <- st_union(st_buffer(linear_join, i))

        if(counter == 1){
          linear_final <- linear_buff
        } else{
          linear_final <- st_union(linear_final, linear_buff)
        }
        counter <- counter + 1
      }

      unique_buffers_areal <- unique(reactive_vals$buffer_tab$Buffer[reactive_vals$buffer_tab$Features == "Areal"]) # get unique buffers
      counter <- 1
      for(i in unique_buffers_areal){
        buff_sub <- reactive_vals$buffer_tab %>%
          filter(Features == "Areal", Buffer == i)
        areal_join <- right_join(areal(), buff_sub, by = c("TYPE_INDUSTRY" = "Industry", "TYPE_DISTURBANCE" = "Disturbance"))
        areal_buff <- st_union(st_buffer(areal_join, i))

        if(counter == 1){
          areal_final <- areal_buff
        } else{
          areal_final <- st_union(areal_final, areal_buff)
        }
        counter <- counter + 1
      }

      st_union(linear_final, areal_final)

    } else{
    v1 <- st_union(st_buffer(linear(), input$buffer1))
    v2 <- st_union(st_buffer(areal(), input$buffer2))
    st_intersection(st_union(v1, v2), bnd())
    }
  })

  ####################################################################################################
  # UPSTREAM SECTION
  ####################################################################################################
  catch_up <- eventReactive(input$goButtonDown, {
    # Generate upstream catchments
    upstream_list <- get_upstream_catchments(consarea(), "RA_ID", catchments())
    upstream_area <- dissolve_catchments_from_table(catchments(), upstream_list, "RA_ID")

    # Tabulate dist area within the FDA
    dist <- st_union(footprint_sf())
    i <- st_intersection(catchments(), dist)
    distArea <- i %>%
      mutate(area_dist = st_area(.) %>% as.numeric()) %>%
      st_drop_geometry()
    catchments <- merge(catchments(), distArea[,c("CATCHNUM", "area_dist")], by = "CATCHNUM", all.x = TRUE)
    catchments$area_dist <- as.numeric(catchments$area_dist)
    catchments$area_dist[is.na(catchments$area_dist)] <- 0
    catchments$intact <- round((catchments$Area_total-catchments$area_dist)/catchments$Area_total, 3)
    catchments$consarea <- NA
    catchments$consarea[which(catchments$CATCHNUM %in% consarea()$CATCHNUM)] <- 1

    # Tabulate dist area per catchment within the upstream area
    upstream_catch <- subset(catchments, catchments$CATCHNUM %in% upstream_list$RA_01)
    upstream_catch <- st_drop_geometry(upstream_catch)
    upstream_catch$up <- 1
    merge(catchments, upstream_catch[,c("CATCHNUM", "up")], by = "CATCHNUM", all.x = TRUE)
  })

  ####################################################################################################
  # DOWNSTREAM STEM SECTION
  ####################################################################################################
  catch_stem <- eventReactive(input$goButtonDown, {
    # downstream stem polygon
    downstream_stem_list <- get_downstream_catchments(consarea(), "RA_ID", catchments())
    catch_stem <- catchments()[catchments()$CATCHNUM %in% downstream_stem_list$RA_01, drop = TRUE]
    catch_stem$stem <- 1
    merge(catch_up(), catch_stem[,c("CATCHNUM", "stem")], by = "CATCHNUM", all.x = TRUE)
  })

  ####################################################################################################
  # DOWNSTREAM SECTION
  ####################################################################################################
  catch_att <- eventReactive(input$goButtonDown, {
    # downstream  polygon
    # Extract upstream catchment along the downstream stem
    downstream_stem_list <- get_downstream_catchments(consarea(), "RA_ID", catchments())
    catch_stem <- extract_catchments_from_table(catchments(), downstream_stem_list, as.character(colnames(downstream_stem_list)), "RA_ID")
    upstem_list <- get_upstream_catchments(catch_stem, "CATCHNUM", catchments())

    # CReate upstream catchment along the downstream stem
    upstream_list <- get_upstream_catchments(consarea(), "RA_ID", catchments())

    #Merge stem catchment with their related upstream catchments
    catchList <- c()
    catchList <- c(unlist(downstream_stem_list), unlist(upstem_list))
    catchList <- catchList %>%
      unique() %>%
      subset(!. %in% unlist(upstream_list)) %>% #drop catchments within the upstream
      subset(!. %in% consarea()$CATCHNUM) %>% #drop catchments within the reference_area
      enframe(., name = NULL, value = as.character(colnames(downstream_stem_list)))

    # Generate downstream polygon by dissolving all downstream catchments
    catch_down<- catchments()[catchments()$CATCHNUM %in% catchList$RA_01, drop = TRUE]
    catch_down$down <- 1
    merge(catch_stem(), catch_down[,c("CATCHNUM", "down")], by = "CATCHNUM", all.x = TRUE)
  })

  ####################################################################################################
  # Map viewer
  ####################################################################################################
  # Render the initial map
  output$map <- renderLeaflet({
    # Re-project
    bnd <- st_transform(bnd(), 4326)
    areal <- st_transform(areal(), 4326)
    linear <- st_transform(linear(), 4326)
    consarea <- st_transform(st_union(consarea()), 4326)
    catch_4326 <- st_transform(catchments(), 4326)
    streams <- st_transform(streams(), 4326)

    map_bounds <- bnd %>% st_bbox() %>% as.character()

    m <- leaflet() %>%
      addProviderTiles("Esri.NatGeoWorldMap", group="Esri.NatGeoWorldMap") %>%
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
      addPolygons(data=bnd, color='black', fill=F, weight=3, group="FDA") %>%
      addPolygons(data=consarea, color='red', fillOpacity=0.6, weight=2, group="Conservation area") %>%
      addPolygons(data=catch_4326, color='black', fill=F, weight=1, group='Catchments') %>%
      addPolylines(data=streams, color='blue', weight=1, group="Streams") %>%

      fitBounds(map_bounds[1], map_bounds[2], map_bounds[3], map_bounds[4]) %>% # set view to the selected FDA
      addPolylines(data=linear, color='azure3', weight=1, group="Linear features") %>%
      addPolygons(data=areal, color='black', fill=T, stroke=F, group="Areal features", fillOpacity=0.5) %>%

      addLayersControl(position = "topright",
                       baseGroups=c("Esri.NatGeoWorldMap", "Esri.WorldImagery"),
                       overlayGroups = c("FDA", "Conservation area", "Catchments", "Streams", "Areal features", "Linear features"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Catchments", "Streams", "Areal features","Linear features"))

    # Render Footprint
    if(input$goButton > 0){
      vv <- st_transform(footprint_sf(), 4326)

      m <- m %>%
      addPolygons(data=vv, color='black', stroke=F, fillOpacity=0.5, group='Footprint') %>%
      addLayersControl(position = "topright",
                       baseGroups=c("Esri.NatGeoWorldMap", "Esri.WorldImagery"),
                       overlayGroups = c("FDA", "Conservation area", "Catchments", "Streams", "Footprint"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
       hideGroup(c("Catchments", "Streams"))
    }

    # Render Upstream / downstream
    if(input$goButtonDown > 0){
      # upstream catchment intactness
      catch_map <- catch_att() %>%
        filter(up ==1) %>%
        st_transform(4326)

      # upstream area
      upstream_area <- catch_map %>%
        st_union(.)

      #downstream stem area
      downstream_stem <- catch_att() %>%
        filter(stem ==1) %>%
        #st_union(.) %>%
        st_transform(4326)

      #downstream area
      downstream_all <- catch_att() %>%
        filter(down ==1) %>%
        st_union(.) %>%
        st_transform(4326)

      ## Create bin palette function for percent
      catchintact = colorBin(
        palette = 'PRGn',
        domain = catch_map$intact,
        bins = 10)

      m <- m %>%
        addPolygons(data=catch_map, color=~catchintact(intact), stroke=F, fillOpacity=0.8, group="Upstream intactness") %>%
        addPolygons(data=upstream_area,  color='#663300', fill=F, weight=3, group="Upstream area") %>%
        addPolygons(data=downstream_stem, color=~catchintact(intact), stroke=F, fillOpacity=0.8, group="Downstream stem intactness") %>%
        addPolygons(data=downstream_all, color='#994C00', fillOpacity=0.6, weight=1, group="Downstream area") %>%
        addLegend(position = "bottomleft", pal = catchintact, values = catch_map$intact, opacity = 1,
                  title = "Percent intactness", labFormat = labelFormat(
                    suffix = "%",
                    transform = function(x) 100 * x),
               group = "Upstream catchment intactness") %>%
        addLayersControl(position = "topright",
                        baseGroups=c("Esri.NatGeoWorldMap", "Esri.WorldImagery"),
                        overlayGroups = c("Conservation area", "Catchments", "Streams", "Footprint", "Upstream area", "Downstream stem intactness", "Downstream area", "Upstream intactness"),
                        options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("Catchments", "Footprint", "Streams", "Downstream stem intactness", "Downstream area"))
    }
    m
  })


  outputtab1 <- reactive({
    # If button has been pressed
    if(input$goButtonDown > 0) {
      # upstream catchment intactness
      catch_map <- catch_att() %>%
        filter(up ==1) %>%
        st_transform(4326)

      # upstream area
      upstream_area <- catch_map %>%
        st_union(.)

      #downstream stem intact
      downstream_stem_int <- catch_att() %>%
        filter(stem ==1) %>%
        st_transform(4326)

      #downstream stem area
      downstream_stem <- catch_att() %>%
        filter(stem ==1) %>%
        st_union(.) %>%
        st_transform(4326)

      #downstream area
      downstream_all <- catch_att() %>%
        filter(down ==1) %>%
        st_union(.) %>%
        st_transform(4326) %>%
        st_make_valid(.)

      x <- tibble(Variables=c("FDA",
                              "Conservation area",
                              "Percent conservation area in FDA",
                              "Upstream area",
                              "Downstream stem area",
                              "Downstream area",
                              "Upstream mean AWI*",
                              "Downstream stem mean AWI*"
                              ), Area_km2=NA, Percent = NA)
      x$Area_km2[x$Variables=="FDA"] <- round(st_area(bnd())/1000000,0)
      x$Area_km2[x$Variables=="Conservation area"] <- round(st_area(st_union(consarea()))/1000000,0)
      x$Percent[x$Variables=="Conservation area"] <- round(st_area(st_union(consarea()))/st_area(bnd())*100,2)
      x$Percent[x$Variables=="Percent conservation area in FDA"] <- round(st_area(st_union(consarea()))/st_area(bnd())*100,2)
      x$Area_km2[x$Variables=="Upstream area"] <- round(st_area(upstream_area)/1000000,0)
      x$Area_km2[x$Variables=="Downstream stem area"] <- round(st_area(downstream_stem)/1000000,0)
      x$Area_km2[x$Variables=="Downstream area"] <- round(st_area(downstream_all)/1000000,0)
      x$Percent[x$Variables=="Upstream mean AWI*"] <- round(((sum(catch_map$Area_total)-sum(catch_map$area_dist))/sum(catch_map$Area_total))*100,1)
      x$Percent[x$Variables=="Downstream stem mean AWI*"] <- round(((sum(downstream_stem_int$Area_total)-sum(downstream_stem_int$area_dist))/sum(downstream_stem_int$Area_total))*100,1)
    }else{
      x <- tibble(Variables=c("FDA",
                              "Conservation area",
                              "Percent conservation area in FDA" ,
                              "Upstream area",
                              "Downstream stem area",
                              "Downstream area",
                              "Upstream mean AWI*",
                              "Downstream stem mean AWI*"
                              ), Area_km2=NA, Percent = NA)
      x$Area_km2[x$Variables=="FDA"] <- round(st_area(bnd())/1000000,0)
      x$Area_km2[x$Variables=="Conservation area"] <- round(st_area(st_union(consarea()))/1000000,0)
      x$Percent[x$Variables=="Conservation area"] <- round(st_area(st_union(consarea()))/st_area(bnd())*100,2)
      x$Percent[x$Variables=="Percent conservation area in FDA"] <- round(st_area(st_union(consarea()))/st_area(bnd())*100,2)
    }
    x
  })

  output$tab1 <- renderTable({
    outputtab1()
  }, digits = 1)

  ####################################################################################################
  # DOWNLOAD GPKG
  ####################################################################################################
  output$downloadCatchment <- downloadHandler(
    filename = function() {'catchments.gpkg'},
    content = function(file) {
      catch_dl <- catch_att() %>%
        mutate(Area_Land = round(Area_land,1),
               Area_Water = round(Area_water,1),
               Area_Total = round(Area_total,1),
               Prop_Intact =round((Area_total - area_dist)/Area_total*100,1),
               isConsArea = ifelse(consarea ==1,1,0),
               isUpstream = ifelse(up ==1,1,0),
               isDownStem = ifelse(stem ==1,1,0),
               isDownstream = ifelse(down ==1,1,0))
      catch_dl <- catch_dl[,c("CATCHNUM", "Area_Land", "Area_Water","Area_Total", "STRAHLER", "Prop_Intact", "isConsArea","isUpstream", "isDownStem", "isDownstream")]
      st_write(catch_dl, dsn=file, layer='catchments')
    }
  )

}

shinyApp(ui, server)
