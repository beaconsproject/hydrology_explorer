ui = dashboardPage(skin="black",
                   title = "BEACONs Hydro Explorer",
                   dashboardHeader(title = tags$div(
                     tags$img(
                       src = "logoblanc.png",  # Replace with your logo file name
                       height = "50px",   # Adjust the height of the logo
                       style = "margin-right: 10px;"  # Add some spacing around the logo
                     ),"BEACONs Hydro Explorer"), titleWidth = 400,
                     # Add Reload Button Next to Sidebar Toggle
                     tags$li(
                       class = "dropdown",
                       actionButton(
                         "reload_btn",
                         label = "Reload",
                         icon = icon("refresh"),
                         style = "color: black; background-color: orange; border: none; font-size: 16px;"
                       ),
                       style = "position: absolute; left: 50px; top: 10px;"  # Adjust margin for placement next to the toggle
                     ),
                     tags$li(
                       class = "dropdown",  # Required for dropdown functionality
                       dropdownMenu(
                         type = "tasks", 
                         badgeStatus = NULL,
                         icon = icon("life-ring"),  # Life-ring icon triggering dropdown
                         headerText = "",  # No header text in dropdown
                         menuItem("Website", href = "https://beaconsproject.ualberta.ca/", icon = icon("globe")),
                         menuItem("GitHub", href = "https://github.com/beaconsproject/", icon = icon("github")),
                         menuItem("Contact us", href = "mailto: beacons@ualberta.ca", icon = icon("address-book"))
                       ),
                       # Plain Text "About Us" Positioned Next to Dropdown
                       tags$span(
                         "About Us", 
                         style = "font-size: 16px; position: relative; top: 15px; right: 10px; white-space: nowrap; color: white;"
                       )
                     )
                   ),
                   
                   dashboardSidebar(
                     width = 300,
                     sidebarMenu(id = "tabs",
                                 menuItem("Welcome", tabName = "overview", icon = icon("th")),
                                 menuItem("Set input parameters", tabName = "tabUpload", icon = icon("th"), startExpanded = FALSE),
                                 menuItem("Set intactness and fire", tabName = "tabIntact", icon = icon(name = "fas fa-plus", lib = "font-awesome")),                
                                 menuItem("Add display elements (OPTIONAL)", tabName = "addLayers", icon = icon(name = "fas fa-plus", lib = "font-awesome")),                
                                 menuItem("Select AOI", tabName = "selectAOI", icon = icon(name = "fas fa-draw-polygon", lib = "font-awesome")),
                                 menuItem("Generate upstream and downstream", tabName = "upstream", icon = icon(name = "fas fa-map", lib = "font-awesome")),
                                 menuItem("Download results", tabName = "download", icon = icon(name = "fas fa-download", lib = "font-awesome")),
                                 hr()
                     ),
                     conditionalPanel(
                       #UPLOAD
                       condition="input.tabs=='tabUpload'",
                       div(style = "margin-top: -20px;", radioButtons("selectsource", "Select source dataset",
                                                                      choices = list("Use demo dataset" = "usedemo", 
                                                                                     "Upload spatial dataset" = "usedata"),
                                                                      selected = character(0), 
                                                                      inline = FALSE)),
                       conditionalPanel(
                         condition = "input.selectsource == 'usedata'",
                         radioButtons("upload_type", "How do you want to upload your data?",
                                      choices = c("Upload individual Shapefile layers" = "layers",
                                                  "Upload CSV with file paths" = "csv_file",
                                                  "Upload GeoPackage with layers" = "gpkg"), selected = character(0))
                       ),
                       # UPLOAD - Individual layers
                       conditionalPanel(
                         condition = "input.selectsource == 'usedata' && input.upload_type == 'layers'",
                         tagList(
                           div(style = "margin-top: -10px;", fileInput("upload_sa", "Study area", multiple = TRUE, accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg'))),
                           div(style = "margin-top: -30px;", fileInput("upload_stream", "Streams dataset", multiple = TRUE, accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg'))),
                           div(style = "margin-top: -30px;", fileInput("upload_catch", "Catchments dataset", multiple = TRUE, accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg')))
                         )
                       ),
                       
                       # UPLOAD - CSV
                       conditionalPanel(
                         condition = "input.selectsource == 'usedata' && input.upload_type == 'csv_file'",
                         fileInput("csv_paths", "Upload CSV with file paths", accept = ".csv")
                       ),
                       
                       # UPLOAD - GeoPackage
                       conditionalPanel(
                         condition = "input.selectsource == 'usedata' && input.upload_type == 'gpkg'",
                         fileInput("gpkg_file", "Upload source dataset (.gpkg)", accept = ".gpkg")
                       ),
                       conditionalPanel(
                         condition = "output.gpkgReady == true && input.selectsource == 'usedata' && input.upload_type == 'gpkg'",
                         div(style = "margin-top: -10px;", selectInput("sa_layer", "Study area", choices = NULL,  multiple = FALSE)),
                         div(style = "margin-top: -20px;", selectInput("catch_layer", "Catchments", choices = NULL, multiple = FALSE)),
                         div(style = "margin-top: -20px;", selectInput("streams_layer", "Streams", choices = NULL,  multiple = FALSE))
                       ),
                       fileInput("upload_gpkg", "Upload GeoPackage from Disturbance Explorer (optional)", multiple = FALSE, accept='.gpkg'),
                       actionButton("previewLayers", "Preview study area", icon = icon(name = "map-location-dot", lib = "font-awesome"), class = "btn-warning", style="width:250px"),
                     ),
                     # UPLOAD - Intactness
                     conditionalPanel(
                       condition = "input.tabs== 'tabIntact'",
                       uiOutput("intactUI")
                     ),
                     
                     # EXTRA LAYERS
                     conditionalPanel(
                       condition="input.tabs=='addLayers'",
                       radioButtons("extraupload", "Select source for extra layers to be displayed:",
                                    choices = list("Shapefile" = "extrashp", 
                                                   "GeoPackage" = "extragpkg"),
                                    selected = character(0), 
                                    inline = TRUE)
                     ),
                     conditionalPanel(
                       condition = "input.tabs=='addLayers' && input.extraupload == 'extrashp'",
                       div(style = "margin-top: -10px;",fileInput(inputId = "display1",   label = HTML('<span style="display:inline-block; width:15px; height:15px; background-color:#663300; margin-right:8px; border:1px solid #000;"></span>Select layer 1'),
                                                                  multiple = TRUE, accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg'), placeholder = "Select a ShapeFile")),
                       div(style = "margin-top: -30px;",fileInput(inputId = "display2", label = HTML('<span style="display:inline-block; width:15px; height:15px; background-color:#330066; margin-right:8px; border:1px solid #000;"></span>Select layer 2'),
                                                                  multiple = TRUE, accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg'), placeholder = "Select a ShapeFile")),
                       div(style = "margin-top: -30px;",fileInput(inputId = "display3", label = HTML('<span style="display:inline-block; width:15px; height:15px; background-color:#003333; margin-right:8px; border:1px solid #000;"></span>Select layer 3'),
                                                                  multiple = TRUE, accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg'), placeholder = "Select a ShapeFile"))
                     ),
                     conditionalPanel(
                       condition = "input.tabs=='addLayers' && input.extraupload == 'extragpkg'",
                       fileInput(inputId = "display4", label = HTML("<h5><b>OPTIONAL - </b>Upload a GeoPackage that contains layers to be displayed on the map.</h5>"),
                                 multiple = FALSE, accept = ".gpkg", placeholder = "Select a GeoPackage"),
                       div(style = "margin-top: -10px;", selectInput("display4a", label = HTML('<span style="display:inline-block; width:15px; height:15px; background-color:#663300; margin-right:8px; border:1px solid #000;"></span>Select layer 1'), choices = NULL)),
                       div(style = "margin-top: -20px;", selectInput("display4b", label = HTML('<span style="display:inline-block; width:15px; height:15px; background-color:#330066; margin-right:8px; border:1px solid #000;"></span>Select layer 2'), choices = NULL)),
                       div(style = "margin-top: -20px;", selectInput("display4c", label = HTML('<span style="display:inline-block; width:15px; height:15px; background-color:#003333; margin-right:8px; border:1px solid #000;"></span>Select layer 3'), choices = NULL))
                     ),
                     conditionalPanel(
                       condition = "input.tabs == 'addLayers'",
                       br(),
                       hr(),
                       br(),
                       actionButton("confExtra", "Confirm", icon = icon(name = "map-location-dot", lib = "font-awesome"), class = "btn-warning", style="width:250px")
                     ),
                     # Select AOI
                     conditionalPanel(
                       condition="input.tabs=='selectAOI'",
                       radioButtons("typeAOI", "Set an Area of Interest (AOI):",
                                    choices = list("Upload an AOI" = "uploadAOI", 
                                                   "Select a set of catchments on the map" = "catchAOI"),
                                    selected = character(0), 
                                    inline = FALSE)
                     ),
                     conditionalPanel(
                       condition="input.tabs=='selectAOI' && input.typeAOI == 'uploadAOI'",
                       radioButtons("sourceAOI", "Select file format",
                                    choices = list("ShapeFile" = "shpAOI", 
                                                   "GeoPackage" = "gpkgAOI"),
                                    selected = character(0), 
                                    inline = TRUE)
                     ),
                     # AOI upload
                     conditionalPanel(
                       condition = "input.tabs=='selectAOI' && input.sourceAOI == 'shpAOI'",
                       div(style = "margin-top: -20px;", fileInput("shp_aoi", "", multiple = TRUE, accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg')))
                     ),
                     conditionalPanel(
                       condition = "input.tabs=='selectAOI' && input.sourceAOI == 'gpkgAOI'",
                       div(style = "margin-top: -20px;", fileInput("gpkg_aoi", "", multiple = FALSE, accept='.gpkg')),
                       div(style = "margin-top: -20px;", selectInput("aoiLayer", "Select AOI layer", choices = "Select AOI layer",  multiple = FALSE))
                     ),
                     conditionalPanel(
                       condition = "input.tabs=='selectAOI' && input.sourceAOI",
                       div(style = "margin-top: -20px;", checkboxInput("editAOI", label = "Enable AOI boundary editing using catchment", value = F))
                     ),
                     conditionalPanel(
                       condition = "input.editAOI == true",
                       div(style = "margin: 10px; font-size: 12px;",
                           "Intersecting catchments are highlighted on the map. Please select or unselect catchments to edit your studyarea.")
                     ),
                     conditionalPanel(
                       condition=" input.tabs=='selectAOI' && input.typeAOI == 'uploadAOI' && input.sourceAOI || input.tabs=='selectAOI' && input.typeAOI == 'catchAOI'",
                       br(),
                       actionButton(inputId = "confAOI", label = div(style = "font-size:13px;background-color:gey;color: black",HTML(" Confirm AOI boundary (Analysis AOI)")), icon = icon(name = "check", lib = "font-awesome"), class = "btn-warning", style="width:250px")
                     ),
                     # tab upstream
                     conditionalPanel(
                       condition="input.tabs=='upstream'",
                       actionButton("confAnalysis", label  = div(style = "font-size:13px;background-color:gey;color: black", HTML("View upstream and
                    <br /> downstream areas")), icon = icon(name = "map-location-dot", lib = "font-awesome"), class = "btn-warning", style="width:250px")
                     ),     
                     conditionalPanel(
                       condition="input.tabs=='download'",
                       tags$style(type="text/css", "#downloadData {background-color:gey;color: black}"),
                       div(style="position:relative; left:calc(10%);", downloadButton("downloadData", "Download results"))
                     )
                   ),     
                   dashboardBody(
                     useShinyjs(),
                     tags$head(tags$link(rel = "icon", type = "image/png", href = "logoblanc.png"),
                               tags$link(rel = "stylesheet", type = "text/css", href = "green-theme.css"),
                               tags$style(HTML("
  body {
    font-size: 16px;
  }
"))
                     ),
                     tabItems(
                       tabItem(tabName="overview",
                               fluidRow(
                                 column(width = 10,  # Adjusted from 6 to 8 for better alignment
                                        tabBox(id = "one", width="8",
                                               tabPanel(HTML("Overview"), includeMarkdown("docs/overview.md")),
                                               tabPanel(HTML("User guide"), includeMarkdown("docs/user_guide.md")),
                                               tabPanel(HTML("Dataset requirements"), includeMarkdown("docs/datasets.md"))
                                        )
                                 ),
                                 absolutePanel(
                                   right = 0, top = 0, width = 250, height = "100%",
                                   #style = "background-color: white; padding: 0px; overflow-y: auto; z-index: 1000;",
                                   style = "background-color: white; padding: 0;margin: 0;border: none; right: 0;overflow: hidden;z-index: 1000;",
                                   tags$img(src = "intact.jpg",width = "100%", style = "display: block;")
                                 )
                               )
                       ),
                       tabItem(tabName="tabUpload",
                               fluidRow(
                                 column(width = 8,
                                        tabBox(id = "mapid", width=NULL,
                                          tabPanel(HTML("<b>Mapview</b>"),
                                                 leafletOutput("map", height = 750) %>% withSpinner()
                                          ),
                                          tabPanel("User Guide",
                                                   # Dynamically update the content of Guidance based on selected tab
                                                   conditionalPanel(
                                                     condition = "input.tabs == 'tabUpload'",
                                                     includeMarkdown("./docs/upload_doc.md")
                                                   ),
                                                   conditionalPanel(
                                                     condition = "input.tabs == 'tabIntact'",
                                                     includeMarkdown("./docs/setIntact_doc.md")
                                                   ),
                                                   conditionalPanel(
                                                     condition = "input.tabs == 'addLayers'",
                                                     includeMarkdown("./docs/addLayers_doc.md")
                                                   ),
                                                   conditionalPanel(
                                                     condition = "input.tabs == 'selectAOI'",
                                                     includeMarkdown("./docs/selectAOI_doc.md")
                                                   ),
                                                   conditionalPanel(
                                                     condition = "input.tabs == 'upstream'",
                                                     includeMarkdown("./docs/upstream_doc.md")
                                                   ),
                                                   conditionalPanel(
                                                     condition = "input.tabs == 'download'",
                                                     includeMarkdown("./docs/download_doc.md")
                                                   )
                                          ),
                                          tabPanel("Summary statistics",
                                                   tags$h4("Summary statistics"),
                                                   div(
                                                     style = "overflow-x: auto; white-space: nowrap; font-size: 16px;",
                                                     tableOutput("stat_tab")
                                                   ),
                                                   div(style = "display: flex; justify-content: center; margin-top: 10px;",
                                                       downloadButton("downloadStats", "Download summary statistics table (.csv)", style='color: #000')
                                                   )
                                          )
                                        
                                 )
                               ),
                               column(width = 4,
                                 tabBox(id = "stat", width=NULL,
                                        tabsetPanel(id="tabset1",
                                                    tabPanel(HTML("<h4>Area Intactness and Hydrology statistics</h4>"), tableOutput("tab1"),
                                                             "*Catchment Area Weighted Intactness",)
                                        )
                                 )),
                               column(width = 4,
                                 uiOutput("dynamicTabs")
                               )
                       )
                     )
                   )
                   )
)
