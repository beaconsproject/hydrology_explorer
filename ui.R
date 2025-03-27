ui = dashboardPage(skin="blue",
                   dashboardHeader(title = div("Hydrology Explorer", 
                                               style = "position: relative;  display:right-align;"
                   ), 
                   #titleWidth=300,
                   tags$li(
                     actionButton("reset_button", 
                                  label = "Reset AOI", 
                                  icon = icon("arrows-rotate"),
                                  style = "background-color: #FF0000; color: white; border: none; width: 100px; height: 50px; font-size: 16px;"),
                     class = "dropdown",
                     style = "background-color: #FF0000; color: white; border: none; width: 100px; height: 50px; font-size: 16px;")
                   ),
                   #dashboardHeader(title = "Hydrology Explorer"),
                   dashboardSidebar(
                     width = 275,
                     sidebarMenu(id = "tabs",
                                 menuItem("Overview", tabName = "overview", icon = icon("th")),
                                 menuItem("Explorer", tabName = "explorer", icon = icon("th"), startExpanded = TRUE,
                                          menuSubItem("Select study area", tabName = "selectSA", icon = icon("th")),                
                                          menuSubItem("Select AOI", tabName = "selectAOI", icon = icon("th")),
                                          menuSubItem("Generate upstream and downstream", tabName = "upstream", icon = icon("arrows-rotate"))),
                                 menuItem("Download results", tabName = "download", icon = icon("th")),
                                 hr(),
                                 hr()
                     ),
                     conditionalPanel(
                       condition="input.tabs=='selectSA'",
                       HTML("<h4>&nbsp; &nbsp; Specify study area</h4>"),
                       selectInput("select_wsd", label= div(style = "font-size:13px","Use watershed unit"), choices=c("Please select", hydrounit_list), selected="Select an FDA"),
                       fileInput(inputId = "upload_dist", label = div(style = "font-size:13px", "Or upload a file. Geopackage (gpkg) is the supported format. 
       Use output from Disturbance Explorer or a custom gpkg.  Study area layer must be named “studyarea”. 
       If the gpkg does not include intactness (“intactness”) and/or human footprint (“footprint”) layers, 
       alternative options are provided below."), multiple = FALSE, accept = ".gpkg"),
                       radioButtons("intactSource", "Select source for catchment intactness:",
                                    choices = list("Value in catchment dataset" = "intcatch", 
                                                   "Intactness layer in geopackage (gpkg)" = "intgpkg",
                                                   "Upload intactness or footprint layer" = "intupload"),
                                    selected = character(0), 
                                    inline = FALSE),
                       conditionalPanel(
                         condition = "input.intactSource == 'intcatch'",
                         selectInput("intactColumnName", "Catchment dataset - select intactness attribute", choices = NULL)
                       ),
                       conditionalPanel(
                         condition = "input.intactSource == 'intupload'",
                         radioButtons("intacttype", "Select the type of custom layer:",
                                      choices = list("Intactness map" = "intmap", 
                                                     "Footprint map" = "footmap"),
                                      selected = character(0),
                                      inline = TRUE),
                         fileInput("shp_intact", "Upload a Shapefile", multiple = TRUE, accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg')),
                       ),
                       actionButton("previewLayers", "Preview study area", icon = icon(name = "map-location-dot", lib = "font-awesome"), class = "btn-warning", style="width:200px"),
                       
                     ),
                     conditionalPanel(
                       condition="input.tabs=='selectAOI'",
                       HTML("<h4>&nbsp; &nbsp; Choose Area of Interest (AOI) from &nbsp; &nbsp; one of the two options:</h4>"),
                       tags$br(),
                       fileInput(inputId = "upload_poly", label = div(style = "font-size:13px", "OPTION 1 - Upload polygon(s) (gpkg) to select underlying catchments"), multiple = FALSE, accept = ".gpkg"),
                       uiOutput("conditional_ui"),
                       hr(),
                       div(style = "margin: 15px; font-size:13px; font-weight: bold", "OPTION 2 - Select a set of catchments on the map"),
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       actionButton(inputId = "gen_aoi_button", label = div(style = "font-size:13px;background-color:gey;color: black",HTML(" Confirm AOI boundary (Analysis AOI)")), icon = icon(name = "check", lib = "font-awesome"), class = "btn-warning", style="width:250px"),
                       tags$br(),
                       tags$br()
                     ), 
                     conditionalPanel(
                       condition="input.tabs=='upstream'",
                       actionButton("goButtonDown", label  = div(style = "font-size:13px;background-color:gey;color: black", HTML("View upstream and
                    <br /> downstream intactness")), icon = icon(name = "map-location-dot", lib = "font-awesome"), class = "btn-warning", style="width:200px")
                     ),     
                     conditionalPanel(
                       condition="input.tabs=='download'",
                       HTML("<h5>&nbsp; &nbsp; Specify EPSG <a href='https://spatialreference.org/'>spatial reference system</a> for <br> &nbsp; &nbsp; gpkg.</h5>"),
                       HTML("<h5>&nbsp; &nbsp; (Default: NAD83 Canada Albers)</h5>"),
                       textInput("textproj", "", value = "102001"),
                       tags$style(type="text/css", "#downloadData {background-color:gey;color: black}"),
                       div(style="position:relative; left:calc(10%);", downloadButton("downloadData", "Download results"))
                     )
                   ),
                   dashboardBody(
                     useShinyjs(),
                     tags$head(tags$style(".skin-blue .sidebar a { color: #8a8a8a; }")),
                     tabItems(
                       tabItem(tabName="overview",
                               fluidRow(
                                 tabBox(id = "one", width="8",
                                        tabPanel(HTML("Overview"), includeMarkdown("docs/overview.md")),
                                        tabPanel(HTML("Quick start"), includeMarkdown("docs/quick_start.md")),
                                        tabPanel(HTML("Dataset"), includeMarkdown("docs/datasets.md"))
                                 ),
                               )
                       ),
                       tabItem(tabName="selectSA",
                               fluidRow(
                                 tabBox(id = "three", width="8",
                                        tabPanel(HTML("<b>Mapview</b>"),
                                                 leafletOutput("map", height = 750) %>% withSpinner()
                                        )
                                 ),
                                 #tabBox(id = "two", width="4", title = HTML("<h4>Area Intactness and Hydrology statistics</h4>"),
                                 tabBox(id = "two", width="4",
                                        tabsetPanel(id="tabset1",
                                                    tabPanel(HTML("<h4>Area Intactness and Hydrology statistics</h4>"), tableOutput("tab1"),
                                                             "*Catchment Area Weighted Intactness",)
                                        )
                                 ),
                                 tabBox(id = "three", width="4",  
                                        tabsetPanel(id="tabset2",
                                                    tabPanel(HTML("<h4>Dendritic Connectivity Index (DCI)</h4>"), tableOutput("tabDCI")),
                                                    tabPanel(HTML("<h4>Fire statistics</h4>"), tableOutput("tabFires"))
                                        )
                                 )
                               )
                       )
                     )
                   )
)
