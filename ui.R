library(shiny)
library(shinyjs)
library(shinyBS)
library(plotly)
library(ggplot2)
library(shinyLi)
library(jsonlite)

shinyUI(
  fluidPage(
    #adding head section to html with links to CSS files
     tags$head(tags$link(href="css/ilincs.css",rel="stylesheet"))
     ,tags$head(tags$style("
      .leftColWidth{max-width: 150px; }"
     ))
    #displaying header
    ,includeHTML("www/html/nav.html")
    , useShinyjs()
    ,titlePanel("Browse LINCS Dose-Response Datasets")
    ,hr()
    # Side column
    ,column(2,class="leftColWidth",
        selectizeInput(
            'dataSet', 'Select Dataset to Browse', choices = c(''), multiple = F
        ),
        actionButton("browseDataset", "Browse")
    ),
    # Main column
    column(10,
        tabsetPanel(id = "tabs",
            # About tab
            tabPanel(value="tab-about",
                "Available Datasets",
                includeHTML("www/AvailableDatasets_top.html"),
                liDoseResponseGrid("drg_demo"),
                bsModal("graphPopupDemo", "Graph Popup Demo", "triggerGraphPopupDemo",
                        size = "large",
                        plotlyOutput("graphPopupPlotDemo")
                ),
                tags$div(tags$link(href="css/AboutGRMetrics.css",rel="stylesheet"),
					    includeHTML("www/AvailableDatasets.html"))
            ),
					  # Data Tables tab
            tabPanel(value="tab-data",
                      "Data Table",
                      fluidRow(
                        column(3, downloadButton('downloadData', label = 'Download data table')),
                        column(2, radioButtons('download_type', label = "", choices = c("csv", "tsv"), inline = T))
                      ),
                      DT::dataTableOutput("input_table"),
                      tags$style(type='text/css', "#downloadData { width:200px; margin-top: 10px; margin-bottom: 20px;}"),
                      tags$head(tags$style("#input_table  {white-space: nowrap;  }"))
            ),
					  # Dose-response grid tab
            tabPanel(value="tab-drc",
                "Dose Response Grid",
                fluidRow(
                  column(12,liDoseResponseGrid(anchorId="dose-response-grid-main"),
                         
                         bsModal("graphPopup", "Graph Popup", "triggerGraphPopup",
                         size = "large",
                         plotlyOutput("graphPopupPlot")
                         )
                  )
                  , width=12
                )
            ),
					  # GR metric comparison (scatterplot) tab
            tabPanel(value="tab-gr",
                     "GR Metric Comparison",
                  fluidRow(
                    column(2, selectInput('pick_parameter', 'Select parameter', choices = c('GR50', 'GRmax', 'GRinf', 'Hill', 'GR_AOC'))),
                    column(2, offset=2, downloadButton('downloadScatter', label = "Download image")),
                    column(1,radioButtons('scatterImageType', label = '', choices = c('.eps', '.tiff')), inline = T),
                    tags$style(type='text/css', "#downloadScatter { margin-top: 20px; margin-bottom: 0px; margin-left: 0px; margin-right: 0px}"),
                    tags$style(type='text/css', "#scatterImageType { margin-top: 0px; margin-bottom: 20px; margin-left: 0px;}"),
                    column(1,offset=1, tags$div(id='plotBoxL2',"Plot height")),
                    column(2, textInput('scatter_height', NULL, value = 700)),
                    tags$style(type='text/css', "#plotBoxL2 { white-space: nowrap; margin-top: 25px; margin-left: 10px; margin-right: 0px}"),
                    tags$style(type='text/css', "#scatter_height { width:50px; margin-top: 20px; margin-left: 20px; margin-right: 0px}"),
                    tags$style(type='text/css', "#clear { margin-top: 10px; margin-bottom: 10px; float: center;}"),
                    tags$style(type='text/css', "#plot_scatter { margin-top: 10px; margin-bottom: 10px; float: center}")
                  ),

                     fluidRow(                                    
                       column(2,
                              selectInput('pick_var', 'Select variable', choices = c()),
                              selectInput('x_scatter', 'Select x-axis value', choices = c()),
                              selectizeInput('y_scatter', 'Select y-axis value', choices = c()),
                              bsButton('plot_scatter', 'Add', size = 'small'),
                              bsButton('clear', 'Clear', size = 'small'),
                              tags$style(type='text/css', "#clear { margin-top: 10px; margin-bottom: 10px; float: center;}"),
                              tags$style(type='text/css', "#plot_scatter { margin-top: 10px; margin-bottom: 10px; float: center;}")
                       ),
                       column(10, uiOutput("scatter1.ui"))
                       
                     )
            )

        )
    )
)
)
