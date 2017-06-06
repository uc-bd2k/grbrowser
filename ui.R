library(shiny)
library(shinyjs)
library(shinyBS)
library(plotly)
library(ggplot2)
library(shinyLi)
library(jsonlite)

shinyUI(
    fluidPage(
        # adding head section to html with links to CSS files
        tags$head(tags$link(href="css/ilincs.css",rel="stylesheet"),
                  tags$link(href="css/grbrowser.css",rel="stylesheet"),
                  tags$link(href="css/AboutGRMetrics.css",rel="stylesheet")),
        # displaying header
        includeHTML("www/html/nav.html"),
        useShinyjs(),
        titlePanel("Browse LINCS Dose-Response Datasets"),
        hr(),
        # Side column
        column(
            2,
            class="leftColWidth",
            radioButtons(
                'dataSet', 'Select Dataset to Browse', choices = c('')
            ),
            actionButton("datasetURL", "Get Bookmark", icon = shiny::icon("link", lib = "glyphicon"))
            ),
        # Main column
        column(
            10,
            uiOutput("datasetInfo"),
            bsModal("URLpopup", "Link to Dataset", "datasetURL", size = "large",
                    textInput("bookmark_input", label= NULL, value = "", width = '100%')
                    ),
            tabsetPanel(
                id = "tabs",
                # Dose-response grid tab
                tabPanel(
                    value="tab-drc",
                    "Dose-Response Grid",
                    fluidRow(
                        column(
                            12,
                            liDoseResponseGrid(anchorId="dose-response-grid-main"),
                            bsModal(
                                "graphPopup", "Graph Popup", "triggerGraphPopup",
                                size="large", plotlyOutput("graphPopupPlot")
                            )
                        ),
                        width=12
                    )
                ),
                # GR metric comparison (scatterplot) tab
                tabPanel(
                    value="tab-gr",
                    "GR Metric Comparison",
                    fluidRow(
                        column(
                            3,
                            radioButtons(
                                'box_scatter', label = '',
                                choices = c("Box plot", "Scatter plot"), inline = T
                            )
                        ),
                        column(
                            2,
                            offset=2,
                            downloadButton('downloadScatter', label = "Download image")
                        ),
                        column(
                            2,
                            radioButtons(
                                'scatterImageType', label = '',
                                choices = c('.pdf', '.tiff'), inline = T
                            )
                        ),
                        column(1, tags$div(id='plotBoxL2',"Plot height")),
                        column(2, textInput('scatter_height', NULL, value = 700))
                    ),
                    fluidRow(
                        column(2, uiOutput("scatter")),
                        column(10, uiOutput("plot.ui2"))
                    ),
                    fluidRow(
                      column(2,
                             textInput('plot_title', 'Plot title')),
                      column(2,
                             sliderInput('axis_label_size', 'X-axis Label Size', min = 1, max = 30, step = 1, value = 11)),
                      column(2,
                             sliderInput('axis_title_size', 'Y-Axis Title Size', min = 1, max = 30, step = 1, value = 16)),
                      column(2,
                             sliderInput('plot_title_size', 'Main Title Size', min = 1, max = 50, step = 1, value = 20)),
                      column(2,
                             sliderInput('bottom_margin', 'Bottom Margin', min = 1, max = 200, step = 5, value = 60)),
                      column(2, 
                             sliderInput('label_rotate', 'X-axis label rotation', min = -90, max = 90, step = 5, value = -45))
                      )
                    
                ),
                # Data Tables tab
                tabPanel(
                    value="tab-data",
                    "Data Table",
                    fluidRow(
                        column(
                            3,
                            downloadButton('downloadData', label = 'Download data table')
                        ),
                        column(
                            2,
                            radioButtons('download_type', label = "",
                                         choices = c("csv", "tsv"), inline = T)
                        )
                    ),
                    DT::dataTableOutput("input_table")
                )
            )
        )
    )
)
