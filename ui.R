library(shiny)
library(shinyjs)
library(shinyBS)
library(plotly)
library(ggplot2)
library(shinyLi)
library(jsonlite)
theme_list = c("theme_grey()","theme_bw()","theme_light()","theme_dark()","theme_minimal()","theme_classic()")

shinyUI(
    fluidPage(
        # adding head section to html with links to CSS files
        tags$head(tags$link(href="css/ilincs.css",rel="stylesheet"),
                  tags$link(href="css/grbrowser.css",rel="stylesheet"),
                  tags$link(href="css/AboutGRMetrics.css",rel="stylesheet")),
        # displaying header
        includeHTML("www/html/nav.html"),
        useShinyjs(),
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
            uiOutput("datasetTitle"),
            conditionalPanel(condition = "input.dataset_title%2==0", uiOutput("datasetInfo")),
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
                                'box_scatter_choice', label = NULL,
                                choices = c("Box plot", "Scatter plot"), inline = T
                            )
                        ),
                        column(2,
                            downloadButton('downloadScatter', label = "Download image")
                        ),
                        column(
                            2,
                            radioButtons(
                                'scatterImageType', label = NULL,
                                choices = c('.pdf', '.tiff'), inline = T
                            )
                        ),
                        column(3, actionButton('options_panel', 'Plot Options'))
                    ),
                    
                    conditionalPanel(condition = "input.options_panel%2==1",
                     fluidRow(
                       column(2,
                              textInput('plot_title', 'Plot title')),
                       column(2,
                              sliderInput('plot_title_size', 'Main Title Size', min = 1, max = 50, step = 1, value = 20)),
                       column(2,
                              sliderInput('axis_title_size', 'Y-Axis Title Size', min = 1, max = 30, step = 1, value = 16)),
                       column(2,
                              sliderInput('axis_label_size', 'X-axis Label Size', min = 1, max = 30, step = 1, value = 11)),
                       column(2, 
                              sliderInput('label_rotate', 'X-axis label rotation', min = -90, max = 90, step = 5, value = -45)),
                       column(2,
                              sliderInput('bottom_margin', 'Bottom Margin', min = 1, max = 200, step = 5, value = 60))
                     ),
                     fluidRow(
                       column(2, selectInput('theme_select', "Select Theme", choices = theme_list)),
                       column(2, sliderInput('plot_height', 'Plot height', min = 300, max = 900, step = 50, value = 600))
                     )
                    ),
                    fluidRow(
                        column(2, uiOutput("scatter")),
                        column(10, uiOutput("grmetric_plot_ui"))
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
