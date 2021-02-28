library(shiny)
library(shinyjs)
library(shinyBS)
library(plotly)
library(ggplot2)
library(shinyLi)
library(jsonlite)
library(shiny.semantic)
library(shinycssloaders)
library(rclipboard)

theme_list = c("theme_grey()","theme_bw()","theme_light()","theme_dark()","theme_minimal()","theme_classic()")

shinyUI(
    #fluidPage(
    semanticPage(
        title = "GR browser - LINCS Dose-Response Datasets",
        # adding head section to html with links to CSS files
        tags$head(
            tags$link(href="css/ilincs.css",rel="stylesheet"),
            tags$link(href="css/grbrowser.css",rel="stylesheet"),
            tags$link(href="css/AboutGRMetrics.css",rel="stylesheet")
        ),
        tags$head(rclipboardSetup()),
        # displaying header
        #includeHTML("www/html/nav.html"),
        useShinyjs(),
        suppressDependencies("bootstrap"),
        # Fix for mobile viewing
        tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
        # CSS for sizing of data table search boxes
        inlineCSS(".form-control {
                box-sizing: border-box;
                }"),
        # CSS for hiding border on horizontal segments
        tags$style(type = "text/css", "
                .ui.noshadow.segments {
                box-shadow: none;
                border: none;
                margin-top: 0px;
                margin-bottom: 0px;
                }"
        ),

        #### about modal start #########
        div(class = "ui small modal", id = "about_modal",
            div(class = "header", "About",
                div(class = "actions", style = "float: right; display: inline-block; vertical-align: top;",
                    div(class = "ui red basic circular cancel icon button", uiicon(type = "window close"))
                )
            ),
            div(class = "ui center aligned basic segment",
                includeMarkdown("www/about.md")
            )
        ),
        ######## about modal end #########
        #### contact modal start #########
        div(class = "ui mini modal", id = "contact_modal",
            div(class = "header", "Contact us",
                div(class = "actions", style = "float: right; display: inline-block; vertical-align: top;",
                    div(class = "ui red basic circular cancel icon button", uiicon(type = "window close"))
                )
            ),
            div(class = "ui center aligned basic segment",
                includeMarkdown("www/contact.md")
            )
        ),
        ######## contact modal end #########

        ####### Bookmark modal start #####
        div(class = "ui small modal", id = "bookmark_modal",
            div(class = "header", "Link to dataset",
                div(class = "actions", style = "float: right; display: inline-block; vertical-align: top;",
                    div(class = "ui red basic circular cancel icon button", uiicon(type = "window close"))
                )
            ),
            div(class = "ui basic segment",
                div(class = "ui fluid action input",
                    tags$input(type = "text", value = "", id = "bookmark_input", style = "width:100%;")#,
                    # div(class = "ui button greay right labeled icon button", id = "clipbtn",
                    #     tags$i(class = "copy icon"),
                    #     # UI ouput for the copy-to-clipboard buttons
                    #     #uiOutput("clipbtn", inline = T),
                    #     "Copy"
                    # )
                ),
                uiOutput("clipbtn", inline = T)
            )
        ),
        ###### Bookmark modal end ########

        ###### graph popup modal start #########
        div(class = "ui small modal", id = "graph_modal",
            div(class = "header", "Graph Popup",
                div(class = "actions", style = "float: right; display: inline-block; vertical-align: top;",
                    div(class = "ui red basic circular cancel icon button", uiicon(type = "window close"))
                )
            ),
            div(class = "ui center aligned basic segment",
                plotlyOutput("graphPopupPlot") %>% withSpinner(type = 2, color = "black", color.background = "#ffffff")
            )
        ),

        ##### graph popup modal end ##########

        # bsModal("graphPopup", "Graph Popup", "triggerGraphPopup",
        #         size="large", plotlyOutput("graphPopupPlot") )

        div(class = "ui container", style = "width: inherit!important; display: flex; min-height: 100vh; flex-direction: column;",
            ######### top menu start ########
            div(class = "ui top attached inverted six item stackable menu",  style = "flex: 0.1;",
                div(class = "ui center aligned container",
                    a(class = "item", img(class = "logo", src = "dcic.png"),
                    href = "http://lincs-dcic.org/"),
                    #a(class = "item", "Home", href = "/grtutorial/Home.html", style = "font-size: 16px; padding: 5px; margin: 0px;"),
                    a(class = "item", "About GR Metrics", href = "/grtutorial/", style = "font-size: 16px; padding: 5px; margin: 0px;"),
                    a(class = "item", "Online GR Calculator", href = "/grcalculator/", style = "font-size: 16px; padding: 5px; margin: 0px;"),
                    a(class = "item", "LINCS Dose-Response Datasets", href = "/grbrowser/", style = "font-size: 16px; padding: 5px; margin: 0px;"),
                    a(class = "item", "Support", href = "/grtutorial/support.html", style = "font-size: 16px; padding: 5px; margin: 0px;"),
                    a(class = "item", img(class = "logo", src = "logo_harvard_150.png"),
                    href = "http://sorger.med.harvard.edu" )
                )
            ),
            ######### top menu end ########
            #div(class = "ui main basic segment", style = "min-height: 70vh",
            #####
            #),
            div(class = "ui two column grid",  style = "min-height: 100vh; max-width: 1200px;",
                    div(class = "row",
                        div(class = "four wide column",
                        div(class="ui basic segment", #style = "padding-top: 0px;,
                            tags$h4("Select Dataset to Browse"),
                            radioButtons('dataSet', '', choices = c('') ),
                            div(class="ui basic center aligned segment",
                                div(class = "ui button action-button", id = "datasetURL", "Get Bookmark", icon = shiny::icon("link", lib = "glyphicon"))
                            ),
                            #actionButton("datasetURL", "Get Bookmark", icon = shiny::icon("link", lib = "glyphicon")),
                            hr(),
                            div(class = "row",
                                actionLink("subset_data", "Subset data")
                            ),
                            div(class = "row",
                                conditionalPanel(condition = "input.subset_data%2==1", uiOutput("subset_selectize"))
                            )
                        )
                        ),
                        div(class = "twelve wide column",
                            uiOutput("datasetTitle"),
                            conditionalPanel(condition = "input.dataset_title%2==0", uiOutput("datasetInfo")),
                            div(class="ui top basic secondary pointing menu", id = "tabs",
                                a(class="active item", `data-tab`="first", "Dose-Response Grid", id = "drc_grid"),
                                a(class="item", `data-tab`="second", "GR Metric Comparison", id = "gr_metrics"),
                                a(class="item", `data-tab`="third", "Data Table", id = "data_tables")
                            ),
                            ######### first tab start #########
                            div(class="ui active bottom center basic tab segment", `data-tab`="first", id = "first_tab",
                                div(class = "ui container", style = "max-width: 1024px!important;",
                                    liDoseResponseGrid(anchorId="dose-response-grid-main")
                                )
                            ),
                            ######### second tab start #########
                            shinyjs::hidden(
                            div(class="ui active bottom center basic tab segment", `data-tab`="second", id = "second_tab",
                                div(class = "ui container", style = "max-width: 1024px!important;",
                                    div(class = "ui four column grid",
                                    div(class = "row",
                                        div(class = "three wide column",
                                            radioButtons('box_scatter_choice', label = NULL, choices = c("Box plot", "Scatter plot"), inline = F)
                                        ),
                                        div(class = "four wide column",
                                            div(class = "ui secondary button",
                                                downloadLink("downloadScatter", "Download image",
                                                style = "color: white;")
                                            )
                                            #downloadButton('downloadScatter', label = "Download image")
                                            #div(class = "ui button action-button", id = "downloadScatter", "Download image")
                                        ),
                                        div(class = "two wide column",
                                            radioButtons('scatterImageType', label = NULL, choices = c('.pdf', '.tiff'), inline = F)
                                        ),
                                        div(class = "four wide column",
                                            div(class = "ui button action-button", id = "options_panel", "Plot Options")
                                            #actionButton('options_panel', 'Plot Options')
                                        )
                                    )
                                    ),
                                    conditionalPanel(condition = "input.options_panel%2==1",
                                    div(class = "ui six column grid",
                                    div(class = "row",
                                        div(class = "two wide column",
                                            sliderInput('plot_height', 'Plot height', min = 300, max = 900, step = 50, value = 600)
                                        ),
                                        div(class = "two wide column",
                                            sliderInput('plot_title_size', 'Main Title Size', min = 1, max = 50, step = 1, value = 20)
                                        ),
                                        div(class = "two wide column",
                                            sliderInput('axis_title_size', 'Y-Axis Title Size', min = 1, max = 30, step = 1, value = 16)
                                        ),
                                        div(class = "two wide column",
                                            sliderInput('axis_label_size', 'X-axis Label Size', min = 1, max = 30, step = 1, value = 11)
                                        ),
                                        div(class = "two wide column",
                                            sliderInput('label_rotate', 'X-axis label rotation', min = -90, max = 90, step = 5, value = -45)
                                        ),
                                        div(class = "two wide column",
                                            sliderInput('bottom_margin', 'Bottom Margin', min = 1, max = 200, step = 5, value = 60)
                                        )
                                    )
                                    ),
                                    div(class = "ui four column grid",
                                    div(class = "row",
                                        div(class = "four wide column",
                                            selectInput('theme_select', "Select Theme", choices = theme_list)
                                        ),
                                        shinyjs::hidden(
                                        div(class = "two wide column",
                                            selectInput('add_units', "Select units", choices = c("", "nanomolar", "micromolar"))
                                        ),
                                        div(class = "two wide column",
                                            div(class = "ui fluid action input",
                                            tags$input(type = "text", value = "", id = "plot_title", placeholder="Plot title", style = "width:100%;")
                                        )
                                            #textInput('plot_title', 'Plot title')
                                        ),
                                        div(class = "two wide column",
                                            div(class = "ui fluid action input",
                                            tags$input(type = "text", value = "", id = "x_label", placeholder="X axis label", style = "width:100%;")
                                        )
                                            #textInput('x_label', 'X axis label')
                                        )
                                        )
                                    )
                                    )
                                    ),
                                    div(class = "ui two column grid",
                                    div(class = "row",
                                        div(class = "four wide column",
                                            uiOutput("scatter")
                                        ),
                                        div(class = "eight wide column",
                                            uiOutput("grmetric_plot_ui")
                                        )
                                    )
                                    )
                                )
                            )
                            ),
                            ######### third tab start #########
                            shinyjs::hidden(
                            div(class="ui active bottom center basic tab segment", `data-tab`="third", id = "third_tab",
                                div(class = "ui container", style = "max-width: 1024px!important;",
                                    # div(class = "row",
                                    #     div(class = "three wide column",
                                    #         downloadButton('downloadData', label = 'Download data table')
                                    #     ),
                                    #     div(class = "two wide column",
                                    #         radioButtons('download_type', label = "", choices = c("csv", "tsv"), inline = T)
                                    #     )
                                    # ),
                                    DT::dataTableOutput("input_table"),
                                    tags$style(type='text/css', "#input_table { white-space: nowrap; text-overflow: ellipsis; overflow: scroll;}")
                                )
                            )
                            )
                        )
                    )
                ),
            ######### footer start #########
            div(class = "ui bottom attached inverted footer segment", style = "margin: 0px; flex: 1; position:sticky;",
                div(class = "ui center aligned container", style = "height: 50px",
                    div(class = "ui horizontal inverted large divided link list",
                        a(class = "item", div(class = "action-button", "About", id = "about") ),
                        a(class = "item", div(class = "action-button", "Contact Us", id = "contact")),
                        a(class = "item", "Github", uiicon("github"), href = "https://github.com/uc-bd2k/grcalculator/")
                    )
                )
            )
        ######### footer end ########
        )
    )
)
