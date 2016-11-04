library(shiny)
library(shinyjs)
library(shinyBS)
library(plotly)
library(ggplot2)
library(shinyLi)
library(jsonlite)

js.extra <- '
$(document).ready(function() {

  // Wait until the server has updated the dataSet radio buttons with the
  // actual list of datasets.
  window.observer = new MutationObserver(function() {

    // Stop observing so we only run this setup code once.
    observer.disconnect();

    // Set a change handler on the radio buttons so that changing the selected
    // dataset automatically loads it (i.e. remove the need to click the
    // Browse button). There is probably a proper Shiny way to do this but I
    // think it would require some heavy refactoring in server.R.
    $("input:radio[name=dataSet]").change(function() {
      $("#browseDataset").trigger("click");
    });

    // Hide the button since the user will not need to click it anymore,
    // and click it to load the default dataset.
    $("#browseDataset").hide().trigger("click");

  });
  var options = {childList: true, subtree: true};
  observer.observe(document.getElementById("dataSet"), options);
});
'

shinyUI(
    fluidPage(
        # adding head section to html with links to CSS files
        tags$head(tags$link(href="css/ilincs.css",rel="stylesheet"),
                  tags$link(href="css/grbrowser.css",rel="stylesheet"),
                  tags$link(href="css/AboutGRMetrics.css",rel="stylesheet"),
                  tags$script(js.extra)),
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
            actionButton("browseDataset", "Browse")
        ),
        # Main column
        column(
            10,
            uiOutput("datasetInfo"),
            tabsetPanel(
                id = "tabs",
                # Dose-response grid tab
                tabPanel(
                    value="tab-drc",
                    "Dose Response Grid",
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
