library(shiny)
library(shinyjs)
library(plotly)
library(ggplot2)
library(drc)


groupingColumns = NULL

source('functions/drawScatter.R', local = T)
source('functions/drawPopup.R')
source('functions/drawExamplePopup.R')
source('functions/extractData.R', local = T)
source('functions/update_browse_selector.R')
source('functions/parseLabel.R')


shinyServer(function(input,output,session) {
  
  values <- reactiveValues(config=c(),data=c(),showtabs=0)

  update_browse_selector(session,c())
  
  drg_demo_data <<- read.table("www/averaged_seeding1250_example_fixed.tsv", sep="\t", header=TRUE, check.names=FALSE, fill=TRUE)
  output$drg_demo <- renderLiDoseResponseGrid(
    input="",
    xmin = -4,
    xmax = 2,
    factors=c("DrugName","CellLine"),
    toggle=1,
    drg_demo_data
  )
  
  output$input_table <- DT::renderDataTable(DT::datatable({
    x<-values$data
    print(colnames(x))
    data.frame(x)
  }, rownames= FALSE))

  observe({
    toggle(condition = values$showtabs, selector = "#tabs li a[data-value=tab-data]")
    toggle(condition = values$showtabs, selector = "#tabs li a[data-value=tab-drc]")
    toggle(condition = values$showtabs, selector = "#tabs li a[data-value=tab-gr]")
  })
  
  observeEvent(input$browseDataset, {
    if (!is.null(input$dataSet) && input$dataSet != "") {

      inFile <- input$dataSet
      
      if (is.null(inFile))
        return(NULL)
      
      cat('file ', inFile, ' received\n')
      json_data <- readLines(inFile)
      
      values$config <- fromJSON(json_data)
      
      output$description <- renderUI({
        HTML(values$config$description)
      })

      values$data <- read.table(values$config$datafile, sep="\t", header=TRUE, check.names=FALSE, fill=TRUE, stringsAsFactors = F)
      
      if (length(values$config$filterColumns)) { values$data <- values$data[values$config$filterColumns] }
      if (length(values$config$renameColumns)) { 
        colnames(values$data)[which(colnames(values$data) %in% names(values$config$renameColumns))] <- unlist(values$config$renameColumns)
      }
      if (length(values$config$groupableColumns)==0) { values$config$groupableColumns = colnames(values$data) }

      updateSelectizeInput(session, 'doseresponsegrid_choiceVar', choices = values$config$groupableColumns, server = TRUE, selected=values$config$doseresponse$defaultChoicevar)
      updateSelectizeInput(session, 'doseresponsegrid_groupingVars', choices = values$config$groupableColumns, server = TRUE, selected=values$config$doseresponse$defaultGroupingVars)

      doseresponsegrid_hideselector <- values$config$doseresponse$hideselector
      if (is.null(doseresponsegrid_hideselector)) { doseresponsegrid_hideselector <- 0; }
      updateSelectizeInput(session, 'doseresponsegrid_hideselector', selected=doseresponsegrid_hideselector)
      
      full_data <<- extractData(input, output, values,
                                           values$config$doseresponse$defaultChoicevar,
                                           values$config$doseresponse$defaultGroupingVars)
      
      output$'dose-response-grid-main' <- renderLiDoseResponseGrid(
          input="",
          xmin = -4,
          xmax = 2,
          factors=c(paste(values$config$doseresponse$defaultGroupingVars,collapse = '_'), values$config$doseresponse$defaultChoicevar),
          toggle=values$config$doseresponse$toggle,
          data=full_data
        )
      groupingColumns <<- values$config$groupableColumns
      values$showtabs=1
    }
  })
 
#======== Example dose-response grid ========= 
  observeEvent(input$drg_demo, {
    if(input$drg_demo != '') {
      q = drawExamplePopup(input, values)
      output$graphPopupPlotDemo <- renderPlotly({
        try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
        ggplotly(q)
        layout(
          xaxis = list(range = c(-4,2),
                       tickmode = 'linear',
                       tick0 = -5,
                       dtick = 1)
        )
       })
      toggleModal(session,"graphPopupDemo")
    }
  })
  
#========== Main dose-response grid =============
  observeEvent(input$'dose-response-grid-main', {
    q = parseLabel(input, values, full_data)
    if (input$'dose-response-grid-main' != '') {
      output$graphPopupPlot <- renderPlotly({
        try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
        ggplotly(q)
        layout(
          xaxis = list(range = c(-4,2),
                       tickmode = 'linear',
                       tick0 = -5,
                       dtick = 1)
        )
      })
      toggleModal(session,"graphPopup")
    }
  })

#========== Download button for scatterplot images =======
  output$downloadScatter = downloadHandler(
    filename = function() {
      return(paste(sub("^(.*)[.].*", "\\1", input$dataSet), input$scatterImageType, sep=''))
    },
    content = function(filename) {
      if(input$scatterImageType == '.eps') {
        ggsave(filename = filename, plot = plotScatter, device = "eps")
      } else {
        ggsave(filename = filename, plot = plotScatter, device = "tiff", units = "in", width = 7, height = 7, dpi = 300)
      }
    }
  )

#========== Download button data tables =======
  output$downloadData <- downloadHandler(
    filename = function() {
      return(paste(sub("^(.*)[.].*", "\\1", input$dataSet), ".", input$download_type, sep=''))
      },
    content = function(filename) {
      data_output = values$data
      if(input$download_type == "tsv") {
        write.table(data_output, file = filename, quote = F, sep = '\t', row.names = F, col.names = T)
      } else if(input$download_type == "csv") {
        write.table(data_output, file = filename, quote = F, sep = ',', row.names = F, col.names = T)
      }
      
    }
    #,
    #contentType = paste('text/', input$download_type, sep = "")
  )

#=========================================

observeEvent(input$plot_scatter, {
  output$plotlyScatter1 <- renderPlotly({
    plot1 = isolate(drawScatter(input, values))
print(1.1)    
try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
    ggplotly(plot1)
    layout(plot1, 
           margin = list(
             r = 10, 
             t = 80, 
             b = 60, 
             l = 100)
           )
print(1.22)    
  })
})

print(1.5)    
observeEvent(input$plot_scatter, {
  drawScatter(input, values)
  output$plotlyScatter1 <- renderPlotly({
    plot1 = isolate(drawScatter(input, values))
    ggplotly(plot1)
    layout(plot1, 
           margin = list(
             r = 10, 
             t = 80, 
             b = 60, 
             l = 100)
    )
    
  })
})

# Make scatterplot reactive to "pick_parameter" after first plot.
observeEvent(input$pick_parameter, {
  if(input$plot_scatter > 0) {
    output$plotlyScatter1 <- renderPlotly({
      plot1 = isolate(drawScatter(input, values))
      ggplotly(plot1)
      layout(plot1,
             margin = list(
               r = 10, 
               t = 80, 
               b = 60, 
               l = 100)
      )
    })
  }
})

output$scatter1.ui <- renderUI({
  plotlyOutput("plotlyScatter1", height = input$scatter_height)
})
  
observeEvent(input$browseDataset, {
  updateSelectInput(
    session, 'pick_var',
    choices = values$config$groupableColumns
  )
  print(values$config$groupableColumns)
  print(values$config$renameColumns)
  print(values$config$filterColumns)
})

observeEvent(input$pick_var, {
  updateSelectInput(
    session, 'x_scatter',
    choices = sort(unique(full_data[[input$pick_var]])),
    selected = NULL
  )
  updateSelectizeInput(
    session, 'y_scatter',
    choices = sort(unique(full_data[[input$pick_var]])),
    selected = NULL
  )
})

#==== Clear scatterplot on "browse" =========

observeEvent(input$browseDataset, {
  output$plotlyScatter1 <- renderPlotly({
    parameter_choice = input$pick_parameter
    if(parameter_choice == 'GR50') {
      parameter_choice = 'log10[GR50]'
    }
    if(parameter_choice == 'Hill') {
      parameter_choice = 'log2[HillSlope]'
    }
    padding = 0.05
    scatter_values = full_data[,parameter_choice]
    finite_values = which(is.finite(scatter_values))
    scatter_values = scatter_values[finite_values]
    x_min = min(scatter_values, na.rm = T)
    x_max = max(scatter_values, na.rm = T)
    y_min = min(scatter_values, na.rm = T)
    y_max = max(scatter_values, na.rm = T)
    all_max = max(abs(c(x_max, y_max, x_min, y_min)), na.rm = T)
    all_range = 2*all_max
    all_max = all_max + padding*all_range
    all_min = -all_max
    #plug in a filler data frame
    p = ggplot(data = mtcars, aes(x = mpg, y = wt)) + geom_abline(slope = 1, intercept = 0, size = .25) + scale_x_continuous(limits = c(all_min, all_max)) + scale_y_continuous(limits = c(all_min, all_max)) + coord_fixed() + xlab('') + ylab('') + ggtitle('') + geom_blank()
    
    df_full <<- NULL
    print(3)
    try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
    ggplotly(p)
    print(4)
    layout(p, hovermode = FALSE)
  })
})

#===== Clear button ========
observeEvent(input$clear, {
  output$plotlyScatter1 <- renderPlotly({
    parameter_choice = input$pick_parameter
    if(parameter_choice == 'GR50') {
      parameter_choice = 'log10[GR50]'
    }
    if(parameter_choice == 'Hill') {
      parameter_choice = 'log2[HillSlope]'
    }
    padding = 0.05
    scatter_values = full_data[,parameter_choice]
    finite_values = which(is.finite(scatter_values))
    scatter_values = scatter_values[finite_values]
    x_min = min(scatter_values, na.rm = T)
    x_max = max(scatter_values, na.rm = T)
    y_min = min(scatter_values, na.rm = T)
    y_max = max(scatter_values, na.rm = T)
    all_max = max(abs(c(x_max, y_max, x_min, y_min)), na.rm = T)
    all_range = 2*all_max
    all_max = all_max + padding*all_range
    all_min = -all_max
    
    p = ggplot(data = df_sub, aes(x = get(paste0(parameter_choice,'.x'), envir = as.environment(df_sub)), y = get(paste0(parameter_choice,'.y'), envir = as.environment(df_sub)))) + geom_abline(slope = 1, intercept = 0, size = .25) + scale_x_continuous(limits = c(all_min, all_max)) + scale_y_continuous(limits = c(all_min, all_max)) + coord_fixed() + xlab('') + ylab('') + ggtitle('') + geom_blank()

    df_full <<- NULL
print(3)
try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
    ggplotly(p)
print(4)
    layout(p, hovermode = FALSE)

  })
  
})
  cancel.onSessionEnded <- session$onSessionEnded(function() {
    graphics.off()
    print('devices off')
  })
})
