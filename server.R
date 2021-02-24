library(shiny)
library(shinyjs)
library(plotly)
library(ggplot2)
library(stringr)
library(markdown)


groupingColumns = NULL

source('functions/drawScatter.R', local = T)
source('functions/drawPopup.R')
source('functions/extractData.R', local = T)
source('functions/update_browse_selector.R')
source('functions/parseLabel.R')

shinyServer(function(input,output,session) {
  update_browse_selector(session,c())
  observeEvent(input$dataSet, {
    if(!is.null(input$dataSet)) {
      urlvalue = paste0("/grbrowser/?dataset=", gsub("^data_\\d+_(.*?)\\.json$", "\\1", input$dataSet), collapse = "")
      print(input$dataSet)
      updateTextInput(session, "bookmark_input", label = "test",
                      value = urlvalue)
    }
  })

  # output$bookmark <- renderText({
  #   paste0("/grbrowser/?dataset=",
  #          gsub("^data_\\d+_(.*?)\\.json$", "\\1", input$dataSet), collapse = "")
  # })
  observe({
    query <- parseQueryString(session$clientData$url_search)
    # Add underscores to dataset names for url
    
    URLnames = gsub(" ", "_", (names(dataset_choices)))
    print('URLnames')
    print(URLnames)
    if(length(query) > 0) {
      if (!is.null(query[['dataset']]) & query[['dataset']] %in% URLnames) {
        # Take out underscores from dataset name in URL
        URLchoice = gsub("_", " ", query[['dataset']])
        print('URLchoice')
        print(URLchoice)
        updateRadioButtons(session, "dataSet",
                           choices = dataset_choices,
                           selected = dataset_choices[[URLchoice]]
        )
      }
    }
  })
  
  observeEvent(input$dataSet, {
    print('dataSet')
    print(input$dataSet)
  })
  
  boxplot_data_global = NULL
  #=========== plotly boxplots ===========================
  redrawPlotlyBox <- function(input, values) {
    subset_data <<- full_data
    all_inputs <- names(input)
    for(col_name in all_inputs[grep("^subset__", all_inputs)]) {
      if (length(input[[col_name]])>0) {
        sel_values_list <- input[[col_name]]
        df_colname <- gsub("^subset__(.*)","\\1",col_name)
        subset_data <<- subset_data[which(subset_data[[df_colname]] %in% sel_values_list),]
      }
    }
    values$sub_data = subset_data
    
    parameter_choice = input$pick_box_y
    print(parameter_choice)
    parameter_choice_format = parameter_choice
    #print(df_sub)
    if(parameter_choice == 'GR50') {
      parameter_choice = 'log10(GR50)'
      parameter_choice_format = "log<sub>10</sub>(GR<sub>50</sub>)"
    }
    if(parameter_choice == 'Hill') {
      parameter_choice = 'log2(h_GR)'
      parameter_choice_format = "log<sub>2</sub>(h<sub>GR</sub>)"
    }
    if(input$dataSet %in% c("data_5_Genentech_Cell_Line_Screening_Initiative_(gCSI).json")) {
      if(parameter_choice == 'IC50') {
        parameter_choice = 'log10(IC50)'
        parameter_choice_format = "log<sub>10</sub>(IC<sub>50</sub>)"
        
      }
    }
    boxplot_data = subset_data[subset_data[[ input$pick_box_x ]] %in% input$pick_box_factors,]
    boxplot_data = boxplot_data[is.finite(boxplot_data[[parameter_choice]]),]
    boxplot_data[[ input$pick_box_x ]] = factor(boxplot_data[[ input$pick_box_x ]])
    
    if(!is.null(input$factorB) & !is.null(input$factorA)) {
      for(i in 1:length(input$factorB)) {
        boxplot_data[[ input$pick_box_x ]] = relevel(boxplot_data[[ input$pick_box_x ]], input$factorB[i])
      }
      for(i in 1:length(input$factorA)) {
        boxplot_data[[ input$pick_box_x ]] = relevel(boxplot_data[[ input$pick_box_x ]], input$factorA[i])
      }
    }
    
    x_factor = factor(get(input$pick_box_x, envir = as.environment(boxplot_data)))
    y_variable = get(parameter_choice, envir = as.environment(boxplot_data))
    point_color = factor(get(input$pick_box_point_color, envir = as.environment(boxplot_data)))
    
    unit_label = gsub("nanomolar", "nM", input$add_units)
    unit_label = gsub("micromolar", paste0("&#956;", "M"), unit_label)
    
    if(dim(boxplot_data)[1] > 0) {
      p <- ggplot(boxplot_data, aes(x = x_factor, y = y_variable)) +
        geom_boxplot(aes(fill = x_factor, alpha = 0.3), outlier.color = NA, show.legend = F) +
        geom_jitter(width = 0.2, show.legend = F, aes(colour = point_color)) +
        eval(parse(text = input$theme_select)) + theme(
          axis.text = element_text(size = input$axis_label_size),
          axis.title = element_text(size = input$axis_title_size),
          plot.title = element_text(size = input$plot_title_size),
          plot.margin = unit(c(5.5, 5.5, input$bottom_margin, 5.5), "points"),
          #top, right, bottom, left
          axis.text.x = element_text(angle = input$label_rotate, 
                                     hjust = 0.5*(1 - sin((-input$label_rotate)*pi/180)),
                                     vjust = 0.5*(1 + cos((-input$label_rotate)*pi/180)))
      ) +
        labs(title = input$plot_title, x = input$x_label)

      p_plotly = plotly_build(p)
      test_box <<- p_plotly
      # Get y range:
      if(is.null(p_plotly$layout)) {
        top_y = p_plotly$x$layout$yaxis$range[2]
        bottom_y = p_plotly$x$layout$yaxis$range[1]
      } else {
        top_y = p_plotly[[2]]$yaxis$range[2]
        bottom_y = p_plotly[[2]]$yaxis$range[1]
      }
      total_y_range = top_y - bottom_y
      
      if(!is.null(values$wilcox)) {
        # Get top of boxplot whiskers
        whiskers = NULL
        #for(i in 1:length(levels(x_factor))) {
        len = length(input$factorA) + length(input$factorB)
        for(i in 1:len) {
          if(is.null(p_plotly$data)) {
            whiskers[i] = fivenum(p_plotly$x$data[[i]]$y)[5]
          } else {
            whiskers[i] = fivenum(p_plotly[[1]][[i]]$y)[5]
          }
        }
        top_whisker = max(whiskers, na.rm = TRUE)
        y_range = (top_y - top_whisker)/total_y_range
        if(y_range < .25) {
          top_y = top_whisker + .25*total_y_range
          #y_range = top_y - top_whisker
        }
        lh = top_whisker + total_y_range*(.1)
        bump = total_y_range*(.05)
        ll = lh - bump
        lenA = length(input$factorA)
        lenB = length(input$factorB)
        
        if(lenA == 1 & lenB == 1) {
          p = p + annotate("text", x = 1.5, y = lh + bump/2, label = paste("p =",values$wilcox)) +
            geom_segment(x = 1, y = lh, xend = 2, yend = lh) +
            geom_segment(x = 1, y = ll, xend = 1, yend = lh) +
            geom_segment(x = 2, y = ll, xend = 2, yend = lh)
        } else if(lenA > 1 & lenB == 1) {
          p = p + annotate("text", x = ((lenA + 1) + ((lenA+1)/2))/2, y = lh + 2*bump, 
                           label = paste("p =",values$wilcox)) +
            geom_segment(x = 1, y = lh, xend = lenA, yend = lh) +
            geom_segment(x = 1, y = ll, xend = 1, yend = lh) +
            geom_segment(x = lenA, y = ll, xend = lenA, yend = lh) +
            geom_segment(x = (lenA+1)/2, y = lh + bump, xend = lenA + 1, yend = lh + bump) +
            geom_segment(x = (lenA+1)/2, y = lh, xend = (lenA+1)/2, yend = lh + bump) +
            geom_segment(x = lenA+1, y = ll, xend = lenA+1, yend = lh + bump)
        } else if(lenA == 1 & lenB > 1) {
          p = p + annotate("text", x = 1.25 + .25*lenB, y = lh + 2*bump, label = paste("p =",values$wilcox)) + 
            geom_segment(x = 1, y = lh+bump, xend = .5*lenB + 1.5, yend = lh+bump) +
            geom_segment(x = 1, y = ll, xend = 1, yend = lh+bump) +
            geom_segment(x = 1.5+.5*lenB, y = lh, xend = 1.5+.5*lenB, yend = lh+bump) +
            geom_segment(x = 2, y = lh, xend = lenB + 1, yend = lh) +
            geom_segment(x = 2, y = ll, xend = 2, yend = lh) +
            geom_segment(x = lenB+1, y = ll, xend = lenB+1, yend = lh)
        } else if(lenA > 1 & lenB > 1) {
          p = p + annotate("text", x = .25*(lenB-1)+.75*(lenA+1), y = lh + 2*bump,
                           label = paste("p =",values$wilcox)) + 
            geom_segment(x = 1, y = lh, xend = lenA, yend = lh) +
            geom_segment(x = 1, y = ll, xend = 1, yend = lh) +
            geom_segment(x = lenA, y = ll, xend = lenA, yend = lh) +
            geom_segment(x = lenA+1, y = lh, xend = lenA+lenB, yend = lh) +
            geom_segment(x = lenA+1, y = ll, xend = lenA+1, yend = lh) +
            geom_segment(x = lenA+lenB, y = ll, xend = lenA+lenB, yend = lh) +
            geom_segment(x = (lenA+1)/2, y = lh+bump, xend = (lenA+1)+((lenB-1)/2), yend = lh+bump) +
            geom_segment(x = (lenA+1)/2, y = lh, xend = (lenA+1)/2, yend = lh+bump) +
            geom_segment(x = (lenA+1)+((lenB-1)/2), y = lh, xend = (lenA+1)+((lenB-1)/2), yend = lh+bump)
        }
        p_plotly = p + labs(y = paste(parameter_choice_format, unit_label))
        if(input$add_units == "micromolar") {
          p_ggplot = p + labs(y = bquote(.(parameter_choice) ~ .(quote(mu)) * "M"))
        } else if(input$add_units == "nanomolar") {
          p_ggplot = p + labs(y = paste(parameter_choice, "nM"))
        } else {
          p_ggplot = p + labs(y = parameter_choice)
        }
        
        plotScatter_box <<- p_ggplot
        
        p_plotly = plotly_build(p_plotly)
        if(is.null(p_plotly$layout)) {
          p_plotly$x$layout$yaxis$range[2] = top_y
        } else {
          p_plotly[[2]]$yaxis$range[2] = top_y
        }
      } else {
        p_plotly = p + labs(y = paste(parameter_choice_format, unit_label))
        if(input$add_units == "micromolar") {
          p_ggplot = p + labs(y = bquote(.(parameter_choice) ~ .(quote(mu)) * "M"))
        } else if(input$add_units == "nanomolar") {
          p_ggplot = p + labs(y = paste(parameter_choice, "nM"))
        } else {
          p_ggplot = p + labs(y = parameter_choice)
        }
        
        plotScatter_box <<- p_ggplot
        p_plotly = plotly_build(p_plotly)
      }
      
      # Current CRAN version of plotly (3.6.0) uses p_plotly$data
      # Latest github version of plotly (4.3.5) uses p_plotly$x$data
      if(is.null(p_plotly$data)) {
        for(i in 1:length(p_plotly$x$data)) {
          if(!is.null(p_plotly$x$data[[i]]$text)) {
            p_plotly$x$data[[i]]$text = gsub('x_factor', input$pick_box_x, p_plotly$x$data[[i]]$text)
            p_plotly$x$data[[i]]$text = gsub('y_variable', parameter_choice, p_plotly$x$data[[i]]$text)
          } 
        }
      } else {
        for(i in 1:length(p_plotly$data)){
          if(!is.null(p_plotly$data[[i]]$text)) {
            p_plotly$data[[i]]$text = gsub('x_factor', input$pick_box_x, p_plotly$data[[i]]$text)
            p_plotly$data[[i]]$text = gsub('y_variable', parameter_choice, p_plotly$data[[i]]$text)
          }
        }
      }
      test_box <<- p_plotly
      return(p_plotly)
    }
  }
  
  values <- reactiveValues(config=c(),data=c(),showtabs=0, showtab_drc=1, sub_data = NULL)
  
  output$input_table <- DT::renderDataTable(DT::datatable({
    x<-values$data
    print(colnames(x))
    data.frame(x)
  }, rownames= FALSE))

  observe({
    toggle(condition = values$showtabs, selector = "#tabs li a[data-value=tab-data]")
    toggle(condition = values$showtabs, selector = "#tabs li a[data-value=tab-gr]")
    toggle(condition = values$showtabs_drc, selector = "#tabs li a[data-value=tab-drc]")
  })
  
  observeEvent(input$dataSet, {
    if (!is.null(input$dataSet) && input$dataSet != "") {
      inFile <- input$dataSet
      
      if (is.null(inFile)) {return(NULL)}
      
      cat('file ', inFile, ' received\n')
      json_data <- readLines(paste0("json/", inFile))
      
      values$config <- fromJSON(json_data)
      
      output$datasetTitle <- renderUI(
        tags$div(tags$h3(actionLink('dataset_title', values$config$title)))
      )
      
      output$datasetInfo <- renderUI(
        HTML(markdownToHTML(text=values$config$description, options=c('fragment_only')))
      )

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
      subset_data <<- full_data
      ####### for testing purposes
        test_full_data <<- full_data
        test_subset_data <<- subset_data
      #######
        if(!input$dataSet %in% c("data_6_Cancer_Therapeutics_Response_Portal_(CTRP).json")) {
          values$showtabs_drc = 1
      output$'dose-response-grid-main' <- renderLiDoseResponseGrid(
          input="",
          xmin = -4,
          xmax = 2,
          factors=c(paste(values$config$doseresponse$defaultGroupingVars,collapse = '_'), values$config$doseresponse$defaultChoicevar),
          toggle=values$config$doseresponse$toggle,
          data=subset_data
        )
      groupingColumns <<- values$config$groupableColumns
      values$showtabs=1
      } else {
        values$showtabs_drc = 0
        updateTabsetPanel(session,"tabs",selected="tab-gr")
      }
    }
  })
  
#========== Main dose-response grid =============
  observeEvent(input$'dose-response-grid-main', {
    q = parseLabel(input, values, subset_data)
    if (input$'dose-response-grid-main' != '' && str_count(input$'dose-response-grid-main', '=') == 1) {
      output$graphPopupPlot <- renderPlotly({
        #try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
        ggplotly(q) %>%
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
      if(input$box_scatter_choice == "Scatter plot") {
        type = '_scatter'
      } else {
        type = '_box'
      }
      return(paste(sub("^(.*)[.].*", "\\1", input$dataSet), type, input$scatterImageType, sep=''))
    },
    content = function(filename) {
      if(input$scatterImageType == '.pdf') {
        ggsave(filename = filename, plot = plotScatter_box, device = "pdf")
      } else {
        ggsave(filename = filename, plot = plotScatter_box, device = "tiff", units = "in", width = 7, height = 7, dpi = 300)
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
#try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
    ggplotly(plot1) %>%
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
    ggplotly(plot1) %>%
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
      ggplotly(plot1) %>%
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

#=========== update select boxes for boxplot ============

observeEvent(input$dataSet, {
  updateSelectInput(
    session, 'pick_box_x',
    choices = values$config$groupableColumns
  )
  updateSelectInput(
    session, 'pick_box_point_color',
    choices = values$config$groupableColumns
  )
})

observeEvent(c(input$dataSet, input$pick_box_x, input$tabs, values$sub_data), {
  if(!is.null(input$pick_box_x)) {
    print("update pick_box_factors")
    picks = sort(unique(subset_data[[input$pick_box_x]]))
    updateSelectizeInput(
      session, 'pick_box_factors',
      choices = picks,
      selected = picks[1:min(10, length(picks))]
    )
  }
})

observeEvent(input$box_scatter_choice, {
  if(!is.null(input$pick_box_x)) {
    picks = sort(unique(subset_data[[input$pick_box_x]]))
    updateSelectizeInput(
      session, 'pick_box_factors',
      choices = picks,
      selected = picks[1:min(10, length(picks))]
    )
  }
})

observeEvent(input$factorA, ignoreNULL = FALSE, {
  print("refresh B")
  picks = sort(input$pick_box_factors)
  picks1 = setdiff(picks, input$factorA)
  updateSelectizeInput(
    session, 'factorB',
    choices = picks1,
    selected = input$factorB
  )
})

observeEvent(input$factorB, ignoreNULL = FALSE, {
  print("refresh A")
  picks = sort(input$pick_box_factors)
  picks2 = setdiff(picks, input$factorB)
  updateSelectizeInput(
    session, 'factorA',
    choices = picks2,
    selected = input$factorA
  )
})

observeEvent(c(input$box_scatter_choice, input$pick_box_x, input$pick_box_factors), {
  if(!is.null(input$pick_box_x)) {
    print(input$pick_box_factors)
    print(typeof(input$pick_box_factors))
    print(class(input$pick_box_factors))
    picks = sort(input$pick_box_factors)
    updateSelectizeInput(
      session, 'factorA',
      #choices = unique(values$GR_table[[input$pick_box_x]]),
      choices = picks,
      selected = NULL
    )
    updateSelectizeInput(
      session, 'factorB',
      #choices = unique(values$GR_table[[input$pick_box_x]]),
      choices = picks,
      selected = NULL
    )
  }
})

#===== update select boxes for scatterplot ==========

observeEvent(input$dataSet, {
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
    choices = sort(unique(subset_data[[input$pick_var]])),
    selected = NULL
  )
  updateSelectizeInput(
    session, 'y_scatter',
    choices = sort(unique(subset_data[[input$pick_var]])),
    selected = NULL
  )
})

#===== Boxplot drawing =========

observeEvent(c(input$dataSet, input$plot_height), {
  output$boxplot <- renderPlotly({
    box = redrawPlotlyBox(input, values)
    if(!is.null(box)) {
      box
    } else {stop()}
  })
})

output$subset_selectize <- renderUI({
  n <- length(values$config$groupableColumns)
  if (n>0) {
    subset_cols = values$config$groupableColumns
    code_output_list <- lapply(1:n, function(i) {
      codeOutput <- paste("subset__", subset_cols[i], sep="")
      subset_choices = sort(unique(full_data[,subset_cols[i]]))
      selectizeInput(codeOutput, subset_cols[i], choices = subset_choices, multiple = TRUE, width = "90%")
    })
  } else code_output_list <- list()
  # Convert the list to a tagList - this is necessary for the list of items
  # to display properly.
  do.call(tagList, code_output_list)
})

output$grmetric_plot_ui <- renderUI({
    if(input$box_scatter_choice == "Box plot") {
      plotlyOutput('boxplot', height = input$plot_height)
    } else {
      plotlyOutput("plotlyScatter1", height = input$plot_height)
    }
})

output$scatter <- renderUI({
  if(input$box_scatter_choice == "Scatter plot") {
    fluidRow(
      ####### Change selectize options to specified in json
      selectInput('pick_parameter', 'Select parameter', choices = c('GR50', 'GRmax', 'GRinf', 'Hill', 'GR_AOC', 'IC50')),
      selectInput('pick_var', 'Select variable', choices = values$config$groupableColumns),
      selectInput('x_scatter', 'Select x-axis value', choices = unique(full_data[[input$pick_box_x]])),
      selectizeInput('y_scatter', 'Select y-axis value', choices = unique(full_data[[input$pick_box_x]])),
      bsButton('plot_scatter', 'Add', size = 'small'),
      bsButton('clear', 'Clear', size = 'small')
    )
  } else {
    fluidRow(
      ###### Change "pick_box_y" metrics to inherit from json for each dataset?
      ###### Add IC (and GI?) metrics to pick_box_y
      selectInput('pick_box_y', 'Select parameter', choices = c('GR50', 'GRmax', 'GRinf', 'h_GR', 'GR_AOC', 'IC50', 'Emax', 'Einf', 'h', 'AUC')),
      selectInput('pick_box_x', 'Select grouping variable', choices = values$config$groupableColumns),
      selectInput('pick_box_point_color', 'Select additional point coloring', choices = values$config$groupableColumns),
      selectizeInput('pick_box_factors', 'Show/hide data', choices = c(), multiple = T),
      actionLink('wilcox_panel', 'Compare boxplots'),
      conditionalPanel(condition = "input.wilcox_panel%2==1",
                       selectizeInput('factorA', 'Wilcoxon rank-sum test', choices = c(), multiple = T),
                       selectizeInput('factorB', '', choices = c(), multiple = T),
                       radioButtons('wilcox_method', label = "",choices = c("One-sided", "Two-sided"), selected = "Two-sided", inline = F),
                       textOutput("wilcox")
      )
      
    )
  }
})

observeEvent(c(input$factorA, input$factorB, input$pick_box_y,
               input$wilcox_method), {
  wil_data = subset_data
  if(!is.null(input$factorA) & !is.null(input$factorB)) {
    print(input$pick_box_x)
    rowsA = wil_data[[input$pick_box_x]] %in% input$factorA
    rowsB = wil_data[[input$pick_box_x]] %in% input$factorB
    wil_dataA = wil_data[rowsA,input$pick_box_y]
    wil_dataB = wil_data[rowsB,input$pick_box_y]
    print(head(wil_dataA))
    print(head(wil_dataB))
    wil_less = wilcox.test(x = wil_dataA, y = wil_dataB, alternative = "less")
    wil_greater = wilcox.test(x = wil_dataA, y = wil_dataB, alternative = "greater")
    wil_two_sided = wilcox.test(x = wil_dataA, y = wil_dataB, alternative = "two.sided")$p.value
    wil_one_sided = min(wil_less$p.value,wil_greater$p.value)
    wil_pval = ifelse(input$wilcox_method == "One-sided", wil_one_sided, wil_two_sided)
    values$wilcox = prettyNum(wil_pval, digits = 2)
    output$wilcox = renderText({
      paste("P-value:", prettyNum(wil_pval, digits = 2))
    })
  } else {
    output$wilcox = renderText({
      paste("P-value: ")
    })
    values$wilcox = NULL
  }
})

#==== Clear scatterplot on "browse" =========

observeEvent(input$dataSet, {
  output$plotlyScatter1 <- renderPlotly({
    parameter_choice = input$pick_parameter
    if(parameter_choice == 'GR50') {
      parameter_choice = 'log10(GR50)'
    }
    if(parameter_choice == 'Hill') {
      parameter_choice = 'log2(h_GR)'
    }
    padding = 0.05
    scatter_values = subset_data[,parameter_choice]
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
    #try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
    ggplotly(p) %>%
    layout(p, hovermode = FALSE)
  })
})

#===== Clear button ========
observeEvent(input$clear, {
  output$plotlyScatter1 <- renderPlotly({
    parameter_choice = input$pick_parameter
    if(parameter_choice == 'GR50') {
      parameter_choice = 'log10(GR50)'
    }
    if(parameter_choice == 'Hill') {
      parameter_choice = 'log2(h_GR)'
    }
    padding = 0.05
    scatter_values = subset_data[,parameter_choice]
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
#try(png(paste("/mnt/raid/tmp/junk1",gsub(" ","_",date()),as.character(as.integer(1000000*runif(1))),".png",sep="_")))
    ggplotly(p) %>%
    layout(p, hovermode = FALSE)

  })
  
})
  # cancel.onSessionEnded <- session$onSessionEnded(function() {
  #   graphics.off()
  #   print('devices off')
  # })
})
