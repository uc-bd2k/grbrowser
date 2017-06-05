full_data = NULL
extractData <- function(input, output, values, choiceVar, groupingVars) {
  print("extractData function start")
  data1 <- values$data
  data1[[choiceVar]] <- values$data[[choiceVar]]
  data1[[paste(groupingVars,collapse = '_')]] <- do.call(paste, c(as.data.frame(values$data[groupingVars], stringsAsFactors=FALSE),sep="_"))
  if(input$dataSet != "data_5_Lapatinib_BRCA_PTEN.json") {
    data1['EC50'] <- values$data[values$config$doseresponse$EC50]
    data1['Einf'] <- values$data[values$config$doseresponse$Einf]
    data1['GRinf'] <- values$data[values$config$scatterplot$GRinf]
    data1['HillSlope'] <- values$data[values$config$doseresponse$HillSlope]
    data1['log10[EC50]'] <- lapply(data1['EC50'], log10)
    data1['log2[HillSlope]'] <- lapply(data1['HillSlope'], log2)
  }
  data1['GR50'] <- values$data[values$config$scatterplot$GR50]
  data1['log10[GR50]'] <- lapply(data1['GR50'], log10)
  data1['GRmax'] <- values$data[values$config$scatterplot$GRmax]
  if(input$dataSet == "data_5_Lapatinib_BRCA_PTEN.json") {
    data1['log10[IC50]'] <- lapply(data1['IC50'], log10)
    data1['Emax'] <- values$data[values$config$scatterplot$Emax]
    }
    
  full_data <<- data1
  #test <<- full_data
}