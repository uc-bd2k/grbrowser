parseLabel = function(input, values, subset_data) {
  print("parseLabel function start")
  graphParams <- input$'dose-response-grid-main'
  print('graphparams_input')
  print(graphParams)
  graphParams = URLdecode(graphParams)
  graphParams = trimws(graphParams)
  if(graphParams == ',') {
    graphParams = ''
  }
  
  if (!graphParams == '') {
    print(1)
    params <- unlist(strsplit(graphParams, split = '='))
    if(grepl(',', params[1])) {
      # if(grepl('_', params[2])) {
      #   pick = gsub('[^,]*$', '', params[1])
      #   pick_vector = unlist(strsplit(pick, split = ','))
      #   print('pick')
      #   print(pick_vector)
      #   params[1] = gsub('.*,' , '', params[1])
      #   params[1] = trimws(params[1]) 
      #   params <- unlist(strsplit(params, split = '_'))
      # } else if(grepl('_', params[1])) {
      #if(grepl('_', params[2])) {
        pick = gsub('[^,]*$', '', params[1])
        pick_vector = unlist(strsplit(pick, split = ','))
        params[1] = gsub('.*,' , ' ', params[1])
        params[1] = trimws(params[1])
        
      #} 
      # else {
      #   print('comma')
      #   pick = gsub('[^,]*$', '', params[1])
      #   pick_vector = unlist(strsplit(pick, split = ','))
      #   params[1] = gsub('.*,' , ' ', params[1])
      #   params[1] = trimws(params[1])
      # }
      # print('params')
      # print(params)
        # Seeding density dataset
        if(params[1] == "Agent_Density") {
          params = unlist(strsplit(params, split = "_"))
        } else if(grepl("_", pick_vector)) {
          pick_vector2 = pick_vector
        }
        print('pick_vector')
        print(pick_vector)
        print('pick_vector params')
        print(params)
    }
    if(length(params) == 4) { #seeding density dataset with no toggle
      col1 = params[1]
      col2 = params[2]
      row1 = params[3]
      row2 = params[4]
      popupData = subset_data[subset_data[[col1]] == row1 & subset_data[[col2]] == row2,]
      cols_vector = c(col1, col2)
      print('cols_vector')
      print(cols_vector)
      group_cols = values$config$groupableColumns
      toggle_vector = group_cols[-which(group_cols %in% cols_vector)]
      print('toggle_vector')
      print(toggle_vector)
      if(pick_vector %in% popupData[[ toggle_vector[2] ]]) {
        popupData = popupData[popupData[[ toggle_vector[2] ]] %in% pick_vector,]
      }
      # 0 for example data on first tab, 1 for dose-response tab
      print('popupData')
      print(popupData)
      q = drawPopup(popupData, values, 1)
      
      
    } else if(length(params) == 2) {
      print(1.11)
      col1 = params[1]
      row1 = params[2]
      popupData = subset_data[subset_data[[col1]] == row1,]
      
      #for seeding density dataset with toggle
      if(exists('pick_vector2')) {
        group_cols = values$config$groupableColumns
        toggle_vector = group_cols[1:2]
        print('toggle_vector')
        print(toggle_vector)
        popupData$combined_cols = paste(popupData[[ toggle_vector[1] ]], popupData[[ toggle_vector[2] ]], sep = '_')
        if(pick_vector2 %in% popupData$combined_cols) {
          popupData = popupData[popupData$combined_cols %in% pick_vector2,]
        }
      } else {
        print('pick_vector')
        print(pick_vector)
        print('choicevar')
        print(values$config$doseresponse$defaultChoicevar)
        print(col1)
        if(sum(pick_vector %in% popupData[[values$config$doseresponse$defaultChoicevar]]) > 0 ) {
          popupData = popupData[popupData[[values$config$doseresponse$defaultChoicevar]] %in% pick_vector,]
        } else if(length(values$config$doseresponse$defaultGroupingVars) == 1) {
          if(sum(pick_vector %in% popupData[[values$config$doseresponse$defaultGroupingVars]]) > 0) {
            popupData = popupData[popupData[[values$config$doseresponse$defaultGroupingVars]] %in% pick_vector,]
          }
        }
      }
      print('popupdata')
      print(head(popupData))
      print('params')
      print(params)
      q = drawPopup(popupData, values, 1)
    } else {
      q = ggplot(subset_data) + geom_blank()
    }
    print("parseLabel function end")
    return(q) 
  }
}