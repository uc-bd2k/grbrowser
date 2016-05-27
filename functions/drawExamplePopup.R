drawExamplePopup = function(input, values) {
  graphParams <- input$drg_demo
  graphParams = URLdecode(graphParams)
  graphParams = trimws(graphParams)
  print('graphParams')
  print(graphParams)
  if(graphParams == ',') {
    graphParams = ''
  }
  if (!graphParams == '') {
    params <- unlist(strsplit(graphParams, split = '='))
    if(grepl(',', params[1])) {
      pick = gsub('[^,]*$', '', params[1])
      pick_vector = unlist(strsplit(pick, split = ','))
      params[1] = gsub('.*,' , ' ', params[1])
      params[1] = trimws(params[1])
    }
    col1 = params[1]
    row1 = params[2]
    popupData = drg_demo_data[drg_demo_data[[col1]] == row1,]
    print(pick_vector)
    if(sum(pick_vector %in% popupData[["CellLine"]]) > 0 ) {
      popupData = popupData[popupData[["CellLine"]] %in% pick_vector,]
    } else if(sum(pick_vector %in% popupData[["DrugName"]]) > 0) {
      popupData = popupData[popupData[["DrugName"]] %in% pick_vector,]
    }
    q = drawPopup(popupData, params, values, 0)
  }
}