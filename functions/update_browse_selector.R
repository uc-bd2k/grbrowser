update_browse_selector <- function(session, prev_selection) {
  print("update_browse_selector function start")
  datasets <- list.files(path = "json", pattern="*.json")
  dataset_choices <<- datasets
  
  if (length(prev_selection)==0 && length(datasets)>0) {
    prev_selection <- dataset_choices[1]
  } 
  
  dataset_choices <<-
      setNames(datasets,
               lapply(datasets,
                      function(xxx) {
                          gsub("_", " ", gsub("^data_\\d+_(.*?)\\.json$", "\\1", xxx))
                      }
                      )
               )
  print(dataset_choices)
  updateRadioButtons(session, 'dataSet', 
                     choices=dataset_choices,
                     selected=prev_selection
  )
  print("update_browse_selector function end")
}
