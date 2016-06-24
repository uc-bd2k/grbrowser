update_browse_selector <- function(session, prev_selection) {
  datasets <- list.files(pattern="*.json")
  dataset_choices <- datasets
  
  if (length(prev_selection)==0 && length(datasets)>0) { prev_selection <- dataset_choices[1] }
  
  datasets_choices <- setNames(datasets, lapply(datasets,function(xxx){substr(xxx,6,nchar(xxx)-5)}))
  
  updateSelectizeInput(session, 'dataSet', 
                       choices = datasets_choices, # s_options, # dataset_choices, 
                       server = TRUE, 
                       selected=prev_selection
  )
}