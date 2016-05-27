df_full = NULL

drawScatter <- function (input, values)
{
  xaxis = full_data[get(input$pick_var, envir = as.environment(full_data)) == input$x_scatter,]
  GR1 = unique(xaxis[[input$pick_var]])
  
  for(i in length(input$y_scatter)) {
    yaxis = full_data[get(input$pick_var, envir = as.environment(full_data)) == input$y_scatter[i],]
    GR2 = unique(yaxis[[input$pick_var]])
    yaxis$cross = paste(GR1, GR2, sep = ' x ')
    xaxis$cross = paste(GR1, GR2, sep = ' x ')
    
    compare_col = which(groupingColumns == input$pick_var)
    print('compare_col')
    print(compare_col)
    merge_cols = c(groupingColumns[-compare_col], 'cross')
    merge_text_cols = c(groupingColumns[-compare_col])
    tmp = xaxis[,merge_cols, drop = F]
    tmp2 = xaxis[,merge_text_cols, drop = F]
    merge_paste = (apply(tmp,1, function(x) (paste(x,collapse=" "))))
    merge_paste2 = (apply(tmp2,1, function(x) (paste(x,collapse=" "))))
    xaxis$merge = merge_paste
    xaxis$merge_text = merge_paste2
    tmp = yaxis[,merge_cols, drop = F]
    merge_paste = (apply(tmp,1, function(x) (paste(x,collapse=" "))))
    yaxis$merge = merge_paste
    test2 <<- yaxis
    test3 <<- xaxis
    df = merge(xaxis, yaxis, by = 'merge')
    print(1.1)
    print(dim(df))
    print(1.2)
    print(dim(df_full))
    print(1.3)
    df_full <<- rbind(df, df_full)
    print(names(df_full))
    
  }
  df_full$cross.x <<- factor(df_full$cross.x)
  df_sub <<- df_full
  
  parameter_choice = input$pick_parameter
  print(parameter_choice)
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
  
  x_var = get(paste0(parameter_choice,'.x'), envir = as.environment(df_sub))
  y_var = get(paste0(parameter_choice,'.y'), envir = as.environment(df_sub))
  print('length before')
  print(length(x_var))
  print(length(y_var))
  print(typeof(x_var))
  print(class(x_var))
  test_finite_x = which(is.finite(x_var))
  test_finite_y = which(is.finite(y_var))
  test_finite = intersect(test_finite_x, test_finite_y)
  x_var = x_var[test_finite]
  y_var = y_var[test_finite]
  print('length after')
  print(length(x_var))
  print(length(y_var))
  df_sub <<- df_sub[test_finite,]

  p = ggplot(data = df_sub, aes(x = x_var, y = y_var, colour = cross.x, text = merge_text)) + geom_point(size=2)+ geom_abline(slope = 1, intercept = 0, size = .25) + scale_x_continuous(limits = c(all_min, all_max)) + scale_y_continuous(limits = c(all_min, all_max)) + coord_fixed()
  if(parameter_choice == 'log10[GR50]') {
    p = p + xlab("log10(GR50)") + ylab("log10(GR50)") + ggtitle("GR50 Scatterplot (log10)") + labs(colour = "")
  } else if(parameter_choice == 'log2[HillSlope]') {
    p = p + xlab("log2(Hill)") + ylab("log2(Hill)") + ggtitle("Hill Scatterplot (log2)") + labs(colour = "")
  } else {
    p = p + xlab(parameter_choice) + ylab(parameter_choice) + ggtitle(paste(parameter_choice, "Scatterplot")) + labs(colour = "")
  }
  plotScatter <<- p
  return(p)
}