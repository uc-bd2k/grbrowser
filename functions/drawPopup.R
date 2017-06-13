drawPopup <- function(curve_plot, values, example) {
  print("drawPopup function start")
  Concentration = 10^(seq(-4, 2, length.out = 200))
  curve_data_all = NULL
  len = dim(curve_plot)[1]
  print(head(curve_plot))
  for(row in 1:len) {
    logistic_3u = function(c){GRinf + (1 - GRinf)/(1 + (c/EC50)^Hill)}
    curve_data = as.matrix(Concentration)
    colnames(curve_data) = "Concentration"
    exper = NULL
    if(values$config$datafile == 'www/20170303_Density_for_GRB.tsv') {
      EC50 = curve_plot$GEC50[row]
      GRinf = curve_plot$GRinf[row]
      Hill = curve_plot$HillSlope[row]
      if(is.finite(curve_plot$GEC50[row])) {
        GR = apply(curve_data, 1, logistic_3u)
      } else {
        GR = curve_plot$GRinf[row]
      }
      curve_data = cbind(curve_data, GR)
      curve_data = as.data.frame(curve_data)
      exper = paste(curve_plot$`Small_Molecule`[row], curve_plot$`Density`[row], curve_plot$`Replicate`[row], curve_plot$`Cell_Line`[row], sep = ' ')
    } else if(values$config$datafile == 'www/20170303_Heiser_for_GRB.tsv') {
      EC50 = curve_plot$`GEC50`[row]
      GRinf = curve_plot$GRinf[row]
      Hill = curve_plot$HillSlope[row]
      if(is.finite(curve_plot$GEC50[row])) {
        GR = apply(curve_data, 1, logistic_3u)
      } else {
        GR = curve_plot$GRinf[row]
      }
      curve_data = cbind(curve_data, GR)
      curve_data = as.data.frame(curve_data)
      exper = paste(curve_plot$Perturbagen[row], curve_plot$Cell_Line[row], curve_plot$Replicate_ID[row], sep = ' ')
    } else if(values$config$datafile == 'www/20170224_LJP_for_GRB.tsv') {
      EC50 = curve_plot$`GEC50`[row]
      GRinf = curve_plot$GRinf[row]
      Hill = curve_plot$HillSlope[row]
      if(curve_plot$GEC50[row] != 0) {
        GR = apply(curve_data, 1, logistic_3u)
      } else {
        GR = curve_plot$GRinf[row]
      }
      curve_data = cbind(curve_data, GR)
      curve_data = as.data.frame(curve_data)
      exper = paste(curve_plot$`Small_Molecule`[row], curve_plot$`Cell_Line`[row], sep = ' ')
    } else if(values$config$datafile == 'www/20170309_MCF10A_for_GRB.tsv') {
      EC50 = curve_plot$`GEC50`[row]
      GRinf = curve_plot$GRinf[row]
      Hill = curve_plot$HillSlope[row]
      if(curve_plot$GEC50[row] != 0) {
        GR = apply(curve_data, 1, logistic_3u)
      } else {
        GR = curve_plot$GRinf[row]
      }
      curve_data = cbind(curve_data, GR)
      curve_data = as.data.frame(curve_data)
      exper = paste(curve_plot$`Small_Molecule`[row], curve_plot$`Cell_Line`[row], curve_plot$Replicate[row], sep = ' ')
    } else if(values$config$datafile == 'www/20170612_gCSI_GRfits.tsv') {
      EC50 = curve_plot$GEC50[row]
      GRinf = curve_plot$GRinf[row]
      Hill = curve_plot$HillSlope[row]
      if(curve_plot$GEC50[row] != 0) {
        GR = apply(curve_data, 1, logistic_3u)
      } else {
        GR = curve_plot$GRinf[row]
      }
      curve_data = cbind(curve_data, GR)
      curve_data = as.data.frame(curve_data)
      exper = paste(curve_plot$Perturbagen[row], curve_plot$`Cell_Line`[row], sep = ' ')
    }
    curve_data$experiment = exper
    if(is.null(curve_data_all)){
      curve_data_all = curve_data
    } else {
      curve_data_all = rbind(curve_data_all, curve_data)
    }
  }
  curve_data_all$experiment = as.factor(curve_data_all$experiment)
  print('curve_data_all')
  print(head(curve_data_all))
  p = ggplot(data = curve_data_all, aes(x = log10(Concentration), y = GR, colour = experiment)) + geom_line() + ggtitle("Concentration vs. GR values") + xlab('Concentration (log10 scale) μM') + ylab('GR value') + labs(colour = "") + geom_hline(yintercept = 1, size = .25) + geom_hline(yintercept = 0, size = .25) + geom_hline(yintercept = -1, size = .25)
  print("drawPopup function end")
  return(p)
}
