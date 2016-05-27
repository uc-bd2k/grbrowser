drawPopup <- function(curve_plot, parameters, values, example) {
  Concentration = 10^(seq(-5, 5, length.out = 200))
  curve_data_all = NULL
  len = dim(curve_plot)[1]
  for(row in 1:len) {
    logistic_3u = function(c){GRinf + (1 - GRinf)/(1 + (c/EC50)^Hill)}
    curve_data = as.matrix(Concentration)
    colnames(curve_data) = "Concentration"
    exper = NULL
    if(example == 0) { # subsetted LJP dataset
      EC50 = curve_plot$GEC50[row]
      GRinf = curve_plot$GRinf[row]
      Hill = curve_plot$Hill_GR[row]
      if(curve_plot$GEC50[row] != 0) {
        GR = apply(curve_data, 1, logistic_3u)
      } else {
        GR = curve_plot$GRinf[row]
      }
      curve_data = cbind(curve_data, GR)
      curve_data = as.data.frame(curve_data)
      exper = paste(curve_plot$DrugName[row], curve_plot$CellLine[row], sep = ' ')
    } else {
      if(values$config$datafile == 'www/SeedingDensity_72h_GR_metrics_2016-01-11.tsv') {
        EC50 = curve_plot$`EC50 for GR curve`[row]
        GRinf = curve_plot$GRinf[row]
        Hill = curve_plot$HillSlope[row]
        if(curve_plot$`GR curve fit`[row] == "sigmoidal fit: x = Einf + (1-Einf)/(1+ (x/EC50)^Hill)") {
          GR = apply(curve_data, 1, logistic_3u)
        } else {
          GR = curve_plot$GRinf[row]
        }
        curve_data = cbind(curve_data, GR)
        curve_data = as.data.frame(curve_data)
        exper = paste(curve_plot$`Agent`[row], curve_plot$`Density`[row], curve_plot$`Replicate`[row] , curve_plot$`Cell line`[row], sep = ' ')
      } else if(values$config$datafile == 'www/Heiser_al_GRmetrics.tsv') {
        EC50 = curve_plot$`GEC50`[row]
        GRinf = curve_plot$GRinf[row]
        Hill = curve_plot$Hill_GR[row]
        if(is.finite(curve_plot$GEC50[row])) {
          GR = apply(curve_data, 1, logistic_3u)
        } else {
          GR = curve_plot$GRinf[row]
        }
        curve_data = cbind(curve_data, GR)
        curve_data = as.data.frame(curve_data)
        exper = paste(curve_plot$DrugName[row], curve_plot$CellLine[row], curve_plot$BiolReplicate[row], sep = ' ')
      } else if(values$config$datafile == 'www/LJP_GRmetrics_merged.tsv') {
        EC50 = curve_plot$`GEC50`[row]
        GRinf = curve_plot$GRinf[row]
        Hill = curve_plot$Hill[row]
        if(curve_plot$GEC50[row] != 0) {
          GR = apply(curve_data, 1, logistic_3u)
        } else {
          GR = curve_plot$GRinf[row]
        }
        curve_data = cbind(curve_data, GR)
        curve_data = as.data.frame(curve_data)
        exper = paste(curve_plot$smallMolecule[row], curve_plot$cellLine[row], sep = ' ')
      } else if(values$config$datafile == 'www/GRmetrics_MCF10A.tsv') {
        EC50 = curve_plot$`GEC50`[row]
        GRinf = curve_plot$GRinf[row]
        Hill = curve_plot$Hill[row]
        if(curve_plot$GEC50[row] != 0) {
          GR = apply(curve_data, 1, logistic_3u)
        } else {
          GR = curve_plot$GRinf[row]
        }
        curve_data = cbind(curve_data, GR)
        curve_data = as.data.frame(curve_data)
        exper = paste(curve_plot$agent[row], curve_plot$cellLine[row], curve_plot$Replicate[row], sep = ' ')
      }
    }
    curve_data$experiment = exper
    if(is.null(curve_data_all)){
      curve_data_all = curve_data
    } else {
      curve_data_all = rbind(curve_data_all, curve_data)
    }
  }
  curve_data_all$experiment = as.factor(curve_data_all$experiment)
  p = ggplot(data = curve_data_all, aes(x = log10(Concentration), y = GR, colour = experiment)) + geom_line() + ggtitle("Concentration vs. GR values") + xlab('Concentration (log10 scale) μM') + ylab('GR value') + labs(colour = "") + geom_hline(yintercept = 1, size = .25) + geom_hline(yintercept = 0, size = .25) + geom_hline(yintercept = -1, size = .25)
  return(p)
}