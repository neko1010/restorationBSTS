library(CausalImpact)
library(zoo)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpubr)

## function to use values from same month in previous and following two years
replace_na_with_mean <- function(df) {
  for (i in 1:ncol(df)) {
    na_indices <- which(is.na(df[, i]))
    for (j in na_indices) {
      if (j < 5) {
        df[j, i] <- mean(c(df[j + 4, i], df[j + 8, i]), na.rm = T) 
      } else if (j >= 5 & j < 9){  
        df[j, i] <- mean(c(df[j - 4, i], df[j + 4, i],df[j + 8, i]), na.rm = T)
      } else if (j < nrow(df) - 5 & j > nrow(df) - 8) {
        df[j, i] <- mean(c(df[j - 4, i], df[j + 4, i],df[j + 8, i]), na.rm = T)
      } else if (j > nrow(df) - 5) {
        df[j, i] <- mean(c(df[j - 4, i], df[j - 8, i]))
      } else {
        df[j, i] <- mean(c(df[j - 8, i], df[j - 4, i], df[j + 4, i],df[j + 8, i]), na.rm = T)
      }
    }
  }
  return(df)
}


process_site = function(i, plotType = NULL){
  
  set.seed(1)
  site = sites[i]
  date = dates[i]
  lab = labs[i]
  
  #data = read.csv(paste0("../data/MVPRestore_", site, "_R_monthly.csv"))
  data = read.csv(paste0("../data/", site, "_med.csv"))
  drought = read.csv(paste0("../data/", site, "_SPEIH.csv"))
  
  ## add date column
  data$date = as.Date(as.yearmon(paste(data$Year, data$Month), "%Y %m"))
  
  ## Removing May, October
  data  = data %>%
    filter(month(date) %in% seq(6,9))
  
  ## drought 
  drought  = drought %>%
    filter(month(Date) %in% seq(6,9)) #%>%
    #filter(year(Date) !=2024)
  
  dataFill= replace_na_with_mean(data)
  dataBSTS =  zoo(cbind(dataFill$Mesic_median, drought$speih), as.Date(dataFill$date,format = "%Y %m")) 
  
  ### define pre and post restoration period
  restIndex = which(data$Year == date)[4]
  
  #pre_period = c(1,index)
  #post_period = c(index + 1, length(dataFill[,1]))
  
  pre_period_date = as.Date(c(index(dataBSTS[1]), index(dataBSTS[restIndex])),format = "%Y-%m-%d")
  post_period_date = as.Date(c(index(dataBSTS[restIndex + 1]), index(dataBSTS[length(dataFill[,1])])), format = "%Y-%m-%d") 
  
  impact = CausalImpact(dataBSTS, pre_period_date , post_period_date , model.args = list(nseasons = 4))
  
  ## write the summary to a text file
  sink(paste0("../output/", site, "_summary.txt"))
  print(summary(impact))
  sink()
  
  #og = plot(impact, c("original", "pointwise")) +
  og = plot(impact, c("original")) +
    annotate("text", x = dataFill$date[1], y = 80, label = lab) +
    #scale_y_continuous(limits = ~c(-130, 130)) +
    ylim(-50, 90) + ## CIs omitted when beyond these limits
    ylab("Mesic Vegetation Area\n (% of valley bottom)") +
    xlab("Date") +
    theme(axis.title = element_text(size = 10))
  
  
  pointwise = plot(impact, c("pointwise")) +
    #ggtitle(paste0(lab, " ", site, " restoration impact")) +
    annotate("text", x = dataFill$date[1], y = 90, label = lab) +
    ylim(-70, 100) +
    ylab("Mesic Vegetation Area\n (% of valley bottom)") +
    xlab("Date")+
    theme(axis.title = element_text(size = 10))
  
  if(plotType == "pointwise"){
    outplot = pointwise
  }
  else{
    outplot = og
  }
  
  #ggsave(paste0("../output/", site, "_plot.png"), width = 6, height = 4)
  return(outplot)
}

sites = c("BOR_D", "BOR_N", "BOR_R")
dates = c(2009, 2009, 2009)
labs = c("A", "B", "C")

indices = seq(1, length(sites))
ogPlots = lapply(indices, process_site, "og")
ptPlots = lapply(indices, process_site, "pointwise")

ggarrange(plotlist = ogPlots, ncol =1, nrow = 3)

ggarrange(plotlist = ptPlots, ncol =1, nrow = 3)

