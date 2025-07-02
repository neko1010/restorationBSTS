library(CausalImpact)
library(zoo)
library(dplyr)
library(ggplot2)
library(lubridate)

## function to use values from same month in previous and following two years
replace_na_with_mean <- function(df) {
  for (i in 1:ncol(df)) {
    na_indices <- which(is.na(df[, i]))
    for (j in na_indices) {
      if (j < 5) {
        df[j, i] <- mean(c(df[j + 4, i], df[j + 8, i]), na.rm = T) ## still an issue here - following 2 are missing...
      } else if (j > 5 & j < 9){  
        df[j, i] <- mean(c(df[j - 4, i], df[j + 4, i],df[j + 8, i]), na.rm = T)
      } else if (j > nrow(df) - 5 & j > nrow(df) - 8) {
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


process_site = function(i){
  site = sites[i]
  date = dates[i]
  
  data = read.csv(paste0("../data/MVPRestore_", site, "_R_monthly.csv"))
  drought = read.csv(paste0("../data/", site, "_R_SPEIH.csv"))
  
  ## add date column
  data$date = as.Date(as.yearmon(paste(data$Year, data$Month), "%Y %m"))
  
  ## Removing May, October
  data  = data %>%
    filter(month(date) %in% seq(6,9))
  
  ## drought 
  drought  = drought %>%
    filter(month(Date) %in% seq(6,9)) %>%
    filter(year(Date) !=2024)
  
  dataFill= replace_na_with_mean(data)
  dataBSTS =  cbind(dataFill$Mesic_median, drought$speih)
  
  ### define pre and post restoration period
  index = which(data$Year == date)[4]
  
  pre_period = c(1,index)
  post_period = c(index + 1, 156)
  
  impact = CausalImpact(dataBSTS, pre_period , post_period , model.args = list(nseasons = 4))
  
  ## write the summary to a text file
  sink(paste0("../output/", site, "_summary.txt"))
  print(summary(impact))
  sink()
  
  plot = plot(impact, c("original", "pointwise")) + ggtitle(paste0(site, " restoration impact"))
  ggsave(paste0("../output/", site, "_plot.png"), width = 6, height = 4)
  
}

sites = c("BOR", "WOR", "LOR", "BID", "YID", "TCID", "SFSPCO", "FMCO")
dates = c(2009, 2016, 2012, 2018, 2015, 2017, 2018, 2015)

indices = seq(1, length(sites))
lapply(indices, process_site)
