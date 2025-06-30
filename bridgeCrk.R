library(CausalImpact)
library(zoo)
library(dplyr)
library(ggplot2)
library(lubridate)

dataD = read.csv("../data/MVPRestore_BOR_D_monthly.csv")
dataN = read.csv("../data/MVPRestore_BOR_N_monthly.csv")
dataR = read.csv("../data/MVPRestore_BOR_R_monthly.csv")

drought = read.csv("../data/bridgeCrkDrought.csv")
## remove 2024
drought = drought[1:240,]
## add date column
dataD$date = as.Date(as.yearmon(paste(dataD$Year, dataD$Month), "%Y %m"))
dataN$date = as.Date(as.yearmon(paste(dataN$Year, dataN$Month), "%Y %m"))
dataR$date = as.Date(as.yearmon(paste(dataR$Year, dataR$Month), "%Y %m"))
drought$date = format(as.Date(drought$system.time_start, "%B %d, %Y"))

## A LOT of missing data - sometimes up to 6 consecutive months! ex 2012-2013
print(paste0("Missing D: ", sum(is.na(dataD$Mesic_median))))
print(paste0("Missing N: ", sum(is.na(dataD$Mesic_median))))

## Try removing May, October, and all of 2012
dataD  = dataD %>%
  filter(month(date) %in% seq(6,9)) %>%
  filter(year(date) !=2012)

print(paste0("Missing D: ", sum(is.na(dataD $Mesic_median)))) ##17 missing

dataN  = dataN %>%
  filter(month(date) %in% seq(6,9)) %>%
  filter(year(date) !=2012)

dataR  = dataR %>%
  filter(month(date) %in% seq(6,9)) %>%
  filter(year(date) !=2012)


drought  = drought %>%
  filter(month(date) %in% seq(6,9)) %>%
  filter(year(date) !=2012)

## Also some missing data in the pre-period which this package can't handle
## Insert 0 for NA for the time being - many of these are may/oct

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

dataD_filled= replace_na_with_mean(dataD)
dataN_filled = replace_na_with_mean(dataN)
dataR_filled = replace_na_with_mean(dataR)

dataD_bsts =  cbind(dataD_filled$Mesic_median, drought$spei1y) ## similar results with pdsi
dataN_bsts =  cbind(dataN_filled$Mesic_median, drought$spei1y) ## where is the 'natural site relative to restoration?
dataR_bsts =  cbind(dataR_filled$Mesic_median, drought$spei1y) ## where is the 'natural site relative to restoration?

### define pre and post restoration period

pre_period = c(1,104)
post_period = c(105, 156)

impactD = CausalImpact(dataD_bsts, pre_period , post_period , model.args = list(nseasons = 4))
impactN = CausalImpact(dataN_bsts, pre_period , post_period , model.args = list(nseasons = 4))
impactR = CausalImpact(dataR_bsts, pre_period , post_period , model.args = list(nseasons = 4))

summary(impactD) 
summary(impactN)
summary(impactR)

plotD = plot(impactD, c("original", "pointwise")) + ggtitle("Degraded")
plotD
plotN = plot(impactN, c("original", "pointwise")) + ggtitle("Natural")
plotN
plotR = plot(impactR, c("original", "pointwise")) + ggtitle("Restored")
plotR
