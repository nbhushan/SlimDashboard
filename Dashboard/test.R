#library(ggplot2) # load ggplot
library(scales)
library(data.table)
#library(reshape2)
#library(plyr)
library(dygraphs)
library(bit64)
library(xts)
library(forecast)

rm(list=ls())

test <- fread("D:/Nitin/ROOT/Buurkracht/Data/Final Dataset/Export/beers.csv", nrows = 0)

datafile <- fread("D:/Nitin/ROOT/Buurkracht/Data/Final Dataset/Export/beers.csv", sep=";", dec=".", fill=TRUE)

# 
# plot_house <- function(date_range =NULL, energy_file=NULL, weather_file=NULL,
#                        pincode, huisnummer, register, category)
# {
category <- "Elektra"

  if (category == "Elektra") {
    energy_unit <- "kWh"
    nat_avg = 9
  } else { 
    energy_unit <- "m^3"
    nat_avg <- 3.835
  }
  
  house <- subset(
    datafile, Postcode == "5437AR" & Huisnummer == "37" & EnergieType == "Elektra" )
  
  house$Datum <-
    as.POSIXct(house$Datum, format = "%Y-%m-%d %H:%M", 
                       tz = "Europe/Amsterdam")
  
  house <-
    subset(house, select = c(Datum, Register, Meetwaarde))
  house$Register <- as.factor(house$Register)
  houseWide <- dcast(house, Datum ~ Register, value.var = "Meetwaarde" )
  if (length(levels(house$Register)) > 1){
    houseWide$`2.8.0` <- houseWide$`2.8.0`* -1 }
  houseWide$net <- houseWide$`1.8.0`+ houseWide$`2.8.0`
  
  energyxts <-
    xts(houseWide[,-1], order.by = houseWide$Datum)
  
  date_range <- "15min"
  if (date_range == "Day") {
    energy <- apply.daily(energyxts, FUN=mean)
  } else if (date_range == "Week") {
    energy <- apply.weekly(energyxts, FUN=mean)
  } else if (date_range == "Month") {
    energy <- apply.monthly(energyxts, FUN=mean)
  }  else if(date_range=="15min"){
    energy <- energyxts
  }
  
  acf(coredata(energy)[,"net"])
  ar.fit <- auto.arima(coredata(energy)[,"net"], stepwise = FALSE, trace=TRUE)
  ar.res <- data.table(date = index(energy), observed = coredata(energy)[,"net"], fit = as.vector(fitted(ar.fit)))
  ar.fit
  ar.res <- as.xts.data.table(ar.res)
  #plot  
  dygraph(ar.res, main = paste("fit: ", as.character(date_range)), group = "arima" ) %>%
    dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))  %>%
    #dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5)) %>%
    dyAxis("x", drawGrid = T) %>%
    dyAxis(
      "y",axisLineWidth = 0.01,drawGrid = T ) %>%
    dyOptions(includeZero = TRUE) 
  
  
  
  
po