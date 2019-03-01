library(scales)
library(dygraphs)
library(bit64)
library(xts)
library(forecast)
library(depmixS4)
library(data.table)
require(reshape2)
require(tidyr)
require(tsbox)
library(lubridate)
source("helpers.R")

rm(list=ls())

datafile <- fread("D:\\Nitin\\ROOT/Buurkracht/Data/FinalFinalDATA/AnonymizedMemBerData/ lazoh .csv", sep=";", dec=".")

#https://stackoverflow.com/questions/8161836/how-do-i-replace-na-values-with-zeros-in-an-r-dataframe
for (j in names(datafile)) set(datafile,which(is.nan(datafile[[j]])),j,0) 

category <- "Elektra"

  if (category == "Elektra") {
    energy_unit <- "kWh"
    nat_avg = 9
  } else { 
    energy_unit <- "m^3"
    nat_avg <- 3.835
  }
  
  house <- subset(
    datafile, EAN = "0e9adaec9f132d7d5418e6a3066ff0ee" & EnergieType == "Elektra" )
  
  
  #"2017-06-28T00:00:00Z"
  house$Datum <-
    as.POSIXct(house$Datum, format="%Y-%m-%dT%H:%M:%SZ", 
                                    tz = "Europe/Amsterdam")
  house <- na.omit(house, cols="Datum", invert=FALSE)
  house <-
    subset(house, select = c(Datum, Register, Meetwaarde))
  
  house$Register <- as.factor(house$Register)
  houseWide <- dcast(house, Datum ~ Register, value.var = "Meetwaarde", fun.aggregate = mean )
  houseWide$`2.8.0` <- replace(houseWide$`2.8.0`, is.na(houseWide$`2.8.0`), 0)
  
  if (length(levels(house$Register)) > 1){
    houseWide$`2.8.0` <- houseWide$`2.8.0`* -1 }
  
  energyxts <-
    xts(houseWide[,-1], order.by = houseWide$Datum)
  
  date_range = "Monthly"
  
  if (date_range == "Daily") {
    energy <- apply.daily(energyxts, FUN=colSums)
    frequency = 7
    seasonal.periods=c(7, 365.25)
  }  else if (date_range == "Hourly") {
    energy <- period.apply(energyxts, endpoints(energyxts, "hours"), colSums)
    frequency = 24*365
    seasonal.periods=c(24,168,8766)
  }  else if (date_range == "Monthly") {
    energy <- apply.monthly(energyxts, FUN=colSums)
    frequency = 12
    seasonal.periods=c(12)
  }  else if(date_range=="15min"){
    energy <- energyxts
    frequency = 60/15*24*365
    seasonal.periods=c(96,336, 70128)
  }
  

  arima.xts <- energy[,1]-energy[,2]
  
  start.year <- year(start(arima.xts)) 
  start.month <- month(start(arima.xts)) 
  end.year <- year(end(arima.xts)) 
  end.month <- month(end(arima.xts)) 
  start.ts <- c(start.year, start.month)
  end.ts <- c(end.year, end.month)
  
  arima.ts <- ts(coredata(arima.xts), 
                 start = start.ts,
                 end = end.ts,
                 frequency = frequency
                 )
  
  arima.msts <- msts(coredata(arima.xts), 
                     start = start.ts,
                     end = end.ts,
                     seasonal.periods =  seasonal.periods)
  
  #arima.ts <- ts_timeSeries(arima.xts)
  arima.fit <- auto.arima(arima.msts,stepwise=FALSE, approximation = FALSE)
  arima.tbats <- tbats(arima.msts)
  arima.forecast <- forecast(arima.tbats, level = c(95), h = 32 )
  all <- cbind(actual = arima.tbats, 
               lwr = arima.forecast$lower,
               upr = arima.forecast$upper,
               pred = arima.forecast$mean)
  t <- as.POSIXct(format(date_decimal(as.vector(time(all))), "%Y-%m-%d %H:%M:%S"))
  all.xts <- xts(data.table(all), order.by = t)

  dygraph(all.xts, "Energy consumption") %>%
    dySeries("actual", label = "Actual") %>%
    dySeries(c("lwr", "pred", "upr"), label = "Predicted")

  


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #HMM
  energy <- energyxts
  k=3
  set.seed(7)
  #kmeans cluster
  cl <- kmeans(coredata(energy)[,"net"], k, nstart = 25)
  means <- as.vector(cl$centers)
  sds <- sqrt(cl$tot.withinss / cl$size)
  #Create HMM model
  resp_init <- c(rbind(means,sds))
  mod <- depmix(net~1, data=energy, nstates=k, respstart = resp_init)
  fit.hmm <- fit(mod, verbose = F) #fit
  probs <- posterior(fit.hmm)        
  # Lets change the name
  colnames(probs)[2:(k+1)] <- paste("S",1:k, sep="-")
  # Create dta.frame
  dfu <- data.table(cbind(datum=index(energy),net=coredata(energy)[,"net"], probs[,2:(k+1)]))
  dfm <- as.xts.data.table(dfu)
  dygraph(dfm)
  dfm <- melt(dfu[1:100], id.vars = "datum",)
  qplot(datum,value,data=dfm,geom="line",
        main = "HMM",
        ylab = "") + 
    facet_grid(variable ~ ., scales="free_y") + theme_bw()
  
  
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