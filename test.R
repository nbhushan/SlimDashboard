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
require(mgcv)
require(car)
source("helpers.R")

rm(list=ls())

datafile <- fread("exampleData/tinut .csv", sep=";", dec=".")

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
  
  
  energyxts <-
    xts(houseWide[,-1], order.by = houseWide$Datum)
  print(periodicity(energyxts))
  
  date_range = "Hourly"
  if (date_range == "Daily") {
    energy <- apply.daily(energyxts, FUN=colSums)
    frequency = 7*365
    seasonal.periods=c(7, 365.25)
    h=14
  }  else if (date_range == "Hourly") {
    energy <- period.apply(energyxts, endpoints(energyxts, "hours"), colSums)
    frequency = 24
    seasonal.periods=c(24,168)
    h=24*7
  }  else if (date_range == "Monthly") {
    energy <- apply.monthly(energyxts, FUN=colSums)
    frequency = 12
    seasonal.periods=c(12)
    h=2
  }  else if(date_range=="15min"){
    energy <- energyxts
    frequency = 60/15*24*365
    seasonal.periods=c(96,336, 70128)
    h=4*24
  }
  print(periodicity(energy))
  dygraph(energy)

  arima.xts <- energy[,1] - energy[,2]
  colnames(arima.xts) <- "Meetwaarde"
  # # print(periodicity(arima.xts))
  # # dygraph(arima.xts)
  # # 
  # # start.year <- year(start(arima.xts)) 
  # # start.month <- month(start(arima.xts)) 
  # # end.year <- year(end(arima.xts)) 
  # # end.month <- month(end(arima.xts)) 
  # # start.ts <- c(start.year, start.month)
  # # end.ts <- c(end.year, end.month)
  # # 
  # # arima.ts <- as.ts(coredata(arima.xts), 
  # #                frequency = frequency
  # #                )
  # # #arima.ts <- as.ts(arima.xts)
  # # 
  # # 
  # # arima.msts <- msts(coredata(arima.xts),
  # #                    seasonal.periods =  seasonal.periods)
  # # 
  # # #arima.ts <- ts_timeSeries(arima.xts)
  # # arima.fit <- auto.arima(arima.msts,seasonal = T, parallel = TRUE)
  # # #arima.tbats <- tbats(arima.msts)
  # # arima.forecast <- forecast(arima.fit, level = c(80), h = h )
  # # all <- cbind(actual = arima.forecast$x, 
  # #              lwr = arima.forecast$lower,
  # #              upr = arima.forecast$upper,
  # #              pred = arima.forecast$mean)
  # # 
  # # fut.time <- tk_make_future_timeseries(index(arima.xts),h)
  # # t <- append(index(arima.xts),fut.time)
  # # all.xts <- xts(data.table(all), order.by = t)
  # # print(periodicity(all.xts))
  # # 
  # dygraph(all.xts, "Energy consumption") %>%
  #   dySeries("actual", label = "Actual") %>%
  #   dySeries(c("lwr", "pred", "upr"), label = "Predicted")

  


  #GAM
  gam.df <- as.data.table(arima.xts)
  gam.df[,'week_day'] <- lubridate::wday(gam.df$index, label=FALSE)
  gam.df[,'hour_of_day'] <- lubridate::hour(gam.df$index)
  gam.df[,'month'] <- lubridate::month(gam.df$index)



  period <- 24
  window <- N / period # number of days in the train set
  
  matrix.gam <- data.table(Load = gam.df[, Meetwaarde],
                           Daily = gam.df[,hour_of_day],
                           Weekly = gam.df[, week_day],
                           Month = gam.df[,month])
  
  gam_1 <- gam(Load ~ s(Daily, bs = "cr", k = period) +
                 s(Weekly, bs = "ps", k = 7)+
                 s(Month, bs = "cc", k = 7),
               data = matrix.gam,
               family = gaussian)
  
  recode(x, "c(1,2)='A'; 
	else='B'")
  matrix.gam <- data.table(Load = gam.df[, Meetwaarde],
                           Daily = gam.df[,hour_of_day],
                           Weekly = gam.df[, week_day],
                           Month = gam.df[,month])
  
  layout(matrix(1:3, nrow = 1))
  plot(gam_1, shade = TRUE)
  
  gam_1 <- gam(Load ~ s(Daily, Weekly,Month,
                         k = c(period, 7, 7),
                         bs = c("cr", "ps", "cc"),
                         full = TRUE),
               data = matrix.gam,
               family = gaussian)
  
  layout(matrix(1:3, nrow = 1))
  plot(gam_1, shade = TRUE)

  
  gam_2 <- gam(Load ~ s(Daily, Weekly),
               data = matrix.gam,
               family = gaussian)
  
  visreg::visreg(gam_1, "Daily", gg = TRUE, ylab="kWh", xlab="Hour of the day" )+theme_bw()
  
  gam_6_ar1 <- mgcv::gamm(Load ~ t2(Daily, Weekly,
                              k = c(period, 7),
                              bs = c("cr", "ps"),
                              full = TRUE),
                    data = matrix.gam,
                    family = gaussian,
                    correlation = corARMA(form = ~ 1|Weekly, p = 1),
                    method = "REML")
  
  layout(matrix(1:2, nrow = 1))
  plot(gam_1, shade = TRUE)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#HMM
  energy <- arima.xts
  k=2
  set.seed(7)
  #kmeans cluster
  cl <- kmeans(coredata(energy), k, nstart = 25)
  means <- as.vector(cl$centers)
  sds <- sqrt(cl$tot.withinss / cl$size)
  #Create HMM model
  resp_init <- c(rbind(means,sds))
  names(energy)<-"Meetwaarde"
  mod <- depmix(Meetwaarde~1, data=energy, nstates=k, respstart = resp_init)
  fit.hmm <- fit(mod, verbose = F) #fit
  probs <- posterior(fit.hmm)   
  vit <- viterbi(fit.hmm)
  # Lets change the name
  colnames(probs)[2:(k+1)] <- paste("state",1:k, sep=" ")
  # Create dta.frame
  df.posterior <- data.table(cbind(datum=index(energy),net=coredata(energy), probs[,2:(k+1)]))

  df.viterbi <- data.table(cbind(datum=index(energy),net=coredata(energy), vit[,2:(k+1)]))

  
  plot.posterior <- melt(dfu[1:100], id.vars = "datum")
  qplot(datum,value,data=dfm,geom="line",
        main = "HMM",
        ylab = "") + 
    facet_grid(variable ~ ., scales="free_y") + theme_bw()
  
  plot.viterbi<- melt(df.viterbi[1:100], id.vars = "datum")
  
  qplot(datum,value,data=plot.viterbi,geom="line",
        main = "HMM",
        ylab = "") + 
    facet_grid(variable ~ ., scales="free_y") + theme_bw()
  
  

  
  
  
po