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
require(MmgraphR)
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
  #dygraph(energy)

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

  

  dygraph(arima.xts)
  #GAM
  gam.df <- as.data.table(arima.xts)
  gam.df[,'week_day'] <- lubridate::wday(gam.df$index, label=FALSE)
  gam.df[,'hour_of_day'] <- lubridate::hour(gam.df$index)
  gam.df[,'month'] <- lubridate::month(gam.df$index)
  gam.df[,'time'] <- as.numeric(gam.df$index)/1000000
  gam.df[,'weekend'] <- as.factor(gam.df$week_day>4)
  gam.df[,'load24'] <- shift(gam.df$Meetwaarde,24)
  #levels(gam.df$weekend) <- c("weekend","weekday")

  matrix.gam <- data.table(Load = gam.df[, Meetwaarde],
                           Daily = gam.df[,hour_of_day],
                           Weekly = gam.df[, week_day],
                           Month = gam.df[,month],
                           Time = gam.df[,time],
                           weekend = gam.df[,weekend],
                           load24 = gam.df[,load24])
  
  gam_1 <- gam(Load ~ load24 + s(Daily, by=weekend, bs = "cc", k = 24) +
                 s(Month, bs = "cc", k = 7),
               data = matrix.gam,
               family = gaussian)

  #gam_1$data <- matrix.gam
  layout(matrix(1:3, nrow = 1))
  plot(gam_1, shade = TRUE)
  
  layout(matrix(1:2, ncol = 2))
  acf(resid(gam_1), lag.max = 36, main = "ACF")
  pacf(resid(gam_1), lag.max = 36, main = "pACF")
  
  ar.mod <- auto.arima(resid(gam_1), D=1)
  
  
  gam_1_ar <- gamm(Load ~ load24 +
                     s(Daily, by=weekend, bs = "cc", k = 24) +
                    s(Month, bs = "cc", k = 7),
                  data = matrix.gam,
                  correlation =corARMA(form = ~ 1, p = 1,q=3),
                  family = gaussian,
                  method = "REML")
  saveRDS(gam_1_ar, "gamARweekendlag24.rds")

  layout(matrix(1:3, nrow = 1))
  plot(gam_1_ar$gam, shade = TRUE)
  
  
  layout(matrix(1:2,  nrow = 1))
  acf(resid(gam_1_ar$lme, type = "normalized"), lag.max = 48, main = "ACF")
  pacf(resid(gam_1_ar$lme, type = "normalized"), lag.max = 48, main = "pACF")
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
#HMM
  energy <- matrix.gam
  energy$Daily <- as.factor(energy$Daily)
  energy$Month <- as.factor(energy$Month)
  
  hr <- as.numeric(energy$Daily)
  xhr = sin(2*pi*hr/24)
  yhr = cos(2*pi*hr/24)

  k=3
  set.seed(7)
  #kmeans cluster
  cl <- kmeans(energy$Load, k, nstart = 25)
  means <- as.vector(cl$centers)
  sds <- sqrt(cl$tot.withinss / cl$size)
  #Create HMM model
  resp_init <- c(rbind(means,sds))
  #names(energy)<-"Meetwaarde"
  
  mod.0 <- depmix(Load~1, data=energy, nstates=k, respstart = resp_init)
  mod.1 <- depmix(Load~1, data=energy, nstates=k, transition = ~ Daily, respstart = resp_init)
  mod.2 <- depmix(Load~1, data=energy, nstates=k, transition = ~ Daily+Month, respstart = resp_init)
  #fit
  fit.hmm.0 <- fit(mod.0, verbose = F) #fit
  fit.hmm.1 <- fit(mod.1, verbose = F) #fit
  fit.hmm.2 <- fit(mod.2, verbose = F) #fit
  
  #plot AIC
  layout(matrix(1:2, nrow = 1))
  plot(1:3,c(BIC(fit.hmm.0),BIC(fit.hmm.1),BIC(fit.hmm.2)),ty="b", ylab="BIC")
  plot(1:3,c(AIC(fit.hmm.0),AIC(fit.hmm.1),AIC(fit.hmm.2)),ty="b", ylab="AIC")
  
  #plot transition models
  trans.mod.1 <- fit.hmm.1@transition
  trans.mod.1.df <- data.frame(matrix(unlist(trans.mod.1), nrow=24, byrow=T),stringsAsFactors=FALSE)
  
  #viterbi
  vit.0 <- viterbi(fit.hmm.0)
  vit.1 <- viterbi(fit.hmm.1)
  vit.2 <- viterbi(fit.hmm.2)
  
  #viterbi sequencing
  post.2 <- viterbi(fit.hmm.2)
  df.viterbi <- data.table(cbind(datum=index(arima.xts),net=energy$Load, post.2))
  plot.viterbi.0 <- melt(df.viterbi[1:500], id.vars = "datum")
  qplot(datum,value,data=plot.viterbi.0,geom="line",
        main = "HMM",
        ylab = "") + 
    facet_grid(variable ~ ., scales="free_y") + theme_bw()
  
  #compute state durations
  post.2 <- viterbi(fit.hmm.2)
  durations <- matrix(0, nrow=nrow(post.2), ncol=k)
  mean.durations <- list()
  for (i in c(1:k)){
    durations[,i] = ifelse(post.2$state==i,1,0)
    t = rle(durations[,i])$lengths
    mean.durations[[i]] <- mean(t[t!=1])/4
  }
  print(fit.hmm.2@response)
  print(mean.durations)

  
  #vit compare
  df.vit.compare <- data.table(cbind(datum=index(arima.xts),net=energy$Load, vit.0))
  plot.viterbi.0 <- melt(df.vit.compare[1:500], id.vars = "datum")
  qplot(datum,value,data=plot.viterbi.0,geom="line",
        main = "HMM",
        ylab = "") + 
    facet_grid(variable ~ ., scales="free_y") + theme_bw()
  
  # Lets change the name
  colnames(probs)[2:(k+1)] <- paste("state",1:k, sep=" ")
  # Create dta.frame
  df.posterior <- data.table(cbind(datum=index(energy),net=coredata(energy), probs[,2:(k+1)]))

  df.viterbi <- data.table(cbind(datum=index(energy),net=coredata(energy), vit[,2:(k+1)]))
  
  df.viterbi <- data.table(cbind(datum=index(arima.xts),net=energy$Daily, vit[,2:(k+1)]))
  
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
  
  
  MmgraphR::trmatplot(fit.hmm)
  
  
  
