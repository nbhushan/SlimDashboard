# # 0. Prepare environment ----
# list.of.packages <-
#   c(
#     "shiny",
#     "ggplot2",
#     "scales",
#     "data.table",
#     "bit64",
#     "dygraphs",
#     "shinythemes",
#     "depmixS4",
#     "wesanderson",
#     "xts",
#     "lubridate",
#     "forecast",
#     "ggfortify",
#     "visreg",
#     "shinycssloaders",
#     "shinyjs"
#   )
# new.packages <-
#   list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
# if (length(new.packages))
#   install.packages(new.packages, repos = "http://cran.us.r-project.org")


library(shiny)
library(ggplot2) # load ggplot
library(scales)
library(dygraphs)
library(xts)
library(forecast)
library(ggfortify)
library(depmixS4)
require(wesanderson)
require(lubridate)
#library(visreg)
library(mgcv)
library(data.table)

dyCrosshair <- function(dygraph,
                        direction = c("both", "horizontal", "vertical")) {
  dyPlugin(
    dygraph = dygraph,
    name = "Crosshair",
    path = system.file("crosshair.js",
                       package = "dygraphs"),
    options = list(direction = match.arg(direction))
  )
}

plot_house <-
  function(date_range = NULL,
           energy_file = NULL,
           weather_file = NULL,
           EAN,
           category,
           use_weather = FALSE)
  {
    if (category == "Elektra") {
      energy_unit <- "kWh"
    } else {
      energy_unit <- "m^3"
    }
    
    house <- subset(energy_file, EAN == EAN &
                      EnergieType == category)
    
    house$Datum <-
      as.POSIXct(house$Datum, format = "%Y-%m-%dT%H:%M:%SZ",
                 tz = "Europe/Amsterdam")
    house <- na.omit(house, cols = "Datum", invert = FALSE)
    house <-
      subset(house, select = c(Datum, Register, Meetwaarde))
    house$Register <- as.factor(house$Register)
    houseWide <-
      dcast(house,
            Datum ~ Register,
            value.var = "Meetwaarde",
            fun.aggregate = mean)
    
    energyxts <-
      xts(houseWide[,-1], order.by = houseWide$Datum)
    
    if (date_range == "Daily") {
      energy <- apply.daily(energyxts, FUN = colSums)
    }    else if (date_range == "Hourly") {
      energy <-
        period.apply(energyxts, endpoints(energyxts, "hours"), colSums)
    }    else if (date_range == "Monthly") {
      energy <- apply.monthly(energyxts, FUN = colSums)
    }    else if (date_range == "15min") {
      energy <- energyxts
    }
    agg.xts <- energy[, 1] - energy[, 2]
    colnames(agg.xts) <- "meetwaarde"
    
    dygraph(agg.xts) %>%
      dyOptions(colors = wes_palette("Darjeeling2"))  %>%
      dyLegend(width = 250) %>%
      dyLimit("0") %>%
      #dyHighlight(highlightSeriesOpts = list(strokeWidth = 1.2)) %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyAxis("y",
             axisLineWidth = 0.01,
             drawGrid = TRUE,
             label = energy_unit) %>%
      dyOptions(
        includeZero = TRUE,
        gridLineColor = "lightgray",
        gridLineWidth = 0.1
      )  %>%
      dyRangeSelector()
  }

# plot_house_box <- function(energy_file=NULL, weather_file=NULL, EAN, register, category, use_weather=FALSE)
# {
#
#   if (category == "Elektra") {
#     energy_unit <- "kWh"
#     nat_avg = 9
#   } else {
#     energy_unit <- "m^3"
#     nat_avg <- 3.835
#   }
#
#   house <- subset(
#     energy_file,EAN == EAN &
#       EnergieType == category & Register == register
#   )
#
#   house <- subset(house, select = c(Datum, EnergieType, Meetwaarde))
#   house$EnergieType <- as.factor(house$EnergieType)
#   house$Datum <- as.POSIXct(house$Datum, format="%Y-%m-%dT%H:%M:%SZ",
#                             tz = "Europe/Amsterdam")
#
#
#   ggplot(house) +
#     geom_boxplot(aes_string(y=Meetwaarde, x=reorder(format(house$Datum, '%b-%Y'), house$Datum),
#                      fill = EnergieType)) + ylab(energy_unit) +
#     labs(title  = "Energy consumption boxplot") +
#     theme(
#       axis.text.x = element_text(
#         angle = 90, vjust = 0.5, hjust = 1
#       ),
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       panel.background = element_blank(),
#       legend.text = element_text(size = 15,face = "bold")
#     )
#
#
# }
#
# plot_pincode <- function(date_range =NULL, energy_file=NULL, weather_file=NULL,  category, use_weather=FALSE)
# {
#   if (category == "Elektra") {
#     energy_unit <- "kWh"
#     nat_avg = 9
#   } else {
#     energy_unit <- "m^3"
#     nat_avg <- 3.835
#   }
#
#   house <- subset(
#     energy_file,  EnergieType == category )
#
#   house$Datum <-
#     as.POSIXct(house$Datum, format = "%Y-%m-%d %H:%M",
#                tz = "Europe/Amsterdam")
#
#   house <-
#     subset(house, select = c(Datum, EAN, Register, Meetwaarde))
#   house$Register <- as.factor(house$Register)
#   house$Huisnummer <- as.factor(house$EAN)
#
#   if(category=="Gas"){
#     house$Meetwaarde[house$EnergieType == "Gas"] <-
#       house$Meetwaarde[house$EnergieType == "Gas"] * 9.769}
#   if (length(levels(house$Register)) > 1){
#     house$Meetwaarde[house$Register == "2.8.0"] <-
#       house$Meetwaarde[house$Register == "2.8.0"] * -1}
#
#   houseWide <- dcast(house, Datum ~ Huisnummer+Register, value.var = "Meetwaarde" )
#   colnames(houseWide) <- c("Datum", "from grid", "to grid")
#   energyxts <-
#     xts(houseWide[,-1], order.by = houseWide$Datum)
#
#
#     if (date_range == "Day") {
#       energy <- apply.daily(energyxts, FUN=mean)
#     }
#     else if (date_range == "Week") {
#       energy <- apply.weekly(energyxts, FUN=mean)
#     }
#     else if (date_range == "Month") {
#       energy <- apply.monthly(energyxts, FUN=mean)
#     }
#     else if(date_range=="15min"){
#       energy <- energyxts
#     }
#
#     dygraph(energy, main = paste(
#       "Energy consumption: ", as.character(category)
#     )) %>%
#       dyLimit(
#         nat_avg, label = "National Mean", labelLoc = "right",
#         color = "grey", strokePattern = "dashed"
#       ) %>%
#       dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))  %>%
#       dyLegend(width = 400) %>%
#       dyHighlight(highlightSeriesOpts = list(strokeWidth = 1.5)) %>%
#       dyAxis("x", drawGrid = FALSE) %>%
#       dyAxis(
#         "y", axisLineWidth = 0.01,drawGrid = FALSE, label = energy_unit
#       ) %>%
#       dyOptions(includeZero = TRUE)  %>%
#       dyRangeSelector()
#   }

plot_arima <- function(arima.ts, date_range)
{
  #acf(coredata(energy)[,"net"])
  ar.fit <- fit_arima(arima.ts)
  #arima.ts <- ts_timeSeries(arima.xts)
  arima.forecast <- forecast(ar.fit, level = c(95), h = 12)
  all <- cbind(
    actual = arima.ts,
    lwr = arima.forecast$lower,
    upr = arima.forecast$upper,
    pred = arima.forecast$mean
  )
  t <-
    as.POSIXct(format(date_decimal(as.vector(time(
      all
    ))), "%Y-%m-%d %H:%M:%S"))
  all.xts <- xts(data.table(all), order.by = t)
  
  dygraph(all.xts) %>%
    dySeries("actual", label = "Actual") %>%
    dySeries(c("lwr", "pred", "upr"), label = "Predicted") %>%
    dyOptions(colors = wes_palette("Darjeeling2", 2))  %>%
    dyLegend(width = 250) %>%
    dyLimit("0") %>%
    #dyHighlight(highlightSeriesOpts = list(strokeWidth = 1.2)) %>%
    dyAxis("x", drawGrid = FALSE) %>%
    dyAxis("y",
           axisLineWidth = 0.01,
           drawGrid = TRUE,
           label = "kWh") %>%
    dyOptions(
      includeZero = TRUE,
      gridLineColor = "lightgray",
      gridLineWidth = 0.1
    )  %>%
    dyRangeSelector()
}

plot_arima_diag <- function(arima.ts, date_range)
{
  if (date_range == "Daily") {
    h = 7
  }    else if (date_range == "Hourly") {
    h = 96
  }    else if (date_range == "Monthly") {
    h = 2
  }    else if (date_range == "15min") {
    h = 96*2
  }  
  #acf(coredata(energy)[,"net"])
  ar.fit <- fit_arima(arima.ts)
  #arima.ts <- ts_timeSeries(arima.xts)
  arima.forecast <- forecast(ar.fit, level = c(95), h = h)
  all <- cbind(
    actual = arima.ts,
    lwr = arima.forecast$lower,
    upr = arima.forecast$upper,
    pred = arima.forecast$mean
  )
  t <-
    as.POSIXct(format(date_decimal(as.vector(time(
      all
    ))), "%Y-%m-%d %H:%M:%S"))
  all.xts <- xts(data.table(all), order.by = t)
  
  dygraph(all.xts, "Energy consumption") %>%
    dySeries("actual", label = "Actual") %>%
    dySeries(c("lwr", "pred", "upr"), label = "Predicted") %>%
    dyOptions(colors = wes_palette("Darjeeling2", 2))  %>%
    dyLegend(width = 250) %>%
    dyLimit("0") %>%
    #dyHighlight(highlightSeriesOpts = list(strokeWidth = 1.2)) %>%
    dyAxis("x", drawGrid = FALSE) %>%
    dyAxis("y",
           axisLineWidth = 0.01,
           drawGrid = TRUE,
           label = "kWh") %>%
    dyOptions(
      includeZero = TRUE,
      gridLineColor = "lightgray",
      gridLineWidth = 0.1
    )  %>%
    dyRangeSelector()
}

plot_arima_tsdiag <- function(energy)
{
  #acf(coredata(energy)[,"net"])
  ar.fit <- fit_arima(energy)
  ggtsdiag(ar.fit)
}

arima_identify <- function(energy) {
  p1 <- autoplot(acf(energy, plot = FALSE))+theme_bw()
  p2 <- autoplot(pacf(energy, plot = FALSE), ylab = "PACF")+theme_bw()
  multiplot(p1, p2, cols = 1)
}

fit_arima <- function(energy) {
  return(
    auto.arima(
      energy,
      stepwise = FALSE,
      approximation = TRUE,
      parallel = TRUE,
      trace = FALSE,
      seasonal = TRUE
    )
  )
}

fit.gamm <- function(gam.df) {
  matrix.gam <- data.table(
    Load = gam.df$Meetwaarde,
    Daily = gam.df$hour_of_day,
    Weekly = gam.df$week_day,
    Month = gam.df$month
  )
  
  gam_1 <- mgcv::gam(
    Load ~ s(Daily, bs = "cr", k = 24) +
      s(Weekly, bs = "ps", k = 7) +
      s(Month, bs = "cc", k = 7),
    data = matrix.gam,
    family = gaussian
  )
  return(list("mod" = gam_1, "data" = matrix.gam))
}

viz.gamm <- function(gam.df) {
  res <- fit.gamm(gam.df)
  gam_1 <- res$mod
  gam_1$data <- res$data
  # g1 <- visreg::visreg(gam_1, "Daily", gg = TRUE, ylab="kWh", xlab="Hour of the day" )+theme_bw()
  # g2 <- visreg::visreg(gam_1, "Weekly", gg = TRUE, ylab="kWh", xlab="Days of the week" )+theme_bw()
  # g3 <- visreg::visreg(gam_1, "Month", gg = TRUE, ylab="kWh", xlab="Month of the year" )+theme_bw()
  # multiplot(g1, g2, g3, cols = 1)
  
  layout(matrix(1:3, nrow = 1))
  plot(gam_1, shade = TRUE)
}

#fitHMM
# fit_hmm <- function(energy,
#                     k = 3,
#                     covariates = NULL) {
#   set.seed(7)
#   
#   gam.df <- as.data.table(energy)
#   gam.df[,'week_day'] <- lubridate::wday(gam.df$index, label=FALSE)
#   gam.df[,'hour_of_day'] <- lubridate::hour(gam.df$index)
#   gam.df[,'month'] <- lubridate::month(gam.df$index)
#   matrix.gam <- data.table(Load = gam.df[, Meetwaarde],
#                            Daily = gam.df[,hour_of_day],
#                            Weekly = gam.df[, week_day],
#                            Month = gam.df[,month])
#   matrix.gam$Daily <- as.factor(matrix.gam$Daily)
#   matrix.gam$Month <- as.factor(matrix.gam$Month)
#   # #kmeans cluster
#   # cl <- kmeans(coredata(energy), k, nstart = 25)
#   # means <- as.vector(cl$centers)
#   # sds <- sqrt(cl$tot.withinss / cl$size)
#   # #Create HMM model
#   # resp_init <- c(rbind(means, sds))
#   #names(energy)<-"Meetwaarde"
#   if (covariates == "none") {
#     mod <-
#       depmix(Load ~ 1,
#              data = matrix.gam,
#              nstates = k
#              #respstart = resp_init
#       )
#   } else {
#     mod <-
#       depmix(
#         Load ~ 1,
#         data = matrix.gam,
#         nstates = k,
#         transition = ~ Daily
#         #respstart = resp_init
#       )
#   }
#   fit.hmm <- fit(mod, verbose = F) #fit
#   #probs <- posterior(fit.hmm)
#   # Lets change the name
#   #colnames(probs)[2:(k + 1)] <- paste("state", 1:k, sep = " ")
#   # Create dta.frame
#   #viterbi sequencing
#   vit <- viterbi(fit.hmm)
#   df.viterbi <- data.table(cbind(datum=index(energy),net=matrix.gam$Load, vit))
#   df.viterbi.wide <- melt(df.viterbi[1:500], id.vars = "datum")
#   #compute state durations
#   durations <- matrix(0, nrow = nrow(vit), ncol = k)
#   mean.durations <- list()
#   for (i in c(1:k)) {
#     durations[, i] = ifelse(vit$state == i, 1, 0)
#     t = rle(durations[, i])$lengths
#     mean.durations[[i]] <- mean(t[t != 1]) / 4
#   }
#   
#   return(list(
#     "mod" = fit.hmm,
#     "vit" = df.viterbi.wide,
#     "durations" = mean.durations
#   ))
# }

multiplot <- function(..., plotlist = NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots / plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i / plotCols)
    curCol = (i - 1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol))
  }
  
}
