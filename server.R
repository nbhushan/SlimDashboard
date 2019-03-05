options(shiny.maxRequestSize = 1024 * 1024 ^ 2)


# TABLE OF CONTENTS
# 0. Prepare environment

# 0. Prepare environment ----
if (FALSE) {
library(shiny)
library(ggplot2)
library(scales)
library(data.table)
library(bit64)
library(dygraphs)
library(xts)
library(shinythemes)
source("helpers.R", local = TRUE)
}

shinyServer(function(input, output) {
  #This function is repsonsible for loading in the selected file
  energyData <- reactive({
    inFile <- input$energy_file
    if (is.null(inFile)) {
      # User has not uploaded a file yet
      #energy.data <- fread("exampleData/famig .csv", sep=";", dec=".", fill=TRUE)
      return(NULL)
    }
    energy.data <-
      fread(inFile$datapath,
            sep = ";",
            dec = ".",
            fill = TRUE)
    return(energy.data)
  })
  
  weatherData <- reactive({
    inFile <- input$weather_file
    if (is.null(inFile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    #read.csv2(inFile$datapath)
    read.table(inFile$datapath)
  })
  
  #Retrieve postal code
  output$EAN <- renderUI({
    if (is.null(input$energy_file)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    selectInput("EAN", "Select EAN", sort(c(as.character(
      unique(energyData()$EAN)
    ))))
  })
  
  #Retrieve category
  output$category <- renderUI({
    if (is.null(input$energy_file) &
        is.null(input$EAN)) {
      # User has not selected a pincode yet
      return(NULL)
    }
    selectInput("category", "Energy type",
                sort(c(as.character(
                  unique(subset(energyData(), EAN == input$EAN)$EnergieType)
                ))), "Elektra")
  })
  
  #PLot household energy consumption
  output$dygraph  <- renderDygraph({
    if (is.null(input$energy_file) &
        is.null(input$EAN) &
        is.null(input$category)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    date.smooth <- input$radio[1]
    plot_house(
      date.smooth,
      energy_file = energyData(),
      weather_file = weatherData(),
      EAN = input$EAN,
      category = input$category,
      use_weather = FALSE
    )
  })
  
  #PLot household energy consumption with temperature
  output$dygraph_weather  <- renderDygraph({
    if (is.null(input$energy_file) &
        is.null(input$weather_file) &
        is.null(input$EAN) &
        is.null(input$category)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    date.smooth <- input$radio[1]
    plot_house(
      date.smooth,
      energy_file = energyData(),
      weather_file = weatherData(),
      EAN = input$EAN,
      category = input$category,
      use_weather = FALSE
    )
  })
  
  output$dygraph_house_box  <- renderPlot({
    if (is.null(input$energy_file) &
        is.null(input$EAN) &
        is.null(input$category)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    if (input$category == "Elektra") {
      energy_unit <- "kWh"
    } else {
      energy_unit <- "m^3"
    }
    date_range <- input$radio[1]
    house <- subset(energyData(),
                    EAN == input$EAN &
                      EnergieType == input$category)
    
    
    house$Datum <-
      as.POSIXct(house$Datum, format = "%Y-%m-%dT%H:%M:%SZ",
                 tz = "Europe/Amsterdam")
    
    #sample[order(sample$Datum),]
    house <-
      subset(house, select = c(Datum, Register, Meetwaarde))
    house$Register <- as.factor(house$Register)
    # if(input$category=="Gas"){
    # house$Meetwaarde[house$EnergieType == "Gas"] <-
    #   house$Meetwaarde[house$EnergieType == "Gas"] * 9.769}
    houseWide <-
      dcast(house,
            Datum ~ Register,
            value.var = "Meetwaarde",
            fun.aggregate = mean)
    if (length(levels(house$Register)) > 1) {
      houseWide$`2.8.0` <- houseWide$`2.8.0` * -1
    }
    energyxts <-
      xts(houseWide[, -1], order.by = houseWide$Datum)
    
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
    colnames(energy) <- c("fromGrid", "toGrid")
    house.df <-  data.frame(Datum = index(energy), coredata(energy))
    house.df.long <- melt(house.df, id.vars =  "Datum")
    
    if (date_range == "Daily") {
      X <-
        reorder(format(house.df.long$Datum, "%A"),
                house.df.long$Datum,
                order = T)
    }    else if (date_range == "Hourly") {
      X <-
        reorder(format(house.df.long$Datum, "%H"),
                house.df.long$Datum,
                order = T)
    }    else if (date_range == "Monthly") {
      X <-
        reorder(format(house.df.long$Datum, "%B"),
                house.df.long$Datum,
                order = T)
    }    else if (date_range == "15min") {
      X <-
        reorder(format(house.df.long$Datum, "%H\n%b\n%Y"),
                house.df.long$Datum,
                order = T)
    }
    
    
    ggplot(house.df.long, aes(y = value, x = X,
                              fill = variable)) +
      #geom_violin(trim=FALSE) +
      ylab("kWh") +  xlab("") + geom_boxplot(outlier.shape = NA) +
      facet_grid(variable ~ ., scales =
                   "free_y") +
      scale_fill_manual(values = wes_palette("Darjeeling1")) +
      theme(
        axis.text.x = element_text(vjust = 0.5, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text      = element_text(size = 15),
        legend.position = "top"
      )
  })
  
  #PLot postal code energy cosumption
  output$pin_dygraph <- renderDygraph({
    if (is.null(input$energy_file) &
        is.null(input$EAN) &
        is.null(input$category)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    date.smooth <- input$radio[1]
    plot_pincode(
      date.smooth,
      energy_file = energyData(),
      weather_file = weatherData(),
      category = input$category,
      use_weather = FALSE
    )
  })
  
  #pin box plot
  output$pin_boxplot <- renderPlot({
    if (is.null(input$energy_file))
    {
      # User has not uploaded a file yet
      return(NULL)
    }
    
    sample <- subset(energyData(),
                     Postcode == input$pincode
                     & EnergieType == input$category)
    sample$Datum <-
      as.POSIXct(sample$Datum, format = "%Y-%m-%d %H:%M",
                 tz = "Europe/Amsterdam")
    
    sample$Huisnummer <- as.factor(sample$Huisnummer)
    sample$Register <- as.factor(sample$Register)
    new_sub <-
      subset(sample,
             select = c(Datum, Postcode, Huisnummer, Register, Meetwaarde))
    
    ggplot(new_sub, aes(
      y = Meetwaarde,
      x = reorder(format(Datum, "%W\n%b\n%Y"), Datum),
      fill = Huisnummer
    )) +  geom_boxplot(outlier.size = NA) + facet_grid(Register ~ ., scales =
                                                         "free_y") +
      #qplot(x=Postcode, y=mean, fill=EnergieType, data=means, geom="bar", stat="identity", position="dodge")+
      xlab("Date") + ylab("kWh") +
      labs(title  = paste("PostCode : ", as.character(input$pincode))) +
      stat_summary(fun.y = mean, geom = "line", aes(group = 1)) +
      theme(
        axis.text.x = element_text(vjust = 0.5, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(size = 15, face = "bold")
      )
    
  })
  
  #arima identification
  output$house_arima_acf <- renderPlot({
    if (is.null(input$energy_file) &
        is.null(input$EAN) &
        is.null(input$category)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    
    #ARIMA
    arimaData <- prepareData()$data.ts
    arima_identify(arimaData)
  })
  
  #arima print estimation
  output$house_arima_estimate <- renderPrint({
    if (is.null(input$energy_file) &
        is.null(input$EAN) &
        is.null(input$category)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    arimaData <- prepareData()$data.ts
    summary(fit_arima(arimaData))
  })
  
  #plot tsdiag
  output$house_arima_tsdiag <- renderPlot({
    if (is.null(input$energy_file) &
        is.null(input$EAN) &
        is.null(input$category)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    arimaData <- prepareData()$data.ts
    plot_arima_tsdiag(arimaData)
  })
  
  #fit and plot arima
  output$house_arima <- renderDygraph({
    if (is.null(input$energy_file) &
        is.null(input$EAN) &
        is.null(input$category)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    arimaData <- prepareData()$data.ts
    date_range <- input$radio[1]
    plot_arima(arimaData, date_range)
  })
  
  output$fitHMM <- renderPrint({
    if (is.null(input$energy_file) &
        is.null(input$EAN) &
        is.null(input$category)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    res <- fit.hmm()
    summary(res$mod)
    if(input$radio[1]!="15min"){
      print(paste("State durations in units:", input$radio[1], sep = " "))
    } else{print(paste("State durations in units:", "Hours", sep = " "))}
    print(res$durations)
  })
  
  #visualise HMM
  output$house_hmm <- renderPlot({
    if (is.null(input$energy_file) &
        is.null(input$EAN) &
        is.null(input$category)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    #k = as.numeric(input$k)
    #covariates <- input$hmm_covariate
    #hmmdata <- prepareData()$data.xts
    res <- fit.hmm()
    vit = res$vit
    qplot(datum,value,data=vit,geom="line",
          main = "Hdden Markov Model: Viterbi decoded sequence",
          ylab = "probability of latent state given observed sequence",xlab="") + 
      facet_grid(variable ~ ., scales="free_y") + theme_bw()
  })
  
  output$fitGAMM <- renderPrint({
    if (is.null(input$energy_file) &
        is.null(input$EAN) &
        is.null(input$category)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    df <- prepareData()
    df.x <- df$data.xts
    gam.df <- as.data.table(df.x)
    gam.df[,'week_day'] <- lubridate::wday(gam.df$index, label=FALSE,week_start=1)
    gam.df[,'hour_of_day'] <- lubridate::hour(gam.df$index)
    gam.df[,'month'] <- lubridate::month(gam.df$index)
    res <- fit.gamm(gam.df)
    summary(res$mod)
  })
  
  #plot GAMM
  output$house_GAM <- renderPlot({
    if (is.null(input$energy_file) &
        is.null(input$EAN) &
        is.null(input$category)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    df <- prepareData()
    df.x <- df$data.xts
    gam.df <- as.data.table(df.x)
    gam.df[,'week_day'] <- lubridate::wday(gam.df$index, label=FALSE,week_start=1)
    gam.df[,'hour_of_day'] <- lubridate::hour(gam.df$index)
    gam.df[,'month'] <- lubridate::month(gam.df$index)
    viz.gamm(gam.df)
  })
  
  
  #prepare data for TS models
  prepareData <- reactive({
    if (is.null(input$energy_file) &
        is.null(input$EAN) &
        is.null(input$category)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    date_range <- input$radio[1]
    house <- subset(energyData(),
                    EAN == input$EAN &
                      EnergieType == input$category)
    
    #"2017-06-28T00:00:00Z"
    house$Datum <-
      as.POSIXct(house$Datum, format = "%Y-%m-%dT%H:%M:%SZ")
    house <- na.omit(house, cols = "Datum", invert = FALSE)
    house <-
      subset(house, select = c(Datum, Register, Meetwaarde))
    
    house$Register <- as.factor(house$Register)
    houseWide <-
      dcast(house,
            Datum ~ Register,
            value.var = "Meetwaarde",
            fun.aggregate = mean)
    houseWide$`2.8.0` <-
      replace(houseWide$`2.8.0`, is.na(houseWide$`2.8.0`), 0)
    
    energyxts <-
      xts(houseWide[, -1], order.by = houseWide$Datum)
    
    if (date_range == "Daily") {
      energy <- apply.daily(energyxts, FUN = colSums)
      frequency = 7 * 365
      seasonal.periods = c(7)
    }  else if (date_range == "Hourly") {
      energy <-
        period.apply(energyxts, endpoints(energyxts, "hours"), colSums)
      frequency = 24 * 365
      seasonal.periods = c(24, 168)
    }  else if (date_range == "Monthly") {
      energy <- apply.monthly(energyxts, FUN = colSums)
      frequency = 12
      seasonal.periods = c(12)
    }  else if (date_range == "15min") {
      energy <- energyxts
      frequency = (60 / 15) * 24 
      seasonal.periods = c(96, 336)
    }
    
    #arima.xts <- energy
    arima.xts <- energy[, 1] - energy[, 2]
    colnames(arima.xts) <- "Meetwaarde"
    start.year <- year(start(arima.xts))
    start.month <- month(start(arima.xts))
    end.year <- year(end(arima.xts))
    end.month <- month(end(arima.xts))
    start.ts <- c(start.year, start.month)
    end.ts <- c(end.year, end.month)
    arima.msts <- msts(
      coredata(arima.xts),
      start = start.ts,
      end = end.ts,
      seasonal.periods =  seasonal.periods
    )
    arima.ts <- ts(
      coredata(arima.xts),
      frequency = frequency
    )
    return(list("data.xts" = arima.xts, "data.ts" = arima.ts))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      sprintf(
        "%s_%s_%s_%s_%s.csv",
        input$energy_file,
        input$pincode,
        input$huisnummer,
        input$category,
        input$radio[1]
      )
    },
    # filename = function() { paste(input$energy_file,'_',
    #                               input$pincode,'_',input$huisnummer,
    #                               '_', input$category, '_', input$radio[1],'.csv', sep='') },
    content = function(file) {
      print(file)
      fwrite(as.data.table(prepareData()), file, sep = ";")
    }
  )
  
  fit.hmm <- reactive({
    if (is.null(input$energy_file) &
        is.null(input$EAN) &
        is.null(input$category)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    date_range = input$radio[1]
    if (date_range == "Daily") {
      den = 1
    }    else if (date_range == "Hourly") {
      den = 1
    }    else if (date_range == "Monthly") {
      den = 1
    }    else if (date_range == "15min") {
      den = 60/15
    }  
    
    set.seed(7)
    k = as.numeric(input$k)
    covariates <- input$hmm_covariate
    hmmdata <- prepareData()$data.xts
    gam.df <- as.data.table(hmmdata)
    gam.df[,'week_day'] <- lubridate::wday(gam.df$index, label=FALSE)
    gam.df[,'hour_of_day'] <- lubridate::hour(gam.df$index)
    gam.df[,'month'] <- lubridate::month(gam.df$index)
    matrix.gam <- data.table(Load = gam.df[, Meetwaarde],
                             Daily = gam.df[,hour_of_day],
                             Weekly = gam.df[, week_day],
                             Month = gam.df[,month])
    matrix.gam$Daily <- as.factor(matrix.gam$Daily)
    matrix.gam$Month <- as.factor(matrix.gam$Month)
    # #kmeans cluster
    # cl <- kmeans(coredata(energy), k, nstart = 25)
    # means <- as.vector(cl$centers)
    # sds <- sqrt(cl$tot.withinss / cl$size)
    # #Create HMM model
    # resp_init <- c(rbind(means, sds))
    #names(energy)<-"Meetwaarde"
    if (covariates == "none") {
      mod <-
        depmix(Load ~ 1,
               data = matrix.gam,
               nstates = k
               #respstart = resp_init
        )
    } else {
      mod <-
        depmix(
          Load ~ 1,
          data = matrix.gam,
          nstates = k,
          transition = ~ Daily
          #respstart = resp_init
        )
    }
    fit.hmm <- fit(mod, verbose = F) #fit
    #probs <- posterior(fit.hmm)
    # Lets change the name
    #colnames(probs)[2:(k + 1)] <- paste("state", 1:k, sep = " ")
    # Create dta.frame
    #viterbi sequencing
    vit <- posterior(fit.hmm)
    df.viterbi <- data.table(cbind(datum=index(hmmdata),net=matrix.gam$Load, vit))
    df.viterbi.wide <- melt(df.viterbi[1:500], id.vars = "datum")
    #compute state durations
    durations <- matrix(0, nrow = nrow(vit), ncol = k)
    mean.durations <- list()
    for (i in c(1:k)) {
      durations[, i] = ifelse(vit$state == i, 1, 0)
      t = rle(durations[, i])$lengths
      mean.durations[[i]] <- mean(t[t != 1]) / den
    }
    
    return(list(
      "mod" = fit.hmm,
      "vit" = df.viterbi.wide,
      "durations" = mean.durations
    ))
  }
  )
  
  
  function(input, output, session) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
})