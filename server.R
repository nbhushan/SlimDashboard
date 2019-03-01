options(shiny.maxRequestSize = 1024 * 1024 ^ 2)


library(shiny)
library(ggplot2)
library(scales)
library(data.table)
library(bit64)
library(dygraphs)
library(xts)
library(shinythemes)
source("helpers.R")


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
      ylab("kWh") +  xlab("") + geom_boxplot(width=0.1)+
      facet_grid(variable ~ ., scales =
                   "free_y") +
      scale_fill_manual(values = wes_palette("Darjeeling1")) +
      theme(
        axis.text.x = element_text(vjust = 0.5, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text      = element_text(size=15),
        legend.position="top"
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
    arimaData <- prepareData()
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
    arimaData <- prepareData()
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
    arimaData <- prepareData()
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
    arimaData <- prepareData()
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
    hmmdata <- prepareData()
    k <- as.numeric(input$k[1])
    summary(fit_hmm(hmmdata, k))
  })
  
  #visualise HMM
  output$house_hmm <- renderPlot({
    if (is.null(input$energy_file) &
        is.null(input$EAN) &
        is.null(input$category)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    k = as.numeric(input$k[1])
    hmmdata <- prepareData()
    fm <- fit_hmm(hmmdata, k)
    probs <- posterior(fm)
    # Lets change the name
    colnames(probs)[2:(k + 1)] <- paste("S", 1:k, sep = "-")
    # Create dta.frame
    dfu <-
      data.table(cbind(
        datum = index(hmmdata),
        net = coredata(hmmdata)[, "net"],
        probs[, 2:(k + 1)]
      ))
    dfm <- melt(dfu[1:100], id.vars = "datum" )
    ggplot(dfm, aes(datum, value)) + geom_line() +
      facet_grid(variable ~ ., scales = "free_y") + theme_bw()
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
    
    #arima.xts <- energy
    arima.xts <- energy[, 1] - energy[, 2]
    colnames(arima.xts) <- "meetwaarde"
    start.year <- year(start(arima.xts))
    start.month <- month(start(arima.xts))
    end.year <- year(end(arima.xts))
    end.month <- month(end(arima.xts))
    start.ts <- c(start.year, start.month)
    end.ts <- c(end.year, end.month)
    
    arima.msts <- msts(coredata(arima.xts), 
                       start = start.ts,
                       end = end.ts,
                       seasonal.periods =  seasonal.periods)
    return(arima.msts)
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
  
  function(input, output, session) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
})