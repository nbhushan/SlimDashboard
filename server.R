options(shiny.maxRequestSize=1024*1024^2)


library(shiny)
library(ggplot2) # load ggplot
library(GGally)
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
      return(NULL)
    }
    energy.data <- fread(inFile$datapath, sep=";", dec=".", fill=TRUE)
    if (input$post_checkbox) {
      energy.data$Postcode <-
        substr(energy.data$Postcode,start = 1, stop = 4)
    }
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
  output$postalCode <- renderUI({
    if (is.null(input$energy_file)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    selectInput("pincode","Select postal code",sort(c(as.character(
      unique(energyData()$Postcode)
    ))))
  })
  
  #Retrieve house number
  output$houseNumber <- renderUI({
    if (is.null(input$pincode)) {
      # User has not selected a pincode yet
      return(NULL)
    }
    selectInput("huisnummer","Select house number",
                sort(c(as.character(
                  unique(subset(
                    energyData(), Postcode == input$pincode
                  )$Huisnummer)
                ))))
  })
  
  #Retrieve category
  output$category <- renderUI({
    if (is.null(input$pincode) &
        is.null(input$huisnummer)) {
      # User has not selected a pincode yet
      return(NULL)
    }
    selectInput("category","Energy type",
                sort(c(as.character(
                  unique(
                    subset(
                      energyData(), Postcode == input$pincode &
                        Huisnummer == input$huisnummer)$EnergieType
                  )
                ))), "Elektra")
  })
  
  #PLot household energy consumption
  output$dygraph  <- renderDygraph({
    if (is.null(input$energy_file) &
        is.null(input$pincode) &
        is.null(input$huisnummer) &
        is.null(input$category)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    date.smooth <- input$radio[1]
    plot_house(
      date.smooth, energy_file = energyData(), weather_file = weatherData(),
      pincode = input$pincode, huisnummer = input$huisnummer,
      category = input$category, use_weather = FALSE )
  })
  
  #PLot household energy consumption with temperature
  output$dygraph_weather  <- renderDygraph({
    if (is.null(input$energy_file) &
        is.null(input$pincode) &
        is.null(input$huisnummer) &
        is.null(input$register) &
        is.null(input$category) &
        is.null(input$weather_file)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    date.smooth <- input$radio[1]
    plot_house(
      date.smooth, energy_file = energyData(), weather_file = weatherData(),
      pincode = input$pincode, huisnummer = input$huisnummer,
      register = input$register, category = input$category, use_weather =
        TRUE
    )
  })
  
  output$dygraph_house_box  <- renderPlot({
    if (is.null(input$energy_file) &
        is.null(input$pincode) &
        is.null(input$huisnummer) &
        is.null(input$category)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    if (input$category == "Elektra") {
      energy_unit <- "kWh"
      nat_avg = 9
    } else {
      energy_unit <- "m^3"
      nat_avg <- 3.835
    }
    
    house <- subset(
      energyData(), Postcode == input$pincode &
        Huisnummer == input$huisnummer
      & EnergieType == input$category
    )
    
    
    house$Datum <- as.POSIXct(house$Datum, format = "%Y-%m-%d %H:%M", 
                              tz = "Europe/Amsterdam")
    
    #sample[order(sample$Datum),]
    house <-
      subset(house, select = c(Datum, Register, Meetwaarde))
    house$EnergieType <- as.factor(house$Register)
    if(input$category=="Gas"){
    house$Meetwaarde[house$EnergieType == "Gas"] <-
      house$Meetwaarde[house$EnergieType == "Gas"] * 9.769}
    
    ggplot(house, aes(
      y = Meetwaarde, x = reorder(format(Datum,"%W\n%b\n%Y"), Datum),
      fill = Register
    )) +
      geom_boxplot() + ylab("kWh") +  xlab("") +
      facet_grid(Register ~ ., scales =
                   "free_y") +      
      labs(title  = paste( as.character(input$category), 
        " consumption per week")) +
      stat_summary(fun.y = mean, geom = "line", aes(group = 1)) +
      theme(
        axis.text.x = element_text(vjust = 0.5, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(size = 15,face = "bold")
      )
  })
  
  #PLot postal code energy cosumption
  output$pin_dygraph <- renderDygraph({
    if (is.null(input$energy_file)
        & is.null(input$pincode) &
        is.null(input$category)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    date.smooth <- input$radio[1]
    plot_pincode(
      date.smooth, energy_file = energyData(), weather_file = weatherData(),
      pincode = input$pincode, category = input$category, use_weather = FALSE )
  })
  
  #pin box plot
  output$pin_boxplot <- renderPlot({
    if (is.null(input$energy_file))
    {
      # User has not uploaded a file yet
      return(NULL)
    }
    
    sample <- subset(energyData(), Postcode == input$pincode
                     & EnergieType == input$category)
    sample$Datum <- as.POSIXct(sample$Datum, format = "%Y-%m-%d %H:%M", 
                                  tz = "Europe/Amsterdam")
 
    sample$Huisnummer <- as.factor(sample$Huisnummer)
    sample$Register <- as.factor(sample$Register)
    new_sub <-
      subset(sample, select = c(Datum, Postcode, Huisnummer, Register, Meetwaarde))

      ggplot(new_sub, aes(
      y = Meetwaarde, x = reorder(format(Datum,"%W\n%b\n%Y"), Datum),
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
        legend.text = element_text(size = 15,face = "bold")
      )
    
  })

  #arima identification
  output$house_arima_acf <- renderPlot({
    if (is.null(input$energy_file)
        | is.null(input$pincode) |
        is.null(input$huisnummer)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    
    #ARIMA
    arimaData <- prepareData()
    arima_identify(arimaData)
  })
  
  #arima print estimation
  output$house_arima_estimate <- renderPrint({
    if (is.null(input$energy_file)
        | is.null(input$pincode) |
        is.null(input$huisnummer)) {
      # User has not uploaded a file yet
      return(NULL)
    }    
    arimaData <- prepareData()
    summary(fit_arima(arimaData))
  })  
  
  #plot tsdiag
  output$house_arima_tsdiag <- renderPlot({
    if (is.null(input$energy_file) &
        is.null(input$pincode) &
        is.null(input$huisnummer) &
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
        is.null(input$pincode) &
        is.null(input$huisnummer) &
        is.null(input$category)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    arimaData <- prepareData()
    date_range <- input$radio[1]
    plot_arima(arimaData, date_range)
  })
  
  output$fitHMM <- renderPrint({
    if (is.null(input$energy_file)
        | is.null(input$pincode) |
        is.null(input$huisnummer)) {
      # User has not uploaded a file yet
      return(NULL)
    }    
    hmmdata <- prepareData()
    summary(fit_hmm(hmmdata))
  })
  
  #visualise HMM
  output$house_hmm <- renderPlot({
    if (is.null(input$energy_file)
        | is.null(input$pincode) |
        is.null(input$huisnummer)) {
      # User has not uploaded a file yet
      return(NULL)
    }    
    k=4
    hmmdata <- prepareData()
    fm <- fit_hmm(hmmdata)
    probs <- posterior(fm)        
    # Lets change the name
    colnames(probs)[2:(k+1)] <- paste("S",1:k, sep="-")
    # Create dta.frame
    dfu <- cbind(datum=index(hmmdata),net=coredata(hmmdata)[,"net"], probs[,2:(k+1)])
    dfm <- melt(dfu, id.vars = "datum")
    qplot(datum,value,data=dfm,geom="line",
          main = "Posterior probablies of states $P(Z_t|Y_t)$",
          ylab = "") + 
      facet_grid(variable ~ ., scales="free_y") + theme_bw()
  })  
  
  
  #prepare data for TS models
  prepareData <- reactive({
    if (is.null(input$energy_file)&
        is.null(input$pincode) &
        is.null(input$huisnummer)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    date_range <- input$radio[1]
    house <- subset(
      energyData(), Postcode == input$pincode &
        Huisnummer == input$huisnummer &
        EnergieType == input$category )
    
    house$Datum <- as.POSIXct(house$Datum, format = "%Y-%m-%d %H:%M", 
                              tz = "Europe/Amsterdam")
    
    house <-
      subset(house, select = c(Datum, Register, Meetwaarde))
    house$Register <- as.factor(house$Register)
    houseWide <- dcast(house, Datum ~ Register, value.var = "Meetwaarde" )
    if (length(levels(house$Register)) > 1){
      houseWide$`2.8.0` <- houseWide$`2.8.0`* -1 
      houseWide$net <- houseWide$`1.8.0`+ houseWide$`2.8.0`
    } else{
      houseWide$net <- houseWide$`1.8.0`
    }
    
    energyxts <-
      xts(houseWide[,-1], order.by = houseWide$Datum)
    
    if (date_range == "Day") {
      energy <- apply.daily(energyxts, FUN=mean)
    } else if (date_range == "Week") {
      energy <- apply.weekly(energyxts, FUN=mean)
    } else if (date_range == "Month") {
      energy <- apply.monthly(energyxts, FUN=mean)
    }  else if(date_range=="15min"){
      energy <- energyxts
    }         
    return(energy)
  })
  
})