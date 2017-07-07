library(shiny)
library(ggplot2) # load ggplot
library(scales)
library(plyr)
library(dygraphs)
library(xts)
library(shinythemes)


shinyUI(fluidPage(theme = shinytheme("cosmo"),
  titlePanel(
  "slimDashboard"
),

sidebarLayout(
  sidebarPanel(
    fileInput(
      'energy_file', 'Choose energy data (.csv)',
      multiple = TRUE,
      accept = c('text/csv',
                 'text/comma-separated-values,text/plain',
                 '.csv')
    ),
    tags$hr(),
    fileInput(
      'weather_file', 'Choose weather data',
      multiple = TRUE,
      accept = c('text/csv',
                 'text/comma-separated-values,text/plain',
                 '.csv')
    ),
    tags$hr(),
    #     fileInput(
    #       'demo_file', 'Choose demographic data (.csv)',
    #       accept = c('text/csv',
    #                  'text/comma-separated-values,text/plain',
    #                  '.csv')
    #     ),

    uiOutput('postalCode'),
    uiOutput('houseNumber'),
    uiOutput('category'),
    tags$hr(),
    # Copy the line below to make a checkbox
    checkboxInput("post_checkbox", label = "Group Postcodes", value = FALSE),    
    
    # Copy the line below to make a set of radio buttons
    radioButtons("radio", label = "Sampling frequency",
                 choices = list("15min", "Day", "Week", "Month"), 
                 selected = "15min"),    
    
    # Copy the line below to make a set of select inputs
    selectInput("k", label = "Number of HMM states",
                 choices = list(2,3,4,5), 
                 selected = 3),  
    # Copy the line below to make a checkbox
    selectInput("hmm_covariate", label = "Covariates on dynamics", 
    choices = list("none", "temperature", "affect", "psychological determinants"), 
                selected = "none"),  
    # Copy the line below to make a checkbox
    checkboxInput("hmm_random", label = "Random effects", value = FALSE), 
    width =2 ),
  
  
  mainPanel(
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    tabsetPanel(
      tabPanel("Household",
               tabsetPanel(
                 tabPanel(
                   "Time series",  dygraphOutput("dygraph", width = "95%", height = "720px")
                 ),
               tabPanel(
                 "Boxplots",  plotOutput("dygraph_house_box", width = "95%", height = "720px")
               ))),
      tabPanel(
        "Neighbourhood", 
        tabsetPanel(
          tabPanel("Seasonal",dygraphOutput("pin_dygraph", width = "95%", height = "720px")),
          tabPanel(
            "Boxplots",  plotOutput("pin_boxplot", width = "95%",height = "720px")
          ))),
      tabPanel(
        "Models",
        tabsetPanel(
          tabPanel(
            "Arima",  
            tabsetPanel(
              tabPanel(
                "Identification",plotOutput("house_arima_acf", width = "95%", height = "720px")
              ),   
              tabPanel(
                "Estimation", verbatimTextOutput("house_arima_estimate")
              ), 
              tabPanel(
                "Fit",
                dygraphOutput("house_arima", width = "95%", height = "720px")
              ),
              tabPanel(
                "Diagnostics",plotOutput("house_arima_tsdiag", width = "95%", height = "720px")
              )
            )),
          tabPanel(
            "HMM",  
            tabsetPanel(
              tabPanel("Estimation", verbatimTextOutput("fitHMM")),                              
              tabPanel("Inference", plotOutput("house_hmm", width = "95%", height = "720px")
              )))
        ))), 
    width =10)
)
))
