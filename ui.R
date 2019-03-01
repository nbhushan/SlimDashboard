library(shiny)
library(ggplot2) # load ggplot
library(scales)
library(plyr)
library(dygraphs)
library(xts)
library(shinythemes)
library(shinycssloaders)

shinyUI(fluidPage(theme = shinytheme("flatly"),
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

    uiOutput('EAN'),
    uiOutput('category'),
    tags$hr(),

    
    # Sampling frequency
    radioButtons("radio", label = "Aggregate data",
                 choices = list("15min", "Hourly", "Daily", "Monthly"), 
                 selected = "Daily"),    

    # # Copy the line below to make a set of select inputs
    # selectInput("k", label = "Number of HMM states",
    #              choices = list(2,3,4,5), 
    #              selected = 3),  
    # # Copy the line below to make a checkbox
    # selectInput("hmm_covariate", label = "Covariates on dynamics", 
    # choices = list("none", "temperature", "affect", "psychological determinants"), 
    #             selected = "none"),  
    # # Copy the line below to make a checkbox
    # checkboxInput("hmm_random", label = "Random effects", value = FALSE), 
    # #Download button
   # downloadButton('downloadData', 'Download'),
    width =2),
  
  
  mainPanel(
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".span8 .well { background-color: #00FFFF; }",
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    tabsetPanel(
      tabPanel("Visualize",
               tabsetPanel(
                 tabPanel(
                   "Time series",  dygraphOutput("dygraph", width = "95%", height = "720px")%>% withSpinner(color="#000000")
                 ),
               tabPanel(
                 "Distributions",  plotOutput("dygraph_house_box", width = "95%", height = "720px")%>% withSpinner(color="#000000")
               ))),
      # tabPanel(
      #   "Neighbourhood", 
      #   tabsetPanel(
      #     tabPanel("Seasonal",dygraphOutput("pin_dygraph", width = "95%", height = "720px")),
      #     tabPanel(
      #       "Boxplots",  plotOutput("pin_boxplot", width = "95%",height = "720px")
      #     ))),
      tabPanel(
        "Model",
        tabsetPanel(
          tabPanel(
            "Auto regressive models",  
            tabsetPanel(
              tabPanel(
                "Identification",plotOutput("house_arima_acf", width = "95%", height = "720px")%>% withSpinner(color="#000000")
              ),   
              tabPanel(
                "Estimation", verbatimTextOutput("house_arima_estimate")%>% withSpinner(color="#000000")
              ), 
              tabPanel(
                "Forecast",
                dygraphOutput("house_arima", width = "95%", height = "720px")%>% withSpinner(color="#000000")
              ),
              tabPanel(
                "Diagnostics",plotOutput("house_arima_tsdiag", width = "95%", height = "720px")%>% withSpinner(color="#000000")
              )
            )),
          tabPanel(
            "Hidden Markov models",  
            tabsetPanel(
              tabPanel("Estimation", verbatimTextOutput("fitHMM")%>% withSpinner(color="#000000")),                              
              tabPanel("Inference", plotOutput("house_hmm", width = "95%", height = "720px")%>% withSpinner(color="#000000")
              ))),
          tabPanel(
            "General Additive Mixture Model",  
            tabsetPanel(
              tabPanel("Estimation", verbatimTextOutput("fitGAMM")%>% withSpinner(color="#000000")),                              
              tabPanel("Inference", plotOutput("house_GAM", width = "95%", height = "720px")%>% withSpinner(color="#000000")
              )))
        ))), 
    width =10)
)
))
