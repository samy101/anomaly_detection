
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

data_path = "E:/ibm/kyab/hourly_distance_matrix/"
house_list = list.dirs(data_path, full.names = FALSE, recursive = FALSE)
# exclude the first dir which is current directory itself.
house_list = house_list[1:length(house_list)]

shinyUI(fluidPage(
  titlePanel("KYAB - Anomaly Detection!"),

  fluidRow(
    column(2,           
           selectInput("house", 
                       label = h5("Select a house"),
                       choices = house_list)
    ),
    column(2,           
           selectInput("sensor", 
                       label = h5("Select a sensor"),
                       choices = ""),
           checkboxInput("plotallsensor", "Plot all sensor ", value = FALSE),
           checkboxInput("plotcorrelation", "Plot correlation matrix ", value = FALSE)
    )    
  ),  
  hr(),
  plotOutput("anomalyPlot"), 
  plotOutput("allDataPlot"), 
  plotOutput("correlationPlot")  
  
))
