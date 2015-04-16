
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# Stratification - HIV exposed // CXR 

library(shiny)

shinyUI(fluidPage(
  titlePanel("PERCH Quantitative Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create you own pie"),
      
#      selectInput("start", 
#                  label = "What do you want to do",
#                  choices = c("Choose Difficulty", "Clean","Run Mighty PERCH Model"),
#                  selected = "See what does PERCH look like"),
      
      checkboxInput("advanced", "Advanced Setting"),


      conditionalPanel(
        condition = "input.advanced == false",
        selectInput("EP", 
                    label = "Choose Etiology Prior",
                    choices = c("Weak Uniform Prior", "Weak informed Prior"),
                    selected = "Weak Uniform Prior"),
        selectInput("TBP", 
                    label = "Choose Sensitivity Prior for Bronze Standard",
                    choices = c("50%-100%", "0%-100%"),
                    selected = "50%-100%"),
        selectInput("TSP", 
                    label = "Choose Sensitivity Prior for Silver Standard",
                    choices = c("10%-20%", "0%-20%"),
                    selected = "10%-20%"),
        selectInput("CI", 
                    label = "Correlation in Measurement",
                    choices = c("Conditional Independence", "Conditional Dependence"),
                    selected = "Conditional Independence")
        
      ),
      conditionalPanel(
        condition = "input.advanced == true",
        sliderInput("slider1", "Choose Etiology Prior for Virus: ", min = 0, max = 100, value = 0),
        uiOutput("slider2"),
        sliderInput("TBP1", 
                    label = "Choose Sensitivity Prior for Bronze Standard",
                    min = 0, max = 100, value = c(50,100)),
        sliderInput("TSP1", 
                    label = "Choose Sensitivity Prior for Silver Standard",
                    min = 0, max = 50, value = c(10,20)),
        sliderInput("CI1", 
                    label = "Choose Number of Subclasses in Cases/Controls",
                    min = 1, max = 5, value = 1 )),
      
      
      actionButton("Go","Go")
    ),
    
    mainPanel(
      textOutput("text1")
    )
  )
))