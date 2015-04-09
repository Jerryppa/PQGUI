
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  titlePanel("PERCH Quantitative Model"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create you own pie"),
      
      selectInput("start", 
                  label = "What do you want to do",
                  choices = c("See what does PERCH look like", "Run Mighty PERCH Model"),
                  selected = "See what does PERCH look like"),
      
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
      
      submitButton("Submit")
      ),
    
    mainPanel(
      textOutput("text1")
    )
  )
))