
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
      
      selectInput("module", 
            label = "Choose Module",
            choices = c("Single Model", "Multiple Model"),
            selected = "Single Model"
            ),

      conditionalPanel(
        condition = "input.module == 'Single Model' ",
        
        checkboxGroupInput("Strat", label = "Choose Stratification Variables",
                           choices = list("HIV groups" = 1, "AGE groups" = 2,
                                          "CXR Diagnoses" = 3),selected = NULL),
        checkboxGroupInput("Adj", label = "Choose Adjustment Variables",
                           choices = list("HIV groups" = 1, "AGE groups" = 2),selected = NULL),
        uiOutput("StratPanel1"),
        uiOutput("StratPanel2"),
        uiOutput("StratPanel3"),
        
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
                    choices = c("Pathogen specific sensitivity", "10%-20% for all silver standard"),
                    selected = "Pathogen specific sensitivity"),
        selectInput("CI", 
                    label = "Correlation in Measurement",
                    choices = c("Conditional Independence", "Conditional Dependence"),
                    selected = "Conditional Independence")
        
      ),

      conditionalPanel(
        condition = "input.module == 'Multiple Model' ",
        
        helpText("Model A"),
        
        checkboxGroupInput("StratA", label = "Choose Stratification Variables",
                           choices = list("HIV groups" = 1, "AGE groups" = 2,
                                          "CXR Diagnoses" = 3),selected = NULL),
        checkboxGroupInput("AdjA", label = "Choose Adjustment Variables",
                           choices = list("HIV groups" = 1, "AGE groups" = 2),selected = NULL),
        uiOutput("StratPanel1A"),
        uiOutput("StratPanel2A"),
        uiOutput("StratPanel3A"),
        selectInput("EPA", 
                    label = "Choose Etiology Prior",
                    choices = c("Weak Uniform Prior", "Weak informed Prior"),
                    selected = "Weak Uniform Prior"),
        selectInput("TBPA", 
                    label = "Choose Sensitivity Prior for Bronze Standard",
                    choices = c("50%-100%", "0%-100%"),
                    selected = "50%-100%"),
        selectInput("TSPA", 
                    label = "Choose Sensitivity Prior for Silver Standard",
                    choices = c("Pathogen specific sensitivity", "10%-20% for all silver standard"),
                    selected = "Pathogen specific sensitivity"),
        selectInput("CIA", 
                    label = "Correlation in Measurement",
                    choices = c("Conditional Independence", "Conditional Dependence"),
                    selected = "Conditional Independence"),
        
        helpText("Model B"),
        
        checkboxGroupInput("StratB", label = "Choose Stratification Variables",
                           choices = list("HIV groups" = 1, "AGE groups" = 2,
                                          "CXR Diagnoses" = 3),selected = NULL),
        checkboxGroupInput("AdjB", label = "Choose Adjustment Variables",
                           choices = list("HIV groups" = 1, "AGE groups" = 2),selected = NULL),
        uiOutput("StratPanel1B"),
        uiOutput("StratPanel2B"),
        uiOutput("StratPanel3B"),
        selectInput("EPB", 
                    label = "Choose Etiology Prior",
                    choices = c("Weak Uniform Prior", "Weak informed Prior"),
                    selected = "Weak Uniform Prior"),
        selectInput("TBPB", 
                    label = "Choose Sensitivity Prior for Bronze Standard",
                    choices = c("50%-100%", "0%-100%"),
                    selected = "50%-100%"),
        selectInput("TSPB", 
                    label = "Choose Sensitivity Prior for Silver Standard",
                    choices = c("Pathogen specific sensitivity", "10%-20% for all silver standard"),
                    selected = "Pathogen specific sensitivity"),
        selectInput("CIB", 
                    label = "Correlation in Measurement",
                    choices = c("Conditional Independence", "Conditional Dependence"),
                    selected = "Conditional Independence"),        
        checkboxInput("Advanced", label = "Advanced Setting"),
        
        
        conditionalPanel(
          condition = "input.Advanced == 1 ",
          helpText("Model A Advanced Setting"),
          checkboxGroupInput("StratA_p", label = "Choose Stratification Variables for Subsetting Music Plot",
                             choices = list("HIV groups" = 1, "AGE groups" = 2,
                                            "CXR Diagnoses" = 3),selected = NULL),
          uiOutput("StratPanel1A_p"),
          uiOutput("StratPanel2A_p"),
          uiOutput("StratPanel3A_p"),

          helpText("Model B Advanced Setting"),
        
          checkboxGroupInput("StratB_p", label = "Choose Stratification Variables for Subsetting Music Plot",
                           choices = list("HIV groups" = 1, "AGE groups" = 2,
                                          "CXR Diagnoses" = 3),selected = NULL),
        uiOutput("StratPanel1B_p"),
        uiOutput("StratPanel2B_p"),
        uiOutput("StratPanel3B_p")
        )

    ),
      
      
      actionButton("Go","Go")
    ),
    
    mainPanel(
      textOutput("text1")
    )
  )
))