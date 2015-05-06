
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
      downloadButton('instruction',label = "Instructions"),
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
                           choices = list("HIV status" = 1, "AGE groups" = 2,
                                          "CXR Conclusions" = 3),selected = NULL),
        checkboxGroupInput("Adj", label = "Choose Adjustment Variables",
                           choices = list("HIV status" = 1, "AGE groups" = 2),selected = NULL),
        uiOutput("StratPanel1"),
        uiOutput("StratPanel2"),
        uiOutput("StratPanel3"),
        helpText("Model Advanced Setting "),
        checkboxInput("Advanced", label = "Show Advanced Setting"),
        
        
        conditionalPanel(
          condition = "input.Advanced == 1 ",
        selectInput("EP", 
                    label = "Choose Etiology Prior",
                    choices = list("Weak Uniform Prior"=1, "Weak informed Prior"=2),
                    selected = 1),
        selectInput("TBP", 
                    label = "Choose Sensitivity Prior for Bronze Standard",
                    choices = list("50%-100%"=1, "0%-100%"=2),
                    selected = 1),
        selectInput("TSP", 
                    label = "Choose Sensitivity Prior for Silver Standard",
                    choices = list("Pathogen specific sensitivity"=1, "10%-20% for all silver standard"=2),
                    selected = 1),
        selectInput("CI", 
                    label = "Correlation in Measurement",
                    choices = list("Conditional Independent"=1, "Conditional Dependent"=2),
                    selected = 1)
        )
        
      ),

      conditionalPanel(
        condition = "input.module == 'Multiple Model' ",
        
        helpText("Model A"),
        
        checkboxGroupInput("StratA", label = "Choose Stratification Variables",
                           choices = list("HIV status" = 1, "AGE groups" = 2,
                                          "CXR Conclusions" = 3),selected = NULL),
        checkboxGroupInput("AdjA", label = "Choose Adjustment Variables",
                           choices = list("HIV status" = 1, "AGE groups" = 2),selected = NULL),
        uiOutput("StratPanel1A"),
        uiOutput("StratPanel2A"),
        uiOutput("StratPanel3A"),
        helpText("Model A Advanced Setting "),
        
        checkboxInput("AdvancedA", label = "Show Advanced Setting"),
        
        
        conditionalPanel(
          condition = "input.AdvancedA == 1 ",
        selectInput("EPA", 
                    label = "Choose Etiology Prior",
                    choices = list("Weak Uniform Prior"=1, "Weak informed Prior"=2),
                    selected = 1),
        selectInput("TBPA", 
                    label = "Choose Sensitivity Prior for Bronze Standard",
                    choices = list("50%-100%"=1, "0%-100%"=2),
                    selected = 1),
        selectInput("TSPA", 
                    label = "Choose Sensitivity Prior for Silver Standard",
                    choices = list("Pathogen specific sensitivity"=1, "10%-20% for all silver standard"=2),
                    selected = 1),
        selectInput("CIA", 
                    label = "Correlation in Measurement",
                    choices = list("Conditional Independent"=1, "Conditional Dependent"=2),
                    selected = 1)
        ),
        
        helpText("Model B"),
        
        checkboxGroupInput("StratB", label = "Choose Stratification Variables",
                           choices = list("HIV status" = 1, "AGE groups" = 2,
                                          "CXR Conclusions" = 3),selected = NULL),
        checkboxGroupInput("AdjB", label = "Choose Adjustment Variables",
                           choices = list("HIV status" = 1, "AGE groups" = 2),selected = NULL),
        uiOutput("StratPanel1B"),
        uiOutput("StratPanel2B"),
        uiOutput("StratPanel3B"),
        helpText("Model B Advanced Setting "),
        
        checkboxInput("AdvancedB", label = "Show Advanced Setting"),
        
        
        conditionalPanel(
          condition = "input.AdvancedB == 1 ",
        selectInput("EPB", 
                    label = "Choose Etiology Prior",
                    choices = list("Weak Uniform Prior"=1, "Weak informed Prior"=2),
                    selected = 1),
        selectInput("TBPB", 
                    label = "Choose Sensitivity Prior for Bronze Standard",
                    choices = list("50%-100%"=1, "0%-100%"=2),
                    selected = 1),
        selectInput("TSPB", 
                    label = "Choose Sensitivity Prior for Silver Standard",
                    choices = list("Pathogen specific sensitivity"=1, "10%-20% for all silver standard"=2),
                    selected = 1),
        selectInput("CIB", 
                    label = "Correlation in Measurement",
                    choices = list("Conditional Independent"=1, "Conditional Dependent"=2),
                    selected = 1)
        ),       
        helpText("Music Plot Advanced Setting "),
        
        checkboxInput("Advanced_p", label = "Music Plot Advanced Setting"),
        
        
        conditionalPanel(
          condition = "input.Advanced_p == 1 ",
          helpText("Model A Advanced Setting"),
          checkboxGroupInput("StratA_p", label = "Choose Stratification Variables for Subsetting Music Plot",
                             choices = list("HIV status" = 1, "AGE groups" = 2,
                                            "CXR Conclusions" = 3),selected = NULL),
          uiOutput("StratPanel1A_p"),
          uiOutput("StratPanel2A_p"),
          uiOutput("StratPanel3A_p"),

          helpText("Model B Advanced Setting"),
        
          checkboxGroupInput("StratB_p", label = "Choose Stratification Variables for Subsetting Music Plot",
                           choices = list("HIV status" = 1, "AGE groups" = 2,
                                          "CXR Conclusions" = 3),selected = NULL),
        uiOutput("StratPanel1B_p"),
        uiOutput("StratPanel2B_p"),
        uiOutput("StratPanel3B_p")
        )

    ),
      
      
        actionButton("Check","Check"),
        actionButton("Go","Go")

    ),
    
    mainPanel(
      htmlOutput("text1"),
      htmlOutput("text2")
    )
  )
))