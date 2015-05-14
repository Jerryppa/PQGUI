### This is user-interface for running PQ Model 

library(shiny)

shinyUI(fluidPage(
  tagList(
      tags$head(
        tags$script(type="text/javascript", 
          src = "disable.js")
      )
    ),
  titlePanel("PERCH Quantitative Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create you own pie"),
      downloadButton('instruction',label = "Instructions"),
   
      selectInput("module", 
            label = "Choose Module",
            choices = c("Single Model", "Multiple Model"),
            selected = "Single Model"
            ),

      conditionalPanel(
        condition = "input.module == 'Single Model' ",
        helpText("(Please choose only one CXR definition)"),
        checkboxGroupInput("Strat", label = "Choose Restriction Variables",
                           choices = list("HIV status" = 1, "AGE groups" = 2,
                                          "PERCH CXR Conclusions" = 3,"Nasreen CXR Readings" = 4),
                           selected = NULL),
        checkboxGroupInput("Adj", label = "Choose Adjustment Variables",
                           choices = list("HIV status" = 1, "AGE groups" = 2),
                           selected = NULL),
        uiOutput("StratPanel1"),
        uiOutput("StratPanel2"),
        uiOutput("StratPanel3"),
        uiOutput("StratPanel4"),
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
                    selected = 1)
        )
        
      ),

      conditionalPanel(
        condition = "input.module == 'Multiple Model' ",
        
        helpText("Model A"),
        helpText("(Please choose only one CXR definition)"),
        
        checkboxGroupInput("StratA", label = "Choose Restriction Variables",
                           choices = list("HIV status" = 1, "AGE groups" = 2,
                                          "PERCH CXR Conclusions" = 3,"Nasreen CXR Readings" = 4),selected = NULL),
        checkboxGroupInput("AdjA", label = "Choose Adjustment Variables",
                           choices = list("HIV status" = 1, "AGE groups" = 2),selected = NULL),
        uiOutput("StratPanel1A"),
        uiOutput("StratPanel2A"),
        uiOutput("StratPanel3A"),
        uiOutput("StratPanel4A"),
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
                    selected = 1)
        ),
        
        helpText("Model B"),
        helpText("(Please choose only one CXR definition)"),
        checkboxGroupInput("StratB", label = "Choose Restriction Variables",
                           choices = list("HIV status" = 1, "AGE groups" = 2,
                                          "PERCH CXR Conclusions" = 3,"Nasreen CXR Readings" = 4),selected = NULL),
        checkboxGroupInput("AdjB", label = "Choose Adjustment Variables",
                           choices = list("HIV status" = 1, "AGE groups" = 2),selected = NULL),
        uiOutput("StratPanel1B"),
        uiOutput("StratPanel2B"),
        uiOutput("StratPanel3B"),
        uiOutput("StratPanel4B"),

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
                    selected = 1)
        ),       
        helpText("Music Plot Advanced Setting "),
        
        checkboxInput("Advanced_p", label = "Music Plot Advanced Setting"),
        
        
        conditionalPanel(
          condition = "input.Advanced_p == 1 ",
          helpText("Model A Advanced Setting"),
                  helpText("(Please choose only one CXR definition)"),

          checkboxGroupInput("StratA_p", label = "Choose Restriction Variables for Subsetting Music Plot",
                             choices = list("HIV status" = 1, "AGE groups" = 2,
                                          "PERCH CXR Conclusions" = 3,"Nasreen CXR Readings" = 4),selected = NULL),
          uiOutput("StratPanel1A_p"),
          uiOutput("StratPanel2A_p"),
          uiOutput("StratPanel3A_p"),
          uiOutput("StratPanel4A_p"),


          helpText("Model B Advanced Setting"),
          helpText("(Please choose only one CXR definition)"),
          checkboxGroupInput("StratB_p", label = "Choose Restriction Variables for Subsetting Music Plot",
                           choices = list("HIV status" = 1, "AGE groups" = 2,
                                          "PERCH CXR Conclusions" = 3,"Nasreen CXR Readings" = 4),selected = NULL),
        uiOutput("StratPanel1B_p"),
        uiOutput("StratPanel2B_p"),
        uiOutput("StratPanel3B_p"),
        uiOutput("StratPanel4B_p")
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