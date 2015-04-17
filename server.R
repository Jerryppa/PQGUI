
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(
  function(input, output) {
    #library(lubridate)
    #library(sets)
    #library(R2WinBUGS)
    #library(gplots)
    #library(RColorBrewer)
    #library(binom)
    #library(coda)
    #library(nplcm)
    
    # for plot group etiologies posteriors:
    #library(ks)
    #library(robCompositions)
    
    
    source("SIM_DATA.R")
    source("SIM_FUN.R")
    
    output$StratPanel1 <- renderUI({
      if (1 %in% input$Strat) {
        checkboxGroupInput("HIV", label = "Choose HIV status",
                           choices = list("HIV Positive" = 1, "HIV Negative but Exposed" = 2,
                                          "HIV Negative and Unexposed" = 3),selected = c(1,2,3))
      } 
      })
    output$StratPanel2 <- renderUI({
      if (2 %in% input$Strat) {
        checkboxGroupInput("AGE", label = "Choose AGE groups",
                           choices = list("1-5m" = 1, "6-11m" = 2,
                                          "12-23m" = 3, "24-59m" =4 ),selected = c(1,2,3,4))
        
      }
    })
    output$StratPanel3 <- renderUI({
      if (3 %in% input$Strat) {
        checkboxGroupInput("CXR", label = "Choose CXR Conclusions",
                           choices = list("Primary End Point" = 1, "Other infiltrate" = 2,
                                          "Primary End Point or Other infiltrate" = 3, "Normal" =4, "Uninterpretable"=5 ),
                           selected = c(1,2,3,4,5))
        
      }
    })
    output$StratPanel1A <- renderUI({
      if (1 %in% input$StratA) {
        checkboxGroupInput("HIVA", label = "Choose HIV status",
                           choices = list("HIV Positive" = 1, "HIV Negative but Exposed" = 2,
                                          "HIV Negative and Unexposed" = 3),selected = c(1,2,3))
      } 
    })
    output$StratPanel2A <- renderUI({
      if (2 %in% input$StratA) {
        checkboxGroupInput("AGEA", label = "Choose AGE groups",
                           choices = list("1-5m" = 1, "6-11m" = 2,
                                          "12-23m" = 3, "24-59m" =4 ),selected = c(1,2,3,4))
        
      }
    })
    output$StratPanel3A <- renderUI({
      if (3 %in% input$StratA) {
        checkboxGroupInput("CXRA", label = "Choose CXR Conclusions",
                           choices = list("Primary End Point" = 1, "Other infiltrate" = 2,
                                          "Primary End Point or Other infiltrate" = 3, "Normal" =4, "Uninterpretable"=5 ),
                           selected = c(1,2,3,4,5))
        
      }
    })    
    output$StratPanel1B <- renderUI({
      if (1 %in% input$StratB) {
        checkboxGroupInput("HIVB", label = "Choose HIV status",
                           choices = list("HIV Positive" = 1, "HIV Negative but Exposed" = 2,
                                          "HIV Negative and Unexposed" = 3),selected = c(1,2,3))
      } 
    })
    output$StratPanel2B <- renderUI({
      if (2 %in% input$StratB) {
        checkboxGroupInput("AGEB", label = "Choose AGE groups",
                           choices = list("1-5m" = 1, "6-11m" = 2,
                                          "12-23m" = 3, "24-59m" =4 ),selected = c(1,2,3,4))
        
      }
    })
    output$StratPanel3B <- renderUI({
      if (3 %in% input$StratB) {
        checkboxGroupInput("CXRB", label = "Choose CXR Conclusions",
                           choices = list("Primary End Point" = 1, "Other infiltrate" = 2,
                                          "Primary End Point or Other infiltrate" = 3, "Normal" =4, "Uninterpretable"=5 ),
                           selected = c(1,2,3,4,5))
        
      }
    })
    output$StratPanel1A_p <- renderUI({
      if (1 %in% input$StratA_p) {
        checkboxGroupInput("HIVA_p", label = "Choose HIV status",
                           choices = list("HIV Positive" = 1, "HIV Negative but Exposed" = 2,
                                          "HIV Negative and Unexposed" = 3),selected = c(1,2,3))
      } 
    })
    output$StratPanel2A_p <- renderUI({
      if (2 %in% input$StratA_p) {
        checkboxGroupInput("AGEA_p", label = "Choose AGE groups",
                           choices = list("1-5m" = 1, "6-11m" = 2,
                                          "12-23m" = 3, "24-59m" =4 ),selected = c(1,2,3,4))
        
      }
    })
    output$StratPanel3A_p <- renderUI({
      if (3 %in% input$StratA_p) {
        checkboxGroupInput("CXRA_p", label = "Choose CXR Conclusions",
                           choices = list("Primary End Point" = 1, "Other infiltrate" = 2,
                                          "Primary End Point or Other infiltrate" = 3, "Normal" =4, "Uninterpretable"=5 ),
                           selected = c(1,2,3,4,5))
        
      }
    })
    output$StratPanel1B_p <- renderUI({
      if (1 %in% input$StratB_p) {
        checkboxGroupInput("HIVB_p", label = "Choose HIV status",
                           choices = list("HIV Positive" = 1, "HIV Negative but Exposed" = 2,
                                          "HIV Negative and Unexposed" = 3),selected = c(1,2,3))
      } 
    })
    output$StratPanel2B_p <- renderUI({
      if (2 %in% input$StratB_p) {
        checkboxGroupInput("AGEB_p", label = "Choose AGE groups",
                           choices = list("1-5m" = 1, "6-11m" = 2,
                                          "12-23m" = 3, "24-59m" =4 ),selected = c(1,2,3,4))
        
      }
    })
    output$StratPanel3B_p <- renderUI({
      if (3 %in% input$StratB_p) {
        checkboxGroupInput("CXRB_p", label = "Choose CXR Conclusions",
                           choices = list("Primary End Point" = 1, "Other infiltrate" = 2,
                                          "Primary End Point or Other infiltrate" = 3, "Normal" =4, "Uninterpretable"=5 ),
                           selected = c(1,2,3,4,5))
        
      }
    })
    
    output$slider2 <- renderUI(sliderInput("slider2", "Choose Etiology Prior for Bacteria: ", min = 0,  max = 100 - input$slider1, value = 0))
    dataInput <- reactive({
      set_parameter <- list(pathogen_BrS = c("A","B","C","D","E","F","G","H","I","J"),
                                 cause_list=c("A","B","C","D","E","F","G","H","I","J"),
                                 PsiBS=c(0.31,0.12,0.12,0.75,0.04,0.04,0.75,0.01,0.01,0.75),
                                 ThetaBS=c(0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75),
                                 PsiSS=c(NA,0,NA,0,0,NA,0,0,NA,0),
                                 ThetaSS=c(NA,0.15,NA,0.15,0.15,NA,0.15,0.15,NA,0.15),
                                 Nd=250,
                                 Nu=250,
                                 Lambda=NULL,
                                 Eta=NULL,
                                 etiology=c(0.37,0.15,0.15,0.15,0.05,0.05,0.05,0.01,0.01,0.01))
      set.seed(20150331)
      simdata <- SIM_DATA(set_parameter)
      return(simdata)
    })
    
    finalInput <- eventReactive(input$Go,{
      message <- "not run"
      if (input$EP=="Weak Uniform Prior" & input$TBP =="50%-100%" & input$TSP=="10%-20%") {
        #SIM_FUN(c("TB0","TS0"),"E0",dataInput())
        message <- paste0("run",input$EP)
      }
      return(input$Strat)
      ####Add other options###
    
        
    })
      
    #simdata<- SIM_DATA(set_parameter_S0C0D0)

    output$text1 <- renderText({ 

      finalInput()
      #count <- input$Submit


    })

    
  }
)
