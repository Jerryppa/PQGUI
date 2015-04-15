
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(
  function(input, output) {
    library(lubridate)
    library(sets)
    library(R2WinBUGS)
    library(gplots)
    library(RColorBrewer)
    library(binom)
    library(coda)
    library(nplcm)
    
    # for plot group etiologies posteriors:
    library(ks)
    library(robCompositions)
    
    
    source("SIM_DATA.R")
    source("SIM_FUN.R")
    
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
      return(message)
      ####Add other options###
    
        
    })
      
    #simdata<- SIM_DATA(set_parameter_S0C0D0)

    output$text1 <- renderText({ 

      finalInput()
      #count <- input$Submit


    })

    
  }
)
