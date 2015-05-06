
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library("knitr")

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
    
    
    #source("SIM_DATA.R")
    #source("SIM_FUN.R")
    source("PQGUI_FUN.R")
    source("TPR_prior_set.R")
    source("nplcm_fit_NoReg_BrSandSS_NoNest_SSonly.R")
    source("nplcm.R")
    source("nplcm_plot_etiology_music_sheet.R")
    output$instruction = downloadHandler(
      filename = 'Instruction.pdf',
      
      content = function(file) file.copy('Prep/Instruction.pdf', file, overwrite = TRUE),
      
      contentType = 'application/pdf'
    )
    
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
    
    isEmpty <- function(x) {
      return(length(x)==0)
    }
    
    finalInput <- eventReactive(input$Check,{
      ##### return PQ specification ######
      Strata_list <- c("HIV status", "AGE groups", "CXR Conclusions")
      Adj_list <- c("HIV status", "AGE groups")
      HIV_list <- c("HIV Positive", "HIV Negative but Exposed",
                    "HIV Negative and Unexposed")
      AGE_list <- c("1-5m" , "6-11m" ,
                    "12-23m" , "24-59m")
      CXR_list <- c("Primary End Point" , "Other infiltrate" ,
                    "Primary End Point or Other infiltrate" , "Normal", "Uninterpretable" )
      EP_list <- c("Weak Uniform Prior", "Weak informed Prior")
      TBP_list <- c("50%-100%", "0%-100%")
      TSP_list <-c("Pathogen specific sensitivity", "10%-20% for all silver standard")
      CI_list <- c("Conditional Independent", "Conditional Dependent")
      
      #### input ####
      Strata <- as.numeric(unlist(input$Strat))
      HIV <- as.numeric(unlist(input$HIV))
      AGE <- as.numeric(unlist(input$AGE))
      CXR <- as.numeric(unlist(input$CXR))
      
      Adj <- as.numeric(unlist(input$Adj))
      
      Advanced <- input$Advanced
      EP <- EP_list[as.numeric(input$EP)]
      TBP <- TBP_list[as.numeric(input$TBP)]
      TSP <- TSP_list[as.numeric(input$TSP)]
      CI <- CI_list[as.numeric(input$CI)]
      
      ##### strata values ####
      Strata_val_list <- list(HIV_list = HIV_list[HIV], AGE_list=AGE_list[AGE],CXR_list=CXR_list[CXR])
      Strata_val_list_col <- sapply(Strata_val_list,function(x) paste("(",paste0(x,collapse = ", "),")"))

      #### Create Message ####
      if (input$module == 'Single Model') {
        #Stratification
        
        if (isEmpty(Strata)) {
          Message1 <- "Model will use the <strong>entire study sample</strong> in South Africa"
        } else {
          #Message <- class(input$Advanced)
          Message1 <- paste0("Model will use study sample restricted by ",
                             paste0("<strong>", Strata_list[Strata], "</strong> ",
                                    Strata_val_list_col[Strata], collapse = ", "))
        }
        # Adjustment 
        if (isEmpty(Adj)) {
          Message2 <- paste0("Output will <strong>NOT BE</strong> adjusted by HIV or CXR")
        } else {
          Message2 <- paste0("Output will be adjusted by ",
                             paste0("<strong>",Adj_list[Adj],"</strong>",collapse = ", "))
        }

        #Model Specification

        if (!Advanced) {
          Message3 <- paste0("Model Setting: etiology prior is <strong>","Weak Uniform Prior",
                             "</strong>, sensitivity prior for bronze standard measurement is <strong>", "50%-100%", 
                             "</strong>, specificity prior for silver standard measurement is <strong>", "Pathogen specific sensitivity",
                             "</strong>, assumed correlation in measurement is <strong>", "Conditional Independent</strong>")
        } else {
          Message3 <- paste0("Model Setting: etiology prior is <strong>",EP,
                             "</strong>, sensitivity prior for bronze standard measurement is <strong>", TBP, 
                             "</strong>, specificity prior for silver standard measurement is <strong>", TSP,
                             "</strong>, assumed correlation in measurement is <strong>", CI,"</strong>")
        }
        
      }
      return(c("<em>You are using <strong>Single Model</strong> Module</em>",Message1,Message2,Message3))
      
      
      
      ####Add other options###
    })

    
    finaloutput <- eventReactive(input$Go,{
      ##### return PQ specification ######
      Strata_list <- c("HIV status", "AGE groups", "CXR Conclusions")
      Adj_list <- c("HIV status", "AGE groups")
      HIV_list <- c("HIV Positive", "HIV Negative but Exposed",
                    "HIV Negative and Unexposed")
      AGE_list <- c("1-5m" , "6-11m" ,
                    "12-23m" , "24-59m")
      CXR_list <- c("Primary End Point" , "Other infiltrate" ,
                    "Primary End Point or Other infiltrate" , "Normal", "Uninterpretable" )
      EP_list <- c("Weak Uniform Prior", "Weak informed Prior")
      TBP_list <- c("50%-100%", "0%-100%")
      TSP_list <-c("Pathogen specific sensitivity", "10%-20% for all silver standard")
      CI_list <- c("Conditional Independent", "Conditional Dependent")
      
      #### input ####
      Strata <- as.numeric(unlist(input$Strat))
      HIV <- as.numeric(unlist(input$HIV))
      AGE <- as.numeric(unlist(input$AGE))
      CXR <- as.numeric(unlist(input$CXR))
      
      Adj <- as.numeric(unlist(input$Adj))
      
      Advanced <- input$Advanced
      EP.n <- as.numeric(input$EP)
      TBP.n <- as.numeric(input$TBP)
      TSP.n <- as.numeric(input$TSP)
      CI.n <- as.numeric(input$CI)
      EP <- EP_list[EP.n]
      TBP <- TBP_list[TBP.n]
      TSP <- TSP_list[TSP.n]
      CI <- CI_list[CI.n]
      
      ##### strata values ####
      Strata_val_list <- list(HIV_list = HIV_list[HIV], AGE_list=AGE_list[AGE],CXR_list=CXR_list[CXR])
      Strata_val_list_col <- sapply(Strata_val_list,function(x) paste("(",paste0(x,collapse = ", "),")"))
      
      
      # Result folder names
      raw_list <- list(HIV,AGE,CXR)
      reference_list <- list(1:length(HIV_list),1:length(AGE_list),1:length(CXR_list))
      out_list <- lapply(1:3,function(i){
        if (length(raw_list[[i]])==0) {
          return(reference_list[[i]])
        } else {
          return(raw_list[[i]])
        }
      })
      Strata.name <- paste0(c("H","A","C"),
                            c(paste0(out_list[[1]],collapse = ""),
                              paste0(out_list[[2]],collapse = ""),
                              paste0(out_list[[3]],collapse = "")),collapse = "")
      Adj.name <- paste0(c("H","A","C")[Adj],collapse = "")
      if (!Advanced) {
        Model.name <- "E1B1S1C1"
      } else {
        Model.name <- paste0(c("E","B","S","C"),
                             c(EP.n,TBP.n,TSP.n,CI.n),collapse = "")
        
      }
      Res.dir <- paste0("PQGUI_","Str_",Strata.name,"_Adj_",Adj.name,"_Mod_",Model.name)
      
      #### Search for results folder ####
      base_dir <- list.dirs(path = "C:/",full.names = F,recursive = F)
      if (!any(base_dir == "PQGUI")){
        Message_op <- "Results folder for requested model cannot be found, New PQ Analysis will be started..." 
      } else {
        res_dir <- list.dirs(path = "C:/PQGUI",full.names = F,recursive = F)
        if (!any(res_dir == Res.dir)) {
          Message_op <- "Results folder for requested model cannot be found, New PQ Analysis will be started..." 
        } else {
          res_files <- list.files(path = Res.dir,full.names = F,recursive = F)
          if (!any(res_files == "Music_Plot.pdf")){
            cat("==Start Plotting Music Plots in ==","\n",Res.dir)
            pdf(paste0("C:/PQGUI/",Res.dir,"/","Music_Plot.pdf"),width=20,height=10)
            #nplcm_plot_etiology_music_sheet(DIR_list=Res.dir,
            #                                DIR_pathogen_displayorder_lookup="data/pathogen_displayorder_lookup.csv")
            dev.off()
            Message_op <- paste0("Result folder for requested model exists, Music plot can be found in <em>C:/PQGUI/",
                                 Res.dir,"</em>")
          } else {
            Message_op <- paste0("Result folder for requested model exists, Music plot can be found in <em>C:/PQGUI/",
                                 Res.dir,"</em>")          
          }

          
        }
      }
      
      #### Run the model ####

      if (Message_op == "Results folder for requested model cannot be found, New PQ Analysis will be started...") {
        message_last<- PQGUI_FUN(current_study_site=5,out_list,EP,TBP,TSP,CI,Res.dir)
          
      } else {
        message_last <- Message_op
      }
      return(message_last)
      
    })
    
    output$text1 <- renderUI({ 
      
      HTML(paste(finalInput(),collapse = "<br/> <br/>"))
      #HTML(paste(finaloutput(),collapse = "<br/> <br/>"))
      
      
    })    

    output$text2 <- renderUI({ 

      #HTML(paste(finalInput(),collapse = "<br/> <br/>"))
      #count <- input$Submit
      HTML(paste("<br/>",paste(finaloutput(),collapse = "<br/> <br/>")))
      

    })
    

    
  }
)
