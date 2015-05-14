
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library("knitr")
library(ggplot2)

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
    source("IcatToIEti.R")
    output$instruction = downloadHandler(
      filename = 'Instruction.pdf',
      
      content = function(file) file.copy('Prep/Instruction.pdf', file, overwrite = TRUE),
      
      contentType = 'application/pdf'
    )
    ### Single Model Group ####
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
        checkboxGroupInput("CXR", label = "Choose PERCH CXR Conclusions",
                           choices = list("Primary End Point" = 1, "Other infiltrate" = 2,
                                          "Primary End Point or Other infiltrate" = 3, "Normal" =4, "Uninterpretable"=5 ),
                           selected = c(1,2,3,4,5))
        
      }
    })
    output$StratPanel4 <- renderUI({
      if (4 %in% input$Strat) {
        checkboxGroupInput("CXR2", label = "Choose Nasreen's CXR readings",
                           choices = list("airspace"=1,
                                          "interstitial"=2,
                                          "cld"=3,
                                          "lymphadenop"=4,
                                          "pleural"=5,
                                          "airtrap"=6,
                                          "cardiomeg"=7,
                                          "WHO ENDPOINT"=8,
                                          "WHO ENDPOINT Only"=9,
                                          "OTHER INFIL ONLY"=10,
                                          "ENDPOINT AND OTHER INFIL"=11,
                                          "NORMAL"=12,
                                          "MISSING"=13),
                           selected = c(1,2,3,4,5,6,7,8,9,10,11,12,13))
        
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
        checkboxGroupInput("CXRA", label = "Choose PERCH CXR Conclusions",
                           choices = list("Primary End Point" = 1, "Other infiltrate" = 2,
                                          "Primary End Point or Other infiltrate" = 3, "Normal" =4, "Uninterpretable"=5 ),
                           selected = c(1,2,3,4,5))
        
      }
    }) 
    output$StratPanel4A <- renderUI({
      if (4 %in% input$StratA) {
        checkboxGroupInput("CXR2A", label = "Choose Nasreen's CXR readings",
                           choices = list("airspace"=1,
                                          "interstitial"=2,
                                          "cld"=3,
                                          "lymphadenop"=4,
                                          "pleural"=5,
                                          "airtrap"=6,
                                          "cardiomeg"=7,
                                          "WHO ENDPOINT"=8,
                                          "WHO ENDPOINT Only"=9,
                                          "OTHER INFIL ONLY"=10,
                                          "ENDPOINT AND OTHER INFIL"=11,
                                          "NORMAL"=12,
                                          "MISSING"=13),
                           selected = c(1,2,3,4,5,6,7,8,9,10,11,12,13))
        
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
        checkboxGroupInput("CXRB", label = "Choose PERCH CXR Conclusions",
                           choices = list("Primary End Point" = 1, "Other infiltrate" = 2,
                                          "Primary End Point or Other infiltrate" = 3, "Normal" =4, "Uninterpretable"=5 ),
                           selected = c(1,2,3,4,5))
        
      }
    })
        output$StratPanel4B <- renderUI({
      if (4 %in% input$StratB) {
        checkboxGroupInput("CXR2B", label = "Choose Nasreen's CXR readings",
                           choices = list("airspace"=1,
                                          "interstitial"=2,
                                          "cld"=3,
                                          "lymphadenop"=4,
                                          "pleural"=5,
                                          "airtrap"=6,
                                          "cardiomeg"=7,
                                          "WHO ENDPOINT"=8,
                                          "WHO ENDPOINT Only"=9,
                                          "OTHER INFIL ONLY"=10,
                                          "ENDPOINT AND OTHER INFIL"=11,
                                          "NORMAL"=12,
                                          "MISSING"=13),
                           selected = c(1,2,3,4,5,6,7,8,9,10,11,12,13))
        
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
        checkboxGroupInput("CXRA_p", label = "Choose PERCH CXR Conclusions",
                           choices = list("Primary End Point" = 1, "Other infiltrate" = 2,
                                          "Primary End Point or Other infiltrate" = 3, "Normal" =4, "Uninterpretable"=5 ),
                           selected = c(1,2,3,4,5))
        
      }
    })
        output$StratPanel4A_p <- renderUI({
      if (4 %in% input$StratA_p) {
        checkboxGroupInput("CXR2A_p", label = "Choose Nasreen's CXR readings",
                           choices = list("airspace"=1,
                                          "interstitial"=2,
                                          "cld"=3,
                                          "lymphadenop"=4,
                                          "pleural"=5,
                                          "airtrap"=6,
                                          "cardiomeg"=7,
                                          "WHO ENDPOINT"=8,
                                          "WHO ENDPOINT Only"=9,
                                          "OTHER INFIL ONLY"=10,
                                          "ENDPOINT AND OTHER INFIL"=11,
                                          "NORMAL"=12,
                                          "MISSING"=13),
                           selected = c(1,2,3,4,5,6,7,8,9,10,11,12,13))
        
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
        checkboxGroupInput("CXRB_p", label = "Choose PERCH CXR Conclusions",
                           choices = list("Primary End Point" = 1, "Other infiltrate" = 2,
                                          "Primary End Point or Other infiltrate" = 3, "Normal" =4, "Uninterpretable"=5 ),
                           selected = c(1,2,3,4,5))
        
      }
    })
        output$StratPanel4B_p <- renderUI({
      if (4 %in% input$StratB_p) {
        checkboxGroupInput("CXR2B_p", label = "Choose Nasreen's CXR readings",
                           choices = list("airspace"=1,
                                          "interstitial"=2,
                                          "cld"=3,
                                          "lymphadenop"=4,
                                          "pleural"=5,
                                          "airtrap"=6,
                                          "cardiomeg"=7,
                                          "WHO ENDPOINT"=8,
                                          "WHO ENDPOINT Only"=9,
                                          "OTHER INFIL ONLY"=10,
                                          "ENDPOINT AND OTHER INFIL"=11,
                                          "NORMAL"=12,
                                          "MISSING"=13),
                           selected = c(1,2,3,4,5,6,7,8,9,10,11,12,13))
        
      }
    })




    output$slider2 <- renderUI(sliderInput("slider2", "Choose Etiology Prior for Bacteria: ", min = 0,  max = 100 - input$slider1, value = 0))
    
    isEmpty <- function(x) {
      return(length(x)==0)
    }

    MessageCheck <- function(Strata,Strata_val_list_col,EP,TBP,TSP,CI,Adj,Advanced){
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
          Message2 <- paste0("Output will <strong>NOT BE</strong> adjusted for HIV or CXR")
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
        return(list(Message1,Message2,Message3))
    }
      Strata_list <- c("HIV status", "AGE groups", "PERCH CXR Conclusions","Nasreen's CXR readings")
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
      CXR2_list <-c("airspace",
                  "interstitial",
                  "cld",
                  "lymphadenop",
                  "pleural",
                  "airtrap",
                  "cardiomeg",
                  "WHO ENDPOINT",
                  "WHO ENDPOINT Only",
                  "OTHER INFIL ONLY",
                  "ENDPOINT AND OTHER INFIL",
                  "NORMAL",
                  "MISSING")
    finalInput <- eventReactive(input$Check,{
      ##### return PQ specification ######

      



      

      #### Create Message ####
      if (input$module == 'Single Model') {
              #### input ####
      Strata <- as.numeric(unlist(input$Strat))
      HIV <- as.numeric(unlist(input$HIV))
      AGE <- as.numeric(unlist(input$AGE))
      CXR <- as.numeric(unlist(input$CXR))
      CXR2 <- as.numeric(unlist(input$CXR2))
      
      Adj <- as.numeric(unlist(input$Adj))
      
      Advanced <- input$Advanced
      EP <- EP_list[as.numeric(input$EP)]
      TBP <- TBP_list[as.numeric(input$TBP)]
      TSP <- TSP_list[as.numeric(input$TSP)]
      CI <- CI_list[as.numeric(input$CI)]
            ##### strata values ####
      Strata_val_list <- list(HIV_list = HIV_list[HIV], 
        AGE_list=AGE_list[AGE],
        CXR_list=CXR_list[CXR],
        CXR2_list=CXR2_list[CXR2])
      Strata_val_list_col <- sapply(Strata_val_list,function(x) paste("(",paste0(x,collapse = ", "),")"))

        #Stratification
        Message_list <- MessageCheck(Strata,Strata_val_list_col,EP,TBP,TSP,CI,Adj,Advanced)
      return(c("<em>You are using <strong>Single Model</strong> Module</em>",
        unlist(Message_list)))
        
      } else if (input$module == 'Multiple Model') {
              #### input ####
      StrataA <- as.numeric(unlist(input$StratA))
      HIVA <- as.numeric(unlist(input$HIVA))
      AGEA <- as.numeric(unlist(input$AGEA))
      CXRA <- as.numeric(unlist(input$CXRA))
      CXR2A <- as.numeric(unlist(input$CXR2A))
      AdjA <- as.numeric(unlist(input$AdjA))
      AdvancedA <- input$AdvancedA
      EPA <- EP_list[as.numeric(input$EPA)]
      TBPA <- TBP_list[as.numeric(input$TBPA)]
      TSPA <- TSP_list[as.numeric(input$TSPA)]
      CIA <- CI_list[as.numeric(input$CIA)]
      
                  ##### strata values ####
      Strata_val_listA <- list(HIV_list = HIV_list[HIVA], 
        AGE_list=AGE_list[AGEA],
        CXR_list=CXR_list[CXRA],
        CXR2_list=CXR2_list[CXR2A])
      Strata_val_list_colA <- sapply(Strata_val_listA,function(x) 
        paste("(",paste0(x,collapse = ", "),")"))

      Message_listA <- MessageCheck(StrataA,Strata_val_list_colA,EPA,TBPA,TSPA,CIA,AdjA,AdvancedA)

      StrataB <- as.numeric(unlist(input$StratB))
      HIVB <- as.numeric(unlist(input$HIVB))
      AGEB <- as.numeric(unlist(input$AGEB))
      CXRB <- as.numeric(unlist(input$CXRB))
      CXR2B <- as.numeric(unlist(input$CXR2B))
      AdjB <- as.numeric(unlist(input$AdjB))
      AdvancedB <- input$AdvancedB
      EPB <- EP_list[as.numeric(input$EPB)]
      TBPB <- TBP_list[as.numeric(input$TBPB)]
      TSPB <- TSP_list[as.numeric(input$TSPB)]
      CIB <- CI_list[as.numeric(input$CIB)]

      Strata_val_listB <- list(HIV_list = HIV_list[HIVB], 
        AGE_list=AGE_list[AGEB],
        CXR_list=CXR_list[CXRB],
        CXR2_list=CXR2_list[CXR2B])
      Strata_val_list_colB <- sapply(Strata_val_listB,function(x) 
        paste("(",paste0(x,collapse = ", "),")"))

      Message_listB <- MessageCheck(StrataB,Strata_val_list_colB,EPB,TBPB,TSPB,CIB,AdjB,AdvancedB)

      return(c("<em>You are using <strong>Multiple Model</strong> Module</em>", "<strong> Model A</strong>",
        unlist(Message_listA),"<strong> Model B</strong>",unlist(Message_listB)))
      }

      
      
      
      ####Add other options###
    })



    Function_GO <- function(Strata,HIV,AGE,CXR,CXR2,EP,TBP,TSP,CI,Adj,Advanced){      ##### strata values ####
      Strata_val_list <- list(HIV_list = HIV_list[HIV], 
        AGE_list=AGE_list[AGE],
        CXR_list=CXR_list[CXR],
        CXR2_list=CXR2_list[CXR2])
      Strata_val_list_col <- sapply(Strata_val_list,function(x) paste("(",paste0(x,collapse = ", "),")"))

      
      # Result folder names 
      # To clean up messy naming due to two different CXR definitions
      if (3 %in% Strata & 4 %in% Strata){
        message <- "Please choose only one CXR definition"
        return(message)
        ##Need build validation check
      } else if (4 %in% Strata & length(CXR2)!=9) {
        CXR_choice <- 2
        raw_list <- list(HIV,AGE,CXR2)
        reference_list <- list(1:length(HIV_list),1:length(AGE_list),1:length(CXR2_list))
        out_list <- lapply(1:3,function(i){
          if (length(raw_list[[i]])==0) {
            return(reference_list[[i]])
          } else {
            return(raw_list[[i]])
          }
        })
        Strata.name <- paste0(c("H","A","CT"),
                              c(paste0(out_list[[1]],collapse = ""),
                                paste0(out_list[[2]],collapse = ""),
                                paste0(out_list[[3]],collapse = "")),collapse = "")
        #return(out_list)

      } else {
        CXR_choice <- 1
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
      }

      Adj.name <- paste0(c("H","A")[Adj],collapse = "")
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
        message_last<- PQGUI_FUN(current_study_site=5,CXR_choice,out_list,EP,TBP,TSP,CI,Res.dir)
                cat("==Start Plotting Music Plots in ==","\n",Res.dir)
        #pdf(paste0("C:/PQGUI/",Res.dir,"/","Music_Plot.pdf"),width=20,height=10)
        nplcm_plot_etiology_music_sheet(DIR_list=paste0("C:/PQGUI/",Res.dir),
                                        DIR_pathogen_displayorder_lookup="data/pathogen_displayorder_lookup.csv")
  
      } else {
        res_dir <- list.dirs(path = "C:/PQGUI",full.names = F,recursive = F)
        if (!any(res_dir == Res.dir)) {
        message_last<- PQGUI_FUN(current_study_site=5,CXR_choice,out_list,EP,TBP,TSP,CI,Res.dir)
                  cat("==Start Plotting Music Plots in ==","\n",Res.dir)
        #pdf(paste0("C:/PQGUI/",Res.dir,"/","Music_Plot.pdf"),width=20,height=10)
        nplcm_plot_etiology_music_sheet(DIR_list=paste0("C:/PQGUI/",Res.dir),
                                        DIR_pathogen_displayorder_lookup="data/pathogen_displayorder_lookup.csv")

        } else {
          res_files <- list.files(path = paste0("C:/PQGUI/",Res.dir),full.names = F,recursive = F)
          if (!any(res_files == "Music_Plot.pdf")){
            cat("==Start Plotting Music Plots in ==","\n",Res.dir)
            #pdf(paste0("C:/PQGUI/",Res.dir,"/","Music_Plot.pdf"),width=20,height=10)
            nplcm_plot_etiology_music_sheet(DIR_list=paste0("C:/PQGUI/",Res.dir),
                                            DIR_pathogen_displayorder_lookup="data/pathogen_displayorder_lookup.csv")
            #plot(1:10,1:10)

            #dev.off()
            message_last <- paste0("Result folder for requested model exists, Music plot can be found in <em>C:/PQGUI/",
                                 Res.dir,"</em>")
          } else {
            message_last <- paste0("Result folder for requested model exists, Music plot can be found in <em>C:/PQGUI/",
                                 Res.dir,"</em>")          
          }

          
        }
      } 
      return(list(message_last,Res.dir))
    }

    
    finaloutput <- eventReactive(input$Go,{
      if (input$module == 'Single Model') {
               #### input ####
      Strata <- as.numeric(unlist(input$Strat))
      HIV <- as.numeric(unlist(input$HIV))
      AGE <- as.numeric(unlist(input$AGE))
      CXR <- as.numeric(unlist(input$CXR))
      CXR2 <- as.numeric(unlist(input$CXR2))
      
      Adj <- as.numeric(unlist(input$Adj))
      
      Advanced <- input$Advanced
      EP <- EP_list[as.numeric(input$EP)]
      TBP <- TBP_list[as.numeric(input$TBP)]
      TSP <- TSP_list[as.numeric(input$TSP)]
      CI <- CI_list[as.numeric(input$CI)]

      finaloutput<- Function_GO(Strata,HIV,AGE,CXR,CXR2,EP,TBP,TSP,CI,Adj,Advanced)
      message_last <- finaloutput[[1]]
      Res.dir <- finaloutput[[2]]


      } else if (input$module == 'Multiple Model') {
      Strata <- as.numeric(unlist(input$StratA))
      HIV <- as.numeric(unlist(input$HIVA))
      AGE <- as.numeric(unlist(input$AGEA))
      CXR <- as.numeric(unlist(input$CXRA))
      CXR2 <- as.numeric(unlist(input$CXR2A))
      Adj <- as.numeric(unlist(input$AdjA))
      EP <- EP_list[as.numeric(input$EPA)]
      TBP <- TBP_list[as.numeric(input$TBPA)]
      TSP <- TSP_list[as.numeric(input$TSPA)]
      CI <- CI_list[as.numeric(input$CIA)]
      Advanced <- input$AdvancedA
      finaloutputA<- Function_GO(Strata,HIV,AGE,CXR,CXR2,EP,TBP,TSP,CI,Adj,Advanced)
      message_lastA <- finaloutputA[[1]]
      Res.dirA <- finaloutputA[[2]]

      Strata <- as.numeric(unlist(input$StratB))
      HIV <- as.numeric(unlist(input$HIVB))
      AGE <- as.numeric(unlist(input$AGEB))
      CXR <- as.numeric(unlist(input$CXRB))
      CXR2 <- as.numeric(unlist(input$CXR2B))
      Adj <- as.numeric(unlist(input$AdjB))
      EP <- EP_list[as.numeric(input$EPB)]
      TBP <- TBP_list[as.numeric(input$TBPB)]
      TSP <- TSP_list[as.numeric(input$TSPB)]
      CI <- CI_list[as.numeric(input$CIB)]
      Advanced <- input$AdvancedB
      finaloutputB<- Function_GO(Strata,HIV,AGE,CXR,CXR2,EP,TBP,TSP,CI,Adj,Advanced)
      message_lastB <- finaloutputB[[1]]
      Res.dirB <- finaloutputB[[2]]
      DIR_list <- list(paste0("C:/PQGUI/",Res.dirA), paste0("C:/PQGUI/",Res.dirB))

      nplcm_plot_etiology_music_sheet(DIR_list=DIR_list,
                DIR_pathogen_displayorder_lookup="data/pathogen_displayorder_lookup.csv",
                T)

      message_last <- c("Model A",message_lastA,"Model B",message_lastB)

      }


      #### Run the model ####


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
