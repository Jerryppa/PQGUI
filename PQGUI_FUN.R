#rm(list=ls())
#
# install and load requisite packages:
#

## only need to run once:
# install.packages("lubridate")
# install.packages("devtools")
# install.packages("sets")
# install.packages("R2WinBUGS")
# install.packages("gplots")
# install.packages("RColorBrewer")
# install.packages("binom")
# install.packages("coda")
# install.packages("ks")
# install.packages("robCompositions")
# devtools::install_github("zhenkewu/nplcm")


## (install only if to modify the pacakge)
# essential packages for building R packages:
# install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
# devtools::install_github("hadley/devtools")
# library(devtools)
# has_devel()

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
library(ggplot2)




PQGUI_FUN <- function(current_study_site=5,CXR_choice,out_list,EP,TBP,TSP,CI,Res.dir,...){
  
  
  newALLSITES             <- c("01KEN","02GAM","03MAL","04ZAM",
                               "05SAF","06THA","07BAN")
  sitename                <- newALLSITES[current_study_site]
  
  # Current implementation requires that: if there are pathogens with both
  # BrS and SS measurements, there are at least 2; if there are pathogens
  # with SS only measurements, then there are at least 2. 
  # Otherwise, we need to write new code.
  
  # pathogen name order does not matter here, will be reordered after data cleaning.
  # So never use this vector of pathogen names for model fitting:
  pathogen_BrS_anyorder  <- c("BOPE","C_PNEU","M_PNEU",
                              "PCP","ADENO","CMV","COR",#WF
                              "FLU_C","HBOV","HMPV_A_B",
                              "FLU_A","FLU_B",
                              "PARA_1","PARA_2","PARA_3","PARA_4",
                              "PV_EV","RHINO","RSV",#without SS measure.
                              "HINF","MCAT","PNEU_VT13","PNEU_NOVT13",
                              "SASP","SAUR")#with SS measures.
  pathogen_SSonly  <- c("ENTRB","NFGNR","FUNGI","HAEMO","OTHSTR","NMEN","TB")
  

  
  # this order will not be changed. Will use this order to specify cause list.
  
  ##### Create stratification variables ####
  
  PQGUI_dataset<- read.csv("data/PQ_SAF_14MAY15.csv")
  PQGUI_dataset$PQGUI_HIV = as.numeric(PQGUI_dataset$X_HIV_3 %in% (3-out_list[[1]]))
  PQGUI_dataset$PQGUI_AGE = as.numeric(PQGUI_dataset$X_AGECAT %in% out_list[[2]])

  if (CXR_choice == 1) {
      PQGUI_dataset$PQGUI_CXR = as.numeric(PQGUI_dataset$X_CXRFIN_5 %in% out_list[[3]])
  } else if (CXR_choice == 2) {
    PQGUICXR_data <- PQGUI_dataset[,c("airspace","interstitial","cld",
"lymphadenop","pleural","airtrap","cardiomeg","WHOENDPOINT",
"OTHERINFILONLY","WHOENDPOINTONLY","BOTH","normal","MISSING"
)][,out_list[[3]]]
    if (length(out_list[[3]]) == 1 ){
      PQGUI_dataset$PQGUI_CXR = PQGUICXR_data
    } else {
      PQGUI_dataset$PQGUI_CXR = apply(PQGUICXR_data,1,max)
    }
  }



  write.csv(PQGUI_dataset,"data/PQ_SAF_14MAY15_GUI.csv")
  
  ## clean PERCH data:
  clean_options <- list (case_def           =  "PQGUI_CXR",
                         case_def_val       =  1,
                         ctrl_def           =  "CASECONT",
                         ctrl_def_val       =  2,
                         X_strat            =  c("newSITE","PQGUI_HIV","PQGUI_AGE"),
                         X_strat_val        =  list(sitename,1,1),
                         pathogen_BrS_anyorder =  pathogen_BrS_anyorder,
                         pathogen_SSonly    =  pathogen_SSonly,
                         # covariates besides case/control:
                         X_extra            = c("newSITE","ENRLDATE","patid","AGECAT","HIV"),
                         # coverates for separately ordering cases/controls:
                         X_order_obs        = c("newSITE","ENRLDATE","AGECAT","HIV"),
                         RawMeasDir         = paste0("data/PQ_SAF_14MAY15_GUI.csv"),
                         write_newSite      = TRUE,
                         newSite_write_Dir  = paste0("data/PERCH_data_with_newSITE.csv"),
                         MeasDir            = paste0("data/PERCH_data_with_newSITE.csv"),
                         PathCatDir         = paste0("data/pathogen_category.csv"),
                         allow_missing      = T)

  
  
  ## get cleaned data:
  cleaned_data <- perch_data_clean(clean_options)
  
  Mobs         <- cleaned_data$Mobs
  Y            <- cleaned_data$Y
  X            <- cleaned_data$X
  pathogen_cat <- cleaned_data$pathogen_cat
  JSS          <- cleaned_data$JSS
  pathogen_SSonly_cat <- cleaned_data$pathogen_SSonly_cat
  JSSonly             <- cleaned_data$JSSonly
  pathogen_BrS        <- cleaned_data$pathogen_MSS_ordered 
  # the pathogen name order associated with cleaned data; 
  # could be different from the order we want to display.
    
  # The list of causes, can be singleton, combinations, or NoA.
  # Please take a look at 'pathogen_BrS' before specifying this list:

    cause_list  <- c(pathogen_BrS,pathogen_SSonly,    # singleton causes in pathogen_SSonly.
                     # any combinations
                     "NoA")

  # always specify "NOA" last if it goes into model.
  
  
  # specify model fitting options (see nplcm function for details):
  model_options <- list(M_use = c("BrS","SS"),
                        site = current_study_site,
                        # has to be a subset of non-NA entries in Mobs.
                        k_subclass = 1,
                        TPR_prior  = c("50%-100%","Pathogen Specific"),
                        TPR = "NORMAL",
                        #same length as M_use.
                        Eti_prior  = "0_1",
                        pathogen_BrS_list = pathogen_BrS,
                        cause_list  = cause_list,
                        X_reg_FPR = NULL,
                        X_reg_Eti = NULL, #change the FPR_regression, and Eti_regressions to NULL values.
                        pathogen_cat = pathogen_cat,
                        SSonly       = pathogen_SSonly,
                        pathogen_SSonly_list = pathogen_SSonly,
                        pathogen_SSonly_cat  = pathogen_SSonly_cat,
                        X_reg = "QUARTER")
  
  result.folder    <- paste0("C:/PQGUI/",Res.dir)
  dir.create(result.folder,recursive = T)
  #
  write.csv(PQGUI_dataset,paste0(result.folder,"/PQ_SAF_14MAY15_GUI.csv"))
  
  # options for MCMC chains:
  mcmc_options <- list(debugstatus = !TRUE,
                       n.chains   = 1,
                       n.itermcmc = 1000,
                       n.burnin   = 500,
                       n.thin     = 5,
                       individual.pred = TRUE,
                       ppd             = TRUE,
                       result.folder = result.folder,
                       bugsmodel.dir = paste0(result.folder,"/bugs/"),
                       winbugs.dir   = "C:/Program Files/WinBUGS14/")
  
  #
  # Record the settings of current analysis:
  #
  cat("==Results stored in: ==","\n",result.folder)
  #data clean options:
  dput(clean_options,paste0(mcmc_options$result.folder,"\\data_clean_options.txt"))
  #model_options:
  dput(model_options,paste0(mcmc_options$result.folder,"\\model_options.txt"))
  #mcmc_options:
  dput(mcmc_options,paste0(mcmc_options$result.folder,"\\mcmc_options.txt"))
  #

  
  #
  # WinBUGS fitting:
  #
  
  #source(nplcm)
  gs <- nplcm(Mobs,Y,X,model_options,mcmc_options)
  #gs <- NULL

  if (!is.null(gs)) {
    cat("==Start Plotting Music Plots in ==","\n",result.folder)
    pdf(paste0(result.folder,"/","Music_Plot.pdf"),width=20,height=10)
    nplcm_plot_etiology_music_sheet(DIR_list=result.folder,
                                    DIR_pathogen_displayorder_lookup="data/pathogen_displayorder_lookup.csv")
    dev.off()
    Message_last <- paste0("Congratulations! PQ Analysis is complete, Music plot can be found in <em>C:/PQGUI/",
           Res.dir,"</em>")
  } else {
    Message_last <- "It seems like something goes wrong with model, please contact Wei Fu (jason.wfu@gmail.edu) or Zhenke Wu (zhenkewu@gmail.com) for further assistantce"
    
  }
  
  
  return(Message_last)
  #
  # Model checking:
  #
  
  # check the code for the combined visualization below in the old code:
  #
  # common patterns:
  
  #
  # pairwise log odds ratios:
  
  
  
  
  
  #
  # Posterior Analysis:
  #
  
  # ## Results visualization ------------------------------------------------
  # # plot 1: three-panel plot:
  #three_panel_height <- (nrow(pathogen_cat)+length(pathogen_SSonly))/2
  #pdf(paste0(result.folder,"\\",sitename,"_three_panel_plot.pdf"),
  #          width=12,height= three_panel_height)
  #nplcm_plot_three_panel(DIR_NPLCM = result.folder,ss_upperlimit = 1,eti_upperlimit = .5)
  #dev.off()
  #
  # # plot 2: individual diagnosis plot:
  # pdf(paste0(result.folder,"\\",sitename,"_individual_diagnosis.pdf"),width=16,height=16)
  # par(mfrow=c(4,4))
  # nplcm_plot_individual_diagnosis(DIR_NPLCM=result.folder,npat=16)
  # dev.off()
  #
  # # plot 3: pathogen group triangle plots:
  # result.folder  = "C:/2014_10_30_02GAM_PNEU"
  # pdf(paste0(result.folder,"\\",sitename,"_group_triangle_plot.pdf"),width=10,height=10)
  # nplcm_plot_group_etiology(DIR_NPLCM=result.folder)
  # dev.off()
  #
  # ## END of Results visualization -----------------------------------------------
  #
  #
  # # save workspace for future replot:
  # save.image(paste0(result.folder,"\\for_replot.RDATA"))
  #
  #
}