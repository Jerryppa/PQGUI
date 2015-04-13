SIM_FUN <-function (TPR_prior,Eti_prior,simdata){
  Mobs         <- simdata$Mobs
  #Mobs[["MBS"]]["D_NPPCR"]=rep(NA,length(Mobs[["MBS"]]["D_NPPCR"]))
  #Mobs[["MSS"]]["C_BCX"]=rep(NA,length(Mobs[["MSS"]]["C_BCX"]))
  Y            <- simdata$Y
  X            <- NULL
  pathogen_cat <- NULL
  JSS          <- NULL
  pathogen_SSonly_cat <- NULL
  JSSonly             <- NULL
  pathogen_BrS        <- c("A","B","C","D","E","F","G") 
  #SS+BrS
  pathogen_B_S <- c("B","D","E","G")
  pathogen_BrS_ord <- c(pathogen_B_S,pathogen_BrS[!(pathogen_BrS %in% pathogen_B_S)])
  ord_BrS <- unlist(lapply(pathogen_BrS_ord, function(x) which(pathogen_BrS==x)))
  ord_SS <- unlist(lapply(pathogen_B_S, function(x) which(pathogen_BrS==x)))
  
  pathogen_SSonly <- NULL
  
  #order Mobs
  Mobs <- list(MBS=Mobs[[1]][,ord_BrS],MSS=Mobs[[2]][,ord_BrS])
  # the pathogen name order associated with cleaned data; 
  # could be different from the order we want to display.
  
  # The list of causes, can be singleton, combinations, or NoA.
  # Please take a look at 'pathogen_BrS' before specifying this list:
  
  cause_list  <- c(pathogen_BrS_ord,pathogen_SSonly    # singleton causes in pathogen_SSonly.
                   # any combinations
                   ,"NoA")
  
  # always specify "NOA" last if it goes into model.
  
  
  # specify model fitting options (see nplcm function for details):
  model_options <- list(M_use = c("BrS","SS"),
                        # has to be a subset of non-NA entries in Mobs.
                        k_subclass = 1,
                        TPR_prior  = TPR_prior,
                        #same length as M_use.
                        Eti_prior  = Eti_prior,
                        pathogen_BrS_list = pathogen_BrS_ord,
                        cause_list  = cause_list,
                        X_reg_FPR = NULL,
                        X_reg_Eti = NULL, #change the FPR_regression, and Eti_regressions to NULL values.
                        pathogen_cat = NULL,
                        SSonly       = NULL,
                        pathogen_SSonly_list = NULL)
  
  working_dir ="C:\\"
  Date     <- gsub("-", "_", Sys.Date())
  fname    <- paste0(working_dir,Date,"_","SIM_TEST")
  dir.create(fname)
  Scenario <- "TEST"
  Num <- NULL
  expname =  paste0(c(Scenario,paste0(c(model_options$Eti_prior,model_options$TPR_prior),collapse="")),collapse="_")
  fullname <- paste0(fname,"\\",expname,"\\","DataSet",Num)
  
  ## for finer scenarios
  result.folder <- fullname
  dir.create(result.folder,recursive=T)
  
  
  # options for MCMC chains:
  mcmc_options <- list(debugstatus = !TRUE,
                       n.chains   = 1,
                       n.itermcmc = 1000,
                       n.burnin   = 500,
                       n.thin     = 5,
                       individual.pred = TRUE,
                       ppd             = TRUE,
                       result.folder = result.folder,
                       bugsmodel.dir = "C:\\PQ_MODEL\\winbugs_model_package\\",
                       winbugs.dir   = "C:\\Program Files\\WinBUGS14\\")
  
  #
  # Record the settings of current analysis:
  #
  cat("==Results stored in: ==","\n",result.folder)
  #model_options:
  dput(model_options,paste0(mcmc_options$result.folder,"\\model_options.txt"))
  #mcmc_options:
  dput(mcmc_options,paste0(mcmc_options$result.folder,"\\mcmc_options.txt"))
  
  
  
  call.bugs <- function(data, inits, parameters, m.file, bugsmodel.dir = mcmc_options$bugsmodel.dir, 
                        winbugs.dir = mcmc_options$winbugs.dir, nitermcmc = mcmc_options$n.itermcmc, 
                        nburnin = mcmc_options$n.burnin, nthin = mcmc_options$n.thin, 
                        nchains = mcmc_options$n.chains, dic = FALSE, is.debug = mcmc_options$debugstatus, 
                        workd = mcmc_options$result.folder, ...) {
    m.file <- paste(bugsmodel.dir, m.file, sep = "")
    f.tmp <- function() {
      gs <- bugs(data, inits, parameters, model.file = m.file, 
                 working.directory = workd, n.chains = nchains, 
                 n.iter = nitermcmc, n.burnin = nburnin, n.thin = nthin, 
                 bugs.directory = winbugs.dir, DIC = dic, debug = is.debug, 
                 ...)
      gs
    }
    bugs.try <- try(rst.bugs <- f.tmp(), silent = FALSE)
    if (class(bugs.try) == "try-error") {
      rst.bugs <- NULL
    }
    rst.bugs
  }
  parsing <- assign_model(Mobs, Y, X, model_options)
  Nd <- sum(Y == 1)
  Nu <- sum(Y == 0)
  cat("==True positive rate (TPR) prior(s) for ==\n", model_options$M_use, 
      "\n", " is(are respectively): \n", model_options$TPR_prior, 
      "\n")
  cause_list <- model_options$cause_list
  pathogen_BrS_list <- model_options$pathogen_BrS_list
  pathogen_SSonly_list <- model_options$pathogen_SSonly_list
  JBrS <- length(pathogen_BrS_list)
  JSSonly <- length(pathogen_SSonly_list)
  Jcause <- length(cause_list)
  template <- rbind(as.matrix(rbind(symb2I(c(cause_list), c(pathogen_BrS_list, 
                                                            pathogen_SSonly_list)))), rep(0, JBrS + JSSonly))
  MBS.case <- Mobs$MBS[Y == 1, ]
  MBS.ctrl <- Mobs$MBS[Y == 0, ]
  MBS <- as.matrix(rbind(MBS.case, MBS.ctrl))
  MSS.case <- Mobs$MSS[Y == 1,]
  MSS.case <- as.matrix(MSS.case)
  SS_index <- which(colMeans(is.na(MSS.case)) < 0.9)
  JSS <- length(SS_index)
  MSS <- MSS.case[, SS_index]
  # alpha <- eti_prior_set(model_options)
  if (model_options$Eti_prior == "E0") {
    alpha <- rep(0.125, 8)
  } else if (model_options$Eti_prior == "E1") {
    alpha <- c(0.3, 0.07,0.3,0.07,0.07,0.07,0.07,0.07)
  } 
  # TPR   
  if (model_options$TPR_prior[1] == "TB0") {
    temp_param.BS <- beta_parms_from_quantiles(c(0.50,0.99), p = c(0.025, 0.975), plot = FALSE)   
    alphaB <- rep(temp_param.BS$a,7)
    betaB <-  rep(temp_param.BS$b,7)  
  } else if (model_options$TPR_prior[1] == "TB1") {
    alphaB <- rep(1,7)
    betaB <-  rep(1,7)  
  } 
  if (model_options$TPR_prior[2] == "TS0") {
    temp_param.SS <- beta_parms_from_quantiles(c(0.10,0.20), p = c(0.025, 0.975), plot = FALSE)
    alphaS <- rep(temp_param.SS$a,7)
    betaS <-  rep(temp_param.SS$b,7)  
  } else if (model_options$TPR_prior[2] == "TS1") {
    temp_param.SS <- beta_parms_from_quantiles(c(0.01,0.20), p = c(0.025, 0.975), plot = FALSE)
    alphaS <- rep(temp_param.SS$a,7)
    betaS <-  rep(temp_param.SS$b,7)  
  }
  
  if (parsing$measurement$SSonly) {
    MSS.only.case <- Mobs$MSS[Y == 1, (1:JSSonly) + JBrS]
    MSS.only <- as.matrix(MSS.only.case)
    alphaS.only <- TPR_prior_list$alphaS.only
    betaS.only <- TPR_prior_list$betaS.only
  }
  mybugs <- function(...) {
    inits <- function() {
      list(thetaBS = rbeta(JBrS, 1, 1), psiBS = rbeta(JBrS, 
                                                      1, 1))
    }
    data <- c("Nd", "Nu", "JBrS", "Jcause", "alpha", "template", 
              "MBS", "JSS", "MSS", "alphaB", "betaB", "alphaS", 
              "betaS")
    if (mcmc_options$individual.pred == FALSE & mcmc_options$ppd == 
          TRUE) {
      parameters <- c("thetaBS", "psiBS", "pEti", "thetaSS", 
                      "MBS.new")
    } else if (mcmc_options$individual.pred == TRUE & mcmc_options$ppd == 
                 TRUE) {
      parameters <- c("thetaBS", "psiBS", "pEti", "thetaSS", 
                      "Icat", "MBS.new")
    } else if (mcmc_options$individual.pred == TRUE & mcmc_options$ppd == 
                 FALSE) {
      parameters <- c("thetaBS", "psiBS", "pEti", "thetaSS", 
                      "Icat")
    } else if (mcmc_options$individual.pred == FALSE & mcmc_options$ppd == 
                 FALSE) {
      parameters <- c("thetaBS", "psiBS", "pEti", "thetaSS")
    }
    rst.bugs <- call.bugs(data, inits, parameters, ...)
    rst.bugs
  }
  if (mcmc_options$ppd == TRUE) {
    gs <- mybugs("model_NoReg_BrSandSS_plcm_ppd.bug")
  } else {
    gs <- mybugs("model_NoReg_BrSandSS_plcm.bug")
  }
  
  
}

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
