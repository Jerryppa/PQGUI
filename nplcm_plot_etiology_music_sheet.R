##' compare the posterior distribution of population etiologies side-by-side
#'
#' The so-called "music-sheet plot". It takes two, or three or more output
#' folders so that the results are combined. There must be a check to make sure
#' that pathogens from one analysis are subset of another. DN: current implementation
#' does not check whether the results are from the same site, or strata. This
#' information can be seen from the folder names, or the figure names within
#'  the folders.
#'
#'
#'@param DIR_list The list of directory paths, each storing a model output.
#'
#'@param DIR_pathogen_displayorder_loookup The directory path to the .csv file
#'that stores the display order of pathogens in the combined music sheet plot.
#'
#'@return A figure that compares posterior etiology distribution stored in
#'two or more folders
#'
#'@export
#'

nplcm_plot_etiology_music_sheet <- function(DIR_list,
                                            DIR_pathogen_displayorder_lookup,
                                            comparision=F){
          ## read in pathogen display order lookup table:
          pathogen_displayorder_lookup <- read.csv(DIR_pathogen_displayorder_lookup)
          f <- pathogen_displayorder_lookup$Pathogen
          display_order <- as.character(levels(f))[f]

          ## read in data from result directories:
                bugs.dat_list <- list()
           model_options_list <- list()
          mcmc_options_list <- list()
                pathogen_list <- list()
                pick_list     <- list()
                Jfull_BrS     <- list()
                Jfull         <- list()
                   pEti_mat   <- list()
                   pEti_mean  <- list()
                   pEti_mean0 <- list()
                 pathogen_names<- list()
                            Nd_list <- list()#WF for sample size label
                 pathogen_names_union <- "INITIAL"
           for (i in seq_along(DIR_list)){

                 bugs.dat <- dget(paste(DIR_list[[i]],"data.txt",sep="/"))
                for (bugs.variable.name in names(bugs.dat)) {
                  if (!is.null(dim(bugs.dat[[bugs.variable.name]]))) {
                    dim(bugs.dat[[bugs.variable.name]]) <- rev(dim(bugs.dat[[bugs.variable.name]]))
                    bugs.dat[[bugs.variable.name]] <- aperm(bugs.dat[[bugs.variable.name]])
                  }
                  assign(bugs.variable.name, bugs.dat[[bugs.variable.name]])
                }

                bugs.dat_list[[i]] <- bugs.dat
                rm("bugs.dat")
                model_options_list[[i]]  <- dget(paste(DIR_list[[i]],"model_options.txt",sep="/"))
                mcmc_options_list[[i]]  <- dget(paste(DIR_list[[i]],"mcmc_options.txt",sep="/"))
                result.folder <- mcmc_options_list[[i]][["result.folder"]]#WF
                pathogen_list[[i]]     <- model_options_list[[i]]$cause_list
                Jfull[[i]]        <- length(pathogen_list[[i]])

                #reading nplcm outputs:
                res_nplcm <- read.coda(paste(DIR_list[[i]],"coda1.txt",sep="/"),
                                       paste(DIR_list[[i]],"codaIndex.txt",sep="/"),
                                       quiet=TRUE)
                Nd_list[[i]] = bugs.dat_list[[i]]$Nd

                #WF
                if (is.null(model_options_list[[i]]$X_reg_FPR)){
                  #Jfull[[i]] <- length(grep("pEti",colnames(res_nplcm)))
                  # extract and process some data and posterior samples:
                  SubVarName <- rep(NA,Jfull[[i]])
                  for (j in 1:Jfull[[i]]){
                    SubVarName[j] = paste("pEti","[",j,"]",sep="")
                    
                  }
                  IcatName <- rep(NA,Nd_list[[i]])
                  for (j in 1:Nd_list[[i]]){
                    IcatName[j] = paste("Icat","[",j,"]",sep="")
                  }
                  Icat_raw = res_nplcm[,IcatName]
                  pEti_raw = res_nplcm[,SubVarName]
                } else {
                  
                  N_grp=bugs.dat_list[[i]]$N_grp
                  grp_info=model_options_list[[i]]$grp_info
                  
                  #Jfull[[i]]=length(grep("pEti",colnames(res_nplcm)))/N_grp
                  #WF
                  VarName <- rep(NA,Jfull[[i]])
                  for (j in 1:Jfull[[i]]){
                    VarName[j] = paste("pEti","[",j,"]",sep="")
                  }

                  
                  pEti_raw=matrix(0,dim(res_nplcm)[1],Jfull[[i]])
                  colnames(pEti_raw)=VarName
                  
                  for (j in 1:Jfull[[i]]){
                    for (l in 1:dim(grp_info)[1]){
                      k=grp_info$Var1[l]
                      tmpname = paste("pEti","[",k,",",j,"]",sep="")
                      pEti_raw[,j]=pEti_raw[,j]+res_nplcm[,tmpname]*grp_info$Freq[grp_info$Var1==k]/Nd_list[[i]]
                    }
                  }
                }



                Icat_mat <- IcatToIEti(Icat_raw)
                colnames(Icat_mat) <- pathogen_list[[i]][as.numeric(colnames(Icat_mat))]
                rownames(Icat_mat) <- 1:nrow(Icat_mat)
                PQ_GUI_dataset <- read.csv(paste0(DIR_list[[i]],"/PQ_SAF_14MAY15_GUI.csv"))
                #PQ_GUI_dataset <- read.csv("data/PQ_SAF_14MAY15_GUI.csv")
                PQ_GUI_subset <- PQ_GUI_dataset[which(PQ_GUI_dataset$PQGUI_CXR==1 & 
                                                         PQ_GUI_dataset$PQGUI_HIV==1 &  
                                                         PQ_GUI_dataset$PQGUI_AGE==1),-1]
                row.names(PQ_GUI_subset)<- 1:nrow(PQ_GUI_subset)
                PQ_GUI_ind_prop<- merge(PQ_GUI_subset,as.data.frame(Icat_mat),by="row.names")
                write.csv(PQ_GUI_ind_prop,paste0(DIR_list[[i]],"/PQ_GUI.csv"))

                #get etiology fraction MCMC samples:
                pEti_mat[[i]]   <- pEti_raw
                pEti_mean[[i]]  <- colMeans(pEti_mat[[i]])
                pEti_mean0[[i]] <- pEti_mean[[i]]

                pathogen_names[[i]] <- model_options_list[[i]]$cause_list

        #         order_tmp <- rep(NA,length(pathogen_names[[i]]))
        #
        #         incre <- 0
        #         for (j in seq_along(display_order)){
        #           if (display_order[j]%in% pathogen_names[[i]]){
        #             incre <- incre + 1
        #             order_tmp[incre] <- which(pathogen_names[[i]]==display_order[j])
        #           }
        #         }
        #         order_list[[i]] <- order_tmp

                pathogen_names_union <- union(pathogen_names_union,pathogen_names[[i]])
           }

          pathogen_names_union <- pathogen_names_union[-grep("INITIAL",pathogen_names_union)]
          pathogen_names_union <- sort(pathogen_names_union)

          # a function to match each model's result to the desired order:
          lookup <- function(disp_order, union_nm, nm_list){
            ord_union <- rep(NA,length(union_nm))
            incre <- 0
            for (j in seq_along(disp_order)){
              if (disp_order[j]%in% union_nm){
                incre <- incre + 1
                ord_union[incre] <- which(union_nm==disp_order[j])
              }
            }
            pick_order <- union_nm[ord_union]
            res <- list()
            for (i in seq_along(nm_list)){
              res[[i]] <- rep(NA,length(pick_order))
              for (j in 1:length(pick_order)){
                if (pick_order[j]%in%nm_list[[i]]){
                  res[[i]][j] <- which(nm_list[[i]]==pick_order[j])
                }
              }
            }
            list(model_res=res, union_res = pick_order)
          }

        #   disp_order <- c("B","E","D","C","F","A")
        #   union_nm   <- c("A","B","C","D","E")
        #   nm_list <-list(c("C","E"),
        #                  c("D","B"),
        #                  c("C","A","E"))
        #   lookup(disp_order,union_nm,nm_list)
          lookup_res <- lookup(display_order,pathogen_names_union,pathogen_list)
          pick_list <- lookup_res$model_res
          union_aligned<- lookup_res$union_res

          ## combine the data sets:



      combine_data_for_boxplot <- function(pEti_mat,pick_list){
        tmp <- list()
        for (i in seq_along(pEti_mat)){
            part1 <- as.data.frame(pEti_mat[[i]][,pick_list[[i]]])
            picked_names <- pathogen_list[[i]][pick_list[[i]]]
#             colnames(part1) <- paste( formatC(1:length(picked_names), width=3, flag="0"),
#                                       picked_names, sep="_")
            picked_names[is.na(picked_names)]="NoA"
            colnames(part1) <- picked_names
            picked_name_temp= picked_names


            ind_category <- sapply(1:length(picked_name_temp),function(j)
                                      which(display_order==picked_name_temp[j]))
            vec_category <- as.character(pathogen_displayorder_lookup$Category[ind_category])
            viral_line_pos <- sum(vec_category=="virus") # draw a line right to this number.
            bacterial_line_pos <- sum(vec_category=="bacterium") # draw a line right to this number.
            other_line_pos <- sum(vec_category=="other") # draw a line right to this number.


            part1$viral     <- apply(part1,1,function(v) sum(v[1:viral_line_pos]))
            part1$bacterial <- apply(part1,1,function(v) sum(v[(viral_line_pos+1):(viral_line_pos+bacterial_line_pos)]))
            part1$other <- apply(part1,1,function(v) sum(v[(viral_line_pos+bacterial_line_pos+1):(viral_line_pos+bacterial_line_pos+other_line_pos)]))

            tmp[[i]] <- reshape(part1,
                        idvar = "iteration",
                        ids = rownames(part1),
                        times = colnames(part1),
                        timevar = "pathogen",
                        varying = colnames(part1),
                        v.names = "etiology",
                        direction="long")
            tmp[[i]]$model <- i
            tmp[[i]]$pathogen <- factor(tmp[[i]]$pathogen,c(picked_names,"viral","bacterial","other"))
        }
        res <- do.call("rbind", tmp)
      }

      data_for_boxplot <- combine_data_for_boxplot(pEti_mat,pick_list)


      ind_category <- sapply(1:length(union_aligned),function(j)
                       which(display_order==union_aligned[j]))
      vec_category <- as.character(pathogen_displayorder_lookup$Category[ind_category])
      viral_line_pos <- sum(vec_category=="virus") # draw a line right to this number.
      bacterial_line_pos <- sum(vec_category=="bacterium")
      other_line_pos <- sum(vec_category=="other")

   ## combine them into the display:
   ## ggplot2:
   f <- function(x) {
      r <- quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
      names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
      r
    }
    mean_with_nm <- function(x){
      r <- rep(mean(x),2)
      names(r)<-c("y","ymax")
      r}
    mean_with_nm_txt <- function(x){
       r <- c(ifelse(max(x)-quantile(x,.97)>0.02,quantile(x,.97)+0.02,max(x)),
              round(mean(x),3),round(mean(x),3)*100)
       names(r)<-c("y","ymax","label")
       r
    }
    n_model <- length(DIR_list)
    DIR_legend_list <- as.character(mapply(function(a,b) paste0(rev(strsplit(a,"[/]")[[1]])[1]," (N=",b,")"), DIR_list,Nd_list))
    ymax <- max(data_for_boxplot$etiology)
    text_df <-data.frame(pathogen =c(viral_line_pos/2,viral_line_pos+(length(union_aligned)-viral_line_pos)/2),
                         etiology = c(1,1),
                         label = c("viral","bacterial"),
                         model = c(1, 1))
    res<-ggplot(data = data_for_boxplot, aes(x = factor(pathogen), y =etiology,
                                          fill = factor(model))) +
                #geom_boxplot(width = 0.8) +
                labs(list(x = "pathogen", y = "etiology"))+theme_bw()+
                theme(axis.text.x  = element_text(angle=90, size=16),
                      axis.title  = element_text(size=16,face="bold"))+
                stat_summary(fun.data = f, geom="boxplot",aes(width=0.8),
                             position=position_dodge(.8))+
                stat_summary(fun.data = mean_with_nm,geom="point",aes(size=1.5),
                             position=position_dodge(.8))+scale_size(guide = 'none')+
                scale_fill_discrete(name="Model",
                                    breaks=1:n_model,
                                    labels=c(DIR_legend_list))+
                theme(legend.text = element_text(colour="blue",
                                                 size = 16, face = "bold"),
                      legend.title = element_text(size=16,face="bold"),
                      legend.position = "top")+
                stat_summary(fun.data = mean_with_nm_txt,geom="text",
                             aes(angle=90),
                             position = position_dodge(width = 0.8))+ylim(0,ymax)+
                geom_vline(xintercept=viral_line_pos+.5,linetype = "longdash")+#separate virus and bacteria.
                geom_vline(xintercept=viral_line_pos+bacterial_line_pos+.5,linetype = "longdash")+#separate virus and bacteria.  
                geom_vline(xintercept=length(union_aligned)+.5,size=1.5)
       res <- res+ annotate("text", label = "viral", x = viral_line_pos/2,
                            y = ymax*3/4, size = 10, colour = "black")+
                    annotate("text", label = "bacterial", x = (viral_line_pos+bacterial_line_pos/2),
                     y = ymax*3/4, size = 10, colour = "black")+
                    annotate("text", label = "other", x = (viral_line_pos+bacterial_line_pos+other_line_pos/2),
                            y = ymax*3/4, size = 10, colour = "black")
      if (comparision ==F ){
            ggsave(file=paste0(DIR_list,"/Music_Plot.pdf"),width=20, height=10)
      } else {
            ggsave(file=paste0("C:/PQGUI/Music_Plot_comparison.pdf"),width=20, height=10)
      }

      ## plain R plots:----------------------------------
      #    ## combine them into the display:
      #    n_path <- length(pathogen_names_union)
      #    n_model <- length(DIR_list)
      #
      # #    delta <- 0.25
      # #    boxwex <- (0.5-delta)/ceiling((n_model-1)/2)
      # #    get_plotat<- function(n){c(n-0.25,n,sapply())}
      #
      #    get_plotat <- function(n,n_model,delta=0.4){
      #      if (n_model%%2==0){
      #        boxwex <- (0.5-delta/2)/(n_model/2)
      #        plotat = c(n-c(boxwex*(1:(n_model/2))-boxwex/2), n+c(boxwex*(1:(n_model/2))-boxwex/2))
      #        res <- list(plotat=plotat,boxwex=boxwex)
      #      }else{
      #         boxwex <- (0.5-delta/2)/(n_model/2-0.5+0.5)
      #         plotat = c(n-boxwex*(1:(n_model/2-0.5)),n,n+boxwex*(1:(n_model/2-0.5)))
      #         res <- list(plotat=plotat,boxwex=boxwex)
      #      }
      #      res
      #    }
      #
      #    plotat <- c(sapply(1:n_path,function(j) get_plotat(j,n_model)$plotat))
      #    boxwex <- get_plotat(1,n_model)$boxwex
      #     colors <-rev(brewer.pal(8,"Paired"))
      #     pal <- colorRampPalette(colors)
      #    bp <- boxplot(etiology~model+pathogen,data=data_for_boxplot,las=2,
      #            outline=FALSE,at = plotat,boxwex = boxwex,xaxt="n",
      #            col=pal(n_model),ylab="etiology",xlab="pathogen")
      #    #axis(1, at = 1:n_path,labels=union_aligned,las=2)
      #    text(1:n_path,par("usr")[3],labels=union_aligned, srt=45,
      #         adj = c(1.1,1.1), xpd = TRUE, cex=.9)
      #
      #    points()
      #
      #    DIR_legend_list = lapply(DIR_list,function(a) rev(strsplit(a,dir_separator)[[1]])[1])
      #    legend("topright",legend = c(DIR_legend_list),col=pal(n_model),lty=1,lwd=5)

}

# pdf(paste0(DIR1,"\\bact_vs_viral_etiology.pdf"),height=10,width=10)
# nplcm_plot_group_etiology(DIR_NPLCM = DIR1,ksFrac = .5)
# dev.off()
#
# pdf(paste0(DIR2,"\\bact_vs_viral_etiology.pdf"),height=10,width=10)
# nplcm_plot_group_etiology(DIR_NPLCM = DIR2,ksFrac = .5)
# dev.off()

