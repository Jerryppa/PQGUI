
SIM_DATA <- function (set_parameter) 
{
  pathogen_BrS <- set_parameter$pathogen_BrS
  cause_list <- set_parameter$cause_list
  PsiBS <- set_parameter$PsiBS
  ThetaBS <- set_parameter$ThetaBS  
  PsiSS <- set_parameter$PsiSS
  ThetaSS <- set_parameter$ThetaSS
  Nd <- set_parameter$Nd
  Nu <- set_parameter$Nu
  Lambda <- set_parameter$Lambda
  Eta <- set_parameter$Eta
  etiology <- set_parameter$etiology
  J_BrS <- length(pathogen_BrS)
  Jcause <- length(cause_list)
  iLcat <- rep(NA, Nd)
  iLall <- matrix(NA, nrow = Nd + Nu, ncol = J_BrS)
  etiologyMat <- matrix(NA, nrow = Nd, ncol = Jcause)
  for (i in 1:Nd) {
    etiologyMat[i, ] <- etiology
    iLcat[i] <- sample(cause_list, 1, prob = etiologyMat[i, 
                                                         ])
  }
  iL <- symb2I(iLcat, pathogen_BrS)
  iLall <- rbind(iL, matrix(0, nrow = Nu, ncol = J_BrS))
  iLcat.case.numeric <- Imat2cat(iL, cause_list, pathogen_BrS)
  iLcatAllnumeric <- c(iLcat.case.numeric, rep(Jcause + 1, 
                                               Nu))
  Zd <- rep(NA, Nd)
  Md <- matrix(NA, nrow = Nd, ncol = J_BrS)
  MdP.BS <- Md
  for (i in 1:Nd) {
    for (j in 1:J_BrS) {
      MdP.BS[i, j] = PsiBS[j] * (1 - iL[i, j]) + iL[i, 
                                                    j] * ThetaBS[j]
    }
  }
  MdP.BS <- rvbern(MdP.BS)
  MdP.SS <- Md
  for (i in 1:Nd) {
    for (j in 1:J_BrS) {
      MdP.SS[i, j] = PsiSS[j] * (1 - iL[i, j]) + iL[i, 
                                                    j] * ThetaSS[j]
    }
  }
  MdP.SS <- rvbern(MdP.SS)
  Zu <- rep(NA, Nu)
  
  Mu <- matrix(NA, nrow = Nu, ncol = J_BrS)
  MuP.BS <- matrix(NA, nrow = Nu, ncol = J_BrS)
  for (i in 1:Nu) {
    Zu[i] <- 1
    for (j in 1:J_BrS) {
      MuP.BS[i, j] <- PsiBS[j]
    }
  }
  MuP.BS <- rvbern(MuP.BS)
  
  MuP.SS <- matrix(NA, nrow = Nu, ncol = J_BrS)
  for (i in 1:Nu) {
    for (j in 1:J_BrS) {
      MuP.SS[i, j] <- PsiSS[j]
    }
  }
  MuP.SS <- rvbern(MuP.SS)  
  
  Y <- data.frame(Y = c(rep(1, Nd), rep(0, Nu)))
  colnames(Y)="Y"
  iL <- data.frame(iLcat = iLcatAllnumeric, 
                   iL = iLall)
  colnames(iL) =c("iLcat", paste("iL", pathogen_BrS, 
                                 sep = "_"))
  MBS = data.frame(rbind(MdP.BS,MuP.BS))
  colnames(MBS)=paste(pathogen_BrS,"NPPCR", sep = "_")
  MSS = data.frame(rbind(MdP.SS,MuP.SS))
  colnames(MSS)=paste(pathogen_BrS,"BCX", sep = "_")
  
  Mobs = list(MBS=MBS,MSS=MSS)
  
  template <- as.matrix(rbind(symb2I(cause_list, pathogen_BrS), 
                              rep(0, J_BrS)))
  colnames(template) <- pathogen_BrS
  rownames(template) <- c(cause_list, "control")
  return(list(template = template,
              pathogen_BrS=pathogen_BrS,
              cause_list=cause_list,
              Y=Y,
              iL=iL,
              Mobs=Mobs))
}


