IcatToIEti <- function(Icat_raw) {
  Icat_table <- apply(Icat_raw,2,function(x) {
    as.data.frame(prop.table(table(x)),stringsAsFactors = F)
    })
  Merged <- Reduce(function(...) {
    merge(...,by="x",all=T)
    },Icat_table)
  Merged.mat <- t(as.matrix(Merged[,-1]))
  colnames(Merged.mat) <- Merged[,1]
  rownames(Merged.mat) <- 1:nrow(Merged.mat)
  return(Merged.mat)
}