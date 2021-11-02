split_and_merge_by_doc <- function(clms, sheet, na.rm = TRUE){
  docs_only <- as.data.frame(sheet[,1]) %>% distinct()
  colnames(docs_only) <- "Document"
  mergedat <- as.data.frame(sheet[,1]) %>% distinct()
  colnames(mergedat) <- "Document"
  for(i in clms){
    clmname <- colnames(sheet)[i]
    print(clmname)
    splitdat <- sheet[,c(1,i)]
    if(na.rm){
      splitdat <- splitdat[!is.na(splitdat[,2]),]
    }else{
      splitdat <- splitdat[!is.na(splitdat[,2]),]
      splitdat <- merge(splitdat, docs_only, all.y = TRUE)
    }
    splitdat <- splitdat %>% distinct()
    mergedat <- merge(mergedat, splitdat)
  }
  
  return(mergedat)
  
}
