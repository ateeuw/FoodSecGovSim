na_to_no <- function(dat, na_col, matchterm, val){
  
  col_nr <- which(colnames(dat)==na_col)
  dat[,col_nr] <- as.character(dat[,col_nr])
  
  for(i in unique(dat$Document)){
    dati <- dat[dat$Document == i,]
    
    if(matchterm %in% dati[, col_nr]){
      dat[dat$Document == i,col_nr] <- val
    }else{
      dat[dat$Document == i,col_nr] <- "no"
    }
  }
  
  return(dat[,col_nr])
}
