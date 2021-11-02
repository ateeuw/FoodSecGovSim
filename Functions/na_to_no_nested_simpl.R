na_to_no_nested_simpl <- function(dat, na_col, matchterms, nest_col){
  
  col_nr <- which(colnames(dat)==na_col)
  nest_col_nr <- which(colnames(dat)==nest_col)
  
  for(i in unique(dat$Document)){
    dati <- dat[dat$Document == i,]
    
    for(j in unique(dati[,nest_col_nr])){
      
      row_nrs <- which(dat$Document == i & dat[,nest_col_nr] == j)
      
      if(any(matchterms %in% dat[row_nrs,col_nr])){
        val <- matchterms[which(matchterms %in% dat[row_nrs,col_nr])]
      }else{
        dat[row_nrs,col_nr] <- "no"
      }
    }
  }
  
  return(dat[,col_nr])
}