#This function goes through a given dictionary and add the class a code belongs to in a new column

dict_classification <- function(sheet, dct, clm, class_clm){
  sheet <- as.data.frame(sheet)
  sheet[, class_clm] <- as.character(sheet[, class_clm])
  sheet[, clm] <- as.character(sheet[, clm])
  for(i in 1:nrow(sheet)){
    #print(sheet[i,clm])
    for(j in names(dct)){
      #print(j)
      if((sheet[i,clm] %in% dct[[j]])){
        sheet[i, class_clm] <- j
        #print(paste(j, "found!!!!!!!!!!!!!"))
      }
    }
  }
  return(sheet)
}
