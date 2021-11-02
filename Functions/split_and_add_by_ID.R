# function to split double codes within one quotation

split_and_add_by_ID <- function(sheet, clm, clm_range){
  
  code_name <- colnames(sheet)[clm]
  
  for(a in unique(sheet$Document)){
    adata <- sheet[sheet$Document==a,]
    
    for(i in unique(adata$ID)){
      idata <- adata[adata$ID==i,]
      mtype <- unique(idata[,clm])[!is.na(unique(idata[,clm]))]
      
      if(length(mtype)==0){
        
        sheet <- sheet[-which(sheet$ID==i),]
        
      }else if(length(mtype)==1){
        sheet[sheet$ID == i, clm] <- mtype
        
      }else{
        
        sheet[sheet$ID == i,clm] <- mtype[1]
        
        for(m in 2:length(mtype)){
          to_add <- sheet[sheet$ID == i,]
          to_add[,clm] <- mtype[m]
          sheet <- rbind(sheet, to_add)
        }
        
      }
    }
    
  }
  
return(sheet)
  
}



# modelling <- quotes_wide
# 
# modelling_codes <- c("modelling - coupling?", "modelling - aim", "modelling - feedback-loop?", "modelling - feedback-loop?", "modelling - sensitivity analysis?", "modelling - validation?", "modelling - data")
# keep <- which(colnames(modelling) %in% c(modelling_codes, "name_id", "Document", "ID"))
# modelling <- modelling[,keep]
# empty_rows <- which(rowSums(is.na(modelling[,4:ncol(modelling)]))==length(4:ncol(modelling)))
# modelling <- modelling[-empty_rows,]
# 
# 
# test <- split_and_add_by_ID(sheet = modelling, clm = 4)
