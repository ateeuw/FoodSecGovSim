# function to split double codes within one quotation

split_and_add_by_doc <- function(sheet, clm){
  
  code_name <- colnames(sheet)[clm]
  
  for(a in unique(sheet$Document)){
    adata <- sheet[sheet$Document==a,]
    mtype <- unique(adata[,clm])[!is.na(unique(adata[,clm]))]
      
    if(length(mtype)==0){
      print(paste0("warning: in this quotation, no '", code_name, "'code has been attached"))
      print(paste("Document = ", a))
      print("")
      
    }else if(length(mtype)==1){
      sheet[sheet$Document == a,clm] <- mtype
      
    }else{
      sheet[sheet$Document == a,clm] <- mtype[1]
      
      for(m in 2:length(mtype)){
        to_add <- sheet[sheet$Document == a,]
        to_add[,clm] <- mtype[m]
        sheet <- rbind(sheet, to_add)
      }
      
    }
    
  }
  
  return(sheet)
}



# modelling <- quotes_wide
# 
# modelling_codes <- c("modelling - aim", "modelling - feedback-loop?", "modelling - sensitivity analysis?", "modelling - validation?", "modelling - data",
#                      "per model - type") #not included: subtype and subdomain
# 
# keep <- which(colnames(modelling) %in% c(modelling_codes, "name_id", "Document", "ID"))
# modelling <- modelling[,keep]
# empty_rows <- which(rowSums(is.na(modelling[,4:ncol(modelling)]))==length(4:ncol(modelling)))
# modelling <- modelling[-empty_rows,]
# 
# # nested data
# nest <- quotes_wide[,which(colnames(quotes_wide) %in% c("per model - type", "modelling - coupling?", "name_id", "Document", "ID"))]
# empty_rows <- which(rowSums(is.na(nest[,4:ncol(nest)]))==length(4:ncol(nest)))
# nest <- nest[-empty_rows,]
# 
# nest <- split_and_add_by_ID(sheet = nest, clm = 4) #coupling
# nest <- nest[-which(is.na(nest$`per model - type`)),]
# nest <- nest[,-which(colnames(nest) %in% c("name_id","ID"))]
# 
# nest2 <- quotes_wide[,which(colnames(quotes_wide) %in% c("per model - type", "per model - domain", "name_id", "Document", "ID"))]
# empty_rows <- which(rowSums(is.na(nest2[,4:ncol(nest2)]))==length(4:ncol(nest2)))
# nest2 <- nest2[-empty_rows,]
# 
# nest2 <- split_and_add_by_ID(sheet = nest2, clm = 4) #domain
# nest2 <- nest2[-which(is.na(nest2$`per model - type`)),]
# nest2 <- nest2[,-which(colnames(nest2) %in% c("name_id","ID"))]
# 
# # level one non-nested data
# for(i in c(4:9)){
#   modelling <- split_and_add_by_doc(sheet = modelling, clm = i)
# }
# 
# modelling <- modelling[,-which(colnames(modelling) %in% c("name_id","ID"))]
# modelling <- modelling %>% distinct()
# 
# modelling <- merge(modelling, nest)
# modelling <- merge(modelling, nest2)



