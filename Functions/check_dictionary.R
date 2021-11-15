check_dictionary <- function(codegroup, codedictionary, dat_long){
  dat <- dat_long[dat_long$code_group == codegroup,]
  dat <- dat[!is.na(dat$code_group),]
  codeclm <- which(colnames(dat) == "name")
  dat$class <- ""
  classclm <- which(colnames(dat) == "class")
  dat <- dict_classification(sheet = dat, dct = codedictionary, clm = codeclm, class_clm = classclm)
  #classes <- unique(dat[,classclm])
  if(sum(dat[,classclm]=="") > 0){
    missing_classes <- which(dat[,classclm]=="")
    missing_codes <- dat[missing_classes,codeclm]
    missing_codes_str <- toString(unique(missing_codes))
    print(paste("    !!!!!!!!!!!! dictionary is NOT up to date :( !!!!!!!!!!!!"))
    print("  ")
    print("    not yet classified:")
    print(missing_codes_str)#)
  }else{
      return(print("    !!!!!!!!!!!! dictionary is up to date :) !!!!!!!!!!!!"))
    }
  
}
