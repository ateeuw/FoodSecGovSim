NATO <- list()

GovClass <- read_excel("./Data/GovClass.xlsx")

for(i in unique(GovClass$NATO_class)){
  
  class_vec <- GovClass$Atlas_code[which(GovClass$NATO_class==i)]
  NATO[[i]] <- class_vec
  
}

rm("GovClass")
rm("class_vec")