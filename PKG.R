#------------------------------------------------------------------------------
# Libraries for the model
#------------------------------------------------------------------------------

librerias<-c("data.table","readxl","xlsx","stringi","lubridate","randomForest"
             ,"plyr","stringr","tidyr","plotly","kmeans")
for (i in librerias){
  
  if(eval( parse(text=paste0("!require(",i,")")))){
    install.packages(i)
  }
  suppressPackageStartupMessages(eval( parse(text=paste0("require(",i,")"))))
  

}

