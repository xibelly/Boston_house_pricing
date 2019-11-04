##################################################################
#
#                            DIAN
#                Solucion problema - Boston-housing
#
##################################################################



#***** DIAN                            *****
#***** Date: 2019-04-21                *****
#***** Author: Xibelly Mosquera        *****
#***** E-mail: xibeline@gmail.com      *****


#+++++ Setting directory structure +++++
dir.ppal      <- "/home/xibelly/Documentos/Problema_kaggle/"
dir.input     <- paste0(dir.ppal, "Input/")
dir.scripts   <- paste0(dir.ppal, "Model/")
dir.output    <- paste0(dir.ppal, "Output/")
archivo_log <- paste(dir.output, "validaciones.txt", sep="")

#+++++ Requiring libraries +++++
cat("Loading Packages\n")
#source(paste0(dir.scripts, "PKG.R"))

#Reading the data sets
name_file1 <- "train.csv"
name_file2 <- "test.csv"

#data train
data_train <- data.table(read.csv(paste0(dir.input, name_file1), header = TRUE, sep = ","))

#data test
data_test <- data.table(read.csv(paste0(dir.input, name_file2), header = TRUE, sep = ","))

#Analizing the data_train
str(data_train)
summary(data_train)

#Exploring the data set 
cat("creating the boxplot\n")
#png(paste0(dir.output,"boxplot.png"), width = 350, height = 350, units = "px", pointsize = 12)
#plot <- boxplot(aplicabilidad~data_train, BD, main="Rentabilidad vs Factor de Riesgo", 
                #ylab="Rentabilidad")
#dev.off()

boxplot(crim~data_train,zn~data_train,rm~data_train,age~data_train,
        main = "Multiple boxplots for comparision",
        at = c(1,2,4,5),
        names = c("crim", "zn", "rm", "age"),
        las = 4,
        col = c("orange","red"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

#Making a K-means clustering 
set.seed(1234)

#Normalize the variables
data_train_n <- as.data.table(scale(data_train)) # standardize variables
data_test_n  <-  as.data.table(scale(data_test))

# Determine number of clusters with the data_tain_n
wss <- (nrow(data_train_n)-1)*sum(apply(data_train_n,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data_train_n, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# Using the K-means algorithm
# K-Means Cluster Analysis
fit <- kmeans(data_test, 8) # 5 cluster solution
# get cluster means 
aggregate(data_test_n,by=list(fit$cluster),FUN=mean)
# append cluster assignment
data_test <- data.frame(data_test, fit$cluster)


#Making a Random forest
#independent variables for the model
columns = as.numeric(ncol(data_train))
X_train <- data_train[,-15]
Y_train <- as.data.table(copy(data_train$medv))
setnames(Y_train,"V1","medv")

#Training the model
modelo <- randomForest::randomForest(X_train, Y_train, ntree=100,mtry=9)
plot(modelo)
varImpPlot(modelo)
modelo


#Predicting
X_test <- data_test
data_test$prediction <- predict(modelo, X_test)
#data_train$prediction <- predict(modelo, X_train)
data_train[,error:=(prediction-medv)^2]




#EVALUATING THE NUMBER OF TREES
#number of trees
arb<-c(100) #,20,50,100,200,500,1000,2000)  # numer of trees
int<-c(9) #9 #Number of variables randomly sampled as candidates at each split
# 
for(i in arb){
  for(j in int){
    #training the RandomForest model
    modelo <- randomForest::randomForest(X_train, Y_train, ntree=100,mtry=9)
    #plot(modelo)
    #varImpPlot(modelo)
    #modelo
  
    Y_train$pred <- predict(modelo, X_train)
    Y_train[,pred:=floor(pred)]
    Y_train[,error:=(pred-medv)^2]
     
    # if(exists("mdes")){
    #  tmp<-data.table(ARBOLES=arb,INTENTOS=int,MSE=mean(Y_train$error,na.rm = T),
    #                          RMSE=sqrt(mean(Y_train$error,na.rm = T)))
    #  mdes<-rbind(mdes,tmp)
    # }else{
    #   mdes<-data.table(ARBOLES=i,INTENTOS=j,MSE=mean(Y_train$error,na.rm = T),
    #                     RMSE=  sqrt(mean(Y_train$error,na.rm = T)))
    # }
    #   
    # Y_train[,c("pred","error"):=NULL]
    cat("Ejecucion para el modelo con",i,"arboles y",j,"intentos \n" )
  }  
}
   
fwrite(mdes,paste0(dir.Output,"Medidas_desempeno.csv"),sep = "|")

#Si no funciona cambiar medv por mdes
