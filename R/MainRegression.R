### PROJECT 1 R Code ### 

## Setting Working Directory to proper pathname
setwd("/Users/zachbaker/Documents/Senior Year/Spring Semester/DS2/Regression-DataScience-II/R/")

## Importing proper libraries
library(ggplot2)
library(mlr)
library(caret)
library(leaps)
library(elasticnet)
library(data.table)

## Reading in the datasets adding in dummy columns as neccesary
autoMPG = read.csv(file = "RawData/AutoMPG.csv")

bikeSharing = read.csv(file = "RawData/Bike Sharing.csv",
                       colClasses = c('factor','integer','factor','integer','factor','factor','numeric','numeric','numeric','numeric','numeric')
                       )
bikeSharing = cbind(createDummyFeatures(bikeSharing[,-11],cols = c("season","mnth","weekday","weathersit")),bikeSharing$cnt)
bikeSharing = within(bikeSharing, rm("season.4","mnth.12","weekday.6","weathersit.3"))

compHardware = read.csv(file = "RawData/Computer Hardware.csv")
compHardware$vendor = as.factor(compHardware$vendor)
compHardware = cbind(createDummyFeatures(compHardware[,-9],cols = c("vendor")),compHardware$ERP)
compHardware = within(compHardware, rm("vendor.30"))

concrete = read.csv(file = "RawData/Concrete_Data.csv")

elecGridStab = read.csv(file = "RawData/Electrical Grid Stability.csv")

energyEff = read.csv(file = "RawData/Energy Effeciency.csv")

forestFire = read.csv(file = "RawData/forestfires.csv")
forestFire$month = as.factor(forestFire$month)
forestFire$day = as.factor(forestFire$day)
forestFire = cbind(createDummyFeatures(forestFire[,-13],cols = c("month","day")),forestFire$area)
forestFire = within(forestFire, rm("month.12","day.7"))

optical = read.csv(file = "RawData/Optical.csv",
                   colClasses = c('integer','integer','factor','integer','numeric','numeric','numeric','numeric')
                  )
optical = cbind(createDummyFeatures(optical[,-8],cols = c("Spatial.Distribution")),optical$Processor.Utilization)
optical = within(optical, rm("Spatial.Distribution.4"))


protein = read.csv(file = "RawData/Protein Tertiary.csv")

wine = read.csv(file = "RawData/winequality.csv")


##Creating lists of datasets to be used in for loop
vecData = list(autoMPG,bikeSharing,compHardware,concrete,elecGridStab,energyEff,forestFire,optical,protein,wine)
vecData2 = list(autoMPG,bikeSharing[,-c(10:20)],compHardware[,-c(8:36)],concrete,elecGridStab,energyEff,forestFire[,-c(11:21)],optical,protein,wine)

## Crossvalidation function
## mat: Takes in a matrix of best columns at each step of forward selection 
## df: dataframe to be crossvalidated
## method: type of analysis to run ex. 'lm', 'lasso', 'ridge'
crossVal = function(mat, df, method){
  cvR = c()
  r2A = c()
  r2  = c()
  for(i in 1:(nrow(mat))){
    print(i)
    train_control <- trainControl(method="cv", number=10) ## cross validation is set as training method
    ## runs linear regression if first column of forward select   
     if(i == 1 && method == "lm"){
      df1 = data.frame(one = df[,which(mat[i,-1]>0)]) ##create dataframe of the 1 column selected
      ##adds in the CV r-sq value
      cvR[i] = max(train(y = df[,ncol(df)], x = df1, trControl=train_control, method=method)$results$Rsquared)
      ## creates train control for not CV R-sq
      train_control <- trainControl(method="cv", number=2, index = list(1:nrow(df), (nrow(df)+1):(nrow(df)*2)))
      df2 = data.frame(one = c(df[,which(mat[i,-1]>0)],df[,which(mat[i,-1]>0)]))
      ## calculates R-sq 
      rS = max(train(y = c(df[,ncol(df)],df[,ncol(df)]), x = df2, trControl=train_control, method=method)$results$Rsquared)
      ## calculates Rbar-Sq
      r2A[i] = 1-(1-rS)*(nrow(df)-1)/(nrow(df)-i)
      r2[i] = rS

     }
    ## Same as above, just with more than 1 column as the x df
    else if(i > 1){
      train_control <- trainControl(method="cv", number=10)
      cvR[i] = max(train(y = df[,ncol(df)], x = df[,which(mat[i,-1]>0)], trControl=train_control, method=method)$results$Rsquared)
      train_control <- trainControl(method="cv", number=2, index = list(1:nrow(df), (nrow(df)+1):(nrow(df)*2)))
      rS = max(train(y = c(df[,ncol(df)],df[,ncol(df)]), x = rbind(df[,which(mat[i,-1]>0)],df[,which(mat[i,-1]>0)]), trControl=train_control, method=method)$results$Rsquared)
      r2A[i] = 1-(1-rS)*(nrow(df)-1)/(nrow(df)-i)
      r2[i] = rS
    }
  }
  return(data.frame(r2,r2A,cvR))
}

## takes in a dataframe and return the dataframe with quadratic terms and interaction terms
resSurface = function(df){
  x = as.matrix(df[,- ncol(df)])
  x2 = poly(x, degree = 2, raw = TRUE)
  return(as.data.frame(cbind(x2, y = df[,ncol(df)])))
}

## takes in a dataframe and return the dataframe with quadratic terms and no interaction terms
quadCols = function(df){
  x = as.matrix(df[,- ncol(df)])
  x2 = poly(x, degree = 2, raw = TRUE)
  dat = as.data.frame(cbind(x2, y = df[,ncol(df)]))
  datR = dat[ ,-which(names(dat) %like% ".*(1).*(1).*")]
  return(datR)
}

# Vector of plot Titles for the graphs
plotTitle = c("Auto MPG","Bike Sharing","Computer Hardware","Concrete","Electric Grid","Energy Effeciency","Forest Fires", "Optical","Protein Tertiary","Wine Quality")

## Running all the linear regressions for the 10 datasets
for(ind in 1:10){
  i = vecData[ind]
  ## Linear, Ridge, Lasso
  # forward selection for the data sets
  a = summary(regsubsets(i[[1]][,ncol(i[[1]])]~.,data=i[[1]][,-ncol(i[[1]])], nvmax = 10000))
  # r is the matrix of r-sq values
  r = crossVal(a$which, i[[1]], "lm")
  print(r)
  # creates data frame of the r-sq values with names
  df = data.frame(x = rep(1:length(r$r2), times = 3), val = c(r$r2,r$r2A,r$cvR), type = rep(c("R-Sq", "R-Sq Adj", "CV R-Sq"), each = length(r$r2)))
  # creates plot of the data frame
  plot = ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=type)) + ggtitle(paste("Linear Regression: ",plotTitle[ind])) +
    xlab("Number of Variables") + ylab("R-Squared")
  print(plot)
  # ridge
  r = crossVal(a$which, i[[1]], "ridge")
  print(r)
  df = data.frame(x = rep(1:length(r$r2), times = 3), val = c(r$r2,r$r2A,r$cvR), type = rep(c("R-Sq", "R-Sq Adj", "CV R-Sq"), each = length(r$r2)))
  plot = ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=type)) + ggtitle(paste("Ridge Regression: ",plotTitle[ind])) +
    xlab("Number of Variables") + ylab("R-Squared")
  print(plot)
  #lasso
  r = crossVal(a$which, i[[1]], "lasso")
  print(r)
  df = data.frame(x = rep(1:length(r$r2), times = 3), val = c(r$r2,r$r2A,r$cvR), type = rep(c("R-Sq", "R-Sq Adj", "CV R-Sq"), each = length(r$r2)))
  plot = ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=type)) + ggtitle(paste("Lasso Regression: ",plotTitle[ind])) +
    xlab("Number of Variables") + ylab("R-Squared")
  print(plot)
  i = vecData2[ind]
  ## Quad and Respinse Surface(commented out)
  #i = quadCols(i[[1]])
  #a = summary(regsubsets(i[,ncol(i)]~.,data=i[,-ncol(i)], nvmax = 10000))
  #r = crossVal(a$which, i, "lm")
  #print(r)
  #df = data.frame(x = rep(1:length(r$r2), times = 3), val = c(r$r2,r$r2A,r$cvR), type = rep(c("R-Sq", "R-Sq Adj", "CV R-Sq"), each = length(r$r2)))
  #plot = ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=type)) + ggtitle(paste("Quad Regression: ",plotTitle[ind])) +
  #  xlab("Number of Variables") + ylab("R-Squared")
  #print(plot)
  #i = resSurface(vecData2[ind])
  #a = summary(regsubsets(i[,ncol(i)]~.,data=i[,-ncol(i)], nvmax = 10000))
  #r = crossVal(a$which, i, "lm")
  #print(r)
  #df = data.frame(x = rep(1:length(r$r2), times = 3), val = c(r$r2,r$r2A,r$cvR), type = rep(c("R-Sq", "R-Sq Adj", "CV R-Sq"), each = length(r$r2)))
  #plot = ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=type)) + ggtitle(paste("Quad Regression: ",plotTitle[ind])) +
  #  xlab("Number of Variables") + ylab("R-Squared")
  #print(plot)
}

