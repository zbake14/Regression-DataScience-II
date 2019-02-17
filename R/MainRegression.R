setwd("/Users/zachbaker/Documents/Senior Year/Spring Semester/DS2/Regression-DataScience-II/R/")
library(ggplot2)

## Reading in the datasets
autoMPG = read.csv(file = "RawData/AutoMPG.csv")

bikeSharing = read.csv(file = "RawData/Bike Sharing.csv",
                       colClasses = c('factor','integer','factor','integer','factor','factor','numeric','numeric','numeric','numeric','numeric')
                       )

compHardware = read.csv(file = "RawData/Computer Hardware.csv")
compHardware$vendor = as.factor(compHardware$vendor)

elecGridStab = read.csv(file = "RawData/Electrical Grid Stability.csv")

energyEff = read.csv(file = "RawData/Energy Effeciency.csv")

forestFire = read.csv(file = "RawData/forestfires.csv")
forestFire$month = as.factor(forestFire$month)
forestFire$day = as.factor(forestFire$day)

#naval = read.csv(file = "RawData/Naval.csv")

optical = read.csv(file = "RawData/Optical.csv",
                   colClasses = c('integer','integer','factor','integer','numeric','numeric','numeric','numeric')
                  )

protein = read.csv(file = "RawData/Protein Tertiary.csv")

wine = read.csv(file = "RawData/winequality.csv")

vecData = list(autoMPG,bikeSharing,compHardware,elecGridStab,energyEff,forestFire,optical,protein,wine)


r2 = c()
r2A = c()
count = 0
for(i in vecData){
  a = summary(regsubsets(i[,ncol(i)]~.,data=i[,-ncol(i)], nvmax = 10000))
  r2 = a$rsq
  r2A = a$adjr2
  df = data.frame(x = rep(1:length(r2), times = 2), val = c(r2,r2A), type = rep(c("R-Sq", "R-Sq Adj"), each = length(r2)))
  plot = ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=type))
  print(plot)
  count = count +1
}

a = summary(regsubsets(quality~., data=wine))
a$r.squared
a$adj.r.squared




df <- data.frame(x=rep(1:5, 9), val=sample(1:100, 45), 
                 variable=rep(paste0("category", 1:9), each=5))
# plot
ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=variable))


