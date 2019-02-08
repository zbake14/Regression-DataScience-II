
## Reading in the datasets
autoMPG = read.csv(file = "RawData/AutoMPG.csv")

bikeSharing = read.csv(file = "RawData/Bike Sharing.csv",
                       colClasses = c('factor','integer','factor','integer','factor','integer','factor','numeric','numeric','numeric','numeric','numeric')
                       )

compHardware = read.csv(file = "RawData/Computer Hardware.csv")
compHardware$vendor = as.factor(compHardware$vendor)

elecGridStab = read.csv(file = "RawData/Electrical Grid Stability.csv")

energyEff = read.csv(file = "RawData/Energy Effeciency.csv")

forestFire = read.csv(file = "RawData/forestfires.csv")
forestFire$month = as.factor(forestFire$month)
forestFire$day = as.factor(forestFire$day)

naval = read.csv(file = "RawData/Naval.csv")

optical = read.csv(file = "RawData/Optical.csv",
                   colClasses = c('integer','integer','factor','integer','numeric','numeric','numeric','numeric')
                  )

protein = read.csv(file = "RawData/Protein Tertiary.csv")

wine = read.csv(file = "RawData/winequality.csv")

vecData = list(autoMPG,bikeSharing,compHardware,elecGridStab,energyEff,forestFire,naval,optical,protein,wine)


for(i in vecData){
  print(i[1,])
}


