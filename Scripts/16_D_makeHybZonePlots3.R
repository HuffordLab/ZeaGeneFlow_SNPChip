#used https://cran.r-project.org/web/packages/dismo/vignettes/sdm.pdf for guidance
#just like script version 2 except, this holds the data for all groups and makes a box plot
library(maps)
library(mapdata)
library(maptools) #for shapefiles
library(scales)   #for transparency
library(dismo)

#import coord data
data = read.table("ZeaAllInfo.14col.noMML", header=FALSE, sep='\t')
dataHyb = data[,c(5,7,6)]
dataPM = data[,c(2,4,7,6)]
dataP = subset(dataPM, V2=="Zea_mays_parviglumis" & V4!="hybrid")
dataM = subset(dataPM, V2=="Zea_mays_mexicana" & V4!="hybrid")
dataCP = subset(dataHyb, V5=="Central_Plateau")
dataSB = subset(dataHyb, V5=="South_Balsas")
dataNB = subset(dataHyb, V5=="North_Balsas")

#Seperate out groups to compare across environmental variables
dataP2 = dataP[,c(-1,-2)]
dataM2 = dataM[,c(-1,-2)]
dataCP2 = dataCP[,-1]
dataSB2 = dataSB[,-1]
dataNB2 = dataNB[,-1]

#import altitudes
dataPMAlt = data[,c(2,4,10)]
dataHybAlt = data[,c(5,10)]
altP = subset(dataPMAlt, V2=="Zea_mays_parviglumis" & V4!="hybrid")
altM = subset(dataPMAlt, V2=="Zea_mays_mexicana" & V4!="hybrid")
altCP = subset(dataHybAlt, V5=="Central_Plateau")
altSG = subset(dataHybAlt, V5=="South_Balsas")
altCB = subset(dataHybAlt, V5=="North_Balsas")

#extract environmental data
files = list.files(path=paste(system.file(package="dismo"),  '/ex', sep=''), pattern='grd', full.names=TRUE )
predictors = stack(files)
envP = extract(predictors, dataP2)
envM = extract(predictors, dataM2)
envCP = extract(predictors, dataCP2)
envSG = extract(predictors, dataSB2)
envCB = extract(predictors, dataNB2)

#make box plots for altitude
titlesP = rep("Parviglumis",nrow(altP))
titlesM = rep("Mexicana",nrow(altM))
titlesCP = rep("Hyb CP",nrow(altCP))
titlesSG = rep("Hyb SG",nrow(altSG))
titlesCB = rep("Hyb CB",nrow(altCB))

altPboxData = matrix(c(titlesP,altP[,3]), ncol=2)
altMboxData = matrix(c(titlesM,altM[,3]), ncol=2)
altCPboxData = matrix(c(titlesCP,altCP[,2]), ncol=2)
altSGboxData = matrix(c(titlesSG,altSG[,2]), ncol=2)
altCBboxData = matrix(c(titlesCB,altCB[,2]), ncol=2)

altAllData = rbind(altPboxData, altMboxData, altCPboxData, altSGboxData, altCBboxData)
boxplot(as.numeric(altAllData[,2]) ~ altAllData[,1], main="Altitudes of Hybrid, Parviglumis, and Mexicana Sampling Sites", ylab="altitude (meters above sea level)")

#make box plots for annual precipitation
prePboxData = matrix(c(titlesP,envP[,2]), ncol=2)
preMboxData = matrix(c(titlesM,envM[,2]), ncol=2)
preCPboxData = matrix(c(titlesCP,envCP[,2]), ncol=2)
preSGboxData = matrix(c(titlesSG,envSG[,2]), ncol=2)
preCBboxData = matrix(c(titlesCB,envCB[,2]), ncol=2)
preAllData = rbind(prePboxData, preMboxData, preCPboxData, preSGboxData, preCBboxData)
boxplot(as.numeric(preAllData[,2]) ~ preAllData[,1], main="Annual Precipitation of Hybrid, Parviglumis,\n and Mexicana Sampling Sites", ylab="annual precipitation (mm)")

#make box plots for annual mean temperature
tempPboxData = matrix(c(titlesP,envP[,1]), ncol=2)
tempMboxData = matrix(c(titlesM,envM[,1]), ncol=2)
tempCPboxData = matrix(c(titlesCP,envCP[,1]), ncol=2)
tempSGboxData = matrix(c(titlesSG,envSG[,1]), ncol=2)
tempCBboxData = matrix(c(titlesCB,envCB[,1]), ncol=2)
tempAllData = rbind(tempPboxData, tempMboxData, tempCPboxData, tempSGboxData, tempCBboxData)
boxplot(as.numeric(tempAllData[,2]) ~ tempAllData[,1], main="Annual Mean Temperature of Hybrid, Parviglumis,\n and Mexicana Sampling Sites", ylab="annual mean temperature (deg Celcius * 10)")


