library(maps)
library(mapdata)
library(maptools) #for shapefiles
library(mapplots) #for pie charts
library(scales)   #for transparency
library(raster)

#Load shape file for states (Mostly from Kat's work)
crswgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
states=readShapePoly("/Users/davehuf/Desktop/Mexstates/mexstates.shp",proj4string=crswgs84,verbose=TRUE)

setwd("/Users/davehuf/Hufford Lab/14/1_ParvMexAdmixtureProject/3_RedoHybridDetermination/4_Dec2016HybridPlots")

data = as.matrix(read.table("ZeaAllInfo.14col.noMML.short", sep="\t"))
df = data.frame(data)
Hybs = subset(df, V2=="hybrid")

plot.new()
#plot big
##plot background
pdf("HybridsBig.pdf")
map("worldHires","Mexico", col="black", fill=FALSE, lwd=0.7)
plot(states, lwd=0.7, add=TRUE)

#plot pie charts
for (cnt in seq(1,nrow(Hybs))) {
add.pie(z=c(as.numeric(as.character(Hybs$V5[cnt])),as.numeric(as.character(Hybs$V6[cnt])),as.numeric(as.character(Hybs$V7[cnt]))), x=as.numeric(as.character(Hybs$V4[cnt])), y=as.numeric(as.character(Hybs$V3[cnt])), labels=NA, radius=.23, border=.4, col=c("blue2","firebrick4","gray38"))
}
dev.off()


#plot small
pdf("HybridsSmall.pdf")
map("worldHires","Mexico", col="black", fill=FALSE, lwd=0.7, xlim=c(-103,-84), ylim=c(15,22))
plot(states, lwd=0.6, add=TRUE)

#plot pie charts
for (cnt in seq(1,nrow(Hybs))) {
add.pie(z=c(as.numeric(as.character(Hybs$V5[cnt])),as.numeric(as.character(Hybs$V6[cnt])),as.numeric(as.character(Hybs$V7[cnt]))), x=as.numeric(as.character(Hybs$V4[cnt])), y=as.numeric(as.character(Hybs$V3[cnt])), labels=NA, radius=.09, border=.4, col=c("blue2","firebrick4","gray38"))
}

#add legend
legend(list(x=-96,y=21.2), c("Parv attr","Mex attr","Maize attr"), col=c("blue2","firebrick4","gray38"), pch=c(19,19,19), lwd=2, lty=0, y.intersp=0.7)
dev.off()


