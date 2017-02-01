library(maps)
library(mapdata)
library(maptools) #for shapefiles
library(scales)   #for transparency
library(raster)

#Load shape file for states (Mostly from Kat's work)
crswgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
states=readShapePoly("/Users/davehuf/Desktop/Mexstates/mexstates.shp",proj4string=crswgs84,verbose=TRUE)

# Load tiles of elevation graphics (Mostly from Kat's work)
block1 <- getData('SRTM', lon=-105, lat=25)
block2 <- getData('SRTM', lon=-100, lat=20)
block3 <- getData('SRTM', lon=-95, lat=20)
block4 <- getData('SRTM', lon=-90, lat=20)
block5 <- getData('SRTM', lon=-110, lat=25)
block6 <- getData('SRTM', lon=-100, lat=25)
block7 <- getData('SRTM', lon=-95, lat=25)
block8 <- getData('SRTM', lon=-90, lat=25)
block9 <- getData('SRTM', lon=-120, lat=30)
block10 <- getData('SRTM', lon=-115, lat=30)
block11 <- getData('SRTM', lon=-105, lat=20)
block12 <- getData('SRTM', lon=-110, lat=30)
block13 <- getData('SRTM', lon=-100, lat=30)
block14 <- getData('SRTM', lon=-115, lat=35)
block15 <- getData('SRTM', lon=-110, lat=35)
block16 <- getData('SRTM', lon=-115, lat=25)
block17 <- getData('SRTM', lon=-105, lat=30)
block18 <- getData('SRTM', lon=-120, lat=35)
block19 <- getData('SRTM', lon=-105, lat=35)

#Mash tiles into one graphic (Mostly from Kat's work)
altitudes <- mosaic(block1, block2, block3, block4, block5, block6, block7, block8, block9, block10, block11, block12, block13, block14, block15, block16, block17, block18, block19, fun=mean)

#create my own color palette which is greyscale
greyscale = c("gray100", "gray99", "gray98", "gray97", "gray96", "gray95", "gray94", "gray93", "gray92", "gray91", "gray90", "gray89", "gray87", "gray86", "gray85", "gray84", "gray83", "gray82", "gray81", "gray80", "gray79", "gray78", "gray77", "gray76", "gray75", "gray74", "gray73", "gray72", "gray71", "gray70", "gray69", "gray68", "gray67", "gray66", "gray65", "gray64", "gray63", "gray62", "gray61", "gray60", "gray59", "gray58", "gray57", "gray56", "gray55", "gray54", "gray53", "gray52", "gray51", "gray50", "gray49", "gray48", "gray47", "gray46", "gray45", "gray44", "gray43", "gray42", "gray41", "gray40", "gray39", "gray38", "gray37", "gray36", "gray35", "gray34", "gray33", "gray32", "gray31", "gray30", "gray29", "gray28", "gray27", "gray26", "gray25", "gray24", "gray23", "gray22", "gray21", "gray20", "gray19", "gray18", "gray17", "gray16", "gray15", "gray14", "gray13", "gray12", "gray11", "gray10", "gray9", "gray8", "gray7", "gray6", "gray5", "gray4", "gray3", "gray2", "gray1", "gray0")


#set directory, import data, and set colors for plotting
setwd("/Users/davehuf/Hufford Lab/14/1_ParvMexAdmixtureProject/3_RedoHybridDetermination/4_Dec2016HybridPlots")
data = as.matrix(read.table("ZeaAllInfo.14col.noMML", sep="\t"))
cnt = 1
for (rowNum in 1:nrow(data)){
	tax = data[rowNum,2]
	hyb = data[rowNum,4]
	if (tax == "Zea_mays_parviglumis"){
		data[cnt,2] = 22
		if (hyb == "parvHC"){
			data[cnt,3] = "blue2" 
		}		
		else if (hyb == "hybrid"){
			data[cnt,3] = "yellowgreen"
		}
		else{
			data[cnt,3] = "cyan" 
		}
	}
	
	else if (tax == "Zea_mays_mexicana"){
		data[cnt,2] = 24
		#print(2)
		if (hyb == "mexHC"){
			data[cnt,3] = "firebrick4"
		}		
		else if (hyb == "hybrid"){
			data[cnt,3] = "yellowgreen"
		}
		else{
			data[cnt,3] = "firebrick1" 
		}
	}
	
	else if (tax == "Zea_mays_mays"){
		#print(3)
		data[cnt,2] = 21
		data[cnt,3] = "gray38"
	}
	
	else{
		print("ERROR1 !")	
	}
	
	cnt = cnt + 1
}


#First make big and small plots
data = data.frame(data) #convert data to a dataframe otherwise points gets mad
plot.new()
pdf("AllDataBig.pdf")
#plot Mexico
map("worldHires","Mexico", col="black", fill=FALSE, lwd=0.7, xlim=c(-117,-83))
#plot altitude
plot(altitudes, add=TRUE, col=greyscale)
#plot states
plot(states, lwd=0.5, add=TRUE)
#plot samples
points(as.numeric(as.character(data$V7)), as.numeric(as.character(data$V6)), pch=as.numeric(as.character(data$V2)), col=as.character(data$V3), cex=.45) 
dev.off()

pdf("AllDataSmall.pdf")
#plot Mexico
map("worldHires","Mexico", col="black", fill=FALSE, lwd=0.7, xlim=c(-107,-84), ylim=c(15,24.5))
#plot altitude
plot(altitudes, add=TRUE, col=greyscale)
#plot states
plot(states, lwd=0.5, add=TRUE)
#plot samples
points(as.numeric(as.character(data$V7)), as.numeric(as.character(data$V6)), pch=as.numeric(as.character(data$V2)), col=as.character(data$V3), cex=.6)
legend(list(x=-96,y=24), c("parvHC","parvAmb", "parvHyb","mexHC","mexAmb","mexHyb","maize"), col=c("blue2","cyan","yellowgreen","firebrick4","firebrick1","yellowgreen","gray38"), pch=c(0,0,0,2,2,2,1), lwd=2, lty=0, y.intersp=1.4)
dev.off()


