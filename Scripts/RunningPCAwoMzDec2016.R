#Created by David E. Hufnagel on Mar 16, 2016 based on RunningPCAwMaize.R
#Parts were taken from Ranal.R written by Joost Van Heervaarden.
#Meant for making plots without maize

setwd("/Users/davehuf/Hufford Lab/16/9_ReReRedoPCA/1_RedoneWNewColorsDec2016")
data = read.table("ZeaAllInfo.14col.OrigForm.PCAmatrix.woMz", row.names=1, skip=1) #SNP file used to subset
info = read.table("ZeaAllInfo.14col.noMML", sep="\t", row.names=1) #big info file

#defining a function for all SNPs of a marker.  Homozygous high freq allele returns 2, heterozygous returns 1, homozygous alternate allele returns 0, any missing data returns NA.
fcdat<-function(x){
	x<-as.character(x)                 
	vec<-unlist(strsplit(x,split="/")) 
	avec<-vec[seq(1,length(vec),2)]    
	bvec<-vec[seq(2,length(vec),2)]    
	nucfreq<-sort(table(c(avec,bvec)),decreasing=TRUE) 
	nucfreq<-nucfreq[names(nucfreq)!="?"] 
	base1=names(nucfreq)[1]
	base2=names(nucfreq)[2]
	avec[avec=="?"]=NA 
	bvec[bvec=="?"]=NA
	avec[avec==base1&is.na(avec)==FALSE]<-"S" 
	avec[avec==base2&is.na(avec)==FALSE]<-"N" 
	bvec[bvec==base1&is.na(bvec)==FALSE]<-"S"
	bvec[bvec==base2&is.na(bvec)==FALSE]<-"N"
	avec[avec=="S"]<-1 
	avec[avec=="N"]<-0
	bvec[bvec=="S"]<-1
	bvec[bvec=="N"]<-0
	vec<-as.numeric(avec)+as.numeric(bvec)
	return(vec)
}

#apply the function and set the rownames back to the individual names
data2 = apply(data,2,fcdat)
rownames(data2) = rownames(data)

#make vectors of column means and variance factors, sqrt(p*(1-p))
mv = apply(data2,2,mean,na.rm=TRUE)
p = mv/2
vv = sqrt(p*(1-p))
#take out monomorphic loci
data2 = data2[,vv>0]

#Set colors to define populations by making a matrix of parv and mex names as rownames and colors in the only column.
spec = info$V2
hyb = info$V4
pop = info$V5
lat = info$V6
lon = info$V7
cntry = info$V8
names = rownames(info)
cols = c() #colors
shapes = c()
for(i in seq(1:nrow(info))){
	cc = "black" #color code
	sh = 11 #shape code
	if(spec[i] == "Zea_mays_parviglumis" && !is.na(spec[i])){ #Dont accept missing values as a match
		sh = 0
		if(hyb[i] == "parvHC"){
			cc = "blue2"
		}
		else if(hyb[i] == "other"){
			cc = "cyan"
		}
	}
	else if(spec[i] == "Zea_mays_mexicana" && !is.na(spec[i])){
		sh = 2
		if(hyb[i] == "mexHC"){
			cc = "firebrick4"
		}
		else if(hyb[i] == "other"){
			cc = "firebrick1"
		}
	}
	else{
		cc = "black" #Other
	}
	
	#If hybrid reset to a hybrid group value
	if((hyb[i] == "hybrid" && !is.na(hyb[i]))){ 
		sh = 8
		if(pop[i] == "Central_Plateau" && !is.na(pop[i])){
			cc = "yellow1" #CP hybrids
		}
		else if(pop[i] == "North_Balsas" && !is.na(pop[i])){
			cc = "orange4" #NB hybrids
		}
		else if(pop[i] == "South_Balsas" && !is.na(pop[i])){
			cc = "wheat3" #SB hybrids
		}
		else if(lat[i] > 18.2 && lat[i] < 18.3 && lon[i] > -99.2 && lon[i] < -99.1 && !is.na(lat[i]) && !is.na(lon[i])){
			cc = "seagreen"#cols = c(cols, "violetred1") #Ahuacatitlan
		}
		else{ #non-grouped hybrids
			cc = "yellowgreen"
		}
	}
	else if((hyb[i] == "other" && !is.na(hyb[i]))){
		
	}
	
	#put cc and sh in list
	if(cc != "black"){
		cols = c(cols,cc)
		names(cols)[length(cols)] = names[i]
		
		shapes = c(shapes,as.numeric(sh))
		names(shapes)[length(shapes)] = names[i]
	}
}

#adding the colors and shapes to the data so they are all in 1 matrix
colM = matrix(cols, length(cols), 1)
shapesM = matrix(shapes, length(shapes), 1)
rownames(colM) = names(cols)
rownames(shapesM) = names(shapes)
data3 = matrix(NA, nrow=nrow(colM), ncol=ncol(data2)+2) #create an empty list of the proper size
cnt = 1
for(i in seq(1:nrow(data2))){
	dName = rownames(data2)[i]
	if(dName %in% rownames(colM)){
		data3[cnt,] = c(data2[i,], shapesM[dName,], colM[dName,])
		cnt = cnt + 1
	}
}
rownames(data3) = names(cols)

data3num = apply(data3, 2, as.numeric, na.rm=TRUE)[,1:ncol(data3)-1]
mv = apply(data3num,2,mean,na.rm=TRUE) #ignore last column
p = mv/2
vv = sqrt(p*(1-p))

#Handle missing data
StNor = t((t(data3num)-mv)/vv) #Converting each marker's data to a standard normal distribution
StNor[is.na(StNor)]<-0 #Set missing data to the mean of the standard normal (0)

#Run PCA, keep any values above 1% total variance explained
newpc = prcomp(StNor)

################
# Plot results
pdf("PC1vPC2.pdf")
plot(newpc$x[,2]~newpc$x[,1],col=data3[,ncol(data3)], pch=as.numeric(data3[,ncol(data3)-1]), ylab="PC 2   (2.10% var. explained)",xlab="PC 1   (6.48% var. explained)")
dev.off()

pdf("PC3vPC4.pdf")
plot(newpc$x[,4]~newpc$x[,3],col=data3[,ncol(data3)], pch=as.numeric(data3[,ncol(data3)-1]) ,ylab="PC 4   (1.08% var. explained)",xlab="PC 3   (1.28% var. explained)")
dev.off()

pdf("PC5vPC6.pdf")
plot(newpc$x[,6]~newpc$x[,5],col=data3[,ncol(data3)], pch=as.numeric(data3[,ncol(data3)-1]) ,ylab="PC 6   (0.91% var. explained)",xlab="PC 5   (0.98% var. explained)")
dev.off()

pdf("PC7vPC8.pdf")
plot(newpc$x[,8]~newpc$x[,7],col=data3[,ncol(data3)], pch=as.numeric(data3[,ncol(data3)-1]) ,ylab="PC 8   (0.76% var. explained)",xlab="PC 7   (0.86% var. explained)")
dev.off()

pdf("PC9vPC10.pdf")
plot(newpc$x[,10]~newpc$x[,9],col=data3[,ncol(data3)], pch=as.numeric(data3[,ncol(data3)-1]) ,ylab="PC 10   (0.69% var. explained)",xlab="PC 9   (0.69% var. explained)")
dev.off()

