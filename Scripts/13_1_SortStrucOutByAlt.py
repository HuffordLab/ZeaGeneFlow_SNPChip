#This script is designed to take a disctruct input file and sort individuals
#by altitude.
#Created by David E. Hufnagel on Jan 17, 2014
import sys

inp = open(sys.argv[1])         #unsorted disctruct input file (*.indivq)
out = open(sys.argv[2], "w")    #sorted disctruct inputfile
info = open(sys.argv[3])        #file containing altitude info on col 20 #jg-otu.txt 
altOut = open(sys.argv[4], "w") #the altitudes in order


def RemoveEmpty(listx):
    newList = []
    for item in listx:
        if not item == "":
            newList.append(item)
    return newList

#Go through inp and make a dict of key: name val: wholeLine
lineDict = {}
for line in inp:
    lineLst = RemoveEmpty(line.strip().split(" "))
    lineDict[lineLst[1]] = line
    
#Go through info and make a list of tuples with (altitude, name)
altLst = []
for line in info.readline().split("\r")[1:]:
    lineLst = line.strip().split("\t")
    name = lineLst[1]
    if name in lineDict:
        if lineLst[19] != "NA":
            alt = int(lineLst[19])
        else:
            alt = lineLst[19]
        newTup = (alt,name)
        altLst.append(newTup)
        
#sort the list by altitude
altLst.sort()

#Go through the list and output whole lines
for pair in altLst:
    name = pair[1]
    out.write(lineDict[name])

    #output to altOut
    group = RemoveEmpty(lineDict[name].split(" "))[3]
    alt = pair[0]
    newLine = "%s\t%s\t%s\n" % (name, group, alt)
    altOut.write(newLine)


inp.close()
out.close()
info.close()
altOut.close()
