"""
This script is designed to take a 14col input and make a hierfstat output 
for the purpose of calculating Fst for the purpose of selecting parents for CH2.
This means that we will create one output for all hybrids vs all potential parent
populations and one for each hybrid group vs all potential parent populations.
Created by David E. Hufnagel on June 1, 2016
Updated to fix errors on Aug 23-26, 2016 Also, the grouped output was removed 
  because only 1 hybrid group is represented in the dataset because it was 
  thresholded by number of individuals per population
"""

import sys
inp = open(sys.argv[1])             #ZeaAllInfo.14col.noMML.mod for most info
extra = open(sys.argv[2])           #genotypes_filtered.txt for parv and mex group info
outAllHyb = open(sys.argv[3], "w")  #hierFstat output all hyb vs each parv/mex pop
#outGroups = open(sys.argv[4], "w")  #hierFstat output each hyb group vs each parv/mex pop
popInfoA = open(sys.argv[4], "w")   #output file to connect pop nums with related info for all vs pops
#popInfoG = open(sys.argv[6], "w")   #output file to connect pop nums with related info for groups vs pops


#Make title line in output
inp.readline()
markers = inp.readline().strip().split("\t")[-1].split(":")[1].split(",")
newLine = "pop\tindiv\t%s\n" % ("\t".join(markers))
outAllHyb.write(newLine)
#outGroups.write(newLine)

#Make a dict of key: name val: group
extra.readline()
grpDict = {}
for line in extra:
    lineLst = line.strip().split("\t")
    name = lineLst[0]; grp = lineLst[2]
    grpDict[name] = grp

#Make other lines in output
indNum = 0
allCnt = 2 #pop num counter for all hyb vs pops
#grpCnt = 4 #pop num counter for hyb groups vs pops
accDictA = {} #list of population values associated with accessions in the dataset for All v pops
#accDictG = {} #for groups
for line in inp:
    lineLst = line.strip().split("\t")
    name = lineLst[0];tax = lineLst[1];acc = "%s__%s-%s" % (lineLst[2],lineLst[5],lineLst[6]) 
    hyb = lineLst[3];pop = lineLst[4];cntry = lineLst[7]; grp = grpDict[name] 
    
    #In order to lump together Mex populations into their races
    if tax == "Zea_mays_mexicana":
        acc = grp
    
    popNumAll = -9
    #popNumGrp = -9

    if (tax == "Zea_mays_parviglumis" or tax == "Zea_mays_mexicana"):
        #Determine popNum
        if hyb == "hybrid":
            popNumAll = 1
#            if pop == "South_Balsas":
#                popNumGrp = 3
#            elif pop == "North_Balsas":
#                popNumGrp = 2
#            elif pop == "Central_Plateau":
#                popNumGrp = 1
        else:
            #sys.exit()   
            #if grp == "ZMXCP" or grp == "ZMPBA":# and hyb == "mexHC":
            if not acc in accDictA:
                accDictA[acc] = allCnt
                popNumAll = allCnt
                allCnt += 1
            else:
                popNumAll = accDictA[acc]
                
#                if not acc in accDictG:
#                    accDictG[acc] = grpCnt
#                    popNumGrp = grpCnt
#                    grpCnt += 1
#                else:
#                    popNumGrp = accDictG[acc]             

        #Determine genos
        genos = lineLst[-1].split(",")
        newGenos = []
        for pair in genos:
            #replace ATCG with 1234
            newPair = (pair[0] + pair[2]).replace("A","1").replace("T","2").replace("C","3").replace("G","4").replace("NN","NA")         
            newGenos.append(newPair)
        newGenos = "\t".join(newGenos)
        
        #Determine indNum        
        indNum += 1
        
        #Output lines
        if popNumAll != -9:
            newLineA = "%s\t%s\t%s\n" % (popNumAll, indNum, newGenos)
            outAllHyb.write(newLineA)
#        if popNumGrp != -9:
#            newLineG = "%s\t%s\t%s\n" % (popNumGrp, indNum, newGenos)
#            outGroups.write(newLineG)

#Output to popInfoA
popInfoA.write("1\thybrid\n")
for k,v in accDictA.items():
    newLine = "%s\t%s\n" % (v,k)
    popInfoA.write(newLine)
    
#Output to popInfoG
#print(len(accDictA))
#popInfoG.write("1\tCP_hybrid\n")
#popInfoG.write("2\tCB_hybrid\n")
#popInfoG.write("3\tSG_hybrid\n")
#for k,v in accDictG.items():
#    newLine = "%s\t%s\n" % (v,k)
#    popInfoG.write(newLine)

inp.close()
extra.close()
outAllHyb.close()
#outGroups.close()
popInfoA.close()
#popInfoG.close()