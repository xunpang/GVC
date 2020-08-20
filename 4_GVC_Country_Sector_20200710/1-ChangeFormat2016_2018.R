WD<-getwd()
if(!is.null(WD))setwd("/Users/sandy/Documents/DataCode/ICIO/") #chang path to your own
getwd()

library(dplyr)
library(tidyr)

#######################################################################################################################################
############################################# Change 2018 data firstly ################################################################
#######################################################################################################################################


############# Read Data 2018 version #################
m<-2005

for (m in 2005:2015) {
  
  filename<-paste("/Users/sandy/Documents/DataCode/ICIO/2018/EXGR_BSCI/","EXGR_BSCI_",m,".csv",sep = "")  #change path to your own
  ValueAdded<-read.csv(filename,sep = '|', header = FALSE)

  #ValueAdded<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2018/EXGR_BSCI/EXGR_BSCI_2005.csv",sep = '|', header = FALSE)
  names(ValueAdded)[1:6]<-c("Source.Country","Source.Industry","Exporting.Country","Exporting.Industry","Year","Value.Added")
  ValueAdded$Source.Country<-as.character(ValueAdded$Source.Country)
  ValueAdded$Source.Industry<-as.character(ValueAdded$Source.Industry)
  ValueAdded$Exporting.Country<-as.character(ValueAdded$Exporting.Country)
  ValueAdded$Exporting.Industry<-as.character(ValueAdded$Exporting.Industry)

  ######################### only include 64 countries plus "the rest of world" and 36 industries, delete all others ####################
  ValueAddedBasic<-ValueAdded
  Region<-c("OECD","NONOECD","APEC","ASEAN","EASIA","EU28","EU15","EU13","EA19","EA12","G20","ZEUR","ZASI","ZNAM","ZOTH","ZSCA","WLD","DXD")
  IndustryAggregation<-c("D05T09","D10T33","D16T18","D19T23","D24T25","D26T27","D29T30","D45T82","D45T56","D58T63","D84T98","D84T88","D90T98","D05T39","D45T98","D58T82","D41T98","DINFO","DTOTAL")

  ValueAddedBasic<-ValueAddedBasic[-which(ValueAddedBasic$Source.Country %in% Region | ValueAddedBasic$Exporting.Country %in% Region),]
  ValueAddedBasic<-ValueAddedBasic[-which(ValueAddedBasic$Source.Industry %in% IndustryAggregation | ValueAddedBasic$Exporting.Industry %in% IndustryAggregation),]

  ValueAddedBasic$Source.Country.Industry<-paste(ValueAddedBasic$Source.Country,ValueAddedBasic$Source.Industry,sep = '_')
  ValueAddedBasic$Exporting.Country.Industry<-paste(ValueAddedBasic$Exporting.Country,ValueAddedBasic$Exporting.Industry,sep = '_')

  rownames(ValueAddedBasic)<-c(1:nrow(ValueAddedBasic))

  #################### change current data format into a matrix ########################
  ValueAddedBasic<-ValueAddedBasic[order(ValueAddedBasic$Exporting.Country.Industry,ValueAddedBasic$Source.Country.Industry),]
  rownames(ValueAddedBasic)<-c(1:nrow(ValueAddedBasic))

  #change long data into wide data
  ValueAddedNew<-ValueAddedBasic[,-c(1:5)]
  ValueAdded_Wide<-spread(ValueAddedNew,Exporting.Country.Industry,Value.Added)
  rownames(ValueAdded_Wide)<-ValueAdded_Wide$Source.Country.Industry
  ValueAdded_Wide<-ValueAdded_Wide[,-1]

  #check whether the order of ColNames is same with RowNames
  sum(colnames(ValueAdded_Wide)==rownames(ValueAdded_Wide))

  filename<-paste("/Users/sandy/Documents/DataCode/ICIO/2018_Wide/","EXGR_BSCI_",m,"_Wide.csv",sep = "")  #change path to your own
  write.csv(ValueAdded_Wide,file =filename )
}


#######################################################################################################################################
############################################# Change 2016 data secondly (Origin)#######################################################
#######################################################################################################################################
n<-1995

for (n in 1995:2011) {
  
  filename<-paste("/Users/sandy/Documents/DataCode/ICIO/2016/","EXGR_BSCI_",n,".csv",sep = "")  #change path to your own
  ValueAdded<-read.csv(filename,sep = '|', header = FALSE)
  
  names(ValueAdded)[1:6]<-c("Source.Country","Source.Industry","Exporting.Country","Exporting.Industry","Year","Value.Added")
  ValueAdded$Source.Country<-as.character(ValueAdded$Source.Country)
  ValueAdded$Source.Industry<-as.character(ValueAdded$Source.Industry)
  ValueAdded$Exporting.Country<-as.character(ValueAdded$Exporting.Country)
  ValueAdded$Exporting.Industry<-as.character(ValueAdded$Exporting.Industry)
  
  ######################### only include 63 countries plus "the rest of world" and 34 industries, delete all others ####################
  ValueAddedBasic<-ValueAdded
  Region<-c("OECD","NONOECD","APEC","ASEAN","EASIA","EU28","EU15","EU13","EA18","EA12","ZEUR","ZASI","NAFTA","ZOTH","ZSCA","WOR","DXD")
  IndustryAggregation<-c("C15T37","C20T22","C23T26","C27T28","C30T33","C34T35","C50T74","C50T55","C60T64","C70T74","C75T95",
                         "C10T41","C45T95","C50T95","C50T64","C65T74","CTOTAL")
  
  
  ValueAddedBasic<-ValueAddedBasic[-which(ValueAddedBasic$Source.Country %in% Region | ValueAddedBasic$Exporting.Country %in% Region),]
  ValueAddedBasic<-ValueAddedBasic[-which(ValueAddedBasic$Source.Industry %in% IndustryAggregation | ValueAddedBasic$Exporting.Industry %in% IndustryAggregation),]
  
  ValueAddedBasic$Source.Country.Industry<-paste(ValueAddedBasic$Source.Country,ValueAddedBasic$Source.Industry,sep = '_')
  ValueAddedBasic$Exporting.Country.Industry<-paste(ValueAddedBasic$Exporting.Country,ValueAddedBasic$Exporting.Industry,sep = '_')
  
  rownames(ValueAddedBasic)<-c(1:nrow(ValueAddedBasic))
  
  #################### change current data format into a matrix ########################
  ValueAddedBasic<-ValueAddedBasic[order(ValueAddedBasic$Exporting.Country.Industry,ValueAddedBasic$Source.Country.Industry),]
  rownames(ValueAddedBasic)<-c(1:nrow(ValueAddedBasic))
  
  #change long data into wide data
  ValueAddedNew<-ValueAddedBasic[,-c(1:5)]
  ValueAdded_Wide<-spread(ValueAddedNew,Exporting.Country.Industry,Value.Added)
  rownames(ValueAdded_Wide)<-ValueAdded_Wide$Source.Country.Industry
  ValueAdded_Wide<-ValueAdded_Wide[,-1]
  
  #check whether the order of ColNames is same with RowNames
  sum(colnames(ValueAdded_Wide)==rownames(ValueAdded_Wide))
  
  filename<-paste("/Users/sandy/Documents/DataCode/ICIO/2016_Wide/","EXGR_BSCI_",n,"_Wide.csv",sep = "")  #change path to your own
  write.csv(ValueAdded_Wide,file =filename )
  
}













