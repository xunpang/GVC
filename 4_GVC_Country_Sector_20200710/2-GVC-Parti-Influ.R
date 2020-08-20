WD<-getwd()
if(!is.null(WD))setwd("/Users/sandy/Documents/DataCode/ICIO/") #change path to your own
getwd()

library(dplyr)
library(tidyr)
library(tidyverse)
library("countrycode")

##########################################################################################################################################
############################################# calculate 2016 data firstly ################################################################
##########################################################################################################################################
ICIO2016_GVCInfluence.Participation_2<-NULL
GrossExport<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2016/EXGR.csv",sep = '|',header = FALSE) #change path to your own
GrossValu<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2016/VALU.csv",sep = '|',header = FALSE) #change path to your own

i<-1995 #begin with 1995

for (i in 1995:2011) {
  
  ##read value added data 
  filename<-paste("/Users/sandy/Documents/DataCode/ICIO/2016_Wide/","EXGR_BSCI_",i,"_Wide.csv",sep = "") #change path to your own, use changed format of EXGR_BASI
  valueadd<-read.csv(filename,row.names = 1)
  
  ##calculate gross exports of each country sector
  Ycolsum<-as.matrix(colSums(valueadd))
  
  ##calculate gross export for each country--use gross export as 分母
  #gross.export<-y.colsum
  #gross.export<-as.data.frame(gross.export)
  #gross.export$country<-substring(rownames(gross.export),1,3)
  #export<-group_by(gross.export,country) %>% summarise(V1=sum(V1))
  #gross.export<-merge(gross.export,export,by="country",all.x = TRUE)
  #names(gross.export)[2:3]<-c("SectExport","CounExport")
  gross.export<-GrossExport
  gross.export<-gross.export[,-1]
  names(gross.export)[1:5]<-c("country","partner","sector","year","export")
  gross.export$country<-as.character(gross.export$country)
  gross.export$partner<-as.character(gross.export$partner)
  gross.export$sector<-as.character(gross.export$sector)
  gross.export<-gross.export[which(gross.export$year==i&gross.export$partner=="WOR"&gross.export$sector=="CTOTAL"),]
  Region<-c("OECD","NONOECD","APEC","ASEAN","EASIA","EU28","EU15","EU13","EA18","EA12","ZEUR","ZASI","NAFTA","ZOTH","ZSCA","WOR","DXD")
  gross.export<-gross.export[-which(gross.export$country %in% Region),]
  gross.export<-gross.export[order(gross.export$country),]
  gross.export<-gross.export[,-c(2,3,4)]
  gross.export<-gross.export[rep(1:nrow(gross.export),each=34),] ##34 is the number of industries in 2016 version
  
  
  ######calucate gross value added for each country sector---this one is for participation.as.supplier.sp
  gross.valu<-GrossValu
  gross.valu<-gross.valu[,-1]
  names(gross.valu)[1:5]<-c("country","partner","sector","year","value")
  gross.valu$country<-as.character(gross.valu$country)
  gross.valu$partner<-as.character(gross.valu$partner)
  gross.valu$sector<-as.character(gross.valu$sector)
  gross.valu<-gross.valu[which(gross.valu$year==i),]
  IndustryAggregation<-c("C15T37","C20T22","C23T26","C27T28","C30T33","C34T35","C50T74","C50T55","C60T64","C70T74","C75T95",
                         "C10T41","C45T95","C50T95","C50T64","C65T74","CTOTAL")
  gross.valu<-gross.valu[-which(gross.valu$sector %in% IndustryAggregation | gross.valu$country %in% Region ),]
  gross.valu<-gross.valu[order(gross.valu$country,gross.valu$sector),]
  gross.valu$value[which(gross.valu$value<0 | gross.valu$value==0)]<-NA
  
  ######## when row countryname equals to colum countryname, set the cell as zero #########
  rowcountryname<-rownames(valueadd)
  rowcountryname<-substring(rowcountryname,1,3)
  colcountryname<-colnames(valueadd)
  colcountryname<-substring(colcountryname,1,3)
  
  n<-1
  m<-1
  for (n in 1:2176) {
    for (m in 1:2176) {
      if(rowcountryname[n]==colcountryname[m]){
        valueadd[n,m]<-0
      }
    }
  }
  
  valueadd[valueadd<0]<-NA
  
  
  y.colsum<-gross.export$export ##by doing so, we do not need to change the following original code
  y.colsum<-as.matrix(y.colsum)
  
  #######################clean data above, following will calculate our indicators ###########################
  ####participation as supplier and participation as buyer,influence as supplier 
  ####and influence as buyer,relative upstreams of a country in particular industry
  
  
  ##############calculate participation as supplier##############
  ##GVC participation as supplier: the sum of a country-sector's value added forward linkages in GVCs to gross export of this country
  ##the total value added generate in this country,which reflects how much the country produces for the production of foreign industries.
  valueaddsupplier<-valueadd
  ##set values in diagonal into ‘0’ as the value in diagonal is the goods for a country's own use not for export
  #valueaddsupplier[row(valueaddsupplier)==col(valueaddsupplier)]<-0
  ##calculate sum of export of goods for each country 
  Exportsum<-rowSums(valueaddsupplier,na.rm = TRUE)
  #Exportsum2<-as.data.frame(Exportsum)
  ##calculate GVC participation as a supplier:sum of export divide its gross value added production for each country
  #Participation.as.Supplier<-Exportsum/y.colsum
  Participation.as.Supplier<-Exportsum/gross.valu$value  #gross.valu$value has valu of zero, therefore here will generate missing 
  
  
  #############calculate participation as buyer##################
  ##GVC participation as buyer: the sum of a country-sector value added imports of inputs in its production of final goods (gross exports), which
  ##is essentially about how much the production of a country depends on the production of foreign sectors
  valueaddbuyer<-valueadd
  ##set values in diagonal into '0' as the value in diagonal is the goods produced by a country's own not by import
  #valueaddbuyer[row(valueaddbuyer)==col(valueaddbuyer)]<-0
  ##calculate sum of import of goods for each country
  Importsum<-colSums(valueaddbuyer,na.rm = TRUE)
  ##calculate GVC participation as a buyer: sum of import divide its gross final production for each country
  Participation.as.Buyer<-Importsum/y.colsum
  
  
  ####sum up###
  Participation<-Participation.as.Buyer+Participation.as.Supplier
  
  
  ###############calculate influence as supplier###################
  ##supplierinfluence:the sum of the logarithm of the share made by a country's value added exports of 
  ##intermediates to the final goods production (gross export) of its partners around the world
  
  ###create matrix including 2176 row and 2176 colum for final goods production in a specific year:repeat first row by 188 times
  y.colsum1<-t(y.colsum)
  y.colsum1<-matrix(rep(y.colsum1[1,],each=2176),nrow =2176)
  
  ###use valueadd devide y.rowsum1 cell-by-cell to calculate the proportion of a country's value added exports
  ###of intermediates to the final goods production of its partners around the world
  SupplierProportion<-valueadd/y.colsum1
  
  ##calculate log for each cell 
  SupplierProportion.log<-log(SupplierProportion+1)
  ##set values in diagonal into ‘0’ as the value in diagonal is the proportion of intermediates for a country's own use
  #SupplierProportion.log[row(SupplierProportion.log)==col(SupplierProportion.log)]<-0
  ##calculate influence as a supplier
  Influence.as.Supplier.log<-rowSums(SupplierProportion.log,na.rm = TRUE)
  
  ##calculate influence as a supplier for each cell (not by log)
  #SupplierProportion[row(SupplierProportion)==col(SupplierProportion)]<-0
  Influence.as.Supplier<-rowSums(SupplierProportion,na.rm = TRUE)
  
  ###############calculate influence as buyer###################
  ###calculate influence as buyer
  ##buyerinfluence:the sum of the logarithm of the share made by a country's value added imports to the gdp of
  ##its partners around the world
  ##repeat first col by 2176 times
  y.colsum2<-matrix(rep(y.colsum[,1],each=2176),ncol =2176,byrow = TRUE)
  
  ###use valueadd devide WorldGDPSupplierM cell-by-cell to calculate the proportion of a country's value added imports
  ###of intermediates to the GDP of its partners around the world
  BuyerProportion<-valueadd/y.colsum2
  
  ###calculate log for each cell 
  BuyerProportion.log<-log(BuyerProportion+1)
  #set values in diagonal into '0' as the value in diagonal is the proportion of intermediates produced by a country's own
  #BuyerProportion.log[row(BuyerProportion.log)==col(BuyerProportion.log)]<-0
  ###calculate influence as a buyer###
  Influence.as.Buyer.log<-colSums(BuyerProportion.log,na.rm = TRUE)
  
  ##calculate influence as a buyer for each cell (not by log)
  #BuyerProportion[row(BuyerProportion)==col(BuyerProportion)]<-0
  Influence.as.Buyer<-colSums(BuyerProportion,na.rm = TRUE)
  
  ######sum up###
  Influence.log<-Influence.as.Buyer.log+Influence.as.Supplier.log
  Influence<-Influence.as.Buyer+Influence.as.Supplier
  
  
  #################calculate relative upstreams of a country in particular industry, this one has no meaning for country-industry #################
  ##relative upstreams of a country in particular industry: measuring upstremness as the log ratio of a 
  ##country's supply of intermediates used in other countries' exports to the use of imported intermediateds in its own production
  
  #valueaddposition<-valueadd
  #x<-rowSums(valueaddposition,na.rm = TRUE)
  #y<-colSums(valueaddposition,na.rm = TRUE)
  #UprestreamsPositions<-log(1+x/y.colsum)-log(1+y/y.colsum)
  
  
  ######################################################################################################################
  ######### Create a panel data with CountryCode,IndustryCode, Country_Industry,Year, 
  #########Influence.as.Buyer,Influence.as.Buyer.log, Influence.as.Supplier,Influence.as.Supplier.log,Influence########
  #########Participation.as.Buyer,Participation.as.Supplier,Participation,UprestreamsPositions
  ######################################################################################################################
 
  Year<-rep(i,times=2176) #2176is the total number of countries involved.
  Country_Industry<-row.names(valueadd)
  CountryCode<-substring(Country_Industry,1,3)
  IndustryCode<-substring(Country_Industry,5,15)
  
  if(i==1995){
  #  ICIO2016_GVCInfluence.Participation_2<-data.frame(CountryCode,IndustryCode,Country_Industry,Year,Influence.as.Buyer,Influence.as.Buyer.log, Influence.as.Supplier,Influence.as.Supplier.log,
  #                                         Influence,Influence.log, Participation.as.Buyer,Participation.as.Supplier,Participation)
    ICIO2016_GVCInfluence.Participation_2<-data.frame(CountryCode,IndustryCode,Country_Industry,Year,Influence.as.Buyer.log, Influence.as.Supplier.log,
                                           Influence.log, Participation.as.Buyer,Participation.as.Supplier)
  }else{
  #  data<-data.frame(CountryCode,IndustryCode,Country_Industry,Year,Influence.as.Buyer,Influence.as.Buyer.log, Influence.as.Supplier,Influence.as.Supplier.log,
  #                   Influence,Influence.log, Participation.as.Buyer,Participation.as.Supplier,Participation)
    
    data<-data.frame(CountryCode,IndustryCode,Country_Industry,Year,Influence.as.Buyer.log, Influence.as.Supplier.log,
                     Influence.log, Participation.as.Buyer,Participation.as.Supplier)
    ICIO2016_GVCInfluence.Participation_2<-merge(data,ICIO2016_GVCInfluence.Participation_2,all=TRUE)
  }
  
  i<i+1
}


###change nande and add two variables
ICIO2016_GVCInfluence.Participation<-ICIO2016_GVCInfluence.Participation_2

names(ICIO2016_GVCInfluence.Participation)[5:9]<-c("Influence.as.Buyer.log.sp", "Influence.as.Supplier.log.sp",
                                                    "Influence.log.sp", "Participation.as.Buyer.sp2","Participation.as.Supplier.sp")


##read EXGR_FVASH.csv and EXGR_DVAFXSH, it does not has industry of C95, therefore has missing
Participation.as.Buyer.sp<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2016/EXGR_FVASH.csv",sep = '|',header = FALSE) #change path to your own
Participation.as.Buyer.sp<-Participation.as.Buyer.sp[,-c(1,3)]
names(Participation.as.Buyer.sp)<-c("CountryCode","IndustryCode","Year","Participation.as.Buyer.sp")
Participation.as.Buyer.sp$Participation.as.Buyer.sp<-Participation.as.Buyer.sp$Participation.as.Buyer.sp/100
ICIO2016_GVCInfluence.Participation<-merge(ICIO2016_GVCInfluence.Participation,Participation.as.Buyer.sp,
                                           by=c("CountryCode","IndustryCode","Year"),all.x  = TRUE)


Participation.as.Supplier.sTp<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2016/EXGR_DVAFXSH.csv",sep = '|',header = FALSE) #change path to your own
Participation.as.Supplier.sTp<-Participation.as.Supplier.sTp[,-c(1,3)]
names(Participation.as.Supplier.sTp)<-c("CountryCode","IndustryCode","Year","Participation.as.Supplier.sTp")
Participation.as.Supplier.sTp$Participation.as.Supplier.sTp<-Participation.as.Supplier.sTp$Participation.as.Supplier.sTp/100
ICIO2016_GVCInfluence.Participation<-merge(ICIO2016_GVCInfluence.Participation,Participation.as.Supplier.sTp,
                                           by=c("CountryCode","IndustryCode","Year"),all.x  = TRUE)



write.csv(ICIO2016_GVCInfluence.Participation,file="/Users/sandy/Documents/DataCode/ICIO/2016_GVC_Indicator/ICIO2016_GVCInfluence.Participation.csv") #change path to your own



##########################################################################################################################################
############################################# calculate 2018 data secondly ################################################################
##########################################################################################################################################
ICIO2018_GVCInfluence.Participation_2<-NULL
GrossExport<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2018/EXGR.csv",sep = '|',header = FALSE) #change path to your own
GrossValu<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2018/VALU.csv",sep = '|',header = FALSE) #change path to your own

i<-2005 #begin with 2005

for (i in 2005:2015) {
  
  ##read value added data 
  filename<-paste("/Users/sandy/Documents/DataCode/ICIO/2018_Wide/","EXGR_BSCI_",i,"_Wide.csv",sep = "") #change path to your own,use changed format of EXGR_BSCI
  valueadd<-read.csv(filename,row.names = 1)
  
  ##calculate gross exports of each country sector, not used here
  Ycolsum<-as.matrix(colSums(valueadd))
  
  
  ## calculate gross export for each country--use gross export as 分母
  #gross.export<-y.colsum
  #gross.export<-as.data.frame(gross.export)
  #gross.export$country<-substring(rownames(gross.export),1,3)
  #export<-group_by(gross.export,country) %>% summarise(V1=sum(V1))
  #gross.export<-merge(gross.export,export,by="country",all.x = TRUE)
  #names(gross.export)[2:3]<-c("SectExport","CounExport")
  gross.export<-GrossExport
  gross.export<-gross.export[,-1]
  names(gross.export)[1:5]<-c("country","partner","sector","year","export")
  gross.export$country<-as.character(gross.export$country)
  gross.export$partner<-as.character(gross.export$partner)
  gross.export$sector<-as.character(gross.export$sector)
  gross.export<-gross.export[which(gross.export$year==i&gross.export$partner=="WLD"&gross.export$sector=="DTOTAL"),]
  Region<-c("OECD","NONOECD","APEC","ASEAN","EASIA","EU28","EU15","EU13","EA19","EA12","G20","ZEUR","ZASI","ZOTH","ZSCA","ZNAM","WLD","DXD")
  gross.export<-gross.export[-which(gross.export$country %in% Region),]
  gross.export<-gross.export[order(gross.export$country),]
  gross.export<-gross.export[,-c(2,3,4)]
  gross.export<-gross.export[rep(1:nrow(gross.export),each=36),] ##36 is the number of industries in 2018 version
  
  ######calucate gross value added for each country sector---this one is for participation.as.supplier.sp
  gross.valu<-GrossValu
  gross.valu<-gross.valu[,-1]
  names(gross.valu)[1:5]<-c("country","partner","sector","year","value")
  gross.valu$country<-as.character(gross.valu$country)
  gross.valu$partner<-as.character(gross.valu$partner)
  gross.valu$sector<-as.character(gross.valu$sector)
  gross.valu<-gross.valu[which(gross.valu$year==i),]
  IndustryAggregation<-c("D05T09","D10T33","D16T18","D19T23","D24T25","D26T27","D29T30","D45T82","D45T56","D58T63","D84T98","D84T88","D90T98","D05T39","D45T98","D58T82","D41T98","DINFO","DTOTAL")
  gross.valu<-gross.valu[-which(gross.valu$sector %in% IndustryAggregation | gross.valu$country %in% Region ),]
  gross.valu<-gross.valu[order(gross.valu$country,gross.valu$sector),]
  gross.valu$value[which(gross.valu$value<0 | gross.valu$value==0)]<-NA
  
  
  
  
  ######## when row countryname equals to colum countryname, set the cell as zero #########
  rowcountryname<-rownames(valueadd)
  rowcountryname<-substring(rowcountryname,1,3)
  colcountryname<-colnames(valueadd)
  colcountryname<-substring(colcountryname,1,3)
  
  n<-1
  m<-1
  for (n in 1:2340) {
    for (m in 1:2340) {
      if(rowcountryname[n]==colcountryname[m]){
        valueadd[n,m]<-0
      }
    }
  }
  
  valueadd[valueadd<0]<-NA
  
  
  y.colsum<-gross.export$export ##by doing so, we do not need to change the following original code
  y.colsum<-as.matrix(y.colsum)
  
  #######################clean data above, following will calculate our indicators ###########################
  ####participation as supplier and participation as buyer,influence as supplier 
  ####and influence as buyer,relative upstreams of a country in particular industry
  
  
  ##############calculate participation as supplier##############
  ##GVC participation as supplier: the sum of a country-sector's value added forward linkages in GVCs to gross export of this country
  ##the total value added generate in this country,which reflects how much the country produces for the production of foreign industries.
  valueaddsupplier<-valueadd
  ##set values in diagonal into ‘0’ as the value in diagonal is the goods for a country's own use not for export
  #valueaddsupplier[row(valueaddsupplier)==col(valueaddsupplier)]<-0
  ##calculate sum of export of goods for each country 
  Exportsum<-rowSums(valueaddsupplier,na.rm = TRUE)
  ##calculate GVC participation as a supplier:sum of export divide its gross value added production for each country
  #Participation.as.Supplier<-Exportsum/y.colsum
  Participation.as.Supplier<-Exportsum/gross.valu$value
  
  #############calculate participation as buyer##################
  ##GVC participation as buyer: the sum of a country-sector value added imports of inputs in its production of final goods (gross exports), which
  ##is essentially about how much the production of a country depends on the production of foreign sectors
  valueaddbuyer<-valueadd
  ##set values in diagonal into '0' as the value in diagonal is the goods produced by a country's own not by import
  #valueaddbuyer[row(valueaddbuyer)==col(valueaddbuyer)]<-0
  ##calculate sum of import of goods for each country
  Importsum<-colSums(valueaddbuyer,na.rm = TRUE)
  ##calculate GVC participation as a buyer: sum of import divide its gross final production for each country
  Participation.as.Buyer<-Importsum/y.colsum

  
  
  ####sum up###
  #Participation<-Participation.as.Buyer+Participation.as.Supplier
  
  
  ###############calculate influence as supplier###################
  ##supplierinfluence:the sum of the logarithm of the share made by a country's value added exports of 
  ##intermediates to the final goods production (gross export) of its partners around the world
  
  ###create matrix including 2176 row and 2176 colum for final goods production in a specific year:repeat first row by 188 times
  y.colsum1<-t(y.colsum)
  y.colsum1<-matrix(rep(y.colsum1[1,],each=2340),nrow =2340)
  
  ###use valueadd devide y.rowsum1 cell-by-cell to calculate the proportion of a country's value added exports
  ###of intermediates to the final goods production of its partners around the world
  SupplierProportion<-valueadd/y.colsum1
  
  ##calculate log for each cell 
  SupplierProportion.log<-log(SupplierProportion+1)
  ##set values in diagonal into ‘0’ as the value in diagonal is the proportion of intermediates for a country's own use
  #SupplierProportion.log[row(SupplierProportion.log)==col(SupplierProportion.log)]<-0
  ##calculate influence as a supplier
  Influence.as.Supplier.log<-rowSums(SupplierProportion.log,na.rm = TRUE)
  
  ##calculate influence as a supplier for each cell (not by log)
  #SupplierProportion[row(SupplierProportion)==col(SupplierProportion)]<-0
  Influence.as.Supplier<-rowSums(SupplierProportion,na.rm = TRUE)
  
  ###############calculate influence as buyer###################
  ###calculate influence as buyer
  ##buyerinfluence:the sum of the logarithm of the share made by a country's value added imports to the gdp of
  ##its partners around the world
  ##repeat first col by 2176 times
  y.colsum2<-matrix(rep(y.colsum[,1],each=2340),ncol =2340,byrow = TRUE)
  
  ###use valueadd devide WorldGDPSupplierM cell-by-cell to calculate the proportion of a country's value added imports
  ###of intermediates to the GDP of its partners around the world
  BuyerProportion<-valueadd/y.colsum2
  
  ###calculate log for each cell 
  BuyerProportion.log<-log(BuyerProportion+1)
  #set values in diagonal into '0' as the value in diagonal is the proportion of intermediates produced by a country's own
  #BuyerProportion.log[row(BuyerProportion.log)==col(BuyerProportion.log)]<-0
  ###calculate influence as a buyer###
  Influence.as.Buyer.log<-colSums(BuyerProportion.log,na.rm = TRUE)
  
  ##calculate influence as a buyer for each cell (not by log)
  #BuyerProportion[row(BuyerProportion)==col(BuyerProportion)]<-0
  Influence.as.Buyer<-colSums(BuyerProportion,na.rm = TRUE)
  
  ######sum up###
  Influence.log<-Influence.as.Buyer.log+Influence.as.Supplier.log
  Influence<-Influence.as.Buyer+Influence.as.Supplier
  
  
  #################calculate relative upstreams of a country in particular industry, has no meaning for country-Industry#################
  ##relative upstreams of a country in particular industry: measuring upstremness as the log ratio of a 
  ##country's supply of intermediates used in other countries' exports to the use of imported intermediateds in its own production
 
  #valueaddposition<-valueadd
  #x<-rowSums(valueaddposition,na.rm = TRUE)
  #y<-colSums(valueaddposition,na.rm = TRUE)
  #UprestreamsPositions<-log(1+x/y.colsum)-log(1+y/y.colsum)
  
  ######################################################################################################################
  ######### Create a panel data with CountryCode,IndustryCode, Country_Industry,Year, 
  #########Influence.as.Buyer,Influence.as.Buyer.log, Influence.as.Supplier,Influence.as.Supplier.log,Influence########
  #########Participation.as.Buyer,Participation.as.Supplier,Participation,UprestreamsPositions
  ######################################################################################################################
  
  Year<-rep(i,times=2340) #2340 is the total number of countries involved.
  Country_Industry<-row.names(valueadd)
  CountryCode<-substring(Country_Industry,1,3)
  IndustryCode<-substring(Country_Industry,5,15)
  
  if(i==2005){
   # ICIO2018_GVCInfluence.Participation_2<-data.frame(CountryCode,IndustryCode,Country_Industry,Year,Influence.as.Buyer,Influence.as.Buyer.log, Influence.as.Supplier,Influence.as.Supplier.log,
   #                                                    Influence,Influence.log, Participation.as.Buyer,Participation.as.Supplier,Participation)
    ICIO2018_GVCInfluence.Participation_2<-data.frame(CountryCode,IndustryCode,Country_Industry,Year,Influence.as.Buyer.log,Influence.as.Supplier.log,
                                                      Influence.log, Participation.as.Buyer,Participation.as.Supplier)  
  }else{
   # data<-data.frame(CountryCode,IndustryCode,Country_Industry,Year,Influence.as.Buyer,Influence.as.Buyer.log, Influence.as.Supplier,Influence.as.Supplier.log,
   #                   Influence,Influence.log, Participation.as.Buyer,Participation.as.Supplier,Participation)
    
    data<-data.frame(CountryCode,IndustryCode,Country_Industry,Year,Influence.as.Buyer.log,Influence.as.Supplier.log,
                     Influence.log, Participation.as.Buyer,Participation.as.Supplier)
    
    ICIO2018_GVCInfluence.Participation_2<-merge(data,ICIO2018_GVCInfluence.Participation_2,all=TRUE)
  }
  
  i<i+1
}




###change nande and add two variables
ICIO2018_GVCInfluence.Participation<-ICIO2018_GVCInfluence.Participation_2

names(ICIO2018_GVCInfluence.Participation)[5:9]<-c("Influence.as.Buyer.log.sp", "Influence.as.Supplier.log.sp",
                                                    "Influence.log.sp", "Participation.as.Buyer.sp2","Participation.as.Supplier.sp")


##read EXGR_FVASH.csv and EXGR_DVAFXSH
Participation.as.Buyer.sp<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2018/EXGR_FVASH.csv",sep = '|',header = FALSE) #change path to your own
Participation.as.Buyer.sp<-Participation.as.Buyer.sp[,-c(1,3)]
names(Participation.as.Buyer.sp)<-c("CountryCode","IndustryCode","Year","Participation.as.Buyer.sp")
Participation.as.Buyer.sp$Participation.as.Buyer.sp<-Participation.as.Buyer.sp$Participation.as.Buyer.sp/100
ICIO2018_GVCInfluence.Participation<-merge(ICIO2018_GVCInfluence.Participation,Participation.as.Buyer.sp,
                                           by=c("CountryCode","IndustryCode","Year"),all.x  = TRUE)


Participation.as.Supplier.sTp<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2018/EXGR_DVAFXSH.csv",sep = '|',header = FALSE) #change path to your own
Participation.as.Supplier.sTp<-Participation.as.Supplier.sTp[,-c(1,3)]
names(Participation.as.Supplier.sTp)<-c("CountryCode","IndustryCode","Year","Participation.as.Supplier.sTp")
Participation.as.Supplier.sTp$Participation.as.Supplier.sTp<-Participation.as.Supplier.sTp$Participation.as.Supplier.sTp/100
ICIO2018_GVCInfluence.Participation<-merge(ICIO2018_GVCInfluence.Participation,Participation.as.Supplier.sTp,
                                           by=c("CountryCode","IndustryCode","Year"),all.x  = TRUE)



write.csv(ICIO2018_GVCInfluence.Participation,file="/Users/sandy/Documents/DataCode/ICIO/2018_GVC_Indicator/ICIO2018_GVCInfluence.Participation.csv") #change path to your own






