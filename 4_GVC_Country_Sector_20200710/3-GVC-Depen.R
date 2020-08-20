WD<-getwd()
if(!is.null(WD))setwd("/Users/sandy/Documents/DataCode/ICIO/")  #change format
getwd()

library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)


##########################################################################################################################################
############################################# calculate 2016 data firstly ################################################################
##########################################################################################################################################
GrossExport<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2016/EXGR.csv",sep = '|',header = FALSE)
GrossValu<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2016/VALU.csv",sep = '|',header = FALSE)

Year.b<-NULL
Country_s<-NULL
CountrySector_rq<-NULL
Sens.Dep.s.rq.supplier.b<-NULL
Sens.Dep.s.rq.buyer.b<-NULL
Sens.Dep.s.rq.b<-NULL
Vul.Dep.s.rq.supplier.b<-NULL
Vul.Dep.s.rq.buyer.b<-NULL
Vul.Dep.s.rq.b<-NULL

Country_r<-NULL
CountrySector_sp<-NULL
Sens.Dep.sp.r.buyer.b<-NULL
Vul.Dep.sp.r.buyer.b<-NULL
Sens.Dep.sp.r.supplier.b<-NULL
Vul.Dep.sp.r.supplier.b<-NULL


j<-1 
jj<-1
i<-1995 #begin with 1995

for (i in 1995:2011) {
  ##read value added data 
  filename<-paste("/Users/sandy/Documents/DataCode/ICIO/2016_Wide/","EXGR_BSCI_",i,"_Wide.csv",sep = "")
  valueadd<-read.csv(filename,row.names = 1)
  
  ##calculate gross exports of each country sector
  Ycolsum<-as.matrix(colSums(valueadd))

  ######## valueadded data when row countryname equals to colum countryname, set the cell as zero #########
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
  
  ##Gross export of each country S
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
  
  
  y.colsum<-gross.export$export
  y.colsum<-as.matrix(y.colsum)
  
  ######calucate gross value added for each country sector
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
  
  
  #######################################################################################################################
  ########################## Dependence of a country S on a country sector RQ ###########################################
  #######################################################################################################################
  
  ##Sens.Dep.s.rq.supplier, how sensitive is a country S as a supplier to a country sector RQ as a buyer.
  valueadd1<-cbind(y.colsum,valueadd)
  Sens.Dep.supplier<-t(apply(valueadd1, 1, function(valueadd1)valueadd1/valueadd1[1])) # 1 for row
  Sens.Dep.supplier<-Sens.Dep.supplier[,-1] 
  Sens.Dep.supplier<-as.data.frame(Sens.Dep.supplier)
  
  #Sens.Dep.s.rq.buyer, how sensitive is a country S as a buyer to a country sector RQ as a supplier. 
  y.colsum1<-t(y.colsum)
  y.colsum1<-matrix(rep(y.colsum1[1,],each=2176),nrow =2176)
  Sens.Dep.buyer<-valueadd/y.colsum1 
  Sens.Dep.buyer<-t(Sens.Dep.buyer) 
  Sens.Dep.buyer<-as.data.frame(Sens.Dep.buyer)
  
  hh<-0 #number of industry
  Sens.Dep.s.rq.supplier<-NULL
  Sens.Dep.s.rq.buyer<-NULL
  while(hh<2176) {
    m<-hh+1
    n<-hh+34
    Midsupplier<-Sens.Dep.supplier[m:n,]
    Midbuyer<-Sens.Dep.buyer[m:n,]
    
    Midsupplier<-colSums(Midsupplier) %>% t()
    Sens.Dep.s.rq.supplier<-rbind(Sens.Dep.s.rq.supplier, Midsupplier)
    
    Midbuyer<-colSums(Midbuyer) %>%  t()
    Sens.Dep.s.rq.buyer<-rbind(Sens.Dep.s.rq.buyer, Midbuyer)
    
    hh<-hh+34

  }
  
  #Sens.Dep.s.rq, the total GVC sensitivity dependence of a country S on a country sector RQ. 
  Sens.Dep.s.rq=Sens.Dep.s.rq.supplier+Sens.Dep.s.rq.buyer
  
  Sens.Dep.s.rq.supplier<-as.data.frame(Sens.Dep.s.rq.supplier)
  Sens.Dep.s.rq.buyer<-as.data.frame(Sens.Dep.s.rq.buyer)
  Sens.Dep.s.rq<-as.data.frame(Sens.Dep.s.rq)
  
  
  #Vul.Dep.s.rq.supplier, how vulnerable a country S is to a country sector RQ in terms of 
  #its dependence on the country sector RQ to absorb the value added produced by country S
  Sens.Dep.s.rq.supplier.V<-Sens.Dep.s.rq.supplier
  HHI.S<-Sens.Dep.s.rq.supplier.V^2
  HHI.S<-as.data.frame(HHI.S)
  
  HHIname<-names(HHI.S)
  HHI.S.SectorName<-substring(HHIname,5,20)
  HHI.S.CountryName<-substring(HHIname,1,3)
  
  HHI.S.Name<-unique(HHI.S.SectorName)
  HHI.S.CountryName<-unique(HHI.S.CountryName)
  hh<-1 #number of industry
  HHI.Vulner<-NULL
  while (hh<35) {
    
    HHI<-HHI.S[,grep(colnames(HHI.S),pattern = HHI.S.Name[hh],fixed = TRUE)]
    HHI.S.Sum<-rowSums(HHI)
    HHI.S.Sum<-as.data.frame(HHI.S.Sum)
    rownames(HHI.S.Sum)<-HHI.S.CountryName
    colnames(HHI.S.Sum)<-HHI.S.Name[hh]
    
    ## HHI of country sector
    if(hh==1){
      HHI.Vulner<-HHI.S.Sum
    }else{
      HHI.Vulner<-cbind(HHI.Vulner,HHI.S.Sum)
    }
    hh<-hh+1
  }
  
  HHI.Copy<-rep(HHI.Vulner,64)
  HHI.Copy<-as.data.frame(HHI.Copy)
  Vul.Dep.s.rq.supplier<-HHI.Copy*Sens.Dep.s.rq.supplier
  
  #Vul.Dep.s.rq.buyer, how vulnerable country S is to country sector RQ in in terms of 
  #its export’s dependence on the value added contribution from country sector RQ.
  Sens.Dep.s.rq.buyer.V<-Sens.Dep.s.rq.buyer  ## we invert the Sens.Dep.s.rq.buyer before, so can use the same way to calculate as Sens.Dep.s.rq.supplier
  HHI.B<-Sens.Dep.s.rq.buyer.V^2
  HHI.B<-as.data.frame(HHI.B)
  
  HHIname<-names(HHI.B)
  HHI.B.SectorName<-substring(HHIname,5,20)
  HHI.B.CountryName<-substring(HHIname,1,3)
  
  HHI.B.Name<-unique(HHI.B.SectorName)
  HHI.B.CountryName<-unique(HHI.B.CountryName)
  hh<-1 #number of industry
  HHI.Vulner<-NULL
  while (hh<35) {
    
    HHI<-HHI.B[,grep(colnames(HHI.B),pattern = HHI.B.Name[hh],fixed = TRUE)]
    HHI.B.Sum<-rowSums(HHI)
    HHI.B.Sum<-as.data.frame(HHI.B.Sum)
    rownames(HHI.B.Sum)<-HHI.B.CountryName
    colnames(HHI.B.Sum)<-HHI.B.Name[hh]
    
    ## HHI of country sector
    if(hh==1){
      HHI.Vulner<-HHI.B.Sum
    }else{
      HHI.Vulner<-cbind(HHI.Vulner,HHI.B.Sum)
    }
    hh<-hh+1
  }
  
  HHI.Copy<-rep(HHI.Vulner,64)
  HHI.Copy<-as.data.frame(HHI.Copy)
  Vul.Dep.s.rq.buyer<-HHI.Copy*Sens.Dep.s.rq.buyer
  
  ##Vul.Dep
  Vul.Dep.s.rq = Vul.Dep.s.rq.supplier + Vul.Dep.s.rq.buyer
  
  
  #######################################################################################################################
  ########################## Dependence of a country sector SP on a country R ###########################################
  #######################################################################################################################  
  
  #Sens.Dep.s.rq.buyer, how sensitive is a country S as a buyer to a country sector RQ as a supplier. 
  y.colsum1<-t(Ycolsum)
  y.colsum1<-matrix(rep(y.colsum1[1,],each=2176),nrow =2176)
  Sens.Dep.buyer<-valueadd/y.colsum1 
  Sens.Dep.buyer<-as.data.frame(Sens.Dep.buyer)
  
  hh<-0 #number of industry
  Sens.Dep.sp.r.buyer<-NULL
  while(hh<2176) {
    m<-hh+1
    n<-hh+34
    Midbuyer<-Sens.Dep.buyer[m:n,]
    
    Midbuyer<-colSums(Midbuyer) %>%  t()
    Sens.Dep.sp.r.buyer<-rbind(Sens.Dep.sp.r.buyer, Midbuyer)
    
    hh<-hh+34
    
  }
  
  Sens.Dep.sp.r.buyer<-as.data.frame(Sens.Dep.sp.r.buyer)
  
  #Vul.Dep.sp.r.buyer, how vulnerable country sector SP is to country R in in terms of its
  #export’s dependence on the value added contribution from country R.
  Sens.Dep.sp.r.buyer.V<-Sens.Dep.sp.r.buyer
  HHI.B<-Sens.Dep.sp.r.buyer.V^2
  HHI.B.Sum<-colSums(HHI.B)
  
  Vul.Dep.sp.r.buyer<-HHI.B.Sum*Sens.Dep.sp.r.buyer
  
  #Sens.Dep.sp.r.supplier, how sensitive is country sector SP as a buyer to country R as a supplier,the denominator is Vasp
  valueadd1<-cbind(gross.valu$value,valueadd)
  Sens.Dep.supplier<-t(apply(valueadd1, 1, function(valueadd1)valueadd1/valueadd1[1])) # 1 for row
  Sens.Dep.supplier<-Sens.Dep.supplier[,-1] 
  Sens.Dep.supplier<-as.data.frame(Sens.Dep.supplier)
  
  
  hh<-0 #number of industry
  Sens.Dep.sp.r.supplier<-NULL
  while(hh<2176) {
    m<-hh+1
    n<-hh+34
    Midsupplier<-Sens.Dep.supplier[,m:n]
    
    Midsupplier<-rowSums(Midsupplier)
    Sens.Dep.sp.r.supplier<-cbind(Sens.Dep.sp.r.supplier, Midsupplier)
    
    hh<-hh+34
    
  }
  
  Sens.Dep.sp.r.supplier<-as.data.frame(Sens.Dep.sp.r.supplier)
  
  #Vul.Dep.sp.r.supplier, how vulnerable country sector SP is to 
  #country R in in terms of how much its value-added production rely on the demand from country R in its export.
  Sens.Dep.sp.r.supplier.V<-Sens.Dep.sp.r.supplier
  HHI.S<-Sens.Dep.sp.r.supplier.V^2
  HHI.S.Sum<-rowSums(HHI.S)
  
  Vul.Dep.sp.r.supplier<-HHI.S*Sens.Dep.sp.r.supplier
  
  
  #######################################################################################################################
  ########################## Vulnerability of Country Sector ###########################################
  #######################################################################################################################  
  
  #HHI.Supplier.sp, we can consider use V asp as denominator to calculate this measure.
  valueadd1<-cbind(gross.valu$value,valueadd)
  Sens.Dep.supplier<-t(apply(valueadd1, 1, function(valueadd1)valueadd1/valueadd1[1])) # 1 for row
  Sens.Dep.supplier<-Sens.Dep.supplier[,-1] 
  Sens.Dep.supplier<-as.data.frame(Sens.Dep.supplier)
  
  Sens.Dep.supplier<-Sens.Dep.supplier^2
  HHI.Supplier.sp<-rowSums(Sens.Dep.supplier)
  
  #HHI.Buyer.sp vulnerability of a country sector SP as buyer depends on how concentrated its suppliers are.
  y.colsum1<-t(Ycolsum)
  y.colsum1<-matrix(rep(y.colsum1[1,],each=2176),nrow =2176)
  Sens.Dep.buyer<-valueadd/y.colsum1 
  Sens.Dep.buyer<-as.data.frame(Sens.Dep.buyer)
  
  Sens.Dep.buyer<- Sens.Dep.buyer^2
  HHI.Buyer.sp<-colSums(Sens.Dep.buyer)

  ############################Store data by dyad (row country_industry-colum country_industry)#############################
  Country_Industry<-names(valueadd)
  IndustryCode<-substring(Country_Industry,5,20) %>% unique()
  CountryCode<-substring(Country_Industry,1,3) %>% unique()
  
  
  
  ### transfer from 2176*64 to 64*2176 
  Sens.Dep.sp.r.supplier<-t(Sens.Dep.sp.r.supplier)
  Vul.Dep.sp.r.supplier<-t(Vul.Dep.sp.r.supplier)
  
  n<-1
  for (n in 1:64) {
    m<-1
    for (m in 1:2176) {
      Year.b[j]<-i
      Country_s[j]<-CountryCode[n]
      CountrySector_rq[j]<-Country_Industry[m]
      Sens.Dep.s.rq.supplier.b[j]<-Sens.Dep.s.rq.supplier[n,m]
      Sens.Dep.s.rq.buyer.b[j]<-Sens.Dep.s.rq.buyer[n,m]
      Sens.Dep.s.rq.b[j]<-Sens.Dep.s.rq[n,m]
      Vul.Dep.s.rq.supplier.b[j]<-Vul.Dep.s.rq.supplier[n,m]
      Vul.Dep.s.rq.buyer.b[j]<-Vul.Dep.s.rq.buyer[n,m]
      Vul.Dep.s.rq.b[j]<-Vul.Dep.s.rq[n,m]
      
      j<-j+1
      m<-m+1 
    }
    n<-n+1
  } 
  
  n<-1
  for (n in 1:64) {
    m<-1
    for (m in 1:2176) {
      Country_r[jj]<-CountryCode[n]
      CountrySector_sp[jj]<-Country_Industry[m]
      Sens.Dep.sp.r.buyer.b[jj]<-Sens.Dep.sp.r.buyer[n,m]
      Vul.Dep.sp.r.buyer.b[jj]<-Vul.Dep.sp.r.buyer[n,m]
      Sens.Dep.sp.r.supplier.b[jj]<-Sens.Dep.sp.r.supplier[n,m]
      Vul.Dep.sp.r.supplier.b[jj]<-Vul.Dep.sp.r.supplier[n,m]
      
      jj<-jj+1
      m<-m+1 
    }
    n<-n+1
  } 
  
    ###################################store HHI#################################
    Year<-rep(i,times=2176) #2176 is the total number of country-sector involved.
    if(i==1995){
      HHI.2016<-data.frame(Country_Industry,Year,HHI.Supplier.sp,HHI.Buyer.sp)
    }else{
      data<-data.frame(Country_Industry,Year,HHI.Supplier.sp,HHI.Buyer.sp)
      HHI.2016<-merge(data,HHI.2016,all=TRUE)
    }
    
    i<-i+1
}      

  GVCDependence.s.rq.2016<-data.frame(Country_s,CountrySector_rq,Year.b,Sens.Dep.s.rq.supplier.b,Sens.Dep.s.rq.buyer.b,Sens.Dep.s.rq.b,
                                      Vul.Dep.s.rq.supplier.b,Vul.Dep.s.rq.buyer.b,Vul.Dep.s.rq.b)
  
  GVCDependence.sp.r.2016<-data.frame(Country_r,CountrySector_sp,Year.b,Sens.Dep.sp.r.buyer.b,Vul.Dep.sp.r.buyer.b,Sens.Dep.sp.r.supplier.b,Vul.Dep.sp.r.supplier.b)
  
  names(GVCDependence.s.rq.2016)[3:9]<-c("Year","Sens.Dep.s.rq.supplier","Sens.Dep.s.rq.buyer","Sens.Dep.s.rq",
                                         "Vul.Dep.s.rq.supplier","Vul.Dep.s.rq.buyer","Vul.Dep.s.rq")
  names(GVCDependence.sp.r.2016)[3:7]<-c("Year","Sens.Dep.sp.r.buyer","Vul.Dep.sp.r.buyer","Sens.Dep.sp.r.supplier","Vul.Dep.sp.r.supplier")

  ##delete self-self
  GVCDependence.s.rq.2016[,c("Country_r","Sector_q")]<-str_split_fixed(GVCDependence.s.rq.2016$CountrySector_rq, "_", 2)
  GVCDependence.s.rq.2016<-GVCDependence.s.rq.2016[GVCDependence.s.rq.2016$Country_s!=GVCDependence.s.rq.2016$Country_r,]
  #GVCDependence.s.rq.2016<-GVCDependence.s.rq.2016[,-c(10,11)]
  
  GVCDependence.sp.r.2016[,c("Country_s","Sector_p")]<-str_split_fixed(GVCDependence.sp.r.2016$CountrySector_sp,"_", 2)
  GVCDependence.sp.r.2016<-GVCDependence.sp.r.2016[GVCDependence.sp.r.2016$Country_r!=GVCDependence.sp.r.2016$Country_s,]
  #GVCDependence.sp.r.2016<-GVCDependence.sp.r.2016[,-c(8,9)]
  
  ##order as year
  HHI.2016<-HHI.2016[order(HHI.2016$Year),]
  
  
  ##write
  write.csv(GVCDependence.s.rq.2016,file="/Users/sandy/Documents/DataCode/ICIO/2016_GVC_Indicator/GVCDependence.s.rq.2016")
  write.csv(GVCDependence.sp.r.2016,file="/Users/sandy/Documents/DataCode/ICIO/2016_GVC_Indicator/GVCDependence.sp.r.2016")
  write.csv(HHI.2016,file="/Users/sandy/Documents/DataCode/ICIO/2016_GVC_Indicator/ICIO2016_HHI.csv")
  
  
  ##########################################################################################################################################
  ############################################# calculate 2018 data secondly ################################################################
  ##########################################################################################################################################
  GrossExport<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2018/EXGR.csv",sep = '|',header = FALSE)
  GrossValu<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2018/VALU.csv",sep = '|',header = FALSE)
  
  Year.b<-NULL
  Country_s<-NULL
  CountrySector_rq<-NULL
  Sens.Dep.s.rq.supplier.b<-NULL
  Sens.Dep.s.rq.buyer.b<-NULL
  Sens.Dep.s.rq.b<-NULL
  Vul.Dep.s.rq.supplier.b<-NULL
  Vul.Dep.s.rq.buyer.b<-NULL
  Vul.Dep.s.rq.b<-NULL
  
  Country_r<-NULL
  CountrySector_sp<-NULL
  Sens.Dep.sp.r.buyer.b<-NULL
  Vul.Dep.sp.r.buyer.b<-NULL
  Sens.Dep.sp.r.supplier.b<-NULL
  Vul.Dep.sp.r.supplier.b<-NULL
  
  
  j<-1 
  jj<-1
  i<-2005 #begin with 2005
  
  for (i in 2005:2015) {
    ##read value added data 
    filename<-paste("/Users/sandy/Documents/DataCode/ICIO/2018_Wide/","EXGR_BSCI_",i,"_Wide.csv",sep = "")
    valueadd<-read.csv(filename,row.names = 1)
    
    ##calculate gross exports of each country sector
    Ycolsum<-as.matrix(colSums(valueadd))
    
    ######## valueadded data when row countryname equals to colum countryname, set the cell as zero #########
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
    
    ##Gross export of each country S
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
    
    
    y.colsum<-gross.export$export
    y.colsum<-as.matrix(y.colsum)
    
    ######calucate gross value added for each country sector
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
    
    
    #######################################################################################################################
    ########################## Dependence of a country S on a country sector RQ ###########################################
    #######################################################################################################################
    
    ##Sens.Dep.s.rq.supplier, how sensitive is a country S as a supplier to a country sector RQ as a buyer.
    valueadd1<-cbind(y.colsum,valueadd)
    Sens.Dep.supplier<-t(apply(valueadd1, 1, function(valueadd1)valueadd1/valueadd1[1])) # 1 for row
    Sens.Dep.supplier<-Sens.Dep.supplier[,-1] 
    Sens.Dep.supplier<-as.data.frame(Sens.Dep.supplier)
    
    #Sens.Dep.s.rq.buyer, how sensitive is a country S as a buyer to a country sector RQ as a supplier. 
    y.colsum1<-t(y.colsum)
    y.colsum1<-matrix(rep(y.colsum1[1,],each=2340),nrow =2340)
    Sens.Dep.buyer<-valueadd/y.colsum1 
    Sens.Dep.buyer<-t(Sens.Dep.buyer) 
    Sens.Dep.buyer<-as.data.frame(Sens.Dep.buyer)
    
    hh<-0 #number of industry
    Sens.Dep.s.rq.supplier<-NULL
    Sens.Dep.s.rq.buyer<-NULL
    while(hh<2340) {
      m<-hh+1
      n<-hh+36
      Midsupplier<-Sens.Dep.supplier[m:n,]
      Midbuyer<-Sens.Dep.buyer[m:n,]
      
      Midsupplier<-colSums(Midsupplier) %>% t()
      Sens.Dep.s.rq.supplier<-rbind(Sens.Dep.s.rq.supplier, Midsupplier)
      
      Midbuyer<-colSums(Midbuyer) %>%  t()
      Sens.Dep.s.rq.buyer<-rbind(Sens.Dep.s.rq.buyer, Midbuyer)
      
      hh<-hh+36
      
    }
    
    #Sens.Dep.s.rq, the total GVC sensitivity dependence of a country S on a country sector RQ. 
    Sens.Dep.s.rq=Sens.Dep.s.rq.supplier+Sens.Dep.s.rq.buyer
    
    Sens.Dep.s.rq.supplier<-as.data.frame(Sens.Dep.s.rq.supplier)
    Sens.Dep.s.rq.buyer<-as.data.frame(Sens.Dep.s.rq.buyer)
    Sens.Dep.s.rq<-as.data.frame(Sens.Dep.s.rq)
    
    
    #Vul.Dep.s.rq.supplier, how vulnerable a country S is to a country sector RQ in terms of 
    #its dependence on the country sector RQ to absorb the value added produced by country S
    Sens.Dep.s.rq.supplier.V<-Sens.Dep.s.rq.supplier
    HHI.S<-Sens.Dep.s.rq.supplier.V^2
    HHI.S<-as.data.frame(HHI.S)
    
    HHIname<-names(HHI.S)
    HHI.S.SectorName<-substring(HHIname,5,20)
    HHI.S.CountryName<-substring(HHIname,1,3)
    
    HHI.S.Name<-unique(HHI.S.SectorName)
    HHI.S.CountryName<-unique(HHI.S.CountryName)
    hh<-1 #number of industry
    HHI.Vulner<-NULL
    while (hh<37) {
      
      HHI<-HHI.S[,grep(colnames(HHI.S),pattern = HHI.S.Name[hh],fixed = TRUE)]
      HHI.S.Sum<-rowSums(HHI)
      HHI.S.Sum<-as.data.frame(HHI.S.Sum)
      rownames(HHI.S.Sum)<-HHI.S.CountryName
      colnames(HHI.S.Sum)<-HHI.S.Name[hh]
      
      ## HHI of country sector
      if(hh==1){
        HHI.Vulner<-HHI.S.Sum
      }else{
        HHI.Vulner<-cbind(HHI.Vulner,HHI.S.Sum)
      }
      hh<-hh+1
    }
    
    HHI.Copy<-rep(HHI.Vulner,65)
    HHI.Copy<-as.data.frame(HHI.Copy)
    Vul.Dep.s.rq.supplier<-HHI.Copy*Sens.Dep.s.rq.supplier
    
    #Vul.Dep.s.rq.buyer, how vulnerable country S is to country sector RQ in in terms of 
    #its export’s dependence on the value added contribution from country sector RQ.
    Sens.Dep.s.rq.buyer.V<-Sens.Dep.s.rq.buyer  ## we invert the Sens.Dep.s.rq.buyer before, so can use the same way to calculate as Sens.Dep.s.rq.supplier
    HHI.B<-Sens.Dep.s.rq.buyer.V^2
    HHI.B<-as.data.frame(HHI.B)
    
    HHIname<-names(HHI.B)
    HHI.B.SectorName<-substring(HHIname,5,20)
    HHI.B.CountryName<-substring(HHIname,1,3)
    
    HHI.B.Name<-unique(HHI.B.SectorName)
    HHI.B.CountryName<-unique(HHI.B.CountryName)
    hh<-1 #number of industry
    HHI.Vulner<-NULL
    while (hh<37) {
      
      HHI<-HHI.B[,grep(colnames(HHI.B),pattern = HHI.B.Name[hh],fixed = TRUE)]
      HHI.B.Sum<-rowSums(HHI)
      HHI.B.Sum<-as.data.frame(HHI.B.Sum)
      rownames(HHI.B.Sum)<-HHI.B.CountryName
      colnames(HHI.B.Sum)<-HHI.B.Name[hh]
      
      ## HHI of country sector
      if(hh==1){
        HHI.Vulner<-HHI.B.Sum
      }else{
        HHI.Vulner<-cbind(HHI.Vulner,HHI.B.Sum)
      }
      hh<-hh+1
    }
    
    HHI.Copy<-rep(HHI.Vulner,65)
    HHI.Copy<-as.data.frame(HHI.Copy)
    Vul.Dep.s.rq.buyer<-HHI.Copy*Sens.Dep.s.rq.buyer
    
    ##Vul.Dep
    Vul.Dep.s.rq = Vul.Dep.s.rq.supplier + Vul.Dep.s.rq.buyer
    
    
    #######################################################################################################################
    ########################## Dependence of a country sector SP on a country R ###########################################
    #######################################################################################################################  
    
    #Sens.Dep.s.rq.buyer, how sensitive is a country S as a buyer to a country sector RQ as a supplier. 
    y.colsum1<-t(Ycolsum)
    y.colsum1<-matrix(rep(y.colsum1[1,],each=2340),nrow =2340)
    Sens.Dep.buyer<-valueadd/y.colsum1 
    Sens.Dep.buyer<-as.data.frame(Sens.Dep.buyer)
    
    hh<-0 #number of industry
    Sens.Dep.sp.r.buyer<-NULL
    while(hh<2340) {
      m<-hh+1
      n<-hh+36
      Midbuyer<-Sens.Dep.buyer[m:n,]
      
      Midbuyer<-colSums(Midbuyer) %>%  t()
      Sens.Dep.sp.r.buyer<-rbind(Sens.Dep.sp.r.buyer, Midbuyer)
      
      hh<-hh+36
      
    }
    
    Sens.Dep.sp.r.buyer<-as.data.frame(Sens.Dep.sp.r.buyer)
    
    #Vul.Dep.sp.r.buyer, how vulnerable country sector SP is to country R in in terms of its
    #export’s dependence on the value added contribution from country R.
    Sens.Dep.sp.r.buyer.V<-Sens.Dep.sp.r.buyer
    HHI.B<-Sens.Dep.sp.r.buyer.V^2
    HHI.B.Sum<-colSums(HHI.B)
    
    Vul.Dep.sp.r.buyer<-HHI.B.Sum*Sens.Dep.sp.r.buyer
    
    #Sens.Dep.sp.r.supplier, how sensitive is country sector SP as a buyer to country R as a supplier,the denominator is Vasp
    valueadd1<-cbind(gross.valu$value,valueadd)
    Sens.Dep.supplier<-t(apply(valueadd1, 1, function(valueadd1)valueadd1/valueadd1[1])) # 1 for row
    Sens.Dep.supplier<-Sens.Dep.supplier[,-1] 
    Sens.Dep.supplier<-as.data.frame(Sens.Dep.supplier)
    
    
    hh<-0 #number of industry
    Sens.Dep.sp.r.supplier<-NULL
    while(hh<2340) {
      m<-hh+1
      n<-hh+36
      Midsupplier<-Sens.Dep.supplier[,m:n]
      
      Midsupplier<-rowSums(Midsupplier)
      Sens.Dep.sp.r.supplier<-cbind(Sens.Dep.sp.r.supplier, Midsupplier)
      
      hh<-hh+36
      
    }
    
    Sens.Dep.sp.r.supplier<-as.data.frame(Sens.Dep.sp.r.supplier)
    
    #Vul.Dep.sp.r.supplier, how vulnerable country sector SP is to 
    #country R in in terms of how much its value-added production rely on the demand from country R in its export.
    Sens.Dep.sp.r.supplier.V<-Sens.Dep.sp.r.supplier
    HHI.S<-Sens.Dep.sp.r.supplier.V^2
    HHI.S.Sum<-rowSums(HHI.S)
    
    Vul.Dep.sp.r.supplier<-HHI.S*Sens.Dep.sp.r.supplier
    
    
    #######################################################################################################################
    ########################## Vulnerability of Country Sector ###########################################
    #######################################################################################################################  
    
    #HHI.Supplier.sp, we can consider use V asp as denominator to calculate this measure.
    valueadd1<-cbind(gross.valu$value,valueadd)
    Sens.Dep.supplier<-t(apply(valueadd1, 1, function(valueadd1)valueadd1/valueadd1[1])) # 1 for row
    Sens.Dep.supplier<-Sens.Dep.supplier[,-1] 
    Sens.Dep.supplier<-as.data.frame(Sens.Dep.supplier)
    
    Sens.Dep.supplier<-Sens.Dep.supplier^2
    HHI.Supplier.sp<-rowSums(Sens.Dep.supplier)
    
    #HHI.Buyer.sp vulnerability of a country sector SP as buyer depends on how concentrated its suppliers are.
    y.colsum1<-t(Ycolsum)
    y.colsum1<-matrix(rep(y.colsum1[1,],each=2340),nrow =2340)
    Sens.Dep.buyer<-valueadd/y.colsum1 
    Sens.Dep.buyer<-as.data.frame(Sens.Dep.buyer)
    
    Sens.Dep.buyer<- Sens.Dep.buyer^2
    HHI.Buyer.sp<-colSums(Sens.Dep.buyer)
    
    ############################Store data by dyad (row country_industry-colum country_industry)#############################
    Country_Industry<-names(valueadd)
    IndustryCode<-substring(Country_Industry,5,20) %>% unique()
    CountryCode<-substring(Country_Industry,1,3) %>% unique()
    
    
    
    ### transfer from 2340*65 to 65*2340
    Sens.Dep.sp.r.supplier<-t(Sens.Dep.sp.r.supplier)
    Vul.Dep.sp.r.supplier<-t(Vul.Dep.sp.r.supplier)
    
    n<-1
    for (n in 1:65) {
      m<-1
      for (m in 1:2340) {
        Year.b[j]<-i
        Country_s[j]<-CountryCode[n]
        CountrySector_rq[j]<-Country_Industry[m]
        Sens.Dep.s.rq.supplier.b[j]<-Sens.Dep.s.rq.supplier[n,m]
        Sens.Dep.s.rq.buyer.b[j]<-Sens.Dep.s.rq.buyer[n,m]
        Sens.Dep.s.rq.b[j]<-Sens.Dep.s.rq[n,m]
        Vul.Dep.s.rq.supplier.b[j]<-Vul.Dep.s.rq.supplier[n,m]
        Vul.Dep.s.rq.buyer.b[j]<-Vul.Dep.s.rq.buyer[n,m]
        Vul.Dep.s.rq.b[j]<-Vul.Dep.s.rq[n,m]
        
        j<-j+1
        m<-m+1 
      }
      n<-n+1
    } 
    
    n<-1
    for (n in 1:65) {
      m<-1
      for (m in 1:2340) {
        Country_r[jj]<-CountryCode[n]
        CountrySector_sp[jj]<-Country_Industry[m]
        Sens.Dep.sp.r.buyer.b[jj]<-Sens.Dep.sp.r.buyer[n,m]
        Vul.Dep.sp.r.buyer.b[jj]<-Vul.Dep.sp.r.buyer[n,m]
        Sens.Dep.sp.r.supplier.b[jj]<-Sens.Dep.sp.r.supplier[n,m]
        Vul.Dep.sp.r.supplier.b[jj]<-Vul.Dep.sp.r.supplier[n,m]
        
        jj<-jj+1
        m<-m+1 
      }
      n<-n+1
    } 
    
    ###################################store HHI#################################
    Year<-rep(i,times=2340) #2176 is the total number of country-sector involved.
    if(i==2005){
      HHI.2018<-data.frame(Country_Industry,Year,HHI.Supplier.sp,HHI.Buyer.sp)
    }else{
      data<-data.frame(Country_Industry,Year,HHI.Supplier.sp,HHI.Buyer.sp)
      HHI.2018<-merge(data,HHI.2018,all=TRUE)
    }
    
    i<-i+1
  }      
  
  GVCDependence.s.rq.2018<-data.frame(Country_s,CountrySector_rq,Year.b,Sens.Dep.s.rq.supplier.b,Sens.Dep.s.rq.buyer.b,Sens.Dep.s.rq.b,
                                      Vul.Dep.s.rq.supplier.b,Vul.Dep.s.rq.buyer.b,Vul.Dep.s.rq.b)
  
  GVCDependence.sp.r.2018<-data.frame(Country_r,CountrySector_sp,Year.b,Sens.Dep.sp.r.buyer.b,Vul.Dep.sp.r.buyer.b,Sens.Dep.sp.r.supplier.b,Vul.Dep.sp.r.supplier.b)
  
  names(GVCDependence.s.rq.2018)[3:9]<-c("Year","Sens.Dep.s.rq.supplier","Sens.Dep.s.rq.buyer","Sens.Dep.s.rq",
                                         "Vul.Dep.s.rq.supplier","Vul.Dep.s.rq.buyer","Vul.Dep.s.rq")
  names(GVCDependence.sp.r.2018)[3:7]<-c("Year","Sens.Dep.sp.r.buyer","Vul.Dep.sp.r.buyer","Sens.Dep.sp.r.supplier","Vul.Dep.sp.r.supplier")
  
  ##delete self-self
  GVCDependence.s.rq.2018[,c("Country_r","Sector_q")]<-str_split_fixed(GVCDependence.s.rq.2018$CountrySector_rq, "_", 2)
  GVCDependence.s.rq.2018<-GVCDependence.s.rq.2018[GVCDependence.s.rq.2018$Country_s!=GVCDependence.s.rq.2018$Country_r,]
  #GVCDependence.s.rq.2018<-GVCDependence.s.rq.2018[,-c(10,11)]
  
  GVCDependence.sp.r.2018[,c("Country_s","Sector_p")]<-str_split_fixed(GVCDependence.sp.r.2018$CountrySector_sp,"_", 2)
  GVCDependence.sp.r.2018<-GVCDependence.sp.r.2018[GVCDependence.sp.r.2018$Country_r!=GVCDependence.sp.r.2018$Country_s,]
  #GVCDependence.sp.r.2018<-GVCDependence.sp.r.2018[,-c(8,9)]
  
  ##order as year
  HHI.2018<-HHI.2018[order(HHI.2018$Year),]
  
  
  ##write
  write.csv(GVCDependence.s.rq.2018,file="/Users/sandy/Documents/DataCode/ICIO/2018_GVC_Indicator/GVCDependence.s.rq.2018")
  write.csv(GVCDependence.sp.r.2018,file="/Users/sandy/Documents/DataCode/ICIO/2018_GVC_Indicator/GVCDependence.sp.r.2018")
  write.csv(HHI.2018,file="/Users/sandy/Documents/DataCode/ICIO/2018_GVC_Indicator/ICIO2018_HHI.csv")
  
  
  
  
  
 