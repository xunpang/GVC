WD<-getwd()
if(!is.null(WD))setwd("/Users/sandy/Documents/DataCode/ICIO/")
getwd()

library(dplyr)
library(tidyr)
library(tidyverse)


##Read Data
ICIO2016_GVCInfluence.Participation<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2016_GVC_Indicator/ICIO2016_GVCInfluence.Participation.csv")
GVCDependence.s.rq.2016<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2016_GVC_Indicator/GVCDependence.s.rq.2016")
GVCDependence.sp.r.2016<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2016_GVC_Indicator/GVCDependence.sp.r.2016")
HHI.2016<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2016_GVC_Indicator/ICIO2016_HHI.csv")

ICIO2018_GVCInfluence.Participation<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2018_GVC_Indicator/ICIO2018_GVCInfluence.Participation.csv")
GVCDependence.s.rq.2018<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2018_GVC_Indicator/GVCDependence.s.rq.2018")
GVCDependence.sp.r.2018<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2018_GVC_Indicator/GVCDependence.sp.r.2018")
HHI.2018<-read.csv("/Users/sandy/Documents/DataCode/ICIO/2018_GVC_Indicator/ICIO2018_HHI.csv")

## Merge HHI with Influence.Participation
ICIO2016_GVCInfluence.Participation<-ICIO2016_GVCInfluence.Participation[,-1]
HHI.2016<-HHI.2016[,-1]
ICIO2016_GVCInfluence.Participation<-merge(ICIO2016_GVCInfluence.Participation,HHI.2016,by=c("Country_Industry","Year"), all.x = TRUE )
#write.csv(ICIO2016_GVCInfluence.Participation,file="/Users/sandy/Documents/DataCode/ICIO/2016_GVC_Indicator/ICIO2016_GVCInfluence.Participation.csv")


ICIO2018_GVCInfluence.Participation<-ICIO2018_GVCInfluence.Participation[,-1]
HHI.2018<-HHI.2018[,-1]
ICIO2018_GVCInfluence.Participation<-merge(ICIO2018_GVCInfluence.Participation,HHI.2018,by=c("Country_Industry","Year"), all.x = TRUE )
#write.csv(ICIO2018_GVCInfluence.Participation,file="/Users/sandy/Documents/DataCode/ICIO/2018_GVC_Indicator/ICIO2018_GVCInfluence.Participation.csv")


####### Merge 2016 GVC Version with 2018 GVC Version

################################################################################################
#### Merge GVCInfluence.Participation ###########################################################
################################################################################################

#change the industry code of 2016 according to the industry code of 2018
IndustryCode.2016<-c("C01T05","C15T16","C17T19","C20","C23","C24",   "C25","C26","C27","C28","C30T33X","C31","C29","C34","C35","C36T37","C45",
                     "C55",   "C72","C65T67","C70","C75","C80","C85","C95")
IndustryCode.2018<-c("D01T03","D10T12","D13T15","D16","D19","D20T21","D22","D23","D24","D25","D26",    "D27","D28","D29","D30","D31T33","D41T43",
                     "D55T56","D62T63","D64T66","D68","D84","D85","D86T88","D97T98")

## choose data from 1995 to 2004 from 2016 GVC Version
GVCInfluence.Participation<-ICIO2016_GVCInfluence.Participation[which(ICIO2016_GVCInfluence.Participation$Year %in% c(1995:2004)),]

GVCInfluence.Participation$IndustryCode<-as.character(GVCInfluence.Participation$IndustryCode)
i<-1
for (i in 1:25) {
  GVCInfluence.Participation$IndustryCode[which(GVCInfluence.Participation$IndustryCode==IndustryCode.2016[i])]<-IndustryCode.2018[i]
}

GVCInfluence.Participation$Country_Industry<-paste(GVCInfluence.Participation$CountryCode,GVCInfluence.Participation$IndustryCode,sep = "_")

#merge
GVCInfluence.Participation<-rbind(GVCInfluence.Participation,ICIO2018_GVCInfluence.Participation)

GVCInfluence.Participation<-GVCInfluence.Participation[order(GVCInfluence.Participation$CountryCode,GVCInfluence.Participation$IndustryCode,GVCInfluence.Participation$Year),]

#write.csv(GVCInfluence.Participation,file="/Users/sandy/Documents/DataCode/ICIO/2018_GVC_Indicator/GVCInfluence.Participation.csv")

#######################################################################################################
#### Merge GVCDependence.s.rq ########################################################################
#######################################################################################################
GVCDependence.s.rq.2016<-GVCDependence.s.rq.2016[,-1]
GVCDependence.s.rq.2018<-GVCDependence.s.rq.2018[,-1]

GVCDependence.s.rq<-GVCDependence.s.rq.2016[which(GVCDependence.s.rq.2016$Year %in% c(1995:2004)),]
GVCDependence.s.rq[,c("Country_r","Sector_q")]<-str_split_fixed(GVCDependence.s.rq$CountrySector_rq, "_", 2)

GVCDependence.s.rq$Sector_q<-as.character(GVCDependence.s.rq$Sector_q)
for (i in 1:25) {
  GVCDependence.s.rq$Sector_q[which(GVCDependence.s.rq$Sector_q==IndustryCode.2016[i])]<-IndustryCode.2018[i]
}

GVCDependence.s.rq$CountrySector_rq<-paste(GVCDependence.s.rq$Country_r,GVCDependence.s.rq$Sector_q,sep="_")

GVCDependence.s.rq<-GVCDependence.s.rq[,-c(10,11)]

#merge
GVCDependence.s.rq<-rbind(GVCDependence.s.rq,GVCDependence.s.rq.2018)

#write.csv(GVCDependence.s.rq,file="/Users/sandy/Documents/DataCode/ICIO/2018_GVC_Indicator/GVCDependence.s.rq.csv")

################################################################################################
### Merge GVCDependence.sp.r ##################################################################
################################################################################################
GVCDependence.sp.r.2016<-GVCDependence.sp.r.2016[,-1]
GVCDependence.sp.r.2018<-GVCDependence.sp.r.2018[,-1]

GVCDependence.sp.r<-GVCDependence.sp.r.2016[which(GVCDependence.sp.r.2016$Year %in% c(1995:2004)),]
GVCDependence.sp.r[,c("Country_s","Sector_p")]<-str_split_fixed(GVCDependence.sp.r$CountrySector_sp,"_", 2)
GVCDependence.sp.r$Sector_p<-as.character(GVCDependence.sp.r$Sector_p)

for(i in 1:25){
  GVCDependence.sp.r$Sector_p[which(GVCDependence.sp.r$Sector_p==IndustryCode.2016[i])]<-IndustryCode.2018[i]
}

GVCDependence.sp.r$CountrySector_sp<-paste(GVCDependence.sp.r$Country_s,GVCDependence.sp.r$Sector_p,sep = "_")

GVCDependence.sp.r<-GVCDependence.sp.r[,-c(8,9)]

#merge
GVCDependence.sp.r<-rbind(GVCDependence.sp.r,GVCDependence.sp.r.2018)

#write.csv(GVCDependence.sp.r,file="/Users/sandy/Documents/DataCode/ICIO/2018_GVC_Indicator/GVCDependence.sp.r.csv")
















