# non-MDF Zwart 5 analysis; 2017-04-27
# JAZ
rm(list=ls())
library(MASS)
library(data.table)
library(R.utils)
# sourceDirectory('/Users/Jake/Desktop/R functions/',modifiedOnly = F)

load('R Data/CR_20170413.RData')
CR=data
load('R Data/EL_20170408.RData')
EL=data
load('R Data/MO_20170412.RData')
MO=data
load('R Data/WL_20170429.RData')
WL=data
load('R Data/HB_20170429.RData')
HB=data

rm(data)

all=data.frame(lakeID=c('CR','EL','MO','WL','HB'),
               Area=rep(NA,5),Vol=rep(NA,5),Perim=rep(NA,5),
               zMix=rep(NA,5),GPP=rep(NA,5),R=rep(NA,5),NEP=rep(NA,5),DOC_In=rep(NA,5),
               POC_In=rep(NA,5),CO2_In=rep(NA,5),DIC_In=rep(NA,5),DOC_Out=rep(NA,5),POC_Out=rep(NA,5),
               C_sed=rep(NA,5),CO2=rep(NA,5),k=rep(NA,5),CO2Flux=rep(NA,5),HRT=rep(NA,5),FracEvap=rep(NA,5),
               FracRet=rep(NA,5),wetlandLoad=rep(NA,5),wetlandDICLoad=rep(NA,5),leafLoad=rep(NA,5),
               hypoDICinc=rep(NA,5),hypoDOCinc=rep(NA,5),hypoPOCinc=rep(NA,5),
               DICinc=rep(NA,5),DOCinc=rep(NA,5),POCinc=rep(NA,5),DIC_Out=rep(NA,5))

all$Area[1]=CR$A0[1]
all$Area[2]=EL$A0[1]
all$Area[3]=MO$A0[1]
all$Area[4]=WL$A0[1]
all$Area[5]=HB$A0[1]

all$Vol[1]=CR$V0[1]
all$Vol[2]=EL$V0[1]
all$Vol[3]=MO$V0[1]
all$Vol[4]=WL$V0[1]
all$Vol[5]=HB$V0[1]

all$Perim=c(3455,800,1050,1311,350)

# annual metabolic rates
all$GPP[1]=mean(CR$GPP,na.rm = T) # mol C day-1 in epi
all$GPP[2]=mean(EL$GPP,na.rm = T) # mol C day-1 in epi
all$GPP[3]=mean(MO$GPP,na.rm = T) # mol C day-1 in epi
all$GPP[4]=mean(WL$GPP,na.rm = T) # mol C day-1 in epi
all$GPP[5]=mean(HB$GPP,na.rm = T) # mol C day-1 in epi

all$sdGPP[1]=mean(CR$sdGPP,na.rm = T) # mol C day-1 in epi
all$sdGPP[2]=mean(EL$sdGPP,na.rm = T) # mol C day-1 in epi
all$sdGPP[3]=mean(MO$sdGPP,na.rm = T) # mol C day-1 in epi
all$sdGPP[4]=mean(WL$sdGPP,na.rm = T) # mol C day-1 in epi
all$sdGPP[5]=mean(HB$sdGPP,na.rm = T) # mol C day-1 in epi

all$R[1]=mean(CR$R,na.rm = T) # mol C day-1 in epi
all$R[2]=mean(EL$R,na.rm = T) # mol C day-1 in epi
all$R[3]=mean(MO$R,na.rm = T) # mol C day-1 in epi
all$R[4]=mean(WL$R,na.rm = T) # mol C day-1 in epi
all$R[5]=mean(HB$R,na.rm = T) # mol C day-1 in epi

all$sdR[1]=mean(CR$sdR,na.rm = T) # mol C day-1 in epi
all$sdR[2]=mean(EL$sdR,na.rm = T) # mol C day-1 in epi
all$sdR[3]=mean(MO$sdR,na.rm = T) # mol C day-1 in epi
all$sdR[4]=mean(WL$sdR,na.rm = T) # mol C day-1 in epi
all$sdR[5]=mean(HB$sdR,na.rm = T) # mol C day-1 in epi

CR$NEP=CR$GPP-CR$R
EL$NEP=EL$GPP-EL$R
MO$NEP=MO$GPP-MO$R
WL$NEP=WL$GPP-WL$R
HB$NEP=HB$GPP-HB$R
all$NEP[1]=mean(CR$NEP,na.rm = T) # mol C day-1 in epi
all$NEP[2]=mean(EL$NEP,na.rm = T) # mol C day-1 in epi
all$NEP[3]=mean(MO$NEP,na.rm = T) # mol C day-1 in epi
all$NEP[4]=mean(WL$NEP,na.rm = T) # mol C day-1 in epi
all$NEP[5]=mean(HB$NEP,na.rm = T) # mol C day-1 in epi

all$sdNEP[1]=sqrt(all$sdGPP[1]^2+all$sdR[1]^2)
all$sdNEP[2]=sqrt(all$sdGPP[2]^2+all$sdR[2]^2)
all$sdNEP[3]=sqrt(all$sdGPP[3]^2+all$sdR[3]^2)
all$sdNEP[4]=sqrt(all$sdGPP[4]^2+all$sdR[4]^2)
all$sdNEP[5]=sqrt(all$sdGPP[5]^2+all$sdR[5]^2)

# annual sedimentation rates
mean(CR$sed_C_day,na.rm = T) # mol C day-1
mean(EL$sed_C_day,na.rm = T) # mol C day-1
mean(MO$sed_C_day,na.rm = T) # mol C day-1
mean(WL$sed_C_day,na.rm = T) # mol C day-1
mean(HB$sed_C_day,na.rm = T) # mol C day-1

mean(CR$sed_C_day/CR$A0,na.rm = T) # mol C m-2 day-1
mean(EL$sed_C_day/EL$A0,na.rm = T) # mol C m-2 day-1
mean(MO$sed_C_day/MO$A0,na.rm = T) # mol C m-2 day-1
mean(WL$sed_C_day/WL$A0,na.rm = T) # mol C m-2 day-1
mean(HB$sed_C_day/HB$A0,na.rm = T) # mol C m-2 day-1

########### long-term carbon burial
crBury=read.csv('Data/FreezeCores/CR.csv',stringsAsFactors = F)
elBury=read.csv('Data/FreezeCores/EL.csv',stringsAsFactors = F)
moBury=read.csv('Data/FreezeCores/MO.csv',stringsAsFactors = F)
wlBury=read.csv('Data/FreezeCores/WL.csv',stringsAsFactors = F)
hbBury=read.csv('Data/FreezeCores/HB.csv',stringsAsFactors = F)

crDryMass=mean(crBury$Sediment_DMAR_g_cm2_yr[1:3]) # g Dry mass cm-2 yr-1
elDryMass=mean(elBury$Sediment_DMAR_g_cm2_yr[1:3]) # g Dry mass cm-2 yr-1
moDryMass=mean(moBury$Sediment_DMAR_g_cm2_yr[1:3]) # g Dry mass cm-2 yr-1
wlDryMass=mean(wlBury$Sediment_DMAR_g_cm2_yr[1:3]) # g Dry mass cm-2 yr-1
hbDryMass=mean(hbBury$Sediment_DMAR_g_cm2_yr[1:3]) # g Dry mass cm-2 yr-1

# average C content in top layer of sediment samples
cr_percentC=29 # % of dry mass as C
el_percentC=33
mo_percentC=25
wl_percentC=26
hb_percentC=28

cr_focus=1.133 # sediment focusing from St. Croix ; look at Lamborg et al. 2013 for methods
el_focus=2.177
mo_focus=0.822
wl_focus=1.133
hb_focus=0.266

crC_bury=crDryMass/cr_focus*cr_percentC/100/12*100*100/365 # mol C m-2 day-1
elC_bury=elDryMass/el_focus*el_percentC/100/12*100*100/365 # mol C m-2 day-1
moC_bury=moDryMass/mo_focus*mo_percentC/100/12*100*100/365 # mol C m-2 day-1
wlC_bury=wlDryMass/wl_focus*wl_percentC/100/12*100*100/365 # mol C m-2 day-1
hbC_bury=hbDryMass/hb_focus*hb_percentC/100/12*100*100/365 # mol C m-2 day-1

all$C_sed[1]=crC_bury*all$Area[1] # mol C day-1
all$C_sed[2]=elC_bury*all$Area[2] # mol C day-1
all$C_sed[3]=moC_bury*all$Area[3] # mol C day-1
all$C_sed[4]=wlC_bury*all$Area[4]
all$C_sed[5]=hbC_bury*all$Area[5] # mol C day-1

############### carbon load
all$DOC_In[1]=mean(CR$docIn,na.rm=T) # mol C day-1
all$DOC_In[2]=mean(EL$docIn,na.rm=T) # mol C day-1
all$DOC_In[3]=mean(MO$docIn,na.rm=T) # mol C day-1
all$DOC_In[4]=mean(WL$docIn,na.rm=T)
all$DOC_In[5]=mean(HB$docIn,na.rm = T) # mol C day-1

all$POC_In[1]=mean(CR$pocIn,na.rm=T)
all$POC_In[2]=mean(EL$pocIn,na.rm=T)
all$POC_In[3]=mean(MO$pocIn,na.rm=T)
all$POC_In[4]=mean(WL$pocIn,na.rm = T)
all$POC_In[5]=mean(HB$pocIn,na.rm = T)

# hydrologic carbon export
CR$doc_Int=CR$doc # epilimnion DOC pool; mol C
CR$doc_Int[1]=CR$doc_Int[min(which(!is.na(CR$doc_Int)))]
CR$doc_Int[nrow(CR)]=CR$doc_Int[max(which(!is.na(CR$doc_Int)))]
CR$doc_Int=approx(1:nrow(CR),CR$doc_Int,1:nrow(CR))$y
CR$docOut=CR$QoutInt*CR$doc_Int/CR$epiVol # mol C day-1

EL$doc_Int=EL$doc # epilimnion DOC pool; mol C
EL$doc_Int[1]=EL$doc_Int[min(which(!is.na(EL$doc_Int)))]
EL$doc_Int[nrow(EL)]=EL$doc_Int[max(which(!is.na(EL$doc_Int)))]
EL$doc_Int=approx(1:nrow(EL),EL$doc_Int,1:nrow(EL))$y
EL$docOut=EL$QoutInt*EL$doc_Int/EL$epiVol # mol C day-1

MO$doc_Int=MO$doc # epilimnion DOC pool; mol C
MO$doc_Int[1]=MO$doc_Int[min(which(!is.na(MO$doc_Int)))]
MO$doc_Int[nrow(MO)]=MO$doc_Int[max(which(!is.na(MO$doc_Int)))]
MO$doc_Int=approx(1:nrow(MO),MO$doc_Int,1:nrow(MO))$y
MO$docOut=MO$QoutInt*MO$doc_Int/MO$epiVol # mol C day-1

WL$doc_Int=WL$doc # epilimnion DOC pool; mol C
WL$doc_Int[1]=WL$doc_Int[min(which(!is.na(WL$doc_Int)))]
WL$doc_Int[nrow(WL)]=WL$doc_Int[max(which(!is.na(WL$doc_Int)))]
WL$doc_Int=approx(1:nrow(WL),WL$doc_Int,1:nrow(WL))$y
WL$docOut=WL$QoutInt*WL$doc_Int/WL$epiVol # mol C day-1

HB$doc_Int=HB$doc # epilimnion DOC pool; mol C
HB$doc_Int[1]=HB$doc_Int[min(which(!is.na(HB$doc_Int)))]
HB$doc_Int[nrow(HB)]=HB$doc_Int[max(which(!is.na(HB$doc_Int)))]
HB$doc_Int=approx(1:nrow(HB),HB$doc_Int,1:nrow(HB))$y
HB$docOut=HB$QoutInt*HB$doc_Int/HB$epiVol # mol C day-1


CR$poc_Int=CR$poc # epilimnion DOC pool; mol C
CR$poc_Int[1]=CR$poc_Int[min(which(!is.na(CR$poc_Int)))]
CR$poc_Int[nrow(CR)]=CR$poc_Int[max(which(!is.na(CR$poc_Int)))]
CR$poc_Int=approx(1:nrow(CR),CR$poc_Int,1:nrow(CR))$y
CR$pocOut=CR$QoutInt*CR$poc_Int/CR$epiVol # mol C day-1

EL$poc_Int=EL$poc # epilimnion DOC pool; mol C
EL$poc_Int[1]=EL$poc_Int[min(which(!is.na(EL$poc_Int)))]
EL$poc_Int[nrow(EL)]=EL$poc_Int[max(which(!is.na(EL$poc_Int)))]
EL$poc_Int=approx(1:nrow(EL),EL$poc_Int,1:nrow(EL))$y
EL$pocOut=EL$QoutInt*EL$poc_Int/EL$epiVol # mol C day-1

MO$poc_Int=MO$poc # epilimnion DOC pool; mol C
MO$poc_Int[1]=MO$poc_Int[min(which(!is.na(MO$poc_Int)))]
MO$poc_Int[nrow(MO)]=MO$poc_Int[max(which(!is.na(MO$poc_Int)))]
MO$poc_Int=approx(1:nrow(MO),MO$poc_Int,1:nrow(MO))$y
MO$pocOut=MO$QoutInt*MO$poc_Int/MO$epiVol # mol C day-1

WL$poc_Int=WL$poc # epilimnion DOC pool; mol C
WL$poc_Int[1]=WL$poc_Int[min(which(!is.na(WL$poc_Int)))]
WL$poc_Int[nrow(WL)]=WL$poc_Int[max(which(!is.na(WL$poc_Int)))]
WL$poc_Int=approx(1:nrow(WL),WL$poc_Int,1:nrow(WL))$y
WL$pocOut=WL$QoutInt*WL$poc_Int/WL$epiVol # mol C day-1

HB$poc_Int=HB$poc # epilimnion DOC pool; mol C
HB$poc_Int[1]=HB$poc_Int[min(which(!is.na(HB$poc_Int)))]
HB$poc_Int[nrow(HB)]=HB$poc_Int[max(which(!is.na(HB$poc_Int)))]
HB$poc_Int=approx(1:nrow(HB),HB$poc_Int,1:nrow(HB))$y
HB$pocOut=HB$QoutInt*HB$poc_Int/HB$epiVol # mol C day-1

CR$dic_Int=CR$dic # epilimnion DIC pool; mol C
CR$dic_Int[1]=CR$dic_Int[min(which(!is.na(CR$dic_Int)))]
CR$dic_Int[nrow(CR)]=CR$dic_Int[max(which(!is.na(CR$dic_Int)))]
CR$dic_Int=approx(1:nrow(CR),CR$dic_Int,1:nrow(CR))$y
CR$dicOut=CR$QoutInt*CR$dic_Int/CR$epiVol # mol C day-1

EL$dic_Int=EL$dic # epilimnion DIC pool; mol C
EL$dic_Int[1]=EL$dic_Int[min(which(!is.na(EL$dic_Int)))]
EL$dic_Int[nrow(EL)]=EL$dic_Int[max(which(!is.na(EL$dic_Int)))]
EL$dic_Int=approx(1:nrow(EL),EL$dic_Int,1:nrow(EL))$y
EL$dicOut=EL$QoutInt*EL$dic_Int/EL$epiVol # mol C day-1

MO$dic_Int=MO$dic # epilimnion DIC pool; mol C
MO$dic_Int[1]=MO$dic_Int[min(which(!is.na(MO$dic_Int)))]
MO$dic_Int[nrow(MO)]=MO$dic_Int[max(which(!is.na(MO$dic_Int)))]
MO$dic_Int=approx(1:nrow(MO),MO$dic_Int,1:nrow(MO))$y
MO$dicOut=MO$QoutInt*MO$dic_Int/MO$epiVol # mol C day-1

WL$dic_Int=WL$dic # epilimnion DIC pool; mol C
WL$dic_Int[1]=WL$dic_Int[min(which(!is.na(WL$dic_Int)))]
WL$dic_Int[nrow(WL)]=WL$dic_Int[max(which(!is.na(WL$dic_Int)))]
WL$dic_Int=approx(1:nrow(WL),WL$dic_Int,1:nrow(WL))$y
WL$dicOut=WL$QoutInt*WL$dic_Int/WL$epiVol # mol C day-1

HB$dic_Int=HB$dic # epilimnion DIC pool; mol C
HB$dic_Int[1]=HB$dic_Int[min(which(!is.na(HB$dic_Int)))]
HB$dic_Int[nrow(HB)]=HB$dic_Int[max(which(!is.na(HB$dic_Int)))]
HB$dic_Int=approx(1:nrow(HB),HB$dic_Int,1:nrow(HB))$y
HB$dicOut=HB$QoutInt*HB$dic_Int/HB$epiVol # mol C day-1


all$DOC_Out[1]=mean(CR$docOut,na.rm = T) # mol C day-1
all$DOC_Out[2]=mean(EL$docOut,na.rm = T) # mol C day-1
all$DOC_Out[3]=mean(MO$docOut,na.rm = T) # mol C day-1
all$DOC_Out[4]=mean(WL$docOut,na.rm = T)
all$DOC_Out[5]=mean(HB$docOut,na.rm = T) # mol C day-1

all$POC_Out[1]=mean(CR$pocOut,na.rm = T) # mol C day-1
all$POC_Out[2]=mean(EL$pocOut,na.rm = T) # mol C day-1
all$POC_Out[3]=mean(MO$pocOut,na.rm = T) # mol C day-1
all$POC_Out[4]=mean(WL$pocOut,na.rm = T) # mol C day-1
all$POC_Out[5]=mean(HB$pocOut,na.rm = T) # mol C day-1

all$DIC_Out[1]=mean(CR$dicOut,na.rm = T) # mol C day-1
all$DIC_Out[2]=mean(EL$dicOut,na.rm = T) # mol C day-1
all$DIC_Out[3]=mean(MO$dicOut,na.rm = T) # mol C day-1
all$DIC_Out[4]=mean(WL$dicOut,na.rm = T) # mol C day-1
all$DIC_Out[5]=mean(HB$dicOut,na.rm = T) # mol C day-1

# gas flux
all$k[1]=mean(CR$kCO2,na.rm = T) # m day-1
all$k[2]=mean(EL$kCO2,na.rm = T)
all$k[3]=mean(MO$kCO2,na.rm = T)
all$k[4]=mean(WL$kCO2,na.rm = T)
all$k[5]=mean(HB$kCO2,na.rm = T)
# setting HB k lower as it is sheltered has lower wind speeds than WL
all$k[5]=0.55

all$CO2[1]=mean(CR$co2,na.rm = T) # mol C in epi
all$CO2[2]=mean(EL$co2,na.rm = T) # mol C in epi
all$CO2[3]=mean(MO$co2,na.rm = T) # mol C in epi
all$CO2[4]=mean(WL$co2,na.rm = T) # mol C in epi
all$CO2[5]=mean(HB$co2,na.rm = T) # mol C in epi

all$zMix[1]=mean(CR$thermo.depth,na.rm = T) # m
all$zMix[2]=mean(EL$thermo.depth,na.rm = T) # m
all$zMix[3]=mean(MO$thermo.depth,na.rm = T) # m
all$zMix[4]=mean(WL$thermo.depth,na.rm = T) # m
all$zMix[5]=mean(HB$thermo.depth,na.rm = T) # m

all$CO2Flux[1]=-1*mean(CR$kCO2/CR$thermo.depth*(CR$DICeq*CR$epiVol-CR$co2),na.rm = T) # mol C day-1 ; positive is out of lake
all$CO2Flux[2]=-1*mean(EL$kCO2/EL$thermo.depth*(EL$DICeq*EL$epiVol-EL$co2),na.rm = T)
all$CO2Flux[3]=-1*mean(MO$kCO2/MO$thermo.depth*(MO$DICeq*MO$epiVol-MO$co2),na.rm = T)
all$CO2Flux[4]=-1*mean(WL$kCO2/WL$thermo.depth*(WL$DICeq*WL$epiVol-WL$co2),na.rm = T)
all$CO2Flux[5]=-1*mean(0.55/HB$thermo.depth*(HB$DICeq*HB$epiVol-HB$co2),na.rm = T)

# co2 and dic in
all$CO2_In[1]=mean(CR$co2In,na.rm = T) # mol C day-1
all$CO2_In[2]=mean(EL$co2In,na.rm = T) # mol C day-1
all$CO2_In[3]=mean(MO$co2In,na.rm = T) # mol C day-1
all$CO2_In[4]=mean(WL$co2In,na.rm = T) # mol C day-1
all$CO2_In[5]=mean(HB$co2In,na.rm = T) # mol C day-1

all$DIC_In[1]=mean(CR$dicIn,na.rm = T) # mol C day-1
all$DIC_In[2]=mean(EL$dicIn,na.rm = T) # mol C day-1
all$DIC_In[3]=mean(MO$dicIn,na.rm = T) # mol C day-1
all$DIC_In[4]=mean(WL$dicIn,na.rm = T) # mol C day-1
all$DIC_In[5]=mean(HB$dicIn,na.rm = T) # mol C day-1

# hypolimnion increase in DIC
years=c(2014,2015)
for(i in 1:nrow(all)){
  cur=eval(parse(text=(paste(all$lakeID[i]))))
  cur$hypoDICinc=NA
  cur$hypoDOCinc=NA
  cur$hypoPOCinc=NA
  cur$DICinc=NA # epi
  cur$DOCinc=NA
  cur$POCinc=NA
  for(j in 1:length(years)){
    if(years[j]==2014){
      cur1=cur[as.Date(cur$datetime)<as.Date('2014-12-31'),]
    }else{
      cur1=cur[as.Date(cur$datetime)>as.Date('2014-12-31'),]
    }
    windows()
    plot(cur1$hypo_dic~DOY(cur1$datetime),pch=16,main=paste(all$lakeID[i],years[j]))
    abline(lm(cur1$hypo_dic~DOY(cur1$datetime)))
    dicInc=summary(lm(cur1$hypo_dic~DOY(cur1$datetime)))$coefficients[2] # hypo dic increase mol C m-3 day-1;
    cur1$hypoVol=cur1$V0-cur1$epiVol # m3
    cur1$hypoDICinc=cur1$hypoVol*dicInc # mol C day-1 increase in hypo
    cur$hypoDICinc[cur$datetime%in%cur1$datetime]=cur1$hypoDICinc

    docInc=summary(lm(cur1$hypo_doc~DOY(cur1$datetime)))$coefficients[2] # hypo doc increase mol C m-3 day-1;
    cur1$hypoDOCinc=cur1$hypoVol*docInc # mol C day-1 increase in hypo
    cur$hypoDOCinc[cur$datetime%in%cur1$datetime]=cur1$hypoDOCinc

    pocInc=summary(lm(cur1$hypo_poc~DOY(cur1$datetime)))$coefficients[2] # hypo poc increase mol C m-3 day-1;
    cur1$hypoPOCinc=cur1$hypoVol*pocInc # mol C day-1 increase in hypo
    cur$hypoPOCinc[cur$datetime%in%cur1$datetime]=cur1$hypoPOCinc

    dicInc=summary(lm(cur1$dic/cur1$epiVol~DOY(cur1$datetime)))$coefficients[2] # hypo dic increase mol C m-3 day-1;
    cur1$DICinc=cur1$epiVol*dicInc # mol C day-1 increase in epi
    cur$DICinc[cur$datetime%in%cur1$datetime]=cur1$DICinc

    docInc=summary(lm(cur1$doc/cur1$epiVol~DOY(cur1$datetime)))$coefficients[2] # hypo doc increase mol C m-3 day-1;
    cur1$DOCinc=cur1$epiVol*docInc # mol C day-1 increase in epi
    cur$DOCinc[cur$datetime%in%cur1$datetime]=cur1$DOCinc

    pocInc=summary(lm(cur1$poc/cur1$epiVol~DOY(cur1$datetime)))$coefficients[2] # hypo poc increase mol C m-3 day-1;
    cur1$POCinc=cur1$epiVol*pocInc # mol C day-1 increase in epi
    cur$POCinc[cur$datetime%in%cur1$datetime]=cur1$POCinc
  }
  assign(as.character(all$lakeID[i]),value = cur)
}

graphics.off()

# cIn=all$DOC_In+all$POC_In
# cOut=all$DOC_Out+all$POC_Out
#
# cIn-cOut
#
# 1-cOut/cIn

# DIC increase in hypo
all$hypoDICinc[1]=mean(CR$hypoDICinc,na.rm = T) # mol C day-1
all$hypoDICinc[2]=mean(EL$hypoDICinc,na.rm = T) # mol C day-1
all$hypoDICinc[3]=mean(MO$hypoDICinc,na.rm = T) # mol C day-1
all$hypoDICinc[4]=mean(WL$hypoDICinc,na.rm = T) # mol C day-1
all$hypoDICinc[5]=mean(HB$hypoDICinc,na.rm = T) # mol C day-1

all$hypoDOCinc[1]=mean(CR$hypoDOCinc,na.rm = T) # mol C day-1
all$hypoDOCinc[2]=mean(EL$hypoDOCinc,na.rm = T) # mol C day-1
all$hypoDOCinc[3]=mean(MO$hypoDOCinc,na.rm = T) # mol C day-1
all$hypoDOCinc[4]=mean(WL$hypoDOCinc,na.rm = T) # mol C day-1
all$hypoDOCinc[5]=mean(HB$hypoDOCinc,na.rm = T) # mol C day-1

all$hypoPOCinc[1]=mean(CR$hypoPOCinc,na.rm = T) # mol C day-1
all$hypoPOCinc[2]=mean(EL$hypoPOCinc,na.rm = T) # mol C day-1
all$hypoPOCinc[3]=mean(MO$hypoPOCinc,na.rm = T) # mol C day-1
all$hypoPOCinc[4]=mean(WL$hypoPOCinc,na.rm = T) # mol C day-1
all$hypoPOCinc[5]=mean(HB$hypoPOCinc,na.rm = T) # mol C day-1

all$DICinc[1]=mean(CR$DICinc,na.rm = T) # mol C day-1
all$DICinc[2]=mean(EL$DICinc,na.rm = T) # mol C day-1
all$DICinc[3]=mean(MO$DICinc,na.rm = T) # mol C day-1
all$DICinc[4]=mean(WL$DICinc,na.rm = T) # mol C day-1
all$DICinc[5]=mean(HB$DICinc,na.rm = T) # mol C day-1

all$DOCinc[1]=mean(CR$DOCinc,na.rm = T) # mol C day-1
all$DOCinc[2]=mean(EL$DOCinc,na.rm = T) # mol C day-1
all$DOCinc[3]=mean(MO$DOCinc,na.rm = T) # mol C day-1
all$DOCinc[4]=mean(WL$DOCinc,na.rm = T) # mol C day-1
all$DOCinc[5]=mean(HB$DOCinc,na.rm = T) # mol C day-1

all$POCinc[1]=mean(CR$POCinc,na.rm = T) # mol C day-1
all$POCinc[2]=mean(EL$POCinc,na.rm = T) # mol C day-1
all$POCinc[3]=mean(MO$POCinc,na.rm = T) # mol C day-1
all$POCinc[4]=mean(WL$POCinc,na.rm = T) # mol C day-1
all$POCinc[5]=mean(HB$POCinc,na.rm = T) # mol C day-1

# HRT
all$HRT[1]=all$Vol[1]/mean(CR$waterLoss,na.rm = T)
all$HRT[2]=all$Vol[2]/mean(EL$waterLoad,na.rm = T)
all$HRT[3]=all$Vol[3]/mean(MO$waterLoad,na.rm = T)
all$HRT[4]=all$Vol[4]/mean(WL$waterLoad,na.rm = T)
all$HRT[5]=all$Vol[5]/mean(HB$waterLoad,na.rm = T)

# Frac export as evap
all$FracEvap[1]=mean(CR$evap,na.rm = T)/mean(CR$waterLoss,na.rm = T)
all$FracEvap[2]=mean(EL$evap,na.rm = T)/mean(EL$waterLoss,na.rm = T)
all$FracEvap[3]=mean(MO$evap,na.rm = T)/mean(MO$waterLoss,na.rm = T)
all$FracEvap[4]=mean(WL$evap,na.rm = T)/mean(WL$waterLoss,na.rm = T)
all$FracEvap[5]=mean(HB$evap,na.rm = T)/mean(HB$waterLoss,na.rm = T)

# wetland loading from Hanson et al. 2014 applied to all lakes
all$wetlandLoad[1]=all$Perim[1]*2/12*.1 # g C m-1 shoreline day-1 to mol C day-1 - estimating fraction wetland
all$wetlandLoad[2]=all$Perim[2]*2/12*.2 # g C m-1 shoreline day-1 to mol C day-1 - estimating fraction wetland
all$wetlandLoad[3]=all$Perim[3]*2/12*.4 # g C m-1 shoreline day-1 to mol C day-1 - estimating fraction wetland
all$wetlandLoad[4]=all$Perim[4]*2/12*.2 # g C m-1 shoreline day-1 to mol C day-1 - estimating fraction wetland
all$wetlandLoad[5]=all$Perim[5]*3/12*1 # g C m-1 shoreline day-1 to mol C day-1 - estimating fraction wetland
# wetland DIC loading
all$wetlandDICLoad[1]=all$Perim[1]*.5/12*.1 # g C m-1 shoreline day-1 to mol C day-1 - estimating fraction wetland
all$wetlandDICLoad[2]=all$Perim[2]*.5/12*.2 # g C m-1 shoreline day-1 to mol C day-1 - estimating fraction wetland
all$wetlandDICLoad[3]=all$Perim[3]*.5/12*.4 # g C m-1 shoreline day-1 to mol C day-1 - estimating fraction wetland
all$wetlandDICLoad[4]=all$Perim[4]*.5/12*.2 # g C m-1 shoreline day-1 to mol C day-1 - estimating fraction wetland
all$wetlandDICLoad[5]=all$Perim[5]*.75/12*1 # g C m-1 shoreline day-1 to mol C day-1 - estimating fraction wetland


# Fraction of DOC retained
all$FracRet[1]=1-mean(CR$docOut)/(mean(CR$docIn)+all$wetlandLoad[1])
all$FracRet[2]=1-mean(EL$docOut)/(mean(EL$docIn)+all$wetlandLoad[2])
all$FracRet[3]=1-mean(MO$docOut)/(mean(MO$docIn)+all$wetlandLoad[3])
all$FracRet[4]=1-mean(WL$docOut)/(mean(WL$docIn)+all$wetlandLoad[4])
all$FracRet[5]=1-mean(HB$docOut)/(mean(HB$docIn)+all$wetlandLoad[5])

# POC load from leaves; estimated to be 300 g m-1 shoreline yr-1
all$leafLoad=300/12*all$Perim/365 # mol C day-1
all=all[sort.list(all$HRT),]

#  carbon fluxes ; g C m-2 yr-1
cIn=(all$DOC_In+all$POC_In+all$wetlandLoad+all$wetlandDICLoad+all$leafLoad+all$DIC_In)/all$Area*12*365 # input
cOut=(all$DOC_Out+all$POC_Out+all$DIC_Out)/all$Area*12*365 # export
cSed=(all$C_sed)/all$Area*12*365 # sed rate
cMin=-1*(all$NEP-all$hypoDICinc)/all$Area*12*365 # mineralization
cFlux=(all$CO2Flux+all$hypoDICinc)/all$Area*12*365 # co2 flux - includes flux to atmosphere and hypo dic increase

cIn-cOut-cSed-cMin
cIn-cOut-cSed-cFlux

# adding wetland load to balance OC budget
extraWetland=all$DOC_In+all$wetlandLoad+all$DIC_In-all$DOC_Out-all$DIC_Out-all$CO2Flux-all$hypoDICinc
extraWetland=ifelse(extraWetland>0,0,extraWetland)
all$wetlandLoad=all$wetlandLoad+extraWetland*-1

#  carbon fluxes ; g C m-2 yr-1
cIn=(all$DOC_In+all$POC_In+all$wetlandLoad+all$leafLoad+all$DIC_In)/all$Area*12*365 # input
cOut=(all$DOC_Out+all$POC_Out+all$DIC_Out)/all$Area*12*365 # export
cSed=(all$C_sed)/all$Area*12*365 # sed rate
cMin=-1*(all$NEP-all$hypoDICinc)/all$Area*12*365 # mineralization
cFlux=(all$CO2Flux+all$hypoDICinc)/all$Area*12*365 # co2 flux - includes flux to atmosphere and hypo dic increase

cIn-cOut-cSed-cMin
cIn-cOut-cSed-cFlux

cSed+cMin
cSed+cFlux

cRet=1-cOut/(cOut+cSed+cFlux)
cRet=1-cOut/cIn

# in lake carbon fluxes
inlakePOCflux=abs(all$POC_Out)+abs(all$C_sed)+abs(all$POCinc)+abs(all$hypoPOCinc)
inlakeDOCflux=abs(all$DOC_Out)+abs(all$DOCinc)+abs(all$hypoDOCinc)
inlakeDICflux=abs(all$DIC_Out)+abs(all$CO2Flux)+abs(all$DICinc)+abs(all$hypoDICinc)

fracPOC=inlakePOCflux/(inlakeDICflux+inlakeDOCflux+inlakePOCflux)
fracDOC=inlakeDOCflux/(inlakeDICflux+inlakeDOCflux+inlakePOCflux)
fracDIC=inlakeDICflux/(inlakeDICflux+inlakeDOCflux+inlakePOCflux)

windows()
png('Figures/fig7_fracC_HRT.png',width = 7,height = 21,units = 'in',res=300)
par(mar=c(6,6,5,2),mfrow=c(3,1))
cex.axis=2.5
cex.lab=2.5
cex=4
ylim=range(fracPOC*100,fracDOC*100,fracDIC*100)
plot(fracPOC*100~all$HRT,pch=16,cex=cex,cex.axis=cex.axis,cex.lab=cex.lab,ylim=ylim,xlab='',ylab='Percent POC of In-Lake Flux')
plot(fracDOC*100~all$HRT,pch=16,cex=cex,cex.axis=cex.axis,cex.lab=cex.lab,ylim=ylim,xlab='',ylab='Percent DOC of In-Lake Flux')
plot(fracDIC*100~all$HRT,pch=16,cex=cex,cex.axis=cex.axis,cex.lab=cex.lab,ylim=ylim,xlab='HRT (Days)',ylab='Percent DIC of In-Lake Flux')
dev.off()

windows()
plot(cRet~all$HRT,pch=16)
# which ones are seepage lakes
all$seepVdrain=c('D','D','S','S','S')
litFracRet<-read.csv('Data/DrainageVseepage_Cretention.csv',
                     stringsAsFactor=F)
png('Figures/Fig1_FracRet_HRT.png',width = 7,
    height=7,res = 300,units = 'in')
par(mar=c(6,6,5,2))
cex.axis=2.5
cex.lab=2.5
cex=2
xlim=c(0,5000)
ylim=c(0,1)

plot(cRet[all$seepVdrain=='D']~all$HRT[all$seepVdrain=='D'],pch=17,cex=cex,cex.axis=cex.axis,
     cex.lab=cex.lab,xlim=xlim,ylim=ylim,xlab='HRT (Days)',
     ylab='Fraction of Carbon Processed')
points(cRet[all$seepVdrain=='S']~all$HRT[all$seepVdrain=='S'],pch=16,cex=cex)
points(litFracRet$HRT_yrs[litFracRet$LakeType=='Drainage']*365,
       litFracRet$C_ret_proportion[litFracRet$LakeType=='Drainage'],pch=17,col='grey',cex=cex)
points(litFracRet$HRT_yrs[litFracRet$LakeType=='Seepage']*365,
       litFracRet$C_ret_proportion[litFracRet$LakeType=='Seepage'],pch=16,col='grey',cex=cex)
points(cRet[all$seepVdrain=='D']~all$HRT[all$seepVdrain=='D'],pch=17,cex=cex)
points(cRet[all$seepVdrain=='S']~all$HRT[all$seepVdrain=='S'],pch=16,cex=cex)
legend('bottomright',legend = c('This Study Drainage','This Study Seepage','Literature Drainage','Literature Seepage'),
       col=c('black','black','grey','grey'),bty = 'n',pch =c(17,16,17,16),cex = 1.5,pt.cex = 2)
dev.off()


# Carbon loads
(all$DOC_In+all$wetlandLoad)/all$Vol

all$TP=c(25.7,17.5,18.1,16.3,8.6)
all$DOC=c(16.8,10.1,23.1,6.7,5.4)

plot(all$GPP/all$Area*12~all$TP)
summary(lm(all$GPP/all$Area*12~all$TP))

plot(all$R/all$Area*12~all$DOC)

plot(all$R/all$Area*12~all$HRT)
plot(all$GPP/all$Area*12~all$HRT)
plot(all$NEP/all$Area*12~all$DOC)


# metabolism
windows()
png('Figures/Fig2_metab.png',width = 7,height=7,units='in',res=300)
all=all[sort.list(all$HRT),]
ylim=c(-(max(all$R/all$Area*12*1000)),max(all$GPP/all$Area*12*1000))
par(mar=c(5,6,4,2))
cex=3
cex.axis=2.5
cex.lab=2.5
plot(all$GPP/all$Area*12*1000,pch=21,cex=cex,ylim=ylim,xaxt='n',ylab=expression(Metabolism~(mg~C~m^-2~day^-1)),
     bg='grey30',xlab='',cex.axis=cex.axis,cex.lab=cex.lab,lwd=2) # mg C m-2 day-1
axis(1,at = c(1,2,3,4,5),labels = all$lakeID,cex.axis=cex.axis)
points(-all$R/all$Area*12*1000,pch=21,cex=cex,col='black',bg='grey30',lwd=2)
points(all$NEP/all$Area*12*1000,pch=21,cex=cex,bg='grey90',lwd=2)
abline(0,0,lty=2,lwd=3)
legend('bottomright',legend = c('GPP or R','NEP'),pt.bg = c('grey30','grey90'),
       bty='n',pch=21,cex=1.5,pt.cex=2,pt.lwd = 2)
dev.off()

# time series
ylim=c(0,1.8)
plot(CR$GPP/CR$epiVol*12~as.Date(CR$datetime),type='l',lwd=2,ylim=ylim)
lines(MO$GPP/MO$epiVol*12~as.Date(MO$datetime),lwd=2,col='blue')
lines(EL$GPP/EL$epiVol*12~as.Date(EL$datetime),lwd=2,col='red')
lines(HB$GPP/HB$epiVol*12~as.Date(HB$datetime),lwd=2,col='orange')
lines(WL$GPP/WL$epiVol*12~as.Date(WL$datetime),lwd=2,col='green')

ylim=c(0,3.1)
plot(CR$R/CR$epiVol*12~as.Date(CR$datetime),type='l',lwd=2,ylim=ylim)
lines(MO$R/MO$epiVol*12~as.Date(MO$datetime),lwd=2,col='blue')
lines(EL$R/EL$epiVol*12~as.Date(EL$datetime),lwd=2,col='red')
lines(HB$R/HB$epiVol*12~as.Date(HB$datetime),lwd=2,col='orange')
lines(WL$R/WL$epiVol*12~as.Date(WL$datetime),lwd=2,col='green')

ylim=c(-2,.2)
plot(CR$NEP/CR$epiVol*12~as.Date(CR$datetime),type='l',lwd=2,ylim=ylim)
lines(MO$NEP/MO$epiVol*12~as.Date(MO$datetime),lwd=2,col='blue')
lines(EL$NEP/EL$epiVol*12~as.Date(EL$datetime),lwd=2,col='red')
lines(HB$NEP/HB$epiVol*12~as.Date(HB$datetime),lwd=2,col='orange')
lines(WL$NEP/WL$epiVol*12~as.Date(WL$datetime),lwd=2,col='green')
abline(0,0,lty=2,lwd=2)


# C fluxes
ylim=c(0,15)
plot(CR$docIn/CR$epiVol*12~as.Date(CR$datetime),type='l',lwd=2,ylim=ylim)
lines(EL$docIn/EL$epiVol*12~as.Date(EL$datetime),type='l',lwd=2,col='red')
lines(MO$docIn/MO$epiVol*12~as.Date(MO$datetime),type='l',lwd=2,col='blue')
lines(HB$docIn/HB$epiVol*12~as.Date(HB$datetime),type='l',lwd=2,col='orange')
lines(WL$docIn/WL$epiVol*12~as.Date(WL$datetime),type='l',lwd=2,col='green')

png('Figures/Fig3_C_in.png',width = 7,height=7,units='in',res=300)
all=all[sort.list(all$HRT),]
temp=all[,c('DOC_In','wetlandLoad','wetlandDICLoad','DIC_In','POC_In','leafLoad')]
temp$DOC_In=temp$DOC_In+temp$wetlandLoad
temp$POC_In=temp$POC_In+temp$leafLoad
temp$DIC_In=temp$DIC_In+temp$wetlandDICLoad
temp=temp[,c('DOC_In','POC_In','DIC_In')]
temp=t(temp[,c('DOC_In','DIC_In','POC_In')])
tempA=t(all[,c('Area')])
for(i in 1:nrow(temp)){
  temp[i,]=temp[i,]/tempA*12*1000 # mg C m-2 day-1
}
ylim=c(0,max(apply(temp,MARGIN = 2,FUN = sum)))
par(mar=c(5,6,4,2))
cex=2
cex.axis=2
cex.lab=2
barplot(temp,pch=21,cex=cex,ylim=ylim,ylab=expression(C~Load~(mg~C~m^-2~day^-1)),
     col=c('grey10','grey','grey40'),xlab='',cex.axis=cex.axis,cex.lab=cex.lab,lwd=2,
     names.arg = all$lakeID) # mg C m-2 day-1
legend('topright',legend = c('POC Load','DIC Load','DOC Load'),pt.bg = c('grey40','grey','grey10'),
       bty='n',pt.lwd = 2,pt.cex=3,pch=22,cex=2)
dev.off()


png('Figures/Fig4_sed_efflux.png',width = 7,height=7,units='in',res=300)
all=all[sort.list(all$HRT),]
temp=all[,c('CO2Flux','C_sed')]
temp$C_sed=temp$C_sed*-1
temp=t(temp)
tempA=t(all[,c('Area')])
for(i in 1:nrow(temp)){
  temp[i,]=temp[i,]/tempA*12*1000 # mg C m-2 day-1
}
ylim=c(min(temp[2,]),max(temp[1,]))
par(mar=c(5,6,4,2))
cex=2
cex.axis=2
cex.lab=2
barplot(temp[1,],pch=21,cex=cex,ylim=ylim,ylab=expression(C~Flux~(mg~C~m^-2~day^-1)),
        col=c('black'),xlab='',cex.axis=cex.axis,cex.lab=cex.lab,lwd=2,names.arg = all$lakeID) # mg C m-2 day-1
barplot(temp[2,],pch=21,cex=cex,ylim=ylim,add=T,
        col=c('grey50'),xlab='',cex.axis=cex.axis,cex.lab=cex.lab,lwd=2,names.arg = all$lakeID) # mg C m-2 day-1
legend('topright',legend = c(expression(CO[2]~Efflux),'Carbon Burial'),pt.bg = c('black','grey50'),
       bty='n',pt.lwd = 1.5,pt.cex=3,pch=22,cex=1.5)
dev.off()


png('Figures/Fig6_NEP_CO2.png',width = 7,height = 7,units = 'in',
    res=300)
par(mar=c(5,6,4,2))
cex=3
cex.axis=2
cex.lab=2
arealCo2=all$CO2Flux/all$Area*12*1000
arealNEP=all$NEP/all$Area*12*1000
NEPic=c(-76.7,-96.8,-159.5,-115.6,-38.3)
NEPoc=c(-216.7,-301.8,-396.7,-81.5,71.9)
ylim=c(-max(abs(arealCo2),abs(arealNEP)),0)
xlim=c(0,max(abs(arealCo2),abs(arealNEP)))
plot(arealNEP~arealCo2,pch=16,cex=cex,cex.axis=cex.axis,cex.lab=cex.lab,ylim=ylim,xlim=xlim,
     ylab=expression(NEP[O2]~(mg~C~m^-2~day^-1)),xlab=expression(CO[2]~Efflux~(mg~C~m^-2~day^-1)))
# points(NEPic~arealCo2,cex=cex,pch=17)
# points(NEPoc~arealCo2,cex=cex,pch=15)
abline(0,-1,lwd=2,lty=2)
dev.off()

all$DIC_In/(-1*all$NEP)

all$DIC_In/(-1*all$NEP+all$DIC_In)

(all$DOCinc+all$hypoDOCinc)/all$Area*12*135 # increase in water column DOC mg C m-2 year-1 ; 135 is number of monitoring days per year
(all$DICinc+all$hypoDICinc)/all$Area*12*135 # increase in water column DOC mg C m-2 year-1 ; 135 is number of monitoring days per year
(all$POCinc+all$hypoPOCinc)/all$Area*12*135 # increase in water column DOC mg C m-2 year-1 ; 135 is number of monitoring days per year


# overland DOC
mean(WL$overlandDOCdisch/WL$A0[1],na.rm = T)# mg C m-2 day-1
mean(HB$overlandDOCdisch/HB$A0[1],na.rm = T)# mg C m-2 day-1

# overland DIC
mean(WL$overland*mean(EL$streamDICdisch/EL$streamWaterdisch,na.rm = T)/WL$A0[1]*1000*12,na.rm = T)# mg C m-2 day-1
mean(HB$overland*mean(EL$streamDICdisch/EL$streamWaterdisch,na.rm = T)/HB$A0[1]*1000*12,na.rm = T)# mg C m-2 day-1


# wetland DOC
all$wetlandLoad/all$Area*12*1000

all$wetlandDICLoad/all$Area*12*1000

# DOC stream export
mean(WL$outletDischarge*WL$doc_Int/WL$epiVol*12*365/WL$A0[1],na.rm = T)
mean(HB$outletDischarge*HB$doc_Int/HB$epiVol*12*365/HB$A0[1],na.rm = T)

# DIC stream export
mean(WL$outletDischarge*WL$dic_Int/WL$epiVol*12*365/WL$A0[1],na.rm = T)
mean(HB$outletDischarge*HB$dic_Int/HB$epiVol*12*365/HB$A0[1],na.rm = T)

# POC stream export
mean(WL$outletDischarge*WL$poc_Int/WL$epiVol*12*365/WL$A0[1],na.rm = T)
mean(HB$outletDischarge*HB$poc_Int/HB$epiVol*12*365/HB$A0[1],na.rm = T)

# different NEP estimates; see table 2
# MO EL HB WL CR
temp=data.frame(NEPic=rep(NA,5))
temp$NEPic=c(-154.2,-122.8,-497.1,-152.1,-45.5)
temp$NEPoc=c(-216.7,-301.8,-519.5,-116.5,71.9)
temp$NEPo2=c(-163.3,-193.6,-137.9,-156.1,30.2)
temp2=data.frame(sdic=rep(NA,5))
temp2$sdic=c(161,43,128,36,15)
temp2$sdoc=c(318,86,146,40,41)
temp2$sdo2=c(147,207,134,108,109)

windows()
png('Figures/Fig5_nep.png',width = 7,height=7,units='in',res=300)
par(mar=c(5,6,4,2))
ylim=range(t(as.matrix(temp))-t(as.matrix(temp2)),t(as.matrix(temp))+t(as.matrix(temp2)))
cex=2
cex.axis=2
cex.lab=2
bp=barplot(t(as.matrix(temp[,1:3])),beside = T,cex=cex,ylab=expression(NEP~(mg~C~m^-2~day^-1)),ylim=ylim,
        col=c('grey','grey90','grey50'),xlab='',cex.axis=cex.axis,cex.lab=cex.lab,lwd=2,names.arg = all$lakeID) # mg C m-2 day-1
arrows(bp,t(as.matrix(temp))-t(as.matrix(temp2)),bp,t(as.matrix(temp))+t(as.matrix(temp2)),lwd=2,angle=90,code=3,length = 0.05)
legend('bottomright',legend = c(expression(NEP[IC]),expression(NEP[OC]),expression(NEP[O2])),
       pt.bg = c('grey','grey90','grey50'),bty='n',pt.lwd = 1.5,pt.cex=3,pch=22,cex=1.5)
dev.off()





