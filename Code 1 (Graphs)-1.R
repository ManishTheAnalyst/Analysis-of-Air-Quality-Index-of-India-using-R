library(ggplot2)
library(dplyr)
library(tidyr)
library(highcharter)
library(lubridate)
library(xts)
library(reshape2)
summary(CleanedAQI_removedNA)
table(CleanedAQI_removedNA$type)

by_state_wise <-CleanedAQI_removedNA%>%group_by(state)%>%summarise(Avg_So2=mean(so2,na.rm=TRUE),
Avg_No2=mean(no2,na.rm=TRUE),
Avg_Rspm=mean(rspm,na.rm=TRUE),
Avg_Spm= mean(spm,na.rm=TRUE))

by_state_wise
ggplot(by_state_wise,aes(x=state,y=Avg_So2,fill=Avg_So2))+
geom_bar(stat="identity") +
scale_fill_gradient(low="lightblue",high="darkblue")+
theme(axis.text.x =element_text(angle=90)) +
ggtitle("Sulphor DiOxide Content-State Wise") +
xlab(label="State") +
ylab(label=" SO2 Content")

ggplot(by_state_wise,aes(x=state,y=Avg_No2,fill=Avg_No2)) +
geom_bar(stat="identity") +
scale_fill_gradient(low="lightblue",high="darkblue")+
theme(axis.text.x =element_text(angle=90)) +
ggtitle("Nitrous DiOxide Content-State Wise") +
ylab(label=" NO2 Content")

ggplot(by_state_wise,aes(x=state,y=Avg_Rspm,fill=Avg_Rspm)) +
geom_bar(stat="identity") +
scale_fill_gradient(low="lightblue",high="darkblue")+
theme(axis.text.x =element_text(angle=90)) +
ggtitle("Respirable Suspended Particulate Matter(RSPM) Content-State Wise") +
xlab(label="State") +
ylab(label="RSPM Content")

ggplot(by_state_wise,aes(x=state,y=Avg_Spm,fill=Avg_Spm)) +
geom_bar(stat="identity") +
scale_fill_gradient(low="lightblue",high="darkblue")+
theme(axis.text.x =element_text(angle=90)) +
ggtitle("Suspended Particulate Matter(SPM) Content-State Wise") +
xlab(label="State") +
ylab(label=" SPM Content")

Delhi <-CleanedAQI_removedNA%>%filter(state=="Delhi")%>%group_by(date,type)%>%summarise(Avg_So2=mean(so2,na.rm=TRUE),
Avg_No2=mean(no2,na.rm=TRUE),
Avg_Rspm=mean(rspm,na.rm=TRUE),
Avg_Spm =mean(spm,na.rm=TRUE))
                                                                                        
                                                                
ggplot(Delhi,aes(x=date,y=Avg_No2)) +
geom_line(size=1,color="darkred") +
geom_point()+
facet_wrap(~type) +
ggtitle("Delhi NO2 Content-Year Wise")+
xlab("Year") +
ylab(" NO2 Content")

ggplot(Delhi,aes(x=date,y=Avg_Rspm)) +
geom_line(size=1,color="darkred") +
geom_point()+
facet_wrap(~type) +
ggtitle("Delhi RSPM Content-Year Wise")+
xlab("Year") +
ylab("RSPM Content")

ggplot(Delhi,aes(x=date,y=Avg_Spm)) +
geom_line(size=1,color="darkred") +
geom_point()+
facet_wrap(~type) +
ggtitle("Delhi SPM Content-Year Wise")+
xlab("Year") +
ylab("SPM Content")

Jharkhand <-CleanedAQI_removedNA%>%filter(state=="Jharkhand")%>%group_by(date,type)%>%summarise(Avg_So2=mean(so2,na.rm=TRUE),
Avg_No2=mean(no2,na.rm=TRUE),
Avg_Rspm=mean(rspm,na.rm=TRUE),
Avg_Spm =mean(spm,na.rm=TRUE))

ggplot(Jharkhand,aes(x=date,y=Avg_So2)) +
geom_line(size=1,color="darkred") +
geom_point()+
facet_wrap(~type) +
ggtitle("Jharkhand SO2 Content-Year Wise")+
xlab("Year") +
ylab(" SO2 Content")


ggplot(Jharkhand,aes(x=date,y=Avg_No2)) +
geom_line(size=1,color="darkred") +
geom_point()+
facet_wrap(~type) +
ggtitle("Jharkhand NO2 Content-Year Wise")+
xlab("Year") +
ylab(" NO2 Content")


UttarPradesh <-CleanedAQI_removedNA%>%filter(state=="Uttar Pradesh")%>%group_by(date,type)%>%summarise(Avg_So2=mean(so2,na.rm=TRUE),
Avg_No2=mean(no2,na.rm=TRUE),
Avg_Rspm=mean(rspm,na.rm=TRUE),
Avg_Spm =mean(spm,na.rm=TRUE))

ggplot(UttarPradesh,aes(x=date,y=Avg_Rspm)) +
geom_line(size=1,color="darkred") +
geom_point()+
facet_wrap(~type) +
ggtitle("Uttar Pradesh RSPM Content-Year Wise")+
xlab("Year") +
ylab("RSPM Content")

ggplot(UttarPradesh,aes(x=date,y=Avg_Spm)) +
geom_line(size=1,color="darkred") +
geom_point()+
facet_wrap(~type) +
ggtitle("Uttar Pradesh SPM Content-Year Wise")+
xlab("Year") +
ylab("SPM Content")


WestBengal <-CleanedAQI_removedNA%>%filter(state=="West Bengal")%>%group_by(date,type)%>%summarise(Avg_So2=mean(so2,na.rm=TRUE),
Avg_No2=mean(no2,na.rm=TRUE),
Avg_Rspm=mean(rspm,na.rm=TRUE),
vg_Spm =mean(spm,na.rm=TRUE))

ggplot(WestBengal,aes(x=date,y=Avg_No2)) +
geom_line(size=1,color="darkred") +
geom_point()+
facet_wrap(~type) +
ggtitle("West Bengal NO2 Content-Year Wise")+
xlab("Year") +
ylab(" NO2 Content")

Bihar <-CleanedAQI_removedNA%>%filter(state=="Bihar")%>%group_by(date,type)%>%summarise(Avg_So2=mean(so2,na.rm=TRUE),
Avg_No2=mean(no2,na.rm=TRUE),
Avg_Rspm=mean(rspm,na.rm=TRUE),
Avg_Spm =mean(spm,na.rm=TRUE))

ggplot(Bihar,aes(x=date,y=Avg_Spm)) +
geom_line(size=1,color="darkred") +
geom_point()+
facet_wrap(~type) +
ggtitle("Bihar SPM Content-Year Wise")+
xlab("Year") +
ylab("SPM Content")
