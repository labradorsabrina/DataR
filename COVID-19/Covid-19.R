library(tidyverse)
library(magrittr)
library(astsa)
library(forecast) 
options(warn = -1)
options(repr.plot.width = 8, repr.plot.height = 8)

setwd("~/Documents/Data Analytics/DataR/COVID-19")
info_cov_india<-read.csv("covid_19_india.csv",stringsAsFactors = F)

info_cov_india$Date<-as.Date.character(info_cov_india$Date,format = c("%d/%m/%y"))
info_cov_india1<-arrange(info_cov_india,Date)%>%group_by(Date)%>% summarize(cured=sum(Cured),deaths=sum(Deaths),case=sum(Confirmed))
ggplot(info_cov_india1,aes(x=Date))+geom_line(aes(y=case,color="Cases"), size=1.5) + geom_line(aes(y=deaths,color="Death"), size=1.5)+ geom_line(aes(y=cured,color="Recovered"), size=1.5)+theme_bw() +ylab("Total Count")+xlab("Period")+ labs(title="Cummulative Count of Covid19 cases, Recovered and Deaths",color = "Legend")+scale_color_manual(values = c("gold","red","springgreen3"))

info_cov_india1<-arrange(info_cov_india,Date)%>%group_by(Date)%>% summarize(cured=sum(Cured),deaths=sum(Deaths),case=sum(Confirmed))
info_cov_india1<-arrange(info_cov_india1,Date)%>%mutate(per_day_case = c(0,diff(case)))

ggplot(info_cov_india1,aes(x=Date,y=per_day_case))+geom_point(color="violetred4")+geom_smooth(method="loess", formula=y~x)+theme_bw()+xlab("Period")+ylab("Cases Per Day")+labs(title="India Daily case trend")

options(repr.plot.width = 15, repr.plot.height = 10)

info_cov_india1<-filter(info_cov_india,Date==max(Date))


ggplot(info_cov_india1, aes(x=State.UnionTerritory, y=Confirmed))+geom_col(fill="orange")+ theme(axis.text.x=element_text(size=20, angle=90, hjust = 1, vjust = 0.5 ) , plot.background = element_rect( colour = NULL,  size = 20,  linetype = NULL,  color = NULL, fill = NULL, inherit.blank = FALSE)) +xlab("States/UTs")+ylab("Total Cases")+ labs(title="State/UT-wise Cases of COVID-19")+ theme_bw() + geom_text(aes(label=round(Confirmed) ), position=position_dodge(width=1.0),vjust=-0.25,angle=270)+coord_flip()
ggplot(info_cov_india1, aes(x=State.UnionTerritory, y=Cured))+geom_col(fill="#00AFBB")+ theme(axis.text.x=element_text(size=10, angle=90, hjust = 1, vjust = 0.5 ) , plot.background = element_rect( colour = NULL,  size = 20,  linetype = NULL,  color = NULL, fill = NULL, inherit.blank = FALSE)) +xlab("States/UTs")+ylab("Recovered")+ labs(title="State/UT-wise Recovered from COVID-19")+ theme_bw()+  geom_text(aes(label=round(Cured) ), position=position_dodge(width=1.0),vjust=-0.25,angle=270)+coord_flip()
ggplot(info_cov_india1, aes(x=State.UnionTerritory, y=Deaths))+geom_col(fill="red")+ theme(axis.text.x=element_text(size=10, angle=90, hjust = 1, vjust = 0.5 ) , plot.background = element_rect( colour = NULL,  size = 20,  linetype = NULL,  color = NULL, fill = NULL, inherit.blank = FALSE)) +xlab("States/UTs")+ylab("Deaths")+ labs(title="State/UT-wise Deaths from COVID-19")+ theme_bw()+  geom_text(aes(label=round(Deaths) ), position=position_dodge(width=1.0),vjust=-0.25,angle=270)+coord_flip()

info_cov_india1<-filter(info_cov_india,Date==max(Date))%>%top_n(9,Confirmed)
info_cov_india1<-inner_join(info_cov_india,info_cov_india1,"State.UnionTerritory")
info_cov_india1<-arrange(info_cov_india1,Date.x)%>%group_by(State.UnionTerritory)%>%mutate(per_day_cases = c(0,diff(Confirmed.x)))

ggplot(info_cov_india1,aes(x=Date.x,y=Confirmed.x))+geom_point(color="violetred4")+geom_smooth(method="loess",formula=y~x)+theme_bw()+xlab("Period")+ylab("Cases Per Day")+labs(title="Daily case trend for top 9 states in India")+facet_wrap(~State.UnionTerritory)+theme(strip.text.x = element_text(size = 20, colour = "black"))


options(repr.plot.width = 8, repr.plot.height = 8)
info_beds_india<-read.csv("HospitalBedsIndia.csv",stringsAsFactors = F)

info_beds_india1<-filter(info_beds_india,State.UT=="All India")

ggplot(filter(info_beds_india,State.UT=="All India") )+geom_col(aes(x="Public",y=NumPublicBeds_HMIS,fill="Public")) + geom_col(aes(x="Rural",y=NumRuralBeds_NHP18,fill="Rural"))+ geom_col(aes(x="Urban",y=NumUrbanBeds_NHP18,fill="Urban"))+theme_bw() +ylab("Total Count")+xlab("Beds")+ labs(title="Category of beds available in India",fill = "Legen: Bed Type")+scale_fill_manual(values = c("gold","indianred3","springgreen3"))

options(repr.plot.width = 15, repr.plot.height = 10)
info_pop_india<-read.csv("population_india_census2011.csv",stringsAsFactors = F)

info_pop_india1<-inner_join(info_pop_india,info_beds_india,by=c("State...Union.Territory"="State.UT"))
info_pop_india1[,"beds_to_pop_ratio"]<-((info_pop_india1$NumPublicBeds_HMIS+info_pop_india1$NumRuralBeds_NHP18+info_pop_india1$NumUrbanBeds_NHP18)/info_pop_india1$Population)

ggplot(info_pop_india1, aes(x=State...Union.Territory, y=beds_to_pop_ratio))+geom_col()+xlab("States/UTs")+theme_bw()+ theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20, angle=90, hjust = 1, vjust = 0.5 ),axis.title=element_text(size=21),plot.title = element_text(face = "bold",size=25))+ylab("Beds/Popolation")+ labs(title="State/UT-wise Beds to population ratio")

info_cov_india1<-filter(info_cov_india,Date==max(Date))
info_cov_india1[,"Active"]<-info_cov_india1$Confirmed-(info_cov_india1$Deaths+info_cov_india1$Cured)
info_cov_india1<-filter(info_cov_india1,Active>0)
info_cov_india1<-inner_join(info_cov_india1,info_beds_india,by=c("State.UnionTerritory"="State.UT"))

info_cov_india1[,"beds_to_act_case_ratio"]<-((info_cov_india1$NumPublicBeds_HMIS+info_cov_india1$NumRuralBeds_NHP18+info_cov_india1$NumUrbanBeds_NHP18)/info_cov_india1$Active)

ggplot(info_cov_india1, aes(x=State.UnionTerritory, y=beds_to_act_case_ratio))+geom_col(fill="#00AFBB")+ theme_bw()+theme(axis.title=element_text(size=21),plot.title = element_text(face = "bold",size=25),axis.text.y=element_text(size=20),axis.text.x=element_text(size=20, angle=90, hjust = 1, vjust = 0.5 ) , plot.background = element_rect( colour = NULL,  size = 20,  linetype = NULL,  color = NULL, fill = NULL, inherit.blank = FALSE)) +xlab("States/UTs")+ylab("Beds per Active Patient")+ labs(title="State/UT-wise Beds to active patients ratio")+  geom_text(aes(label=round(beds_to_act_case_ratio) ), position=position_dodge(width=1.0), vjust=-0.25)

info_lab_india<-read.csv("ICMRTestingLabs.csv",stringsAsFactors = F)

info_lab_india1<-info_lab_india%>%group_by(state,type)%>%tally()

ggplot(info_lab_india1, aes(x=state, y=n) )+geom_col(fill="turquoise4")+theme_bw()+ theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20, angle=90, hjust = 1, vjust = 0.5 ),axis.title=element_text(size=21),plot.title = element_text(face = "bold",size=25))+xlab("States/UTs")+ylab("Count")+ labs(title="State/UT-wise Labs Distribution")
ggplot(info_lab_india1, aes(x=state, y=n) )+geom_col(fill="turquoise4")+theme_bw()+ theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20, angle=90, hjust = 1, vjust = 0.5 ),axis.title=element_text(size=21),plot.title = element_text(face = "bold",size=25),strip.text.x = element_text(size = 15, colour = "black"))+xlab("States/UTs")+ylab("Count")+ labs(title="State/UT-wise Labs Distribution")+facet_grid(~type)+
  coord_flip()

info_test_details_india<-read.csv("StatewiseTestingDetails.csv",stringsAsFactors = F)
info_test_details_india[,"NegClean"]<-ifelse(is.na(info_test_details_india$Negative),(info_test_details_india$TotalSamples-info_test_details_india$Positive),info_test_details_india$Negative)

info_test_details_india$Date<-as.Date.character(info_test_details_india$Date,format = c("%Y-%m-%d"))

info_test_details_india1<-arrange(info_test_details_india,Date)%>%group_by(Date)%>% summarize(Total=sum(TotalSamples),Negative=sum(NegClean),Positive=sum(Positive))

ggplot(info_test_details_india1,aes(x=Date))+geom_line(aes(y=Total,color="Total Samples"),size=1) + geom_line(aes(y=Positive,color="Positive"), size=1)+ geom_line(aes(y=Negative,color="Negative"), size=1)+theme_bw() +ylab("Total Count")+xlab("Period")+ labs(title="Total Testing with Positive and Negative Results",color = "Cases")+scale_color_manual(values = c("springgreen3","red","gold"))+theme(axis.title=element_text(size=20),axis.text=element_text(size=15),plot.title = element_text(face = "bold",size=25))

ggplot(info_test_details_india1,aes(x=Date))+geom_area(aes(y=Total,fill="Total Samples"),position = "stack",alpha=0.5) + geom_area(aes(y=Positive,fill="Positive"),position = "stack",alpha=0.5)+ geom_area(aes(y=Negative,fill="Negative"),position = "stack",alpha=0.5)+theme_bw() +ylab("Total Count")+xlab("Period")+ labs(title="Total Testing with Positive and Negative Results",fill = "Cases")+scale_fill_manual(values = c("springgreen3","red","gold"))

info_cov_india1<-arrange(info_cov_india,Date)%>%group_by(Date)%>% summarize(cured=sum(Cured),deaths=sum(Deaths),case=sum(Confirmed))%>%mutate(per_day_cases=c(0,diff(case)),per_day_deaths=c(0,diff(deaths)),per_day_cured=c(0,diff(cured)))

info_cov_india1$per_day_cases<-ifelse(info_cov_india1$Date==min(info_cov_india1$Date),info_cov_india1$case,info_cov_india1$per_day_cases)
info_cov_india1$per_day_deaths<-ifelse(info_cov_india1$Date==min(info_cov_india1$Date),info_cov_india1$deaths,info_cov_india1$per_day_deaths)
info_cov_india1$per_day_cured<-ifelse(info_cov_india1$Date==min(info_cov_india1$Date),info_cov_india1$cured,info_cov_india1$per_day_cured)
info_cov_india1%<>%mutate(rate_case=(per_day_cases/case),rate_death = (per_day_deaths/deaths),rate_cure = (per_day_cured/cured))

ggplot(info_cov_india1,aes(x=Date))+geom_col(aes(y=rate_case),color="black",fill="gray70")+ geom_point(aes(y=rate_case),color="red4") +theme_bw()+theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),legend.position = "none") +ylab("(Daily Case)/(Total Cases)")+xlab("Period")+ labs(title="Ratio of Daily cases to total")

info_cov_india1<-arrange(info_cov_india,Date)%>%group_by(State.UnionTerritory)%>%mutate(per_day_cases = c(0,diff(Confirmed)),per_day_deaths = c(0,diff(Deaths)),per_day_recovered = c(0,diff(Cured)))

#Heat maps to identify stressful days and in which state its increasing and which state people recovering in more number
ggplot(filter(info_cov_india1,per_day_cases>0),aes(x=Date,y=State.UnionTerritory,fill=per_day_cases))+geom_tile()+scale_fill_distiller(palette = "Spectral")+xlab("States/UTs")+ylab("Period")+ labs(title="Heatmap for per-day Covid-19 cases") + theme_bw()+theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20))

ggplot(filter(info_cov_india1,per_day_deaths>0),aes(x=Date,y=State.UnionTerritory,fill=per_day_deaths))+geom_tile()+ scale_fill_distiller(palette = "PuOr")+xlab("States/UTs")+ylab("Period")+ labs(title="Heatmap for per-day Deaths") + theme_bw()+theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20))

ggplot(filter(info_cov_india1,per_day_recovered>0),aes(x=Date,y=State.UnionTerritory,fill=per_day_recovered))+geom_tile()+scale_fill_distiller(palette = "BrBG") +xlab("States/UTs")+ylab("Period")+ labs(title="Heatmap for per-day Recovered") + theme_bw()+theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20))

#Exponential Forecasting Model Fitting

options(repr.plot.width = 8, repr.plot.height = 8)
info_cov_india1<-arrange(info_cov_india,Date)%>%group_by(Date)%>% summarize(cured=sum(Cured),deaths=sum(Deaths),case=sum(Confirmed))

ts.info_cov_india1<-ts(diff(info_cov_india1$case),     
                       start = c(1),
                       frequency = 15)

decompose.ts.info_cov_india1 <- decompose(ts.info_cov_india1)
plot(decompose.ts.info_cov_india1)

#If I break the time series in 15-day periods, then there is a definitive seasonality
#As this is very small period and I will consider the seasonality so that prediction can be abit more closer
#Below is the Series after removing seasonality

ts.info_cov_india1.seas.adj <- ts.info_cov_india1 - decompose.ts.info_cov_india1$seasonal
plot(ts.info_cov_india1.seas.adj)

fitted_model<-HoltWinters(ts.info_cov_india1)
plot(fitted_model,main="fitting a model to the daily cases")

#Lets check the residuals and their plots


forecast.India.total.cases<-forecast(fitted_model,10)
acf(na.omit(resid(forecast.India.total.cases)), lag.max=20)

Box.test(forecast.India.total.cases$residuals, lag=20, type="Ljung-Box")


#We ran Ljung-Box test to see the p-value, which is vey significant and we cannot reject the null hypothesis i.e. there is no correlation between errors
#Also ran the autocorrelation visualization which shows residuals have no significant correlation left

#Below is the forecasting graph and the prediction table with 3rd order exponential forecasting model

autoplot(forecast.India.total.cases,fcol = "red") + geom_forecast(h=10) + theme_classic()+labs(title="Covid-19 India Cases Prediction using Exponential Forecasting")+xlab("Period")+ylab("Case Count")

given.last.date<-max(info_cov_india1$Date)
given.start.date<-min(info_cov_india1$Date)


#plot(forecast.India.total.cases, xaxt='n',main="Covid-19 India Cases Prediction using Exponential Forecasting",fcol="red")
#axis(1, at=seq(1, as.numeric(given.last.date-given.start.date), by=1), las=2, labels=seq(given.start.date, given.last.date, length.out=as.numeric(given.last.date-given.start.date) ))

forecast.India.total.cases%<>%as_tibble()
forecast.India.total.cases[,"Day"]<-given.last.date+as.numeric(row.names(forecast.India.total.cases))
forecast.India.total.cases<-as.data.frame(forecast.India.total.cases[,c(6,1)])
forecast.India.total.cases

#ARIMA/SARIMA Model Fitting

#Assumtion: Considered 15 day as frequency of time-series

info_cov_india1<-arrange(info_cov_india,Date)%>%group_by(Date)%>% summarize(cured=sum(Cured),deaths=sum(Deaths),case=sum(Confirmed))

ts.info_cov_india1<-ts(diff(info_cov_india1$case))

#Running Box Test to see if the series has no correlation and series is stationary

Box.test(ts.info_cov_india1, lag = log(length(ts.info_cov_india1)))

#Very low p-value indicates that correlation exsists and series is not stationary
#Lets check the acf to confirm our observation if correation exists between observations

acf(ts.info_cov_india1)

#Clearly series is not stationary yet
#Checking the the order of AR and MA part with auto routine using no approximation

#without approximation
auto.arima(ts.info_cov_india1,ic="aic",approximation=F)

#Auto routine suggesting there is no AR part in the series. Series just contain the MA part
#Lets Validate our observation

acf(diff(diff(ts.info_cov_india1)))

#There are 2 significant spikes which tells MA part/coeffients will be 2-3
#Lets check the AR part by checking PACF

pacf(diff(diff(ts.info_cov_india1)))

#This tells we do have significant PACF for upto 8 lags.
#Lets run and fit the model now for multiple values of AR and MA parts in the series. We will be taking AIC as the measure to consider best fit model.


d=2
DD=0
#assumption there is seasonality after 15 days
per=15
for(p in 1:5){
  for(q in 1:4){
    for(i in 1:2){
      for(j in 1:1){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=ts.info_cov_india1, order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2) 
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

#Based on above output, We are considering 1,2,1,1,0,0,15 as best model


fitted_model<-arima(ts.info_cov_india1,order=c(1,2,1),seasonal = list(order=c(1,0,0), period=per))


forecast.India.total.cases<-forecast(fitted_model,10)
acf(na.omit(resid(forecast.India.total.cases)), lag.max=20)

Box.test(forecast.India.total.cases$residuals, lag=20, type="Ljung-Box")

#Below is the forecasting graph and the prediction table with 3rd order exponential forecasting model**

#autoplot(forecast.India.total.cases,fcol = "red") + geom_forecast(h=10) + theme_classic()+labs(title="Covid-19 India Cases Prediction Using Sarima Model")+xlab("Period")+ylab("Case Count")
given.last.date<-max(info_cov_india1$Date)
given.start.date<-min(info_cov_india1$Date)
forecast.India.total.cases<-forecast(fitted_model,10)
plot(forecast.India.total.cases, xaxt='n',main="Covid-19 India Cases Prediction Using Sarima Model",fcol="red")
axis(1, at=seq(1, as.numeric(given.last.date-given.start.date), by=4), las=2, labels=seq(given.start.date, given.last.date, length.out=as.numeric(given.last.date-given.start.date)/4 ))


forecast.India.total.cases%<>%as_tibble()
forecast.India.total.cases[,"Day"]<-given.last.date+as.numeric(row.names(forecast.India.total.cases))
forecast.India.total.cases<-as.data.frame(forecast.India.total.cases[,c(6,1)]) 
forecast.India.total.cases

