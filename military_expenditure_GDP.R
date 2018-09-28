##Final Project

##Set Working Directory
setwd("/Users/madie/Downloads/Stats Class/Final_Project")

##Load data
Data<-read.csv("Military_Expenditure_Dataset.csv")

View(Data)

##Finding Difference in Means
mean(Data$Share.of.GDP, na.rm=T)

mean_Sunni<-mean(Data$Share.of.GDP[Data$Sunni==1], na.rm=T)

mean(Data$Share.of.GDP[Data$Shi.ite==1], na.rm=T)

mean(Data$Share.of.GDP[Data$Jewish==1], na.rm=T)

mean(Data$Share.of.GDP[Data$Sunni==1], na.rm=T) - mean(Data$Share.of.GDP[Moneydata$Shi.ite==1], na.rm=T)

mean(Data$Share.of.GDP[Data$Sunni==1], na.rm=T) - mean(Data$Share.of.GDP[Moneydata$Jewish==1], na.rm=T)

mean(Data$Share.of.GDP[Data$Shi.ite==1], na.rm=T) - mean(Data$Share.of.GDP[Moneydata$Jewish==1], na.rm=T)

##Graphing median Countries for each religion
median(Data$Share.of.GDP[Data$Sunni==1], na.rm=T)

median(Data$Share.of.GDP[Data$Sunni], na.rm=T)

median(Data$Share.of.GDP[Data$Jewish], na.rm=T)

plot(Egypt_Share.of.GDP_ts,type="o", col="blue", ylim=c(0,.4), xlab="", ylab="", main="") 

lines(Bahrain_Share.of.GDP_ts, type="o", col="orange", ylim=c(0,.4), xlab="", ylab="", main="")

lines(Israel_Share.of.GDP_ts, type="o", col="dark green", ylim=c(0,.4), xlab="", ylab="", main="")

legend(1990, .4, legend=c("Jewish", "Shiite", "Sunni"),
       col=c("dark green", "orange", 300),lty=1:2, cex=0.8)
       
       
##Making Seperate data sets for Sunni Countries
Egypt_data<-Data[Data$Country=="Egypt",]
Iraq_data<-Data[Data$Country=="Iraq",]
Jordan_data<-Data[Data$Country=="Jordan",]
Kuwait_data<-Data[Data$Country=="Kuwait",]
Lebanon_data<-Data[Data$Country=="Lebanon",]
Oman_data<-Data[Data$Country=="Oman",]
Qatar_data<-Data[Data$Country=="Qatar",]
SaudiArabia_data<-Data[Data$Country=="Saudi Arabia",]
Syria_data<-Data[Data$Country=="Syria",]
Turkey_data<-Data[Data$Country=="Turkey",]
UAE_data<-Data[Data$Country=="UAE",]
Yemen_data<-Data[Data$Country=="Yemen",]


#Making ts functions for Sunni Countries
Egypt_Share.of.GDP_ts<-ts(Egypt_data$Share.of.GDP, start=c(1988, 1), frequency=1)
Iraq_Share.of.GDP_ts<-ts(Iraq_data$Share.of.GDP, start=c(1988, 1), frequency=1)
Jordan_Share.of.GDP_ts<-ts(Jordan_data$Share.of.GDP, start=c(1988, 1), frequency=1)
Kuwait_Share.of.GDP_ts<-ts(Kuwait_data$Share.of.GDP, start=c(1988,1), frequency=1)
Lebanon_Share.of.GDP_ts<-ts(Lebanon_data$Share.of.GDP, start=c(1988, 1), frequency=1)
Oman_Share.of.GDP_ts<-ts(Oman_data$Share.of.GDP, start=c(1988, 1), frequency=1)
Qatar_Share.of.GDP_ts<-ts(Qatar_data$Share.of.GDP, start=c(1988, 1), frequency=1)
SaudiArabia_Share.of.GDP_ts<-ts(SaudiArabia_data$Share.of.GDP, start=c(1988, 1), frequency=1)
Syria_Share.of.GDP_ts<-ts(Syria_data$Share.of.GDP, start=c(1988, 1), frequency=1)
Turkey_Share.of.GDP_ts<-ts(Turkey_data$Share.of.GDP, start=c(1988, 1), frequency=1)
Yemen_Share.of.GDP_ts<-ts(Yemen_data$Share.of.GDP, start=c(1988, 1), frequency=1)



###Share of GDP graphed over time for Sunni's
# Egypt Iraq Jordan Kuwait Lebanon Oman Qatar Saudi Arabia Syria Turkey UAE Yemen

plot(Egypt_Share.of.GDP_ts, type="o", col="blue", ylim=c(0,.4), xlab="", ylab="", main="")

lines(Iraq_Share.of.GDP_ts, type="o", pch=22, lty=2, col="red")

lines(Jordan_Share.of.GDP_ts, type="o", pch=20, lty=2, col="purple")

lines(Kuwait_Share.of.GDP_ts, type="o", pch= 23, lty=2, col="green")

lines(Lebanon_Share.of.GDP_ts, type="o", pch= 23, lty=2, col="orange")

lines(Oman_Share.of.GDP_ts, type="o", pch= 23, lty=2, col="red4")

lines(Qatar_Share.of.GDP_ts, type="o", pch= 23, lty=2, col="dark green")

lines(SaudiArabia_Share.of.GDP_ts, type="o", pch= 23, lty=2, col="pink")

lines(Syria_Share.of.GDP_ts, type="o", pch= 23, lty=2, col="mediumpurple1")

lines(Turkey_Share.of.GDP_ts, type="o", pch= 23, lty=2, col="saddlebrown")

lines(Yemen_Share.of.GDP_ts, type="o", pch=23, lty=2, col="slateblue")

##Plotting Sunni relationship
plot(Yemen_Share.of.GDP_ts, type="o", col="slateblue", ylim=c(0,.2), xlab="", ylab="", main="")

lines(Lebanon_Share.of.GDP_ts, type="o", pch=23, lty=2, col="orange")

##Data sets for Shi'ite countries
#Bahrain Iran
Bahrain_data<-Data[Data$Country=="Bahrain",]
Iran_data<-Data[Data$Country=="Iran",]


##Making ts functions for Shi'ite countries
Bahrain_Share.of.GDP_ts<-ts(Bahrain_data$Share.of.GDP, start=c(1988, 1), frequency=1)
Iran_Share.of.GDP_ts<-ts(Iran_data$Share.of.GDP, start=c(1988, 1), frequency=1)

##Graphing Share of GDP for Shi'ite
plot(Bahrain_Share.of.GDP_ts, type="o", col="orange", ylim=c(0,.4), xlab="", ylab="", main="")

lines(Iran_Share.of.GDP_ts, type="o", col="red4")

##Making Separate Data Sets for Jewish Countries
Israel_data<-Data[Data$Country=="Israel",]

##Making ts functions for Jewish Countries
Israel_Share.of.GDP_ts<-ts(Israel_data$Share.of.GDP, start=c(1988, 1), frequency=1)


##Graphing Share of GDP for Jewish 
plot(Israel_Share.of.GDP_ts, type="o", col="dark green", ylim=c(0,.4), ylab="", main="", xlab="")

  


sum(Data$Sunni==1, na.rm=T)
