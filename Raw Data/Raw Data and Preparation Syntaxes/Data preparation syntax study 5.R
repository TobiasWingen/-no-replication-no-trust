#make sure the  pakages are installed
#require packages####
require(car)
require(effsize)
require(ggplot2)
require(dplyr)
require(psych)
require(rockchalk)
require(rio)
require(lsr)
#set working directory to where the data is located

dataimp<-read.csv2("rawdata_study5.csv", stringsAsFactors = FALSE,  na.strings=c("", "NA"))

#data preparation####
data1<-dataimp

#delete cases where everything is NA
data1$na<-is.na(data1$mc_experiments)
data2<-subset(data1, data1$na==FALSE)

#recode reverse trust variables
#reverse coded
data2$trust1<-8-as.numeric(data2$trust1)
data2$trust4<-8-as.numeric(data2$trust4)
data2$trust5<-8-as.numeric(data2$trust5)
str(data2)
#change all variables types to numeric
data2a <- data.frame(lapply(data2, function(x) as.numeric(x)))
#restore non numeric values
data2b <- data2a[,-c(12,14:17)]
data2c<- data2[,c(12,14:17)]
data2d<-cbind(data2b,data2c)
#get final dataset
data2<-data2d

#rename the item mc_surveys to: mc_rpp, item name was incorrect on qualtrics. 
names(data2)[names(data2) == 'Q49'] <- 'mc_rpp'


#calculate mean trust for  analysis and add to the data frame
data3<-data2
#trust in psychology
data3$mean_trust<-(data2$trust1+data2$trust2+data2$trust3+data2$trust4+data2$trust5)/5

#exclude participants####
#exclude more than 1 attention check failures
#check for failures according to our preregistration (0 = pass, 1 = failure)
mc1<-ifelse(data3$mc_experiments<5, 1, 0)
mc2<-ifelse(data3$mc_rpp<5, 1, 0)
mc2[is.na(mc2)] <- 0
mc3<-ifelse(data3$mc_rpp2<5, 1, 0)
mc3[is.na(mc3)] <- 0
mc4<-ifelse(data3$mc_stats>3, 1, 0)
#count and bind
mcfailure<-mc1+mc2+mc3+mc4
dataex0<-cbind(data3,mcfailure)
#exclude with more than 1 failure
dataex1<-subset(dataex0, mcfailure < 2)
#save(dataex1, file="data_prepared_study5.rda")
#write.csv2(dataex1, file="data_prepared_study5.csv")
