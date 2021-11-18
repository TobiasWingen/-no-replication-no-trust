#make sure the following pakages are installed:"car","effsize","ggplot2","dplyr","psych","rockchalk","rio"
#require packages####
require(car)
require(effsize)
require(ggplot2)
require(dplyr)
require(psych)
require(rockchalk)
require(rio)
#set working directory to where the data is located

dataimp<-read.csv2("rawdata_study3.csv", stringsAsFactors = FALSE)

#data preparation####
data1<-dataimp


#delete cases where everything is NA
data2<-data1
data2$na<-is.na(data2$mc_experiments)
data2<-subset(data2, data2$na==FALSE)

#sample size before exclusions (number of responses on first item)
length(data2$mc_experiments)

#recode reverse trust variablesn)
#reverse coded
data2$trust1<-8-as.numeric(data2$trust1)
data2$trust4<-8-as.numeric(data2$trust4)
data2$trust5<-8-as.numeric(data2$trust5)

#get final dataset where all variable types are correct####
#change all variables types to numeric and later restore characters as characters
data2a <- data.frame(lapply(data2, function(x) as.numeric(x)))
#restore non numeric values
data2b <- data2a[,-c(12,14:17)]
data2c<- data2[,c(12,14:17)]
data2d<-cbind(data2b,data2c)
data2<-data2d

#rename the item mc_surveys to: mc_rpp,lity and mc_change to: mc_trans, because item name was incorrect on qualtrics.
names(data2)[names(data2) == 'mc_surveys'] <- 'mc_rpp'
names(data2)[names(data2) == 'mc_change'] <- 'mc_trans'


#calculate mean trust for  analysis and add to the data frame
data3<-data2
str(data3)
#trust in psychology
data3$mean_trust<-(data2$trust1+data2$trust2+data2$trust3+data2$trust4+data2$trust5)/5

#exclude participants based on preregistered replication criteria####
#exclude more than 1 attention check failures
#check for failures according to our preregistration (0 = pass, 1 = failure)
mc1<-ifelse(data3$mc_experiments>3, 0, 1)
mc2<-ifelse(data3$mc_rpp>3, 0, 1)
mc3<-ifelse(data3$mc_stats<5, 0, 1)
#count and bind
mcfailure<-mc1+mc2+mc3
dataex0<-cbind(data3,mcfailure)
#exclude with more than 1 failure
#create new dataset dataex1 with exclusions
dataex1<-subset(data3, mcfailure < 2)
#save(dataex1, file="data_prepared_study3.rda")
#write.csv2(dataex1, file="data_prepared_study3.csv")
