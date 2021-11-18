#make sure packages are installed
#require packages####
require("car")
require("effsize")
require("ggplot2")
require("dplyr")
require("psych")
require("rockchalk")
#set wd to where the data is located and load data#

dataimp<-read.csv2("rawdata_study2.csv", stringsAsFactors= F, na.strings = "")
#data preparation####
data1<-dataimp
#delete cases where everything is NA
data1$na<-is.na(data1$mc_experiments)
data2<-subset(data1, data1$na==FALSE)

#change variable types and recode reverse coded trust and concept of science variables
data2$mc_stats<-as.numeric(data2$mc_stats)
data2$mc_surveys<-as.numeric(data2$mc_surveys)
data2$mc_experiments<-as.numeric(data2$mc_experiments)
data2$mc_replication<-as.numeric(data2$mc_replication)
#reverse coded
data2$trust1<-8-as.numeric(data2$trust1)
data2$trust2<-as.numeric(data2$trust2)
data2$trust3<-as.numeric(data2$trust3)
#reverse coded
data2$trust4<-8-as.numeric(data2$trust4)
#reverse coded
data2$trust5<-8-as.numeric(data2$trust5)
data2$value1<-as.numeric(data2$value1)
data2$value2<-as.numeric(data2$value2)
data2$value3<-as.numeric(data2$value3)
data2$value4<-as.numeric(data2$value4)
#reverse coded
data2$science1<-8-as.numeric(data2$science1)
data2$science2<-as.numeric(data2$science2)
data2$science3<-as.numeric(data2$science3)
data2$error_int<-as.numeric(data2$error_int)
data2$error_ext<-as.numeric(data2$error_ext)
data2$error_comm1<-as.numeric(data2$error_comm1)
data2$error_comm2<-as.numeric(data2$error_comm2)
data2$error_comm3<-as.numeric(data2$error_comm3)
data2$error_comm4<-as.numeric(data2$error_comm4)
data2$error_impro1<-as.numeric(data2$error_impro1)
data2$error_impro2<-as.numeric(data2$error_impro2)
data2$error_impro3<-as.numeric(data2$error_impro3)
data2$age<-as.numeric(data2$age)
#rename the item mc_surveys to: mc_rpp, item name was incorrect on qualtrics (item refers to understanding that the Reproducibility Project: Psychology was conducted)
names(data2)[names(data2) == 'mc_surveys'] <- 'mc_rpp'

#calculate means for analysis and add them to the data frame
data3<-data2
#trust in psychology
data3$mean_trust<-(data2$trust1+data2$trust2+data2$trust3+data2$trust4+data2$trust5)/5
#value of psychology
data3$mean_value<-(data2$value1+data2$value2+data2$value3+data2$value4)/4
#belief on the nature of science truth vs. debate
data3$mean_science<-(data2$science1+data2$science2+data2$science3)/3
#errors should be communicated
data3$mean_errorcomm<-(data2$error_comm1+data2$error_comm2+data2$error_comm3+data2$error_comm4)/4
#errors improve my work
data3$mean_errorimpro<-(data2$error_impro1+data2$error_impro2+data2$error_impro3)/3
#calculate difference between internal and external attribution
data3$mean_attr<-(data2$error_ext-data2$error_int)
#save(data3, file="data_prepared_study2.rda")
#write.csv2(data3, file="data_prepared_study2.csv")

