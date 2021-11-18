#define functions####
#function to select complete cases
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#require packages####
require(dplyr)
require(psych)
require(ggplot2)

#set working directory to where the data is located

#read data####
dataimp<-read.csv2("rawdata_study1.csv", stringsAsFactors= F)


#data preparation####
raw.data<-dataimp

#rename raw.data into data 2
data1<-raw.data
#change variable types and recode reverse coded trust variables (7->1... 1-> 7)
data1$replication_4<-as.numeric(data1$replication_4)
#Trust 1:Reverse coded
data1$trust1<-8-as.numeric(data1$trust1)
data1$trust2<-as.numeric(data1$trust2)
data1$trust3<-as.numeric(data1$trust3)
#Trust 4:Reverse coded
data1$trust4<-8-as.numeric(data1$trust4)
#Trust 5:Reverse coded
data1$trust5<-8-as.numeric(data1$trust5)
data1$value1<-as.numeric(data1$value1)
data1$value2<-as.numeric(data1$value2)
data1$value3<-as.numeric(data1$value3)
data1$value4<-as.numeric(data1$value4)
data1$age<-as.numeric(data1$age)

#the central DV "estimated replication rate" was called "replication_4" in Qualtrics and is thus renamed to "replication_rate"
names(data1)[names(data1) == 'replication_4'] <- 'replication_rate'
#change name of active dataset to data2
data2<-data1
#calculate means for preregistered ANOVA and add to dataframe data2
data2$mean_trust<-(data2$trust1+data2$trust2+data2$trust3+data2$trust4+data2$trust5)/5
data2$mean_value<-(data2$value1+data2$value2+data2$value3+data2$value4)/4

#save(data2, file="data_prepared_study1.rda")
#write.csv2(data2, file="data_prepared_study1.csv")
