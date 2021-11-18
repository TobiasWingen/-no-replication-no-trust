#require packages - make sure packages are installed####
require(dplyr)
require(psych)
require(ggplot2)
require(lavaan)
require(ggExtra)

#define function for excluding missing rows from the dataset, required for plotting
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
#ensure working directory is in the right location

#load the data,
load("data_prepared_study1.rda")

#due to our workflow the data is named "data2" rename into data_ana
data_ana<-data2

#sample descriptives####
#calculate sample size by counting responses - NAs on the first item in the survey
nrow(data_ana)-sum(is.na(data_ana$replication_rate)) #271
#gender
#men are coded as 1
table(data_ana$gender)
menpercent<-144/(144+121)*100 
menpercent
#age 
describe(data_ana$age)


#test how many people know the outcome of the Reprodicibility Project
#have heard of it = 1
table(data_ana$knowledge1)
#know the exact outcome = 1 
table(data_ana$knowledge2)
#number of people that reported to know the outcome of the Reprodicibility Project (only one person stated to know the outcome. This person reported  that the outcome was 14 (see dataframe), which is incorrect) 
table(data_ana$knowledge2)[1]

#describe estimated replication rate
describe(data_ana$replication_rate)
#describe trust
describe(data_ana$mean_trust)


#check for reliability of the trust measure
#select relevant variables
trust <- select(data_ana, 2:6)
#check reliability
psych::alpha(trust)

#descriptives
#test the correlation between estimated replication rate and trust
cor.test(data_ana$mean_trust,data_ana$replication_rate, alternative = "greater")
#while we had a one-sided hypothesis, two.sided tests are calculated in order to provide two.sided confidence intervals
cor.test(data_ana$mean_trust,data_ana$replication_rate, alternative = "two.sided")


#plotting
#scatter plot for trust
#create the plot

#exclude missing
datagra2<-completeFun(data_ana, c("replication_rate", "mean_trust"))
plot_center = ggplot(datagra2, aes(x=replication_rate, y=mean_trust)) +
  #add points
  geom_point(shape=16, col = "#B7CFE8", size = 2) +    # Use hollow circles
  #add line
  geom_smooth(method=lm, se = F, col = "#FAB3B8", linetype=7, size = 1) + 
  #scales
  scale_x_continuous(name = "Estimated Replication Rate",
                     breaks = seq(0, 100, 10),
                     limits=c(0, 100)) +
  scale_y_continuous(name = "Trust in Psychology (1-7)",breaks = seq(1, 7, 1),
                     limits=c(1, 7)) +
  theme_classic() +
  theme(axis.text.x = element_text(colour="black",size=15,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="black",size=15,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="black",size=25,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="black",size=25,angle=90,hjust=.5,vjust=.5,face="plain"))
ggMarginal(plot_center, type="histogram", fill="#B9E0C5",   xparams = list(breaks=seq(0, 100, by=10)), yparams = list(breaks=seq(1, 7, by=0.5)))

#supplemental analyses####
#The relationship between perceived value und replicability####
#reliability of the value measure
#select variables
value <- select(data_ana, 7:10)
#calculate alpha
psych::alpha(value)

#test the correlation between estimated replication rate and perceived value
cor.test(data_ana$mean_value,data_ana$replication_rate, alternative = "greater")
#while we had a one-sided hypothesis, two.sided tests are calculated in order to provide two.sided confidence intervals
cor.test(data_ana$mean_value,data_ana$replication_rate, alternative = "two.sided")


#Confirmatory Factor Analyses of the used trust measure
#get fit indices
#model
model = "Trust =~ trust1 + trust2 + trust3 + trust4 + trust5"
#assess fit
fit = cfa(model, data=data_ana)
summary(fit, standardized=T, fit.measures=T, rsq=T)
#get modification indices to see how fit could be improved
mi <- modindices(fit)
mi
#fit new model where trust 2 and trust 3 error variances are allowed to covary
model2 = "Trust =~ trust1 + trust2 + trust3 + trust4 + trust5
trust2 ~~ trust3
"
#get fit indices of this new model
fit2 = cfa(model2, data=data_ana)
summary(fit2, standardized=T, fit.measures=T, rsq=T)

#robustness check####
#rerun analyses with trust in science scores from this new model
#predict trust in science (trust_new) from the CFA
trust_new<-lavPredict(fit2)
#only select cases with complete information on the trust scale, to allow merging 
data_robust<-subset(data_ana, mean_trust>0)
#code trust_new as numeric and name it trust_new
data_robust$trust_new<-as.numeric(trust_new)

#test the correlation between estimated replication rate and trust factor scores
cor.test(data_robust$trust_new,data_robust$replication_rate, alternative = "greater")
#while we had a one-sided hypothesis, two.sided tests are calculated in order to provide two.sided confidence intervals
cor.test(data_robust$trust_new,data_robust$replication_rate, alternative = "two.sided")
