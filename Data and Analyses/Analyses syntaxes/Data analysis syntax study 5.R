#make sure the following pakages are installed:"car","effsize","ggplot2","dplyr","psych","rockchalk","rio", "lsr"
#require packages####
require(car)
require(effsize)
require(ggplot2)
require(dplyr)
require(psych)
require(rockchalk)
require(rio)
require(lsr)
require(MBESS)
require(lavaan)
require(yarrr)
require(TOSTER)

#set working directory to where the data is located and load the data
load("data_prepared_study5.rda")
#the data is called dataex1, rename into data_ana
data_ana<-dataex1


#sample descriptives####
#sample size
length(data_ana$mc_experiments)-sum(is.na(data_ana$mc_experiments))
#gender
#men are coded as 1
table(data_ana$gender)
#percentage men
menpercent<-180/278*100
menpercent
#age
describe(data_ana$age)

#preliminary analysis####
##calculate reliabilites##
#select subset of variables
trust <- dplyr::select(data_ana, 6:10)

#calculate reliability
psych::alpha(trust) 

#descriptives
#overall trust
describe(data_ana$mean_trust)
#within conditions (LOW = Low Replicability, RESLOW = Still Low Replicability, RESHIGH = Now High Replicability)
describeBy(data_ana$mean_trust, group = data_ana$CON)


#compare  conditions on trust
#now highreplicability condition with still low replicability condition
t.test(data_ana$mean_trust[data_ana$CON=="RESHIGH"], data_ana$mean_trust[data_ana$CON=="RESLOW"], alternative = "greater", var.equal = T)
effsize::cohen.d(data_ana$mean_trust[data_ana$CON=="RESHIGH"], data_ana$mean_trust[data_ana$CON=="RESLOW"],pooled=TRUE,paired=FALSE,
        na.rm=T, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)


#now high replicability condition with low replicability condition
t.test(data_ana$mean_trust[data_ana$CON=="RESHIGH"], data_ana$mean_trust[data_ana$CON=="LOW"], alternative = "greater", var.equal = T)
effsize::cohen.d(data_ana$mean_trust[data_ana$CON=="RESHIGH"], data_ana$mean_trust[data_ana$CON=="LOW"],pooled=TRUE,paired=FALSE,
        na.rm=T, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)



#plotting
#create new dataset for plotting
data_plot<-data_ana
#rename condition values
#
data_plot$CON[data_plot$CON == "RESLOW"] <- "Low "
data_plot$CON[data_plot$CON == "LOW"] <- "Still Low"
data_plot$CON[data_plot$CON == "RESHIGH"] <- "Now High"


#create pirate plot
#for trust in science

#formula
pirateplot(mean_trust~CON,data=data_plot,
           
           #methods for "error bars"
           inf.method = "se",
           #x-lab description
           xlab="Described Replicability",
           #y-lab description
           ylab="Trust in Psychology (1-7)",
           #sort "bars" by mean
           sortx = "mean",
           #some graphical adjustment to text and point sizes
           point.o=1,
           cex.lab = 1.5,
           cex.names=1.3)

dev.off()



#supplemental analyses####

##manipulation check Psychological research is now more replicable##
#descriptives
aggregate( mc_replicable2 ~ CON, data_ana, mean )
aggregate( mc_replicable2 ~ CON, data_ana, sd) 


#compare now high replicability condition with still low replicability condition
t.test(data_ana$mc_replicable2[data_ana$CON=="RESHIGH"], data_ana$mc_replicable2[data_ana$CON=="RESLOW"], alternative = "greater", var.equal = T)
effsize::cohen.d(data_ana$mc_replicable2[data_ana$CON=="RESHIGH"], data_ana$mc_replicable2[data_ana$CON=="RESLOW"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)

#equivalence test####
#testing whether information about increased replicability (compared to no information) has an effect equivalent to d = |.2|
TOSTtwo(m1=4.35,m2=4.54,sd1=1.3,sd2=1.14,n1=98,n2= 90,low_eqbound=-0.2, high_eqbound=0.2, alpha = 0.05, var.equal=TRUE)


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
#get a new dataset without missing on mean_trust to allow merging
data_robust<-subset(data_ana, mean_trust>0)
#code trust_new as numeric and name it trust_new
data_robust$trust_new<-as.numeric(trust_new)

#get descriptives
aggregate(trust_new ~ CON, data_robust, mean ) 
aggregate( trust_new ~ CON, data_robust, sd) 

#compare now high replicability condition with low replicability condition
t.test(data_robust$trust_new[data_robust$CON=="RESHIGH"], data_robust$trust_new[data_robust$CON=="LOW"], alternative = "greater", var.equal = T)
effsize::cohen.d(data_robust$trust_new[data_robust$CON=="RESHIGH"], data_robust$trust_new[data_robust$CON=="LOW"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)
