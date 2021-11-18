#make sure the following pakages are installed:"car","effsize","ggplot2","dplyr","psych","rockchalk","rio"
#require packages####
require(car)
require(effsize)
require(ggplot2)
require(dplyr)
require(psych)
require(rockchalk)
require(rio)
require(lsr)
require(TOSTER)
require(MBESS)
require(lavaan)
require(yarrr)


#set working directory to where the data is located and load the data
load("data_prepared_study3.rda")
#the data is called dataex1, rename into data_ana
data_ana<-dataex1


#sample descriptives####
#sample size
length(data_ana$mc_experiments)-sum(is.na(data_ana$mc_experiments)) 
#gender
#men are coded as 1
table(data_ana$gender)
#percentage men
menpercent<-169/297*100
menpercent
#age
describe(data_ana$age)


#preliminary analysis####
##calculate reliabilites##
#select subset of variables
trust <- dplyr::select(data_ana, 6:10)

#calculate reliability
psych::alpha(trust) 


####preregistered t-tests
#compare all conditions on trust
#compare low replicability with high replicability condition
t.test(data_ana$mean_trust[data_ana$CON=="LOWNOTRANS"], data_ana$mean_trust[data_ana$CON=="HIGH"], alternative = "less", var.equal = T)
effsize::cohen.d(data_ana$mean_trust[data_ana$CON=="LOWNOTRANS"], data_ana$mean_trust[data_ana$CON=="HIGH"],pooled=TRUE,paired=FALSE,
na.rm=T, hedges.correction=FALSE,
conf.level=0.95,noncentral=FALSE)
#compare low replicability with low replicability/transparency condition
t.test(data_ana$mean_trust[data_ana$CON=="LOWNOTRANS"], data_ana$mean_trust[data_ana$CON=="LOWTRANS"], alternative = "less", var.equal = T)
effsize::cohen.d(data_ana$mean_trust[data_ana$CON=="LOWNOTRANS"], data_ana$mean_trust[data_ana$CON=="LOWTRANS"],pooled=TRUE,paired=FALSE,
na.rm=T, hedges.correction=FALSE,
conf.level=0.95,noncentral=FALSE)

#compare low replicability/transparency with high replicability condition
t.test(data_ana$mean_trust[data_ana$CON=="LOWTRANS"], data_ana$mean_trust[data_ana$CON=="HIGH"], alternative = "two.sided", var.equal = T)
effsize::cohen.d(data_ana$mean_trust[data_ana$CON=="LOWTRANS"], data_ana$mean_trust[data_ana$CON=="HIGH"],pooled=TRUE,paired=FALSE,
na.rm=T, hedges.correction=FALSE,
conf.level=0.95,noncentral=FALSE)


#descriptives
#Correlation Replicability with overall trust
describe(data_ana$mean_trust)
#within conditions (LOWNOTRANS = Low Replicability, LOWTRANS =	Transparency, HIGH = High Replicability)
describeBy(data_ana$mean_trust, group = data_ana$CON)
#correlation within conditions
#HIGH
data_high<-subset(data_ana, CON=="HIGH")
cor.test(data_high$mc_replicable,data_high$mean_trust)
#LOWNOTRANS
data_lownotrans<-subset(data_ana, CON=="LOWNOTRANS")
cor.test(data_lownotrans$mc_replicable,data_lownotrans$mean_trust)
#LOWTRANS
data_lowtrans<-subset(data_ana, CON=="LOWTRANS")
cor.test(data_lowtrans$mc_replicable,data_lowtrans$mean_trust)



#plotting
#create new dataset for plotting
data_plot<-data_ana
#rename condition values

data_plot$CON[data_plot$CON == "LOWNOTRANS"] <- "Low"
data_plot$CON[data_plot$CON == "HIGH"] <- "High "
data_plot$CON[data_plot$CON == "LOWTRANS"] <- "Low but Transparency"

#create pirate plot
#for trust in science
#formula
pirateplot(mean_trust~CON,data=data_plot,
           
           #methods for "error bars"
           inf.method = "se",
           #x-lab description
           xlab="Conditions",
           #y-lab description
           ylab="Trust in Psychology (1-7)",
           #sort "bars" by mean
           sortx = "mean",
           #some graphical adjustment to text and point sizes
           point.o=1,
           cex.lab = 1.5,
           cex.names=1.3)


#supplemental analyses####

##manipulation checks####
#Psychological research is replicable
#descriptives
aggregate( mc_replicable ~ CON, data_ana, mean )
aggregate( mc_replicable ~ CON, data_ana, sd) 

#compare low replicability with high replicability condition
t.test(data_ana$mc_replicable[data_ana$CON=="LOWNOTRANS"], data_ana$mc_replicable[data_ana$CON=="HIGH"], alternative = "less", var.equal = T)
effsize::cohen.d(data_ana$mc_replicable[data_ana$CON=="LOWNOTRANS"], data_ana$mc_replicable[data_ana$CON=="HIGH"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)

#compare low replicability/transparency with high replicability condition
t.test(data_ana$mc_replicable[data_ana$CON=="LOWTRANS"], data_ana$mc_replicable[data_ana$CON=="HIGH"], alternative = "less", var.equal = T)
effsize::cohen.d(data_ana$mc_replicable[data_ana$CON=="LOWTRANS"], data_ana$mc_replicable[data_ana$CON=="HIGH"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)

#Psychological research is transparent
#descriptives
aggregate( mc_trans ~ CON, data_ana, mean )
aggregate( mc_trans ~ CON, data_ana, sd) 

#compare low replicability with low replicability/transparency condition
t.test(data_ana$mc_trans[data_ana$CON=="LOWNOTRANS"], data_ana$mc_trans[data_ana$CON=="LOWTRANS"], alternative = "less", var.equal = T)
effsize::cohen.d(data_ana$mc_trans[data_ana$CON=="LOWNOTRANS"], data_ana$mc_trans[data_ana$CON=="LOWTRANS"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)


#compare high replicability with low replicability/transparency condition
t.test(data_ana$mc_trans[data_ana$CON=="HIGH"], data_ana$mc_trans[data_ana$CON=="LOWTRANS"], alternative = "two.sided", var.equal = T)
effsize::cohen.d(data_ana$mc_trans[data_ana$CON=="HIGH"], data_ana$mc_trans[data_ana$CON=="LOWTRANS"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)


#equivalence test####
#testing whether information about increased transparency (compared to no information) has an effect equivalent to d = |.2|
TOSTtwo(m1=4.66 ,m2=4.8,sd1=1.36,sd2=1.33 ,n1= 100,n2=  96 ,low_eqbound=-0.2, high_eqbound=0.2, alpha = 0.05, var.equal=TRUE)

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
#get a new dataset without missing on mean_trust
data_robust<-subset(data_ana, mean_trust>0)
#code trust_new as numeric and name it trust_new
data_robust$trust_new<-as.numeric(trust_new)

#get descriptives
aggregate(trust_new ~ CON, data_robust, mean ) 
aggregate( trust_new ~ CON, data_robust, sd) 

#compare low replicability with high replicability condition
t.test(data_robust$trust_new[data_robust$CON=="LOWNOTRANS"], data_robust$trust_new[data_robust$CON=="HIGH"], alternative = "less", var.equal = T)
effsize::cohen.d(data_robust$trust_new[data_robust$CON=="LOWNOTRANS"], data_robust$trust_new[data_robust$CON=="HIGH"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)

#compare low replicability with low replicability/transparency condition
t.test(data_robust$trust_new[data_robust$CON=="LOWNOTRANS"], data_robust$trust_new[data_robust$CON=="LOWTRANS"], alternative = "less", var.equal = T)
effsize::cohen.d(data_robust$trust_new[data_robust$CON=="LOWNOTRANS"], data_robust$trust_new[data_robust$CON=="LOWTRANS"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)


