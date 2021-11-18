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
require(MBESS)
require(lavaan)
require(yarrr)
require(TOSTER)

#set working directory to where the data is located and load the data
load("data_prepared_study4.rda")
#the data is called dataex1, rename into data_ana
data_ana<-dataex1


#sample descriptives####
#sample size
length(data_ana$mc_experiments)-sum(is.na(data_ana$mc_experiments))
#gender
#men are coded as 1
table(data_ana$gender)
#percentage men
menpercent<-157/283*100
menpercent
#age
describe(data_ana$age)


#preliminary analysis####
##calculate reliabilites##
#select subset of variables
trust <- dplyr::select(data_ana, 6:10)

#calculate reliability
psych::alpha(trust) 

#compare all conditions on trust
####t-tests
#descriptives
#overall
describe(data_ana$mean_trust)
#within conditions (LOW =	Low Replicability, LOWMOD =	Hidden Moderators, LOWQRP =	QRPs))
describeBy(data_ana$mean_trust, group = data_ana$CON)

#compare hidden moderator condition vs QRPs condition  
t.test(data_ana$mean_trust[data_ana$CON=="LOWMOD"], data_ana$mean_trust[data_ana$CON=="LOWQRP"], alternative = "greater", var.equal = T)
effsize::cohen.d(data_ana$mean_trust[data_ana$CON=="LOWMOD"], data_ana$mean_trust[data_ana$CON=="LOWQRP"],pooled=TRUE,paired=FALSE,
        na.rm=T, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#compare control vs hidden morator condition  
t.test(data_ana$mean_trust[data_ana$CON=="LOWMOD"], data_ana$mean_trust[data_ana$CON=="LOW"], alternative = "two.sided", var.equal = T)
effsize::cohen.d(data_ana$mean_trust[data_ana$CON=="LOWMOD"], data_ana$mean_trust[data_ana$CON=="LOW"],pooled=TRUE,paired=FALSE,
          na.rm=T, hedges.correction=FALSE,
          conf.level=0.95,noncentral=FALSE)

#compare control vs QRPs condition
t.test(data_ana$mean_trust[data_ana$CON=="LOWQRP"], data_ana$mean_trust[data_ana$CON=="LOW"], alternative = "two.sided", var.equal = T)
effsize::cohen.d(data_ana$mean_trust[data_ana$CON=="LOWQRP"], data_ana$mean_trust[data_ana$CON=="LOW"],pooled=TRUE,paired=FALSE,
        na.rm=T, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)



#plotting
#create new dataset for plotting
data_plot<-data_ana
#rename condition values
data_plot$CON[data_plot$CON == "LOWQRP"] <- "QRPs"
data_plot$CON[data_plot$CON == "LOWMOD"] <- "Hidden Moderator"
data_plot$CON[data_plot$CON == "LOW"] <- "Low Replicability"

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
#hidden or QRP
#descriptives "Unknown or hidden factors explain the low replication rate."
aggregate( mc_hidden ~ CON, data_ana, mean )
aggregate( mc_hidden ~ CON, data_ana, sd) 

#descriptives  "Questionable research practices explain the low replication rate."
aggregate( mc_qrp ~ CON, data_ana, mean )
aggregate( mc_qrp ~ CON, data_ana, sd) 


#test effects on manipulation checks
#hidden moderator condition vs QRPs condition on hidden moderator manipulation check
t.test(data_ana$mc_hidden[data_ana$CON=="LOWMOD"], data_ana$mc_hidden[data_ana$CON=="LOWQRP"], alternative = "greater", var.equal = T)
effsize::cohen.d(data_ana$mc_hidden[data_ana$CON=="LOWMOD"], data_ana$mc_hidden[data_ana$CON=="LOWQRP"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)

#hidden moderator condition vs QRPs condition on QRP manipulation check
t.test(data_ana$mc_qrp[data_ana$CON=="LOWMOD"], data_ana$mc_qrp[data_ana$CON=="LOWQRP"], alternative = "less", var.equal = T)
effsize::cohen.d(data_ana$mc_qrp[data_ana$CON=="LOWMOD"], data_ana$mc_qrp[data_ana$CON=="LOWQRP"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)


#equivalence tests####
#equivalence tests for hidden moderator condition
TOSTtwo(m1=4.76,m2=4.8,sd1=1.34,sd2=1.09,n1=94,n2= 96 ,low_eqbound=-.2, high_eqbound=0.2, alpha = 0.05, var.equal=TRUE)

#equivalence tests for QRP condition
TOSTtwo(m1=4.76,m2=4.45,sd1=1.34,sd2=1.16,n1=94,n2= 91 ,low_eqbound=-.2, high_eqbound=0.2, alpha = 0.05, var.equal=TRUE)

#Confirmatory Factor Analyses of the used trust measure####
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
#compare hidden moderator condition vs QRPs condition  
t.test(data_robust$trust_new[data_robust$CON=="LOWMOD"], data_robust$trust_new[data_robust$CON=="LOWQRP"], alternative = "greater", var.equal = T)
effsize::cohen.d(data_robust$trust_new[data_robust$CON=="LOWMOD"], data_robust$trust_new[data_robust$CON=="LOWQRP"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)

#compare control vs hidden morator condition  
t.test(data_robust$trust_new[data_robust$CON=="LOWMOD"], data_robust$trust_new[data_robust$CON=="LOW"], alternative = "two.sided", var.equal = T)
effsize::cohen.d(data_robust$trust_new[data_robust$CON=="LOWMOD"], data_robust$trust_new[data_robust$CON=="LOW"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)
#compare control vs QRPs condition
t.test(data_robust$trust_new[data_robust$CON=="LOWQRP"], data_robust$trust_new[data_robust$CON=="LOW"], alternative = "two.sided", var.equal = T)
effsize::cohen.d(data_robust$trust_new[data_robust$CON=="LOWQRP"], data_robust$trust_new[data_robust$CON=="LOW"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)

