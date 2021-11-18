#make sure the  packages are installed
#require packages####
require("car")
require("effsize")
require("ggplot2")
require("dplyr")
require("psych")
require("rockchalk")
require ("QuantPsyc")
require("lsr")
require("MBESS")
require("yarrr")
require("lavaan")
#set wd to where the data is located and load data#
load("data_prepared_study2.rda")

#rename dataset into data_ana
data_ana<-data3
#sample descriptives####
#sample size
length(data_ana$mc_experiments)-sum(is.na(data_ana$mc_experiments))
#gender
#men are coded as 1
table(data_ana$gender)
#percentage men
menpercent<-161/269*100
menpercent
#age
describe(data_ana$age)


#check reliability of trust measure
#select subset of variables
trust <- dplyr::select(data_ana, 5:9)
#check reliability
psych::alpha(trust)

#central analysis
#anova on trust
trustmod = lm(mean_trust ~ CON, data = data_ana)
summary(trustmod)
anova(trustmod)

#calculate eta squared 
etaSquared(trustmod, type = 2, anova = FALSE )
#save values required for CI calculation
df1<-anova(trustmod)[1,1]
df2<-anova(trustmod)[2,1]
f_value<-anova(trustmod)[1,4]
#calculating N by the formula df1+df2+1
samplesize<-df1+df2+1
#calculate eta squared  confidence interval (90% CI for eta-squared is equivalent to 95%,  according to http://daniellakens.blogspot.com/2014/06/calculating-confidence-intervals-for.html)
ci.pvaf(F.value=f_value, df.1=df1, df.2=df2, N=samplesize,conf.level=.90)

#descriptives trust
#overall
describe(data_ana$mean_trust)

#within conditions (LOW = Low Replicability , MEIDUM = Medium Replicability, HIGH = High Replicability)
describeBy(data_ana$mean_trust,data_ana$CON)

#low replicability vs high replicability condition on trust
t.test(data_ana$mean_trust[data_ana$CON=="LOW"], data_ana$mean_trust[data_ana$CON=="HIGH"], alternative = "less", var.equal = T)
effsize::cohen.d(data_ana$mean_trust[data_ana$CON=="LOW"], data_ana$mean_trust[data_ana$CON=="HIGH"],pooled=TRUE,paired=FALSE,
        na.rm=T, hedges.correction=FALSE,
        conf.level=0.95,noncentral=FALSE)

#low replicability vs medium replicability condition on trust
t.test(data_ana$mean_trust[data_ana$CON=="LOW"], data_ana$mean_trust[data_ana$CON=="MEDIUM"], alternative = "less", var.equal = T)
effsize::cohen.d(data_ana$mean_trust[data_ana$CON=="MEDIUM"], data_ana$mean_trust[data_ana$CON=="LOW"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)

#high replicability vs medium replicability condition on trust
t.test(data_ana$mean_trust[data_ana$CON=="MEDIUM"], data_ana$mean_trust[data_ana$CON=="HIGH"], alternative = "less", var.equal = T)
effsize::cohen.d(data_ana$mean_trust[data_ana$CON=="HIGH"], data_ana$mean_trust[data_ana$CON=="MEDIUM"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)

#descriptives
#correlation between trust measure and replication manipulation check in the different conditions
#HIGH
data_high<-subset(data_ana, CON=="HIGH")
cor.test(data_high$mc_replication,data_high$mean_trust)
#LOW
data_low<-subset(data_ana, CON=="LOW")
cor.test(data_low$mc_replication,data_low$mean_trust)
#MEDIUM
data_medium<-subset(data_ana, CON=="MEDIUM")
cor.test(data_medium$mc_replication,data_medium$mean_trust)



#plotting####

#create new dataset for plotting
data_plot<-data_ana
#rename condition values
#
data_plot$CON[data_plot$CON == "LOW"] <- "Low"
data_plot$CON[data_plot$CON == "MEDIUM"] <- "Medium"
data_plot$CON[data_plot$CON == "HIGH"] <- "High"

#create pirate plot

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






#supplemental analyses####

##calculate reliabilites of perceived value and moderators##
#select subset of variables
value <- dplyr::select(data_ana, 10:13)
science <- dplyr::select(data_ana, 14:16)
error_impro <- dplyr::select(data_ana, 23:25)
error_comm  <- dplyr::select(data_ana, 19:22)
attr <- dplyr::select(data_ana, 17:18)
#calculate reliabilites
psych::alpha(value)
psych::alpha(science)
psych::alpha(error_impro)
psych::alpha(error_comm)
#calculate correlation between internal and external attribution
cor.test(data_ana$error_int,data_ana$error_ext)


#Analyses with second DV: perceived value of psychological science
#overall ANOVA
valuemod = lm(mean_value ~ CON, data = data_ana)
summary(valuemod)
anova(valuemod)

#calculate eta squared 
etaSquared(valuemod, type = 2, anova = FALSE )
#save values required for CI calculation
df1<-anova(valuemod)[1,1]
df2<-anova(valuemod)[2,1]
f_value<-anova(valuemod)[1,4]
#calculating N by the formula df1+df2+1
samplesize<-df1+df2+1
#calculate eta squared  confidence interval (90% CI for eta-squared is equivalent to 95%,  according to http://daniellakens.blogspot.com/2014/06/calculating-confidence-intervals-for.html)
ci.pvaf(F.value=f_value, df.1=df1, df.2=df2, N=samplesize,conf.level=.90)

#descriptives value
aggregate( mean_value ~ CON, data_ana, mean )
aggregate( mean_value ~ CON, data_ana, sd )

#low replicability vs high replicability condition on value
t.test(data_ana$mean_value[data_ana$CON=="LOW"], data_ana$mean_value[data_ana$CON=="HIGH"], alternative = "less", var.equal = T)
effsize::cohen.d(data_ana$mean_value[data_ana$CON=="LOW"], data_ana$mean_value[data_ana$CON=="HIGH"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)

#low replicability vs medium replicability condition on value
t.test(data_ana$mean_value[data_ana$CON=="LOW"], data_ana$mean_value[data_ana$CON=="MEDIUM"], alternative = "less", var.equal = T)
effsize::cohen.d(data_ana$mean_value[data_ana$CON=="LOW"], data_ana$mean_value[data_ana$CON=="MEDIUM"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)

#high replicability vs medium replicability condition on value
t.test(data_ana$mean_value[data_ana$CON=="MEDIUM"], data_ana$mean_value[data_ana$CON=="HIGH"], alternative = "less", var.equal = T)
effsize::cohen.d(data_ana$mean_value[data_ana$CON=="HIGH"], data_ana$mean_value[data_ana$CON=="MEDIUM"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)


#moderator analyses####
#creating a subset, as we are mostly interested in whether the difference between low and high is moderated by our moderators
data_mod<-subset(data_ana, CON == "LOW" | CON == "HIGH")

#testing if the effect on trust is moderated
#moderator:centered belief in science
model_trust_belief<-lm(mean_trust~CON*scale(mean_science, scale = F), data=data_mod)
summary(model_trust_belief)

#moderator: centered attribution. These results should probably not be interpreted, as those items seem to lack validity
model_trust_attribution<-lm(mean_trust~CON*scale(mean_attr, scale = F), data=data_mod)
summary(model_trust_attribution)

#moderator:centered error improvement
model_trust_error_improvement<-lm(mean_trust~CON*scale(mean_errorimpro, scale = F), data=data_mod)
summary(model_trust_error_improvement)

#moderator:centered error communication
model_trust_communication_error<-lm(mean_trust~CON*scale(mean_errorcomm, scale = F), data=data_mod)
summary(model_trust_communication_error)

#is the relationship towards perceived value moderated?
#moderator:centered belief in science####
model_value_belief<-lm(mean_value~CON*scale(mean_science, scale = F), data=data_mod)
summary(model_value_belief)

#moderator: centered attribution
model_value_attribution<-lm(mean_value~CON*scale(mean_attr, scale = F), data=data_mod)
summary(model_value_attribution)

#moderator:centered error improvment
model_value_error_improvement<-lm(mean_value~CON*scale(mean_errorimpro, scale = F), data=data_mod)
summary(model_value_error_improvement)

#moderator:centered error communication
model_value_communication_error<-lm(mean_value~CON*scale(mean_errorcomm, scale = F), data=data_mod)
summary(model_value_communication_error)


##manipulation checks#####
aggregate( mc_replication ~ CON, data_ana, mean )
aggregate( mc_replication ~ CON, data_ana, sd)

#compare low replicability with high replicability condition
t.test(data_ana$mc_replication[data_ana$CON=="LOW"], data_ana$mc_replication[data_ana$CON=="HIGH"], alternative = "less", var.equal = T)
effsize::cohen.d(data_ana$mc_replication[data_ana$CON=="LOW"], data_ana$mc_replication[data_ana$CON=="HIGH"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)

#compare medium replicability with high replicability condition
t.test(data_ana$mc_replication[data_ana$CON=="MEDIUM"], data_ana$mc_replication[data_ana$CON=="HIGH"], alternative = "less", var.equal = T)
effsize::cohen.d(data_ana$mc_replication[data_ana$CON=="MEDIUM"], data_ana$mc_replication[data_ana$CON=="HIGH"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)

#compare low replicability with medium replicability condition
t.test(data_ana$mc_replication[data_ana$CON=="LOW"], data_ana$mc_replication[data_ana$CON=="MEDIUM"], alternative = "less", var.equal = T)
effsize::cohen.d(data_ana$mc_replication[data_ana$CON=="LOW"], data_ana$mc_replication[data_ana$CON=="MEDIUM"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)


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
aggregate(trust_new ~ CON, data_robust, sd ) 

#low replicability vs high replicability condition on trust factor scores
t.test(data_robust$trust_new[data_robust$CON=="LOW"], data_robust$trust_new[data_robust$CON=="HIGH"], alternative = "less", var.equal = T)
effsize::cohen.d(data_robust$trust_new[data_robust$CON=="LOW"], data_robust$trust_new[data_robust$CON=="HIGH"],pooled=TRUE,paired=FALSE,
                 na.rm=T, hedges.correction=FALSE,
                 conf.level=0.95,noncentral=FALSE)


