## R code for Trust in Everyday Life 
## Weiss, A., Michels, C., Burgmer, P., Mussweiler, T., Ockenfels, A., & Hofmann, W. (in press). Trust in everyday life.
## Script Version June-2020
## Written by: Wilhelm Hofmann


install.packages("psych")
install.packages("nlme")
install.packages("lme4")
install.packages("lmerTest")
install.packages("effects")
install.packages("ggplot2")
install.packages("car")
install.packages("dplyr")
install.packages("ggridges")
install.packages("GGally")
install.packages("gridExtra")
install.packages("sjPlot")
install.packages("lattice")
install.packages("sjstats")
install.packages("ggeffects")
install.packages("gmodels")
install.packages("cramer")
install.packages("sjlabelled")
install.packages("sjmisc")
install.packages("ggpubr")
install.packages("EMAtools")
install.packages("rmcorr")
install.packages("apaTables")
install.packages("stargazer")
install.packages("bootnet")
install.packages("xlsx")
install.packages("memisc")
install.packages("plyr")
install.packages("reghelper")
install.packages("merTools")
install.packages("devtools")
install_github("kassambara/easyGgplot2",force=TRUE)



#clear environment
rm(list=ls())
options(scipen = 999)


library("tidyverse")
library("dplyr")
library("devtools")
library ("psych")
library ("foreign")
library ("nlme")
library ("lme4")
library ("lmerTest")
library ("effects")
library ("ggplot2")
library ("RSA")
library ("car")
library("ggridges")
library("GGally")
library("gridExtra")
library("sjPlot")
library("lattice")
library("sjstats")
library("stats")
library("gmodels")
library("cramer")
library("sjlabelled")
library("sjmisc")
library("ggpubr")
library("easyGgplot2")
library("EMAtools")
library("rmcorr")
library("apaTables")
library("broom.mixed")
library("stargazer")
library("xlsx")
library("memisc")
library("corrplot")
library("apaTables")
library("reghelper")
library("merTools")
library("emmeans")
library("easyGgplot2")
library("wesanderson")
library ("pscl")
library ("MASS")

######################################################MAIN Analyses############################################
######################################################MAIN Analyses############################################
######################################################MAIN Analyses############################################
######################################################MAIN Analyses############################################
######################################################MAIN Analyses############################################


#paper sections:

##################################################################################################################################
######################################################  Descriptive and Frequency Data  ############################################
##################################################################################################################################
#here and elsewhere: set directory to your working directory

setwd ("your directory here")


d = readRDS("Trust_final_OSF.RDS")

names (d)

### Sample description in methods part
#demographics for gender and age
describe(d$oneline [d$oneline == 1])

levels(d$gender)
summary(d$gender[d$oneline == 1])
prop.table(table(d$gender[d$oneline == 1]))

#age
describe(d$age[d$oneline==1]) 


#education
edu <- prop.table(table(d$education[d$oneline==1],exclude=NA))
margin.table(table(d$education[d$oneline==1]),1)



#employed
acti <- prop.table(table(d$activity[d$oneline==1],exclude=NA))
acti


#median delay in responding
describe(d$timedelay_minutes)$median

#other
summary(d$PolitikOrient)

#overall number of surveys
describe(as.numeric(d$start))

#average response rate
describe(d$pct[d$oneline==1])
describe(d$Nu_sig[d$oneline==1])


#median completion/survey time in seconds
sts = describe(d$surveytime)$mean
sts/60

#partial completion
pcom = summary(d$Finished)
pcom/8176

### supplementary table 4 intercorrelations of level 1 vars


###Multilevel Correlations for Supplement
###routine for computing all rmcorrs for given matrix of any length, with clustering variable (subject) last (with big thanks to xxxx yyyy)

#recode self-disclosure
d$Self_disclosure_r <- 4-d$Self_disclosure


rmcsel<- dplyr::select(d,trust_tot,Close_tot,Warmth, Competence, Morality, 
                       conflict,inf_certainty,mut_dependence,fut_interdependence,power_sis,
                       Cooperation, Self_disclosure_r,mentalizing,Moral_self_percept,
                       happiness,satisfaction,loneliness,arousal,control,
                       subject)


myCor <- matrix(nrow = ncol(rmcsel)-1, 
                ncol = ncol(rmcsel)-1)
row.names(myCor) <- names(rmcsel[,1:ncol(myCor)])
colnames(myCor) <- row.names(myCor)
diag(myCor) <- 1
myP <- myCor

library(rmcorr)

for(i in 1:(ncol(rmcsel)-2)) {
  for(j in (i+1):(ncol(rmcsel)-1)) {
    x <- rmcorr(rmcsel[,ncol(rmcsel)], 
                rmcsel[,i], rmcsel[,j], rmcsel)
    myCor[j,i] <- round(x$r,3)
    #myCor[i,j] <- round(x$r,3)
    myP[j,i] <- round(x$p,3)
    #myP[i,j] <- round(x$p,3)
    
    print(paste(names(rmcsel)[i], "with", 
                names(rmcsel)[j], "correlates at", 
                round(x$r,3)))
  }}

myCor
myP




###############



### Results part (reported for overall dataset)
#event frequencies: number of social interactions
summary(d$start)
prop.table(table (d$start))

# participants
summary(nobs<-sum(as.numeric(d$oneline), na.rm = TRUE))

#type of social interactions
prop.table(table (d$Target))


ds <- subset(d, start == "Ja")

#pct data of social interactions (plyr)
#typeofinter <- ds %>% group_by(Target) %>% summarise(freq=n(),na.rm=TRUE) %>% arrange(desc(freq))%>% mutate(rel.freq = 100*freq/sum(freq))
         



##################################################################################################################################
######################################################   Grand average of trust and distrust  ############################################
##################################################################################################################################
#set directory


d = readRDS("Trust_final_OSF.RDS")

names (d)

#average level of trust and distrust
m0 <- lmer(trust ~ 1 + (1 | subject) + (1 | subject:TargetID), data=d) 
summary(m0)

m0 <- lmer(distrust ~ 1 + (1 | subject) + (1 | subject:TargetID), data=d) 
summary(m0)


describe(d$trust)
describe(d$distrust)                                                         

table(d$trust)
table(d$distrust)

#test against mean of scale
d$trust_against_scale_mean <- d$trust-2
d$distrust_against_scale_mean <- 2-d$distrust

summary(mdiff0 <- lmer(trust_against_scale_mean ~ 1 + (1 | subject) + (1 | subject:TargetID), data=d))
summary(mdiff0 <- lmer(distrust_against_scale_mean ~ 1 + (1 | subject) + (1 | subject:TargetID), data=d))



##################################################################################################################################
######################################################  Dimensionality of trust and distrust  ############################################
##################################################################################################################################
#set directory


d = readRDS("Trust_final_OSF.RDS")

names (d)

#trust and distrust

td <- subset(d, select = c("subject","trust","distrust"))


#multilevel model (repeated measures) correlation using package rmcorr:

td_corr <- rmcorr(subject, trust, distrust, td)
td_corr




##################################################################################################################################
######################################################  Variability in trust  ############################################
##################################################################################################################################


names (d)

#SD
describe(d$trust_tot)

m0 <- lmer(trust_tot ~ 1 + (1 | subject) + (1 | subject:TargetID), data=d) 
summary(m0)


#supplementary figure 1: within-between person variability of trust illustration
d$subject<-as.numeric(d$subject)
subset <- na.omit(subset(d, subset = subject <= 45 & Nu_sig >=5,
                 select = c("MOC", "subject", "trust_tot","Nu_sig")))
nrow(subset)

o <- order(subset$subject, subset$MOC)
subset <- subset[o, ]
head(subset)

#Figure S1
xyplot(trust_tot ~ MOC |as.factor(subject),subset,type= "b", col.line="black", layout=c(8,5,1),
       main = '',ylab="Trust",xlab="Measurement Occasion",pch=19,cex=0.75,ylim=c(-1,5)) 

d$subject<-as.factor(d$subject)

###  Variance Decomposition

#random intercept model
m0 <- lmer(trust_tot ~ 1 + (1 | subject) + (1 | subject:TargetID), data=d)
summary(m0)


fix <- fixef(m0)
rand <- ranef(m0)
paramsSubject <- cbind((rand$subject[1]+fix[1]))

colnames(paramsSubject) <- c("Person_Mean")


#plot Figure S2 (density distributions)


p <- ggplot2.histogram(data=paramsSubject, xName='Person_Mean',
                  fill="white", color="black",
                  addDensityCurve=TRUE, densityFill="#FF6666",
                  addMeanLine=TRUE, meanLineColor="dark red",
                  meanLineType="dashed", meanLineSize=1,
                  orientation="horizontal",xtitle="Trust Person Mean",ytitle="Density",xlim=c(1,4.5),ylim=c(0,2)) 

p + theme(axis.title.x = element_text(size = 10), 
          axis.title.y = element_text(size = 10),
          axis.text.x = element_text(size=8),
          axis.text.y = element_text(size=8))


#plot also within-person distribution for illustration

p <- ggplot2.histogram(data=d, xName='trust_tot_c',
                  fill="white", color="black",
                  addDensityCurve=TRUE, densityFill="orange",
                  addMeanLine=TRUE, meanLineColor="dark red",
                  meanLineType="dashed", meanLineSize=1,
                  orientation="horizontal",xtitle="Trust Person-Centered Scores",ytitle="Density",xlim=c(-3,3),ylim=c(0,2.0))


p + theme(axis.title.x = element_text(size = 10), 
          axis.title.y = element_text(size = 10),
          axis.text.x = element_text(size=8),
          axis.text.y = element_text(size=8))




#get intraclass correlation coefficient (ICC): ratio of between-person to within-person variance 

icc(m0)


##################################################################################################################################
######################################################  Predictors of fluctuation in trust  ############################################
##################################################################################################################################


#############################################################################################################################################
########################################## Surface-level characteristics Target, Duration, Language, Medium, Distance  ############################################
#############################################################################################################################################

d = readRDS("Trust_final_OSF.RDS")


fm <- lmer(trust_tot ~ Target  + (1 | subject) + (1 | subject:TargetID), data=d)
summary(fm)

###effects coding

contrasts(d$Target) <- contr.sum(13, base=11)
contrasts(d$Target) 

levels(d$Duration)
contrasts(d$Duration) <- contr.sum(8, base=8)
contrasts(d$Duration)

levels(d$Mothertongue)
contrasts(d$Mothertongue) <- contr.sum(2, base=2)
contrasts(d$Mothertongue)

levels(d$Medium)
contrasts(d$Medium) <- contr.sum(6, base=6)
contrasts(d$Medium)

levels(d$Distance)
contrasts(d$Distance) <- contr.sum(7, base=7)
contrasts(d$Distance)


#Table S3 left column

fm <- lmer(trust_tot ~ Target + Duration  + Mothertongue + Medium + Distance + (1 | subject) + (1 | subject:TargetID), data=d)
anova(fm)
summary(fm)

#Figure S3

em <- emmeans(fm, "Target")
em
x1 <- as.data.frame(em)
x1 <- x1 %>% dplyr::rename(fit = emmean, se = SE)
x1



cdashed <- c("blank","blank","blank","blank","blank","blank","blank","blank","dotted","blank","blank","blank","blank")


p <- ggplot(data=x1, aes(reorder(Target,fit), fit, fill=fit)) + geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width = .5) +
  geom_bar(stat="identity", color = "black", linetype=cdashed, size=0.5) + 
  coord_flip() +  ylab("Trust") +  xlab("Type of Target") + guides(fill=FALSE) +
  geom_hline(yintercept= 3.1151, linetype="dashed", size=0.5, color="grey" )
p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))




em <- emmeans(fm, "Duration")
em
x2 <- as.data.frame(em)
x2 <- x2 %>% dplyr::rename(fit = emmean, se = SE)
x2

cdashed <- c("blank","dotted","blank","dotted","dotted","dotted","blank","blank")


p <- ggplot(data=x2, aes(reorder(Duration,fit), fit, fill=fit)) + geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width = .5) +
  geom_bar(stat="identity", color = "black", linetype=cdashed, size=0.5) + 
  coord_flip() +  ylab("Trust") +  xlab("Duration") + guides(fill=FALSE) +
  geom_hline(yintercept= 3.1151, linetype="dashed", size=0.5, color="grey" )
p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



em <- emmeans(fm, "Mothertongue")
em
x3 <- as.data.frame(em)
x3 <- x3 %>% dplyr::rename(fit = emmean, se = SE)
x3

cdashed <- c("blank","blank")


p <- ggplot(data=x3, aes(reorder(Mothertongue,fit), fit, fill=fit)) + geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width = .5) +
  geom_bar(stat="identity", color = "black", linetype=cdashed, size=0.5) + 
  coord_flip() +  ylab("Trust") +  xlab("Language") + guides(fill=FALSE) +
  geom_hline(yintercept= 3.1151, linetype="dashed", size=0.5, color="grey" )
p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))




em <- emmeans(fm, "Distance")
em
x4 <- as.data.frame(em)
x4 <- x4 %>% dplyr::rename(fit = emmean, se = SE)
x4

cdashed <- c("dotted","dotted","dotted","dotted","dotted","blank","blank")


png(filename = "Distance.png", height = 6.00, width = 10.00, units = "cm", res = 300)
p <- ggplot(data=x4, aes(reorder(Distance,fit), fit, fill=fit)) + geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width = .5) +
  geom_bar(stat="identity", color = "black", linetype=cdashed, size=0.5) + 
  coord_flip() +  ylab("Trust") +  xlab("Distance") + guides(fill=FALSE) +
  geom_hline(yintercept= 3.1151, linetype="dashed", size=.5, color="grey" )
p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
dev.off()



#############################################################
### Trustee Perception: Warmth, Competence, Morality
#############################################################

d = readRDS("Trust_final_OSF.RDS")


#Table S3 (middle column)
fm <- lmer(trust_tot ~ Warmth_c + Competence_c + Morality_c + (1 | subject) + (1 | subject:TargetID), data=d);
summary(fm)
effectsize::standardize_parameters(fm)





#closeness index strong predictor of trust
fm <- lmer(trust_tot ~ Close_tot_c + (1 | subject) + (1 | subject:TargetID), data=d); 
summary(fm)
effectsize::standardize_parameters(fm)


###inspection of intercorrelation among SIS dimensions

SIS <-  d[,c("conflict_c", "inf_certainty_c", "mut_dependence_c","fut_interdependence_c", "power_sis_c")]
colnames(SIS) <- c("Conflict","Information Certainty","Mutual Dependence","Future Interdependence","Power")


#pairs plots (corr and distribution among vars)
# Create function to make histogram with density superimposed
panel.hist <- function(x, ...) {
  # Set user coordinates of plotting region
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  par(new=TRUE)
  # Do not start new plot
  hist(x, prob=TRUE, axes=FALSE, xlab="", ylab="",
       main="", col="royalblue1")
  lines(density(x, na.rm=TRUE))
  # Add density curve
}

panel.l <- function(x, y, ...) {
  points(x,y, pch=20,col="lightgrey",lwd=0.5)
  abline(lm(y~x, na.rm=TRUE),col="black", lty=c(1), lwd=c(2)) 
}

# Create function to compute and print r

panel.r <- function(x, y, digits=2, cex.cor, ...) {
  # Set user coordinates of plotting region
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use="complete.obs") # Compute r
  txt <- format(c(r, 0.123456789), digits=2)[1]
  if(missing(cex.cor)) cex.cor <- 1/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1.5+(0.5+(r/8)))
}

#Figure S4

pairs(SIS,
      lower.panel = panel.l, upper.panel=panel.r,
      diag.panel=panel.hist, xlim=c(-3,3), pch=20, ylim=c(-3.5,3.5), cex.axis=0.8, col = "lightgrey", ellipses=TRUE)




### ### Table S3: All relational Aspects (right column)

fm <- lmer(trust_tot ~ mut_dependence_c + power_sis_c  + I(power_sis_c^2) + conflict_c 
           + fut_interdependence_c + inf_certainty_c + Close_tot_c 
           + (1 | subject) + (1 | subject:TargetID), data=d)
anova(fm)
summary(fm)
effectsize::standardize_parameters(fm)

#Power supplementary figure
ef1 <- effect(term="power_sis_c", xlevels= list(power_sis_c=100), mod=fm)
efdata1<-as.data.frame(ef1) #convert the effects list to a data frame
efdata1

png(filename = "power_q.png", height = 7.27, width = 9.68, units = "cm", res = 300)
p <- ggplot(efdata1, aes(x=power_sis_c, y=fit)) + geom_line(size=1, color = "darkred") +
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill = "darkred"),alpha=0.3,show.legend =FALSE) + 
  labs( x= "Power", y="Trust") +   scale_y_continuous(breaks = c(3.0,3.25,3.5,3.75)) +
  theme_classic() + theme(text=element_text(size=15)) + coord_cartesian(ylim = c(3.0,3.75),xlim = c(-2.5,2.5))
p + theme(plot.title = element_blank(), legend.position=c(0.9,0.85), text=element_text(size=12))
dev.off()




### MODEL 4: including all deep level factors to accunt for surface level effects
###effects coding

d = readRDS("Trust_final_OSF.RDS")

contrasts(d$Target) <- contr.sum(13, base=11)
contrasts(d$Target) 

levels(d$Duration)
contrasts(d$Duration) <- contr.sum(8, base=8)
contrasts(d$Duration)

levels(d$Mothertongue)
contrasts(d$Mothertongue) <- contr.sum(2, base=2)
contrasts(d$Mothertongue)

levels(d$Medium)
contrasts(d$Medium) <- contr.sum(6, base=6)
contrasts(d$Medium)

levels(d$Distance)
contrasts(d$Distance) <- contr.sum(7, base=7)
contrasts(d$Distance)

#Table 1 (Model 1)

M1 <- lmer(trust_tot ~ Target + Duration  + Mothertongue + Medium + Distance + Close_tot_c + Warmth_c + Competence_c + Morality_c 
           + mut_dependence_c + power_sis_c + I(power_sis_c^2) + conflict_c + fut_interdependence_c + inf_certainty_c 
           + (1 | subject) + (1 | subject:TargetID), data=d)
anova(M1)
summary(M1)
effectsize::standardize_parameters(M1)


##############################################################################################################################
######################################################   Interactive Effects of conflict and other SIS dimensions  ############################################
##############################################################################################################################


d = readRDS("Trust_final_OSF.RDS")


SIS <-  d[,c("subject", "TargetID","trust_tot", "mut_dependence_c", "power_sis_c","conflict_c","fut_interdependence_c","inf_certainty_c","Cooperation_c","Competition_c","trust_tot_c","Cooperation","Competition")]
colnames(SIS) <- c("subject", "TargetID","Trust", "Mutual_Dependence","Power","Conflict","Future_Interd","Inf_Certainty","Cooperation_c","Competition_c","Trust_c","Cooperation","Competition")



### Table S4
options(scipen = 999)
fm <- lmer(Trust ~ Conflict + Inf_Certainty +  Mutual_Dependence + Future_Interd + Power + I(Power^2)   
           + Inf_Certainty:Conflict +  Mutual_Dependence:Conflict + Future_Interd:Conflict + Power:Conflict + I(Power^2):Conflict 
            + (1 | subject) + (1 | subject:TargetID), data=SIS)
summary(fm)
effectsize::standardize_parameters(fm)


#plot IA with ggplot
describe(SIS$Conflict)

###Figure 2 --  interaction with conflict, 4 panels (A-D)

#Informaion Certainty


#Figure 1 -- 4panels
#A inf certainty
ef1 <- effect(term="Conflict:Inf_Certainty", xlevels= list(Conflict=c(-.98, .98),Inf_Certainty = 100), mod=fm)
efdata1<-as.data.frame(ef1) #convert the effects list to a data frame
efdata1

efdata1$Conflict<-as.factor(efdata1$Conflict)


p <- ggplot(efdata1, aes(x=Inf_Certainty, y=fit,color=Conflict,group=Conflict))  + geom_line(size=1) +
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=Conflict),alpha=0.3,show.legend=FALSE) + xlim(-2.5,2.5) + ylim(2.5,4) +
  labs(title = "", x= "Information Certainty", y="Trust", color="Conflict", fill="Conflict") + scale_color_hue(labels = c("-1 SD", "+1 SD")) + 
  theme_classic() + theme(text=element_text(size=15))  
p + theme(plot.title = element_blank(), legend.position=c(0.85,0.28), text=element_text(size=12))+
  scale_color_manual(values = c("orange","dark red"),labels = c("-1 SD", "+1 SD")) + 
  scale_fill_manual(values = c("orange","dark red"))

#B mutual dependence

ef1 <- effect(term="Conflict:Mutual_Dependence", xlevels= list(Conflict=c(-.98, .98),Mutual_Dependence=20), mod=fm)
efdata1<-as.data.frame(ef1) #convert the effects list to a data frame
#ef1 <- allEffects(fm,xlevels= list(Conflict=c(-.98, .98),Mutual_Dependence=20))
#efdata1<- as.data.frame(ef1[[4]]) #convert the effects list to a data frame
efdata1

efdata1$Conflict<-as.factor(efdata1$Conflict)


p <- ggplot(efdata1, aes(x=Mutual_Dependence, y=fit,color=Conflict,group=Conflict)) + geom_line(size=1) +
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=Conflict),alpha=0.3,show.legend=FALSE) + xlim(-2.5,2.5) + ylim(2.5,4) +
  labs(title = "", x= "Mutual Dependence", y="Trust", color="Conflict", fill="Conflict") + scale_color_hue(labels = c("-1 SD", "+1 SD")) +
  theme_classic() + theme(text=element_text(size=15)) 
p + theme(plot.title = element_blank(), legend.position=c(0.85,0.88), text=element_text(size=12)) +
  scale_color_manual(values = c("orange","dark red"),labels = c("-1 SD", "+1 SD")) + 
  scale_fill_manual(values = c("orange","dark red"))


#C Future Interdependence
ef1 <- effect(term="Conflict:Future_Interd", xlevels= list(Conflict=c(-.98, .98),Future_Interd=20), mod=fm)
efdata1<-as.data.frame(ef1) #convert the effects list to a data frame
efdata1

efdata1$Conflict<-as.factor(efdata1$Conflict)


p <- ggplot(efdata1, aes(x=Future_Interd, y=fit,color=Conflict,group=Conflict)) + geom_line(size=1) +
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=Conflict),alpha=0.3,show.legend=FALSE) + xlim(-2.5,2.5) + ylim(2.5,4) +
  labs(title = "", x= "Future Interdependence", y="Trust", color="Conflict", fill="Conflict") + scale_color_hue(labels = c("-1 SD", "+1 SD")) +
  theme_classic() + theme(text=element_text(size=15))  
p + theme(plot.title = element_blank(), legend.position=c(0.85,0.88), text=element_text(size=12)) +
  scale_color_manual(values = c("orange","dark red"),labels = c("-1 SD", "+1 SD")) + 
  scale_fill_manual(values = c("orange","dark red"))




#D Power (quadratic)

ef2 <- allEffects(fm,xlevels= list(Conflict=c(-.98, .98),Power=100))
efdata2 <- as.data.frame(ef2[[5]])
efdata2
efdata2$Conflict<-as.factor(efdata2$Conflict)


p <- ggplot(efdata2, aes(x=Power, y=fit,color=Conflict,group=Conflict)) + geom_line(size=1) +
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=Conflict),alpha=0.3,show.legend=FALSE) + 
  labs(title = "", x= "Relative Power", y="Trust", color="Conflict", fill="Conflict") + scale_color_hue(labels = c("-1 SD", "+1 SD")) +
  theme_classic() + theme(text=element_text(size=15)) + coord_cartesian(ylim = c(2.5,4),xlim = c(-2.5,2.5))
p + theme(plot.title = element_blank(), legend.position=c(0.85,0.88), text=element_text(size=12)) +
  scale_color_manual(values = c("orange","dark red"),labels = c("-1 SD", "+1 SD")) + 
  scale_fill_manual(values = c("orange","dark red"))



##################################################################################################################################
######################################################  Model 2 Dispositional predictors of trust  ############################################
##################################################################################################################################

d = readRDS("Trust_final_OSF.RDS")

#compute within-participant standard deviation of trust_tot

tsd <- d %>% 
  group_by(subject) %>% 
  dplyr::summarise(trust_tot_sd=sd(trust_tot,na.rm = T))
tsd

#match
d <- merge(d, tsd, by="subject")

#select variables
Model2 <- d[,c("subject","trust_tot","oneline","gender", "age", "Political_Orientation", "Reli_intrins_M",
                "TrustScale","Distrust_M",
               "moral_id","ZSB_M","SVOAngle","trust_tot_sd","Gender_e","TargetID")]


colnames(Model2) <- c("subject","trust_tot","oneline","Gender","Age","Political_Orientation", "Religiosity",
                      "TrustScale","Distrust",
                      "Moral_Identity","Zero_Sum_Belief",
                      "SVO","Trust_sd","Gender_e","TargetID")


Disponeline <- subset(Model2, oneline == 1,select=Gender:Trust_sd)

hist(x=Disponeline$SVO, main="",xlab="SVO",col="skyblue", border="white")

describe(d$trust_tot)

describe(Disponeline$Trust_sd)


###prepare Gender
levels(Disponeline$Gender)

summary(Disponeline$Gender)

Disponeline$Gender[Disponeline$Gender=="No response"] <- NA
Disponeline$Gender[Disponeline$Gender=="other"] <- NA
summary(Disponeline$Gender)

#convert gendert to numeric using effects coding?
Disponeline$Gender <- as.numeric(Disponeline$Gender)
Disponeline$Gender[Disponeline$Gender==1] <- 0
Disponeline$Gender[Disponeline$Gender==2] <- 1
summary(Disponeline$Gender)

#get n for all the measures.
describe(Disponeline)

### Table S5: means and correlation table among all traits including trust variability measure
apa.cor.table(Disponeline, show.conf.interval = FALSE)


#grand mean centering 
Model2$Age_c = Model2$Age - mean(Model2$Age[Model2$oneline == 1],na.rm=TRUE)
Model2$Political_Orientation_c = Model2$Political_Orientation - mean(Model2$Political_Orientation[Model2$oneline == 1],na.rm=TRUE)
Model2$Religiosity_c=Model2$Religiosity - mean(Model2$Religiosity[Model2$oneline == 1],na.rm=TRUE)
Model2$TrustScale_c=Model2$TrustScale - mean(Model2$TrustScale[Model2$oneline == 1],na.rm=TRUE)
Model2$Distrust_c=Model2$Distrust - mean(Model2$Distrust[Model2$oneline == 1],na.rm=TRUE)
Model2$Moral_Identity_c=Model2$Moral_Identity - mean(Model2$Moral_Identity[Model2$oneline == 1],na.rm=TRUE)
Model2$Zero_Sum_Belief_c=Model2$Zero_Sum_Belief - mean(Model2$Zero_Sum_Belief[Model2$oneline == 1],na.rm=TRUE)
Model2$SVO_c=Model2$SVO - mean(Model2$SVO[Model2$oneline == 1],na.rm=TRUE)

test <- subset(Model2, oneline == 1,select=Age_c:SVO_c)
describe(test)



### Table 1 (Model 2 - dispositional)

M2 <- lmer(trust_tot ~ 1 + Gender_e + Age_c + Political_Orientation_c + Religiosity_c +
             TrustScale_c + Distrust_c +
             Moral_Identity_c + Zero_Sum_Belief_c + SVO_c
           + (1 | subject) + (1 | subject:TargetID), data=Model2)
summary(M2)
effectsize::standardize_parameters(M2)



##################################################################################################################################
######################################################  Model 3 - Final Joint Model  ############################################
##################################################################################################################################


d = readRDS("Trust_final_OSF.RDS")

#compute within-participant standard deviation of trust_tot

tsd <- d %>% 
  group_by(subject) %>% 
  dplyr::summarise(trust_tot_sd=sd(trust_tot,na.rm = T))
tsd


#match data frames
d <- merge(d, tsd, by="subject")

#effects coding
contrasts(d$Target) <- contr.sum(13, base=11)
contrasts(d$Target) 

levels(d$Duration)
contrasts(d$Duration) <- contr.sum(8, base=8)
contrasts(d$Duration)

levels(d$Mothertongue)
contrasts(d$Mothertongue) <- contr.sum(2, base=2)
contrasts(d$Mothertongue)

levels(d$Medium)
contrasts(d$Medium) <- contr.sum(6, base=6)
contrasts(d$Medium)

levels(d$Distance)
contrasts(d$Distance) <- contr.sum(7, base=7)
contrasts(d$Distance)


Model3 <- d[,c("subject","trust_tot","oneline",
               "gender", "age", "Political_Orientation", "Reli_intrins_M",
               "TrustScale", "Distrust_M",
               "moral_id","ZSB_M","SVOAngle","Gender_e",
               "Target" , "Duration"  , "Mothertongue" , "Medium" , "Distance",
               "Close_tot_c" , "Warmth_c" , "Competence_c" , "Morality_c", 
               "mut_dependence_c" , "power_sis_c" , "conflict_c" , "fut_interdependence_c" , "inf_certainty_c", "trust_tot_sd","TargetID","TrustGameAk","TrustGameAs")]

colnames(Model3) <- c("subject","trust_tot","oneline",
                      "Gender","Age","Political_Orientation", "Religiosity",
                      "Trust_Scale","Distrust",
                      "Moral_Identity", "Zero_Sum_Belief","SVO","Gender_e",
                      "Target" , "Duration"  , "Mothertongue" , "Medium" , "Distance",
                      "Close_tot_c" , "Warmth_c" , "Competence_c" , "Morality_c", 
                      "mut_dependence_c" , "power_sis_c" , "conflict_c" , "fut_interdependence_c" , "inf_certainty_c", "Trust_sd","TargetID","TrustGameAk","TrustGameAs")


#grand mean centering using oneline
Model3$Age_c = Model3$Age - mean(Model3$Age[Model3$oneline == 1],na.rm=TRUE)
Model3$Political_Orientation_c = Model3$Political_Orientation - mean(Model3$Political_Orientation[Model3$oneline == 1],na.rm=TRUE)
Model3$Religiosity_c=Model3$Religiosity - mean(Model3$Religiosity[Model3$oneline == 1],na.rm=TRUE)
Model3$Trust_Scale_c=Model3$Trust_Scale - mean(Model3$Trust_Scale[Model3$oneline == 1],na.rm=TRUE)
Model3$Distrust_c=Model3$Distrust - mean(Model3$Distrust[Model3$oneline == 1],na.rm=TRUE)
Model3$Moral_Identity_c=Model3$Moral_Identity - mean(Model3$Moral_Identity[Model3$oneline == 1],na.rm=TRUE)
Model3$Zero_Sum_Belief_c=Model3$Zero_Sum_Belief - mean(Model3$Zero_Sum_Belief[Model3$oneline == 1],na.rm=TRUE)
Model3$SVO_c=Model3$SVO - mean(Model3$SVO[Model3$oneline == 1],na.rm=TRUE)

describe (subset(Model3, oneline == 1,select=Age_c:SVO_c))


### Table 1 - final joint model Model 3


M3 <- lmer(trust_tot ~ 1 + Target + Duration  + Mothertongue + Medium + Distance
           + Close_tot_c + Warmth_c + Competence_c + Morality_c 
           + mut_dependence_c + power_sis_c + I(power_sis_c^2) + conflict_c + fut_interdependence_c + inf_certainty_c
           + Gender_e + Age_c + Political_Orientation_c + Religiosity_c 
           + Trust_Scale_c + Distrust_c 
           + Moral_Identity_c + Zero_Sum_Belief_c + SVO_c
           + (1 | subject) + (1 | subject:TargetID), data=Model3)
anova(M3)
summary(M3)
effectsize::standardize_parameters(M3)

saveRDS(Model3, file="Model3.RDS")


##################################################################################################################################
########### Social Scope Analysis: Trust-Related Traits on ED Trust as function of closeness  ############################################
##################################################################################################################################


# Table S7: adding IAs to final model
M3ia <- lmer(trust_tot ~ 1 + Target + Duration  + Mothertongue + Medium + Distance
           + Close_tot_c + Warmth_c + Competence_c + Morality_c 
           + mut_dependence_c + power_sis_c + I(power_sis_c^2)+ conflict_c + fut_interdependence_c + inf_certainty_c
           + Gender_e + Age_c + Political_Orientation_c + Religiosity_c 
           + Trust_Scale_c + Distrust_c 
           + Moral_Identity_c + Zero_Sum_Belief_c + SVO_c
           + Trust_Scale_c:Close_tot_c + Distrust_c:Close_tot_c 
           + Moral_Identity_c:Close_tot_c + Zero_Sum_Belief_c:Close_tot_c + SVO_c:Close_tot_c
           + (1 | subject) + (1 | subject:TargetID), data=Model3)
anova(M3ia)
summary(M3ia)
effectsize::standardize_parameters(M3ia)


### Figure 3: illustrating the significant social scope interactions (Panels A - C)


##Save SDs of Traits
#check numers or compute via dplyr!
describe (subset(Model3, oneline == 1,select=Age_c:SVO_c))

#get SDs
SDs <- Model3 %>% 
  filter(oneline == 1) %>% 
  summarise (SD_Trust = SD(Trust_Scale_c),
             SD_Distrust = SD(Distrust_c),
             SD_Moral = SD(Moral_Identity_c),
             SD_ZSB = SD(Zero_Sum_Belief_c),
             SD_SVO = SD(SVO_c)) 
SDs <- as.list(SDs)

SD_Close <- d %>% summarise(SD_Close = SD(Close_tot_c))
SD_Close <- as.list(SD_Close)
SD_Close <- SD_Close$SD_Close



# Panel A: General Distrust

SD_x <- SDs$SD_Distrust

eft1 <- effect(term="Close_tot_c:Distrust_c", xlevels= list(Close_tot_c=c(-1*(SD_Close),0, 1*(SD_Close)),Distrust_c=c(-2*(SD_x),-1*(SD_x),0, 1*(SD_x),2*(SD_x))), mod=M3ia)
efdata1<-as.data.frame(eft1) #convert the effects list to a data frame
efdata1

efdata1$Close_tot_c<-as.factor(efdata1$Close_tot_c)

breaks.minor=c(-1*(SD_x),0, 1*(SD_x))
colors1=c("black","lightsalmon4","lightsalmon1")

p <- ggplot(efdata1, aes(x=Distrust_c, y=fit,color=Close_tot_c,group=Close_tot_c)) + geom_line(size=1) +
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=Close_tot_c),alpha=0.7,show.legend=FALSE) + xlim(-1.6,1.6) + ylim(2.99,4) +
  labs( x= "General Distrust", y="Trust", color="Closeness")  +
  theme_classic() + theme(text=element_text(size=12))  
p + theme(plot.title = element_blank(), legend.position=c(0.8,0.88),legend.margin = margin(6, 6, 6, 6), legend.text = element_text(size=10)) +
  scale_color_manual(values = colors1,labels = c("-1 SD","mean", "+1 SD")) + 
  scale_fill_manual(values = colors1) + 
  scale_x_continuous(breaks=breaks.minor,labels=c("-1 SD","mean","+1 SD"))



# Panel B: Social Value Orientation

SD_x <- SDs$SD_SVO

eft2 <- effect(term="Close_tot_c:SVO_c", xlevels= list(Close_tot_c=c(-1*(SD_Close),0, 1*(SD_Close)),SVO_c=c(-2*(SD_x),-1*(SD_x),0, 1*(SD_x),2*(SD_x))), mod=M3ia)
efdata2<-as.data.frame(eft2) #convert the effects list to a data frame
efdata2

efdata2$Close_tot_c<-as.factor(efdata2$Close_tot_c)

breaks.minor=c(-1*(SD_x),0, 1*(SD_x))



p <- ggplot(efdata2, aes(x=SVO_c, y=fit,color=Close_tot_c,group=Close_tot_c)) + geom_line(size=1) + 
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=Close_tot_c),alpha=0.7,show.legend=FALSE) + xlim(-1.6,1.6) + ylim(2.95,4) +
  labs( x= "Social Value Orientation", y="Trust", color="Closeness")  +
  theme_classic() + theme(text=element_text(size=12))  
p + theme(plot.title = element_blank(), legend.position=c(0.8,0.88),legend.margin = margin(6, 6, 6, 6), legend.text = element_text(size=10)) +
  scale_color_manual(values = colors1,labels = c("-1 SD","mean", "+1 SD")) + 
  scale_fill_manual(values = colors1) + 
  scale_x_continuous(breaks=breaks.minor,labels=c("-1 SD","mean","+1 SD"))



# Panel C: Moral Identity

SD_x <- SDs$SD_Moral

eft3 <- effect(term="Close_tot_c:Moral_Identity_c", xlevels= list(Close_tot_c=c(-1*(SD_Close),0, 1*(SD_Close)),Moral_Identity_c=c(-2*(SD_x),-1*(SD_x),0, 1*(SD_x),2*(SD_x))), mod=M3ia)
efdata3<-as.data.frame(eft3) #convert the effects list to a data frame
efdata3

efdata3$Close_tot_c<-as.factor(efdata1$Close_tot_c)

breaks.minor=c(-1*(SD_x),0, 1*(SD_x))



p <- ggplot(efdata3, aes(x=Moral_Identity_c, y=fit,color=Close_tot_c,group=Close_tot_c)) + geom_line(size=1) +
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=Close_tot_c),alpha=0.7,show.legend=FALSE) + xlim(-1.6,1.6) + ylim(2.98,4) +
  labs( x= "Moral Identity", y="Trust", color="Closeness")  +
  theme_classic() + theme(text=element_text(size=12))  
p + theme(plot.title = element_blank(), legend.position=c(0.8,0.88),legend.margin = margin(6, 6, 6, 6), legend.text = element_text(size=10)) +
  scale_color_manual(values = colors1,labels = c("-1 SD","mean", "+1 SD")) + 
  scale_fill_manual(values = colors1) + 
  scale_x_continuous(breaks=breaks.minor,labels=c("-1 SD","mean","+1 SD"))




#######################################################
#### Lab and Field: Trust Game
#######################################################


demo <- subset(d, oneline == 1) 

summary (demo$TrustGameA)

#percentage of keepers and senders
nt <- sum(as.numeric(demo$TrustGameA[demo$TrustGameA == 1]))
table(demo$TrustGameA)
prop.table(table(demo$TrustGameA))


#main effect trust game (+ intercept denotes mean level of everyday trust in keepers)
table(d$TrustGameAk)
table(d$TrustGameAs)

M_TG <- lmer(trust_tot ~ 1 + Target + Duration  + Mothertongue + Medium + Distance
             + Close_tot_c + Warmth_c + Competence_c + Morality_c 
             + mut_dependence_c + power_sis_c + I(power_sis_c^2)+ conflict_c + fut_interdependence_c + inf_certainty_c
             + TrustGameAk 
             + (1 | subject) + (1 | subject:TargetID), data=Model3)
anova(M_TG)
summary(M_TG)
effectsize::standardize_parameters(M_TG)

# other mean (intercept for senders, via  reversed coding)
M_TG2 <- lmer(trust_tot ~ 1 + Target + Duration  + Mothertongue + Medium + Distance
             + Close_tot_c + Warmth_c + Competence_c + Morality_c 
             + mut_dependence_c + power_sis_c + I(power_sis_c^2)+ conflict_c + fut_interdependence_c + inf_certainty_c
             + TrustGameAs 
             + (1 | subject) + (1 | subject:TargetID), data=Model3)
anova(M_TG2)
summary(M_TG2)



#adding Trust Game interaction to Model 1 as single dispositional predictor (note: limited sample of Player A)

M_TG_IA <- lmer(trust_tot ~ 1 + Target + Duration  + Mothertongue + Medium + Distance
              + Warmth_c + Competence_c + Morality_c 
              + mut_dependence_c + power_sis_c + I(power_sis_c^2)+ conflict_c + fut_interdependence_c + inf_certainty_c
              + TrustGameAk*Close_tot_c
              + (1 | subject) + (1 | subject:TargetID), data=Model3)
anova(M_TG_IA)
summary(M_TG_IA)
effectsize::standardize_parameters(M_TG_IA)


###visualizing the trust game effect

eflf <- effect(term="TrustGameAk*Close_tot_c", xlevels= list(TrustGameAk=c(0,1)), mod=M_TG_IA)
efdata1<-as.data.frame(eflf) #convert the effects list to a data frame
efdata1


efdata1$TrustGameAk<-as.factor(efdata1$TrustGameAk)
xf3 <- as.data.frame(efdata1)


#category means on person centered closeness for graph

fmc <- lmer(Close_tot_c ~ Target + (1 | subject) + (1 | subject:TargetID), data=d)
anova(fmc)
effectsize::standardize_parameters(fmc)
summary(fmc)


efc <- effect("Target", fmc)
summary(efc)
xc <- as.data.frame(efc)

#create vectors
xc <- xc[order(xc$fit),] 
breaks.minor <- c(xc$fit)
labels.minor <- c(as.character(xc$Target))


#floodlight analysis: manually estimate at certain levels of close_tot_c to identify region of sig

Model3$Close_tot_cv <- Model3$Close_tot_c+0.776

M_ROS <- lmer(trust_tot ~ 1 + TrustGameAk 
             +  Target + Duration  + Mothertongue + Medium + Distance
             + Close_tot_cv + Warmth_c + Competence_c + Morality_c 
             + mut_dependence_c + power_sis_c + I(power_sis_c^2)+ conflict_c + fut_interdependence_c + inf_certainty_c
             + TrustGameAk*Close_tot_cv
             + (1 | subject) + (1 | subject:TargetID), data=Model3)
summary(M_ROS)


###-> identified value for graph is:
ros <- -.776

#plot Figure 4

p <- ggplot(xf3, aes(x=Close_tot_c, y=fit,color=TrustGameAk,group=TrustGameAk)) + geom_line(size=1) +
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=TrustGameAk),alpha=0.3,show.legend=FALSE) +
  labs(title = "", x= "Closeness Index", y="Everyday Trust Level (ESM)", color="Trust Game Decision", fill="Trust Game Decision") + scale_color_hue(labels = c("Keep", "Send")) + 
  theme_classic() + theme(text=element_text(size=15)) + coord_cartesian(ylim = c(1.9,4.5),xlim = c(-2.5,1.5))+ 
  geom_vline(xintercept= ros, linetype="dotted") + annotate("rect", xmin = -Inf, xmax = ros, ymin = -Inf, ymax = Inf,
                                                            alpha = .1) + annotate("text", x = -1.8, y = 3.45, label = "Region of Significance", size=3.5) +
  scale_x_continuous(breaks=c(-2.5,-2,-1.5,-1,-0.5,0,0.5,1, 1.5))


p + theme(plot.title = element_blank(),legend.position=c(0.75,0.90),legend.background = element_rect(fill="transparent",size=0.5), text=element_text(size=18)) + 
  annotate(geom = "segment", x=c(breaks.minor), xend = c(breaks.minor), y=1.8, 
           yend=c(1.88, 1.88,1.88, 1.88, 1.88, 1.88,1.88, 1.88, 1.88, 1.88, 1.88, 1.88, 1.88)) +
  annotate("text", x = c(breaks.minor), y = c(1.9, 1.9,1.9, 1.9,1.9, 1.9,1.9, 1.9,1.9, 1.9, 2.4, 1.9,1.9),
           label = c(labels.minor), size=3.2, angle=90, hjust=0.0, vjust=c(-0.0, -0.0, -0.0, -0.2, 0.8, 0.8, 0.8, -0.0, -0.0, -0.0, 0.8, -0.2, 0.8)) +
  annotate(geom = "segment", x=xc$fit[xc$Target=="Parent(s)"], xend = xc$fit[xc$Target=="Parent(s)"], y=2.30, yend = 2.38) + 
  scale_color_manual(values = c("orange","dark red"),labels = c("Keep", "Send")) + 
  scale_fill_manual(values = c("orange","dark red"))





#########################################################################################################################################
######################################################  Testing for Selection Effects: Trust and Frequency of reporting social interaction ############################################
#########################################################################################################################################

d = readRDS("Trust_final_OSF.RDS")

#compute within-participant number of social interactions

#compute number of interactions.
levels(d$start)

d$interact <- as.numeric(d$start)
d$interact[d$start=="Ja"] <- 1
d$interact[d$start=="Nein"] <- 0

describe(d$interact)


nsi <- d %>% 
  group_by(subject) %>% 
  dplyr::summarise(n_interact = sum(interact, na.rm = T))
nsi

#match
d <- merge(d, nsi, by="subject")


#select variables
Model_RSI <- d[,c("subject","trust_tot","oneline","gender", "age", "Political_Orientation", "Reli_intrins_M",
                "TrustScale","Distrust_M",
                "moral_id","ZSB_M","SVOAngle","Gender_e","n_interact", "interact","Nu_sig","TargetID")]


colnames(Model_RSI) <- c("subject","trust_tot","oneline",
                       "Gender","Age","Political_Orientation", "Religiosity",
                       "Trust_Scale","Distrust",
                       "Moral_Identity","Zero_Sum_Belief",
                       "SVO","Gender_e","Number_of_Social_Interactions","Likelihood_of_social_interaction","Nu_sig","TargetID")


Disponeline <- subset(Model_RSI, oneline == 1,select=Gender:Number_of_Social_Interactions)

hist(x=Disponeline$Number_of_Social_Interactions, main="",xlab="Number of Social Interactions",col="skyblue", border="white")


#grand mean centering 
Model_RSI$Age_c = Model_RSI$Age - mean(Model_RSI$Age[Model_RSI$oneline == 1],na.rm=TRUE)
Model_RSI$Political_Orientation_c = Model_RSI$Political_Orientation - mean(Model_RSI$Political_Orientation[Model_RSI$oneline == 1],na.rm=TRUE)
Model_RSI$Religiosity_c=Model_RSI$Religiosity - mean(Model_RSI$Religiosity[Model_RSI$oneline == 1],na.rm=TRUE)
Model_RSI$Trust_Scale_c=Model_RSI$Trust_Scale - mean(Model_RSI$Trust_Scale[Model_RSI$oneline == 1],na.rm=TRUE)
Model_RSI$Distrust_c=Model_RSI$Distrust - mean(Model_RSI$Distrust[Model_RSI$oneline == 1],na.rm=TRUE)
Model_RSI$Moral_Identity_c=Model_RSI$Moral_Identity - mean(Model_RSI$Moral_Identity[Model_RSI$oneline == 1],na.rm=TRUE)
Model_RSI$Zero_Sum_Belief_c=Model_RSI$Zero_Sum_Belief - mean(Model_RSI$Zero_Sum_Belief[Model_RSI$oneline == 1],na.rm=TRUE)
Model_RSI$SVO_c=Model_RSI$SVO - mean(Model_RSI$SVO[Model_RSI$oneline == 1],na.rm=TRUE)

test <- subset(Model_RSI, oneline == 1,select=Age_c:SVO_c)
describe(test)


###Poisson regression

Disponeline2 <- subset(Model_RSI, oneline == 1)
describe(Disponeline2)

class(Disponeline2$Number_of_Social_Interactions)
hist(Disponeline2$Number_of_Social_Interactions)
describe(Disponeline2$Number_of_Social_Interactions)

###Poisson model (with log link), controlling for response rate

# Table S6 (left column)
summary(Poisson_RSI <- glm(Number_of_Social_Interactions ~ 1 + Gender_e + Age_c + Political_Orientation_c + Religiosity_c +
                           Trust_Scale_c + Distrust_c + Moral_Identity_c + Zero_Sum_Belief_c + SVO_c + Nu_sig, family="poisson", data = Disponeline2))


################################################################################################################################
######################################################  Testing for Selection Effects: Trust and Likelihood of having interacted with strangers ############################################
################################################################################################################################

d = readRDS("Trust_final_OSF.RDS")

#compute number of interactions.
levels(d$start)

d$interacts <- as.numeric(d$start)

d$interacts[d$Target != "Stranger"] <- 0
d$interacts[d$Target == "Stranger"] <- 1
d$interacts[d$start == "Nein"] <- 0

table(d$interacts)

describe(d$interacts)

nsa <- d %>% 
  group_by(subject) %>% 
  dplyr::summarise(n_interact=sum(interacts,na.rm = T))


#match
d <- merge(d, nsa, by="subject")

d$n_interact <- as.numeric(d$n_interact)

class(d$n_interact)
hist(d$n_interact[d$oneline == 1])



#select variables

Model_STR <- d[,c("subject","trust_tot","oneline","gender", "age", "Political_Orientation", "Reli_intrins_M",
                "TrustScale","Distrust_M",
                "moral_id","ZSB_M","SVOAngle","Gender_e","interacts","start","n_interact","Nu_sig","TargetID")]


colnames(Model_STR) <- c("subject","trust_tot","oneline",
                       "Gender","Age","Political_Orientation", "Religiosity",
                       "Trust_Scale","Distrust",
                       "Moral_Identity", "Zero_Sum_Belief",
                       "SVO","Gender_e","Likelihood_of_stranger_interaction","start","Number_of_stranger_Interactions","Nu_sig","TargetID")


Model_STR <- subset(Model_STR, start == "Ja", select=subject:Nu_sig)

table(Model_STR$Likelihood_of_stranger_interaction)
table(Model_STR$Number_of_stranger_Interactions)

Disponeline <- subset(Model_STR, oneline == 1,select=Gender:Number_of_stranger_Interactions)

hist(x=Disponeline$Number_of_stranger_Interactions, main="",xlab="Number of Stranger Interactions",col="skyblue", border="white")


#grand mean centering 
Model_STR$Age_c = Model_STR$Age - mean(Model_STR$Age[Model_STR$oneline == 1],na.rm=TRUE)
Model_STR$Political_Orientation_c = Model_STR$Political_Orientation - mean(Model_STR$Political_Orientation[Model_STR$oneline == 1],na.rm=TRUE)
Model_STR$Religiosity_c=Model_STR$Religiosity - mean(Model_STR$Religiosity[Model_STR$oneline == 1],na.rm=TRUE)
Model_STR$Trust_Scale_c=Model_STR$Trust_Scale - mean(Model_STR$Trust_Scale[Model_STR$oneline == 1],na.rm=TRUE)
Model_STR$Distrust_c=Model_STR$Distrust - mean(Model_STR$Distrust[Model_STR$oneline == 1],na.rm=TRUE)
Model_STR$Moral_Identity_c=Model_STR$Moral_Identity - mean(Model_STR$Moral_Identity[Model_STR$oneline == 1],na.rm=TRUE)
Model_STR$Zero_Sum_Belief_c=Model_STR$Zero_Sum_Belief - mean(Model_STR$Zero_Sum_Belief[Model_STR$oneline == 1],na.rm=TRUE)
Model_STR$SVO_c=Model_STR$SVO - mean(Model_STR$SVO[Model_STR$oneline == 1],na.rm=TRUE)

test <- subset(Model_STR, oneline == 1,select=Age_c:SVO_c)
describe(test)

###Poisson regression on Disponeline

Disponeline3 <- subset(Model_STR, oneline == 1)
describe(Disponeline3)

### Table S6 (right column): zeroinflated negative binomial model (with log link), controlling for number of responses. 
summary(Poisson_STR <- zeroinfl(Number_of_stranger_Interactions ~ 1 + Gender_e + Age_c + Political_Orientation_c + Religiosity_c +
                                Trust_Scale_c + Distrust_c + Moral_Identity_c + Zero_Sum_Belief_c + SVO_c + Nu_sig |1,dist="poisson", data = Disponeline3))




##################################################################################################################################
###################################################### Motivational and behavioral implications of trust and distrust  ############################################
##################################################################################################################################


d = readRDS("Trust_final_OSF.RDS")

#### cooperation as outcome
fm <- lmer(Cooperation ~ trust_tot_c  + 
             (1 | subject) + (1 | subject:TargetID), data=d); summary(fm)
effectsize::standardize_parameters(fm)


fm <- lmer(Cooperation ~ trust_tot_c + mut_dependence_c + power_sis_c + conflict_c + fut_interdependence_c + inf_certainty_c 
           + (1 | subject) + (1 | subject:TargetID), data=d)
summary(fm)
effectsize::standardize_parameters(fm)

### Figure S6 Panels A and B
### relation between trust and cooperaton for levels of conflict

fm <- lmer(Cooperation ~ trust_tot_c + mut_dependence_c + power_sis_c + conflict_c + fut_interdependence_c + inf_certainty_c + 
trust_tot_c*conflict_c + (1 | subject) + (1 | subject:TargetID), data=d); summary(fm)
effectsize::standardize_parameters(fm)

### Figure S6 (Panel A): illustration of trust x conflict interaction on cooperation

describe(d$conflict_c)

ef1 <- effect(term="trust_tot_c*conflict_c", xlevels= list(conflict_c=c(-.98, .98)), mod=fm)
efdata1<-as.data.frame(ef1) #convert the effects list to a data frame
efdata1

efdata1$conflict_c<-as.factor(efdata1$conflict_c)


p <- ggplot(efdata1, aes(x=trust_tot_c, y=fit,color=conflict_c,group=conflict_c)) + geom_line(size=1) +
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=conflict_c),alpha=0.3,show.legend=FALSE) + 
  labs(title = "", x= "Trust", y="Cooperation", color="Conflict", fill="Conflict") + scale_color_hue(labels = c("-1 SD", "+1 SD")) + 
  theme_classic() + theme(text=element_text(size=14)) + coord_cartesian(ylim = c(0,4.5),xlim = c(-2.5,2.5)) 
p + theme(plot.title = element_blank(), legend.position=c(0.8,0.25), text=element_text(size=14)) +
  scale_color_manual(values = c("orange","dark red"),labels = c("-1 SD", "+1 SD")) + 
  scale_fill_manual(values = c("orange","dark red"))


###supplementary analysis competition as outcome

fm <- lmer(Competition ~ trust_tot_c  + 
             (1 | subject) + (1 | subject:TargetID), data=d); summary(fm)
effectsize::standardize_parameters(fm)

fm <- lmer(Competition ~ trust_tot_c + mut_dependence_c + power_sis_c + conflict_c + fut_interdependence_c + inf_certainty_c + (1 | subject) + (1 | subject:TargetID), data=d)
summary(fm)
effectsize::standardize_parameters(fm)



### relation between trust and Competition for levels of conflict
fm <- lmer(Competition ~ trust_tot_c + mut_dependence_c + power_sis_c + conflict_c + fut_interdependence_c + inf_certainty_c + 
             trust_tot_c*conflict_c + (1 | subject) + (1 | subject:TargetID), data=d); summary(fm)
effectsize::standardize_parameters(fm)

#Figure S6 (Panel B)

ef1 <- effect(term="trust_tot_c*conflict_c", xlevels= list(conflict_c=c(-.99, .99)), mod=fm)
efdata1<-as.data.frame(ef1) #convert the effects list to a data frame
efdata1

efdata1$conflict_c<-as.factor(efdata1$conflict_c)


p <- ggplot(efdata1, aes(x=trust_tot_c, y=fit,color=conflict_c,group=conflict_c)) + geom_line(size=1) +
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=conflict_c),alpha=0.3,show.legend=FALSE) + 
  labs(title = "", x= "Trust", y="Competition", color="Conflict", fill="Conflict") + scale_color_hue(labels = c("-1 SD", "+1 SD")) + 
  theme_classic() + theme(text=element_text(size=14)) + coord_cartesian(ylim = c(0,4.5),xlim = c(-2.5,2.5)) 
p + theme(plot.title = element_blank(), legend.position=c(0.8,0.45), text=element_text(size=14)) +
  scale_color_manual(values = c("orange","dark red"),labels = c("-1 SD", "+1 SD")) + 
  scale_fill_manual(values = c("orange","dark red"))




#### Self-Disclosure and Mentalizing

#recode self-disclosure: higher values = more self-disclosure
d$Self_disclosure_r <- 4-d$Self_disclosure

#Self-Disclosure

fm <- lmer(Self_disclosure_r ~ trust_tot_c + (1 | subject) + (1 | subject:TargetID), data=d)
summary(fm)
effectsize::standardize_parameters(fm)

#Mentalizing

fm <- lmer(mentalizing ~ trust_tot_c + (1 | subject) + (1 | subject:TargetID), data=d)
summary(fm)
effectsize::standardize_parameters(fm)



##################################################################################################################################
######################################################  8 Psychological Network Analysis  ############################################
##################################################################################################################################


d = readRDS("Trust_final_OSF.RDS")

names (d)

library("psych")
library ("mlVAR")
library("lavaan")
library("qgraph")
library("bootnet")
library("dplyr")
library("EMAtools")
library("tidyverse")
library("GGally")
library("RColorBrewer")

#install.packages("patchwork")
library("patchwork")


#recode self-disclosure: higher values = more self-disclosure
d$Self_disclosure_r <- 4-d$Self_disclosure

d$Self_disclosure_r_c <- pcenter(d$subject,d$Self_disclosure_r)


n <- dplyr::select(d, start, subject, day, sig, MOC, trust_tot, Cooperation, Close_tot, Warmth, Competence,  Morality, 
                   mut_dependence, power_sis, conflict, fut_interdependence, inf_certainty,  
                   Self_disclosure_r, mentalizing, Moral_self_percept, 
                   happiness, satisfaction, loneliness, arousal, control) 


n$SubID <- n$subject
n$Time <- n$MOC
n$TRUST <- n$trust_tot
n$COOP <- n$Cooperation
n$CLOSE <- n$Close_tot
n$WARM <- n$Warmth
n$COMP <- n$Competence
n$MORAL <- n$Morality
n$DEP <- n$mut_dependence
n$POW <- n$power_sis
n$POW2 <- n$power_sis*n$power_sis
n$dPOW <- abs(n$power_sis)
n$CONF <- n$conflict
n$FUT <- n$fut_interdependence
n$CERT <- n$inf_certainty
n$DISC <- n$Self_disclosure_r
n$MENT <- n$mentalizing
n$MSW <- n$Moral_self_percept
n$HAPPY <- n$happiness
n$SATIS <- n$satisfaction
n$LONE  <- n$loneliness
n$AROU <- n$arousal
n$CONTR <- n$control

names(n)


n <- n[ n$start == "Ja",]

s <- dplyr::select(n, SubID, Time, day, sig, TRUST, COOP, CLOSE, 
            WARM, COMP, MORAL,
            DEP, dPOW, CONF, FUT, CERT,
            HAPPY, SATIS, LONE, AROU, CONTR, MSW,
            DISC,MENT) 

#s <- select(n, SubID, Time, day, sig, TRUST, COOP, CLOSE) 

name <- names(s[5:ncol(s)])

#eliminate all rows with missing values!
s <- s %>% drop_na(5:ncol(s))



#sort by subject, moc..
so<- order(s$SubID, s$Time)

s <- s[so, ]

#estimate network
NW <- mlVAR(s, vars=name, estimator = "lmer", temporal = "fixed",
            idvar="SubID", dayvar = "day", 
            contemporaneous = "orthogonal")

#for graph
Groups <- factor(c(1,6,2,2,2,2,4,4,4,4,4,8,8,8,8,8,8,6,6))



RPalette <- palette(brewer.pal(n = 9, name = "Greys"))
Palette <- "R"
Theme <- "classic"

Groups <- recode_factor(Groups, '1' = "State Trust", '2' = "Trustee Perception", "4" = "Trustee-Trustor Relationship", "6" = "Behavioral Implications", "8" = "General Psychological States", "9" = "")


Labels <- c("State Trust", "Cooperation", "Closeness", "Warmth", "Competence","Morality","Mutual Dependence","Power Difference","Conflict","Future Interdependence","Information Certainty",
            "Momentary Happines","State Life Satisfaction","Loneliness","Arousal","Sense of Control","Moral Self-Worth","Self-Disclosure","Mentalizing")

plot(NW, "contemporaneous", title = "",layout = "spring", nonsig="hide" , groups=Groups,legend=TRUE, 
     legend.mode="style2",layoutOffset=c(-0.1,0),layoutScale=c(1.1,1.1), nodeNames=Labels,
     palette=Palette, theme=Theme, esize=6, lty=c("solid","dotted"), colFactor=0.1, vsize=6, 
     posCol="royalblue1", negCol="black", negDashed=TRUE)


cp <- getNet(NW, "contemporaneous", title = "",layout = "spring", nonsig="hide" , groups=Groups,legend=TRUE, 
             legend.mode="style2",layoutOffset=c(-0.1,0),layoutScale=c(1.1,1.1), nodeNames=Labels,
             palette=Palette, theme=Theme, esize=6, lty=c("solid","dotted"), colFactor=0.1, vsize=6, 
             posCol="royalblue1", negCol="black", negDashed=TRUE)
cp


p1 <- centralityPlot(cp, include = "Strength", orderBy = "Strength")
p2 <- centralityPlot(cp, include = "Closeness", orderBy = "Closeness")
p3 <- centralityPlot(cp, include = "Betweenness", orderBy = "Betweenness")

p1 <- p1 + theme_linedraw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p2 <- p2 + theme_linedraw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p3 <- p3 + theme_linedraw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p1 + p2 + p3

