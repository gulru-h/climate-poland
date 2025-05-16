getwd()

#""X:/LS-KESSELS/ALLGEMEIN/Gülru/digipatch/klima/klima polish replication/klima poland
library(careless)
library(ggplot2)
library(car)
library(psych)
library(dplyr)
library(gtsummary)
library(lavaan)
library(tidySEM)
library(haven)
library(semTools)
library(corrplot)
library(lavaan)
aa <- read_sav("corrected_dataset_preliminary_cleaned.sav")
raw <- read.csv("corrected_dataset_preliminary_cleaned.csv", sep = ";", dec = ".")

clean <- raw[,c(23:214 )]
names(clean)

########Datenbereinigung######
#combine variables
#START HERE
#finance manipulation
sub <- subset(clean[,c(60,65)])
clean$man_finance <- apply(sub, 1, sum, na.rm=T)

#efficacy
class(clean$effectiveness_exp)
clean$effectiveness_exp <- as.numeric(clean$effectiveness_exp)
sub <- subset(clean[,c(61,66)])
clean$man_eff <- apply(sub, 1, sum, na.rm=T)

#acceptability
sub <- subset(clean[,c(63,68)])
clean$man_acc <- apply(sub, 1, sum, na.rm=T)


####build means####

clean[,c(1:163)] <- clean[,c(1:163)] %>% mutate_if(is.character, as.integer)

class(clean$financial_sit_3)
finan <- subset(clean[,c(31:33)])
clean$finan <- apply(finan, 1, mean, na.rm=T)

tsec <- subset(clean[,c(34:36)])
clean$tsec <- apply(tsec, 1, mean, na.rm=T)

#last three security items are actually freedom
tfree <- subset(clean[,c(37:39)])
clean$tfree <- apply(tfree, 1, mean, na.rm=T)

ssec <- subset(clean[,c(52:54)])
clean$ssec <- apply(ssec, 1, mean, na.rm=T)

sfree <- subset(clean[,c(55:58)])
clean$sfree <- apply(sfree, 1, mean, na.rm=T)

clean$finan[clean$finan == "1888"]<- NA



##narratives####
#perform EFA
narr <- subset(mand[,c(70,72,74,76,78,80,82,84)])
library(RcmdrMisc)
narr <- na.omit(narr)
rcorr.adjust(narr) # This function is build into R Commander.

## If you want to run this before eliminating missing values use: 
rcorr.adjust(narr, use="pairwise.complete.obs") 

write.csv(cor(narr)>0.8, file="Suspect_Correlations.csv")
write.csv(cor(narr), file="Correlation_Values.csv")

library(psych)

KMO(narr)
cortest.bartlett(narr)

ev <- eigen(cor(narr, use="pairwise.complete.obs")) # get eigenvalues
ev$values

scree(narr, pc=FALSE)
fa.parallel(narr, fa="fa")

Nfacs <- 2  

fit <- factanal(narr, Nfacs, rotation="promax", na.omit(narr))

print(fit, digits=2, cutoff=0.3, sort=TRUE)

load <- fit$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(narr),cex=.7)

#subsetting and building means 


micronarratives <- subset(mand[,c(69,71,73,75)])
mand$micronarratives <- apply(micronarratives, 1, mean, na.rm=T)

mainstream <- subset(mand[,c(77,79,81,83)])
mand$mainstream <- apply(mainstream, 1, mean, na.rm=T)


micronarratives_ag <- subset(mand[,c(70,72,74,76)])
mand$micronarratives_ag <- apply(micronarratives_ag, 1, mean, na.rm=T)

mainstream_ag <- subset(mand[,c(78,80,82,84)])
mand$mainstream_ag <- apply(mainstream_ag, 1, mean, na.rm=T)


#acceptability assessed with one item

#save manipulation indicator as a factor
class(clean$gr)
clean$gr <- as.factor(clean$gr)
#1 control
#2 voluntary
#3 obligatory
table(mand$gr)



#create a manipulation only group without the control group
rm(onlymanipulation)
onlymanipulation <- mand[mand$gr ==1|mand$gr ==2 , ]

#create the two levels 
onlymanipulation$gr <- ordered(onlymanipulation$gr, levels = c("1", "2"))






