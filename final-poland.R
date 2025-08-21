getwd()

renv::init()
renv::snapshot()

#usethis::use_github()
usethis::gh_token_help()

library(careless)
library(ggplot2)
library(car)
library(psych)
library(dplyr)
library(gtsummary)
library(haven)
library(corrplot)
library(corrtable)
aa <- read_sav("corrected_dataset_preliminary_cleaned.sav")
raw <- read.csv("corrected_dataset_preliminary_cleaned.csv", sep = ";", dec = ".")

clean <- raw[,c(23:214 )]
names(clean)

########data cleaning######
#combine variables
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

mand <- clean

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

contr <- mand[mand$gr ==3, ]
vol<- mand[mand$gr ==2, ]
mandat<- mand[mand$gr ==1, ]

#create the two levels 
onlymanipulation$gr <- ordered(onlymanipulation$gr, levels = c("1", "2"))

#install.packages("fastDummies")

# Load the library
library(fastDummies)

# Create dummy variable
onlymanipulation <- fastDummies::dummy_cols(onlymanipulation, 
                                            select_columns = "gr")
mand <- dummy_cols(mand, select_columns = "gr")

#creating sums of social media variables ####
microlikes <- subset(forsem[,c("q1_sm_2","q2_sm_2","q3_sm_2","q4_sm_2")])
microlikes1 <- subset(mand[,c("q1_sm_2","q2_sm_2","q3_sm_2","q4_sm_2")])

forsem$microlikes <- apply(microlikes, 1, sum, na.rm=T)
mand$microlikes <- apply(microlikes1, 1, sum, na.rm=T)

mainstreamlikes <- subset(forsem[,c("q5_sm_2","q6_sm_2","q7_sm_2","q8_sm_2")])
mainstreamlikes1 <- subset(mand[,c("q5_sm_2","q6_sm_2","q7_sm_2","q8_sm_2")])

forsem$mainstreamlikes <- apply(mainstreamlikes, 1, sum, na.rm=T)
mand$mainstreamlikes <- apply(mainstreamlikes1, 1, sum, na.rm=T)

microdislikes <- subset(forsem[,c("q1_sm_6","q2_sm_6","q3_sm_6","q4_sm_6")])
forsem$microdislikes <- apply(microdislikes, 1, sum, na.rm=T)
microdislikes1 <- subset(mand[,c("q1_sm_6","q2_sm_6","q3_sm_6","q4_sm_6")])
mand$microdislikes <- apply(microdislikes1, 1, sum, na.rm=T)

mainstreamdislikes <- subset(forsem[,c("q5_sm_6","q6_sm_6","q7_sm_6","q8_sm_6")])
forsem$mainstreamdislikes <- apply(mainstreamdislikes, 1, sum, na.rm=T)
mainstreamdislikes1 <- subset(mand[,c("q5_sm_6","q6_sm_6","q7_sm_6","q8_sm_6")])
mand$mainstreamdislikes <- apply(mainstreamdislikes1, 1, sum, na.rm=T)


microshares <- subset(forsem[,c("q1_sm_3","q2_sm_3","q3_sm_3","q4_sm_3")])
microshares1 <- subset(mand[,c("q1_sm_3","q2_sm_3","q3_sm_3","q4_sm_3")])
forsem$microshares <- apply(microshares, 1, sum, na.rm=T)
mand$microshares <- apply(microshares1, 1, sum, na.rm=T)

mainstreamshares <- subset(forsem[,c("q5_sm_3","q6_sm_3","q7_sm_3","q8_sm_3")])
mainstreamshares1 <- subset(mand[,c("q5_sm_3","q6_sm_3","q7_sm_3","q8_sm_3")])

forsem$mainstreamshares <- apply(mainstreamshares, 1, sum, na.rm=T)
mand$mainstreamshares <- apply(mainstreamshares1, 1, sum, na.rm=T)

microcomment <- subset(forsem[,c("q1_sm_4","q2_sm_4","q3_sm_4","q4_sm_4")])
microcomment1 <- subset(mand[,c("q1_sm_4","q2_sm_4","q3_sm_4","q4_sm_4")])

forsem$microcomment <- apply(microcomment, 1, sum, na.rm=T)
mand$microcomment <- apply(microcomment1, 1, sum, na.rm=T)

mainstreamcomment <- subset(forsem[,c("q5_sm_4","q6_sm_4","q7_sm_4","q8_sm_4")])
mainstreamcomment1 <- subset(mand[,c("q5_sm_4","q6_sm_4","q7_sm_4","q8_sm_4")])

forsem$mainstreamcomment <- apply(mainstreamcomment, 1, sum, na.rm=T)
mand$mainstreamcomment <- apply(mainstreamcomment1, 1, sum, na.rm=T)

microhash <- subset(forsem[,c("q1_sm_5","q2_sm_5","q3_sm_5","q4_sm_5")])
forsem$microhash <- apply(microhash, 1, sum, na.rm=T)

mainstreamhash <- subset(forsem[,c("q5_sm_5","q6_sm_5","q7_sm_5","q8_sm_5")])
forsem$mainstreamhash <- apply(mainstreamhash, 1, sum, na.rm=T)

######

cors <- subset(mand[,c(16,15, 14, 28:29, 196,193, 150, 197:200, 195, 201:204, 149)])
cors1 <- cors[mand$gr==1, ]
cors2 <- cors[mand$gr==2, ]
cors3 <- cors[mand$gr==3, ]

save_correlation_matrix(cors1,
                        filename = "cors1.csv",
                        digits= 3, use="lower")

save_correlation_matrix(cors2,
                        filename = "cors2.csv",
                        digits= 3, use="lower")

save_correlation_matrix(cors3,
                        filename = "cors3.csv",
                        digits= 3, use="lower")

sapply(cors1, mean, na.rm=T)
sapply(cors2, mean, na.rm=T)
sapply(cors3, mean, na.rm=T)


sapply(cors1, sd, na.rm=T)
sapply(cors2, sd, na.rm=T)
sapply(cors3, sd, na.rm=T)


#trust

trustsec <- lm(institution_trust_5~gr_1, data= onlymanipulation)
summary(trustsec)

#(exploratory) H4: Active media use will moderate the relationship between 
#perceived threat to security and the choice of micro and mainstream narratives.

m1 <- lm(micronarratives~ ssec*active_soc_media+tsec, data=onlymanipulation)
m1.a <- summary(m1)
ggpredict(m1, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()
#non-sig

m2 <- lm(mainstream~ ssec*active_soc_media+tsec, data=onlymanipulation)
m2.a <- summary(m2)
ggpredict(m2, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()


m3 <- lm(mainstream_ag~ ssec*active_soc_media+tsec, data=onlymanipulation)
m3.a <- summary(m3)
ggpredict(m3, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()


m4 <- lm(micronarratives_ag~ ssec*active_soc_media+tsec, data=onlymanipulation)
m4.a <- summary(m4)
ggpredict(m4, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()
#non-sig



#H5: The perceived effectiveness of the proposed policy will not differ between 
#the mandatory and the voluntary conditions.
a <- aov(man_eff~gr, data=onlymanipulation)
summary(a)
TukeyHSD(a)
#mandatory policy seen as more effective


#H6: Participants in the mandatory condition will perceive the policy to be less 
#acceptable in comparison to those in the voluntary and control conditions.

b <- aov(man_acc~gr, data=onlymanipulation)
summary(b)
TukeyHSD(b)



mean(mand$age_5, na.rm=T)
sd(mand$age_5, na.rm=T)




###SEM Time#####
library(lavaan)
library(semptools)
library(semPlot)
library(tidySEM)

library(future)
plan(multisession) 
set.seed(545)


#install.packages("fastDummies")

# Load the library
library(fastDummies)


# Create dummy variable
onlymanipulation <- fastDummies::dummy_cols(onlymanipulation, 
                                            select_columns = "gr")
mand <- dummy_cols(mand, select_columns = "gr")

forsem <- onlymanipulation

which(is.na(forsem$gr))
which(is.na(mand$gr))
forsem <- forsem[-c(123:125, 187, 188, 262,263), ]
mand <- mand[-c(191, 192, 193 ,284 ,285, 400 ,401), ] 


#base model interest

nint.model <- '
  Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  Micronarratives ~ gr_1
  Mainstream ~gr_1
  Mainstream~~Micronarratives

'

nint.fit <- sem(nint.model, data = forsem, estimator = "ML", missing = "FIML")
summary(nint.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


#base model agreement

set.seed(545)

nag.model <- '
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 

  Micronarratives ~ gr_1
  Mainstream ~gr_1
  
  Mainstream~~Micronarratives
    
'

nag.fit <- sem(nag.model, data = forsem, estimator = "ML", missing = "FIML")
summary(nag.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)



#SECURITY

#Interest security base

medint.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedsecurity=~ security_1+security_2+security_3
  finance=~financial_sit_1+financial_sit_2+financial_sit_3
  
  Micronarratives ~b1*needsecurity+gr_1+traitneedsecurity+finance
  Mainstream ~b2*needsecurity+gr_1+traitneedsecurity+finance
  needsecurity ~ a1*gr_1+traitneedsecurity+finance
  Mainstream~~Micronarratives
  
  ind1 := a1*b1
  ind2 := a1*b2
 
'


medint.fit <- sem(medint.model, data = forsem, estimator = "ML",
                  missing = "FIML",se = "bootstrap",bootstrap = 5000L, 
                  parallel ="multicore", verbose= T)
summary(medint.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci = T)

#Security - agreement base

med.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedsecurity=~ security_1+security_2+security_3  
  finance=~financial_sit_1+financial_sit_2+financial_sit_3


  Micronarratives ~b1*needsecurity+gr_1+traitneedsecurity+finance
  Mainstream ~b2*needsecurity+gr_1+traitneedsecurity+finance
  needsecurity ~ a1*gr_1+traitneedsecurity+finance

  Mainstream~~Micronarratives
    
  ind1 := a1*b1
  ind2 := a1*b2

'

med.fit <- sem(med.model, data = forsem, estimator = "ML", 
               missing = "FIML", se = "bootstrap",bootstrap = 5000L, 
               parallel ="multicore", verbose= T)
summary(med.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci= T)

#security, interest, trust

int_talt.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  intMainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  intMicronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedsecurity=~ security_1+security_2+security_3
  finance=~financial_sit_1+financial_sit_2+financial_sit_3


  institution_trust_5 ~ traitneedsecurity+b1*intMicronarratives + b2*intMainstream +gr_1 +needsecurity+finance
  intMicronarratives ~a1*needsecurity+gr_1+traitneedsecurity+finance
  intMainstream ~ a2*needsecurity+gr_1+traitneedsecurity+finance
  needsecurity ~ a3*gr_1+traitneedsecurity+finance

  ind1 := a1*b1
  ind2 := a2*b2
  ind3 := a3*a1
  ind4 := a3*a2
  
  intMicronarratives~~intMainstream


'

int_talt.fit <- sem(int_talt.model, data = forsem, estimator = "ML", 
                    missing = "FIML", se = "bootstrap",bootstrap = 5000L, 
                    parallel ="multicore", verbose= T)
summary(int_talt.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci = T)

#Trust - agreement security

talt.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedsecurity=~ security_1+security_2+security_3
  finance=~financial_sit_1+financial_sit_2+financial_sit_3
 

  institution_trust_5 ~ b1*Micronarratives + b2*Mainstream +gr_1 +needsecurity+traitneedsecurity+finance
  Micronarratives ~a1*needsecurity+gr_1+traitneedsecurity+finance
  Mainstream ~ a2*needsecurity+gr_1+traitneedsecurity+finance
  needsecurity ~ a3*gr_1+traitneedsecurity+finance

  ind1 := a1*b1
  ind2 := a2*b2
  ind3 := a3*a1
  ind4 := a3*a2

  Micronarratives~~Mainstream

'


talt.fit <- sem(talt.model, data = forsem, estimator = "ML", 
                missing = "FIML", se = "bootstrap",bootstrap = 5000L, 
                parallel ="multicore", verbose= T)
summary(talt.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci= T)


#active media use approach security 

dmcforsem <- forsem

#double mean centering
library(semTools)
dmcforsem <- indProd(dmcforsem, var1= c("security_freedom_1", "security_freedom_2", "security_freedom_3"),
                     var2=c("active_soc_media"),
                     match = FALSE , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 


amedint.model <- '
  stateneedsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedsecurity=~ security_1+security_2+security_3
  interaction=~ security_freedom_1.active_soc_media +security_freedom_2.active_soc_media + security_freedom_3.active_soc_media 
  
  
  Micronarratives ~stateneedsecurity+traitneedsecurity+active_soc_media+interaction+gr_1 
  Mainstream ~stateneedsecurity+traitneedsecurity+active_soc_media+interaction+gr_1 

  institution_trust_5 ~ Micronarratives+traitneedsecurity+Mainstream+stateneedsecurity+gr_1 
  stateneedsecurity ~ gr_1+active_soc_media+traitneedsecurity

  Micronarratives~~Mainstream
'

amedint.fit <- sem(amedint.model, data = dmcforsem, estimator = "MLM")
summary(amedint.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)

#active media use agreement security 
dmcforsem <- forsem

#double mean centering
library(semTools)
dmcforsem <- indProd(dmcforsem, var1= c("security_freedom_1", "security_freedom_2", "security_freedom_3"),
                     var2=c("active_soc_media"),
                     match = FALSE , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 
amed.model <- '
stateneedsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedsecurity=~ security_1+security_2+security_3
  interaction=~ security_freedom_1.active_soc_media +security_freedom_2.active_soc_media + security_freedom_3.active_soc_media 
  
  Micronarratives ~stateneedsecurity+traitneedsecurity+active_soc_media+interaction+gr_1 
  Mainstream ~stateneedsecurity+traitneedsecurity+active_soc_media+interaction+gr_1 

  institution_trust_5 ~ Micronarratives+traitneedsecurity+Mainstream+stateneedsecurity+gr_1 
  stateneedsecurity ~ gr_1+active_soc_media+traitneedsecurity

  Micronarratives~~Mainstream
'

amed.fit <- sem(amed.model, data = dmcforsem, estimator = "MLM")
summary(amed.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)

#FREEDOM

#freedom approach base

frintmed.model <- '
  needfreedom =~ security_freedom_4+security_freedom_5+security_freedom_6
  Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedfreedom=~ security_4+security_5+security_6
  finance=~financial_sit_1+financial_sit_2+financial_sit_3

  Micronarratives ~b1*needfreedom+gr_1+traitneedfreedom+finance
  Mainstream ~b2*needfreedom+gr_1+traitneedfreedom+finance
  needfreedom ~ a1*gr_1+traitneedfreedom+finance

  Mainstream~~Micronarratives
  
  ind1 := a1*b1
  ind2 := a1*b2

'

frintmed.fit <- sem(frintmed.model, data = forsem, estimator = "ML", 
                    missing = "FIML", se = "bootstrap",
                    bootstrap = 5000L,parallel ="multicore", verbose= T)
summary(frintmed.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)

#freedom -- agreement base model


frmed.model <- '
  needfreedom =~ security_freedom_4+security_freedom_5+security_freedom_6
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedfreedom=~ security_4+security_5+security_6
  finance=~financial_sit_1+financial_sit_2+financial_sit_3

  Micronarratives ~b1*needfreedom+gr_1+traitneedfreedom+finance
  Mainstream ~b2*needfreedom+gr_1+traitneedfreedom+finance

  needfreedom ~ a1*gr_1+traitneedfreedom+finance

  Mainstream~~Micronarratives

  ind1 := a1*b1
  ind2 := a1*b2

'

frmed.fit <- sem(frmed.model, data = forsem, estimator = "ML", 
                 missing = "FIML", se = "bootstrap",
                 bootstrap = 5000L,parallel ="multicore", verbose= T)
summary(frmed.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


#freedom approach trust

frtaltint.model <- '
  needfreedom =~ security_freedom_4+security_freedom_5+security_freedom_6
  Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedfreedom=~ security_4+security_5+security_6
  finance=~financial_sit_1+financial_sit_2+financial_sit_3

  institution_trust_5 ~ b1*Micronarratives+b2*Mainstream+gr_1+needfreedom+traitneedfreedom+finance
  Micronarratives ~a1*needfreedom+gr_1+traitneedfreedom+finance
  Mainstream ~ a2*needfreedom+gr_1+traitneedfreedom+finance
  needfreedom ~ a3*gr_1 +traitneedfreedom+finance
  institution_trust_5~traitneedfreedom+finance

      
  ind1 := a1*b1
  ind2 := a2*b2
  ind3:= a3*a1
  ind4:= a3*a2
  
  Mainstream~~Micronarratives

'

frtaltint.fit <- sem(frtaltint.model, data = forsem, estimator = "ML", 
                     missing = "FIML", se = "bootstrap",
                     bootstrap = 5000L,parallel ="multicore", verbose= T)
summary(frtaltint.fit, fit.measures=T, standardized = T, rsquare=TRUE,
        ci=T)


#freedom agreement trust



frtalt.model <- '
  needfreedom =~ security_freedom_4+security_freedom_5+security_freedom_6
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedfreedom=~ security_4+security_5+security_6
  finance=~financial_sit_1+financial_sit_2+financial_sit_3

  institution_trust_5 ~ b1*Micronarratives+b2*Mainstream+gr_1+needfreedom+traitneedfreedom+finance
  Micronarratives ~a1*needfreedom+gr_1+traitneedfreedom+finance
  Mainstream ~ a2*needfreedom+gr_1+traitneedfreedom+finance
  needfreedom ~ a3*gr_1 +traitneedfreedom+finance

      
  ind1 := a1*b1
  ind2 := a2*b2
  ind3:= a3*a1
  ind4:= a3*a2
  Mainstream~~Micronarratives

'


frtalt.fit <- sem(frtalt.model, data = forsem, estimator = "ML", 
                  missing = "FIML", se = "bootstrap",
                  bootstrap = 5000L,parallel ="multicore", verbose= T)
summary(frtalt.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


#freedom approach trust active media use

dmcforsem <- forsem

#double mean centering
library(semTools)
dmcforsem <- indProd(dmcforsem, var1= c("security_freedom_4", "security_freedom_5", "security_freedom_6"),
                     var2=c("active_soc_media"),
                     match = FALSE , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 
frintamed.model <- '
  stateneedfreedom =~ security_freedom_4+security_freedom_5+security_freedom_6
  Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedfreedom=~ security_4+security_5+security_6
  interaction=~ security_freedom_4.active_soc_media +security_freedom_5.active_soc_media + security_freedom_6.active_soc_media 
  
  
  Micronarratives ~stateneedfreedom+active_soc_media+interaction+gr_1+traitneedfreedom
  Mainstream ~stateneedfreedom+active_soc_media+interaction+gr_1+traitneedfreedom
  active_soc_media ~ traitneedfreedom

  institution_trust_5 ~ Micronarratives+Mainstream+stateneedfreedom+traitneedfreedom+gr_1
  stateneedfreedom ~ gr_1+traitneedfreedom+active_soc_media
  
  Mainstream~~Micronarratives

'

frintamed.fit <- sem(frintamed.model, data = dmcforsem, estimator = "MLM")
summary(frintamed.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


#freedom agreement trust active media use
dmcforsem <- forsem

#double mean centering
library(semTools)
dmcforsem <- indProd(dmcforsem, var1= c("security_freedom_4", "security_freedom_5", "security_freedom_6"),
                     var2=c("active_soc_media"),
                     match = FALSE , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 
fragamed.model <- '
  stateneedfreedom =~ security_freedom_4+security_freedom_5+security_freedom_6
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedfreedom=~ security_4+security_5+security_6
  interaction=~ security_freedom_4.active_soc_media +security_freedom_5.active_soc_media + security_freedom_6.active_soc_media 
  
  
   
  Micronarratives ~stateneedfreedom+active_soc_media+interaction+gr_1+traitneedfreedom
  Mainstream ~stateneedfreedom+active_soc_media+interaction+gr_1+traitneedfreedom
  active_soc_media ~ traitneedfreedom

  institution_trust_5 ~ Micronarratives+Mainstream+stateneedfreedom+traitneedfreedom+gr_1
  stateneedfreedom ~ gr_1+traitneedfreedom+active_soc_media
  
  Mainstream~~Micronarratives


'

fragamed.fit <- sem(fragamed.model, data = dmcforsem, estimator = "MLM")
summary(fragamed.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


#MEDIA VARIABLES
#Moderation active media use likes and dislikes


dmcforsem <- forsem

#double mean centering
library(semTools)
dmcforsem <- indProd(dmcforsem, var1= c("security_freedom_1", "security_freedom_2", "security_freedom_3"),
                     var2=c("active_soc_media"),
                     match = FALSE , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 


#Number of Likes and Dislikes, need for security, active media use

ldla.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  likeMainstream=~mainstreamlikes           #q5_sm_2 +q6_sm_2+q7_sm_2 +q8_sm_2 
  likeMicronarratives=~microlikes            #q1_sm_2 +q2_sm_2+q3_sm_2+q4_sm_2 
  dislikeMainstream=~mainstreamdislikes     #q5_sm_6 +q6_sm_6+q7_sm_6#+q8_sm_6 
  dislikeMicronarratives=~microdislikes     #q1_sm_6 +q2_sm_6+q3_sm_6+q4_sm_6 
  traitneedsecurity=~ security_1+security_2+security_3
  finance=~financial_sit_1+financial_sit_2+financial_sit_3


  
  likeMicronarratives ~a1*needsecurity+gr_1+c1*active_soc_media+traitneedsecurity+finance
  likeMainstream ~a2*needsecurity+gr_1+c2*active_soc_media+traitneedsecurity+finance

  dislikeMicronarratives ~a3*needsecurity+gr_1+c3*active_soc_media+traitneedsecurity+finance
  dislikeMainstream ~a4*needsecurity+gr_1+c4*active_soc_media+traitneedsecurity+finance
  
  needsecurity ~ gr_1 +traitneedsecurity+finance
  institution_trust_5 ~ needsecurity +active_soc_media+gr_1+traitneedsecurity
  +b1*likeMicronarratives+b2*likeMainstream+b3*dislikeMicronarratives+
  b4*dislikeMainstream + finance

  active_soc_media~traitneedsecurity+finance
  ind1 := a1*b1
  ind2 := a2*b2
  ind3 := a3*b3
  ind4 := a4*b4
  
  ind5 := c1*b1
  ind6 := c2*b2
  ind7 := c3*b3
  ind8 := c4*b4
  
  #q3_sm_2~~q3_sm_6
  #q1_sm_2~~q1_sm_6
  #q7_sm_2~~q7_sm_6
  #q6_sm_2~~q6_sm_6
  #q5_sm_2~~q5_sm_6
  #q2_sm_2~~q2_sm_6
  #q4_sm_2~~q4_sm_6
  #q8_sm_2~~q8_sm_6
  
  likeMainstream~~dislikeMicronarratives
  likeMicronarratives~~dislikeMainstream
  likeMainstream~~likeMicronarratives
  dislikeMainstream~~dislikeMicronarratives
  likeMainstream~~dislikeMainstream
  likeMicronarratives~~dislikeMicronarratives
  
  active_soc_media~~needsecurity


'

ldla.fit <- sem(ldla.model, data = forsem, estimator = "ML", 
                missing = "FIML", se = "bootstrap",bootstrap = 5000L, 
                parallel ="multicore", verbose= T)
summary(ldla.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


#Number of Likes and Dislikes, need for freedom, active media use

#both
fldl.model <- '
  needfreedom =~ security_freedom_4+security_freedom_5+security_freedom_6
  likeMainstream=~mainstreamlikes           #q5_sm_2 +q6_sm_2+q7_sm_2 +q8_sm_2 
  likeMicronarratives=~microlikes            #q1_sm_2 +q2_sm_2+q3_sm_2+q4_sm_2 
  dislikeMainstream=~mainstreamdislikes     #q5_sm_6 +q6_sm_6+q7_sm_6#+q8_sm_6 
  dislikeMicronarratives=~microdislikes     #q1_sm_6 +q2_sm_6+q3_sm_6+q4_sm_6 
  traitneedfreedom=~ security_4+security_5+security_6
  finance=~financial_sit_1+financial_sit_2+financial_sit_3

  
    
  likeMicronarratives ~a1*needfreedom+gr_1+c1*active_soc_media+traitneedfreedom+finance
  likeMainstream ~a2*needfreedom+gr_1+c2*active_soc_media+traitneedfreedom+finance

  dislikeMicronarratives ~a3*needfreedom+gr_1+c3*active_soc_media+traitneedfreedom+finance
  dislikeMainstream ~a4*needfreedom+gr_1+c4*active_soc_media+traitneedfreedom+finance
  
  needfreedom ~ gr_1+traitneedfreedom+finance
  institution_trust_5 ~ gr_1+active_soc_media+gr_1+traitneedfreedom
  +b1*likeMicronarratives+b2*likeMainstream+b3*dislikeMicronarratives+
  b4*dislikeMainstream + finance
    
  ind1 := a1*b1
  ind2 := a2*b2
  ind3 := a3*b3
  ind4 := a4*b4
  
  ind5 := c1*b1
  ind6 := c2*b2
  ind7 := c3*b3
  ind8 := c4*b4
  
  
  likeMainstream~~dislikeMicronarratives
  likeMicronarratives~~dislikeMainstream
  likeMainstream~~likeMicronarratives
  dislikeMainstream~~dislikeMicronarratives
  likeMainstream~~dislikeMainstream
  likeMicronarratives~~dislikeMicronarratives
  
  active_soc_media~~needfreedom
'

fldl.fit <- sem(fldl.model, data = forsem, estimator = "ML", 
                missing = "FIML", se = "bootstrap",bootstrap = 5000L, 
                parallel ="multicore", verbose= T)
summary(fldl.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


