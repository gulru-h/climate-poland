
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
library(semptools)
library(semPlot)
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



#0 model interest

set.seed(545)
nint.model <- '
  Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  Micronarratives ~ gr_1
  Mainstream ~gr_1
  Mainstream~~Micronarratives

'

nint.fit <- sem(nint.model, data = forsem, estimator = "ML", missing = "FIML")
summary(nint.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_nint.fit <- parameterEstimates(nint.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_nint.fit)

#0 model agree

set.seed(545)

nag.model <- '
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 

  Micronarratives ~ gr_1
  Mainstream ~gr_1
  
  Mainstream~~Micronarratives
    
'

nag.fit <- sem(nag.model, data = forsem, estimator = "ML", missing = "FIML")
summary(nag.fit, fit.measures=T, standardized = T, rsquare=TRUE)

estimates_nag.fit <- parameterEstimates(nag.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,zstat = FALSE, pvalue = FALSE, output = "data.frame")
View(estimates_nag.fit)



#SECURITY

#Interest security base


set.seed(545)

dmcforsem <- forsem

#double mean centering
library(semTools)
dmcforsem <- indProd(forsem, var1= c("security_freedom_3", "security_freedom_2", "security_freedom_1"),
                     var2=c("security_2", "security_1", "security_3"),
                     match = F , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 
dmcforsem <- indProd(forsem, var1= c("ssec"),
                     var2=c("tsec"),
                     match = T , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 
#ssectsec=~security_freedom_3.security_2+security_freedom_2.security_1+
#security_freedom_1.security_3

medint.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedsecurity=~ security_1+security_2+security_3
  
  Micronarratives ~b1*needsecurity+gr_1
  Mainstream ~b2*needsecurity+gr_1
  needsecurity ~ a1*gr_1+traitneedsecurity+ssec.tsec
  Mainstream~~Micronarratives
  
  ind1 := a1*b1
  ind2 := a1*b2
 
'
library(future)
plan(multisession) 

medint.fit <- sem(medint.model, data = dmcforsem, estimator = "ML",
                  missing = "FIML",se = "bootstrap",bootstrap = 5000L, 
                  parallel ="multicore", verbose= T)
summary(medint.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci = T)
estimates_medint.fit <- parameterEstimates(medint.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,zstat = FALSE, pvalue = FALSE, output = "data.frame")

mi <- modificationindices(medint.fit)
View(mi)
View(estimates_medint.fit)
write.csv(estimates_medint.fit, "estimates_medint.fit.csv")

#Security - agreement base

set.seed(545)

med.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedsecurity=~ security_1+security_2+security_3
  #ssectsec=~security_freedom_1.security_1+security_freedom_2.security_2+security_freedom_3.security_3

  Micronarratives ~b1*needsecurity+gr_1
  Mainstream ~b2*needsecurity+gr_1


  needsecurity ~ a1*gr_1+traitneedsecurity+ssec.tsec

  Mainstream~~Micronarratives
    
  ind1 := a1*b1
  ind2 := a1*b2

'

med.fit <- sem(med.model, data = dmcforsem, estimator = "ML", 
               missing = "FIML", se = "bootstrap",bootstrap = 5000L, 
               parallel ="multicore", verbose= T)
summary(med.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci= T)
estimates_med.fit <- parameterEstimates(med.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,zstat = FALSE, pvalue = FALSE, output = "data.frame")

mi <- modificationindices(med.fit)
View(mi)
View(estimates_med.fit)
write.csv(estimates_med.fit, "estimates_med.fit.csv")


#visualisation#####

pm_no_covs <- semptools::drop_nodes(
  semPlotModel(med.fit),
  c("security_freedom_1", "security_freedom_2", "security_freedom_3", "nar5_end",
    "nar6_end", "nar7_end", "nar8_end", "nar1_end", "nar2_end", "nar3_end",
    "nar4_end", "security_1", "security_2", "security_3"))

pfad_layout<- get_layout("traitneedsecurity","", "", "","Micronarratives",
                         "","", "","","",
                         "stateneedsecurity","","","","",
                         "","","","","Mainstream",
                         "gr","","","","",
                         "security_freedom_1", "security_freedom_2", "security_freedom_3", "nar5_end", "",
                         "nar6_end", "nar7_end", "nar8_end", "nar1_end", "nar2_end", 
                         "nar3_end", "nar4_end", "security_1", "security_2", "security_3",
                         rows = 8)


pm <- semPaths(pm_no_covs, what= "std", layout = pfad_layout, residuals = FALSE,
               nCharNodes = 0, fade= FALSE, sizeLat = 9, sizeMan = 9, label.cex = 1.5, edge.label.cex =1,
               intercepts = F, equalizeManifests=F, exoCov = F)


#add statistical significance asteriscs
library(semptools)
my_label_list <- list(list(node = "traitneedsecurity", to = "(T) Need for \nsecurity"),
                      list(node = "stateneedsecurity", to = "(S) Need for \nsecurity"),
                      list(node = "Micronarratives", to = "Anti-mainstream \nnarratives"),
                      list(node = "Mainstream", to = "Mainstream \nnarratives"),
                      list(node = "gr", to = "Group \n(vol=0, mand=1")
)

p_pa2 <- mark_sig(pm, med.fit)
p_pa2 <- change_node_label(p_pa2, my_label_list)

plot(p_pa2)
######


#security, interest, trust

set.seed(545)

int_talt.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  intMainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  intMicronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedsecurity=~ security_1+security_2+security_3
  #ssectsec=~security_freedom_1.security_1+security_freedom_2.security_2+security_freedom_3.security_3

  institution_trust_5 ~ b1*intMicronarratives + b2*intMainstream +gr_1 +needsecurity
  intMicronarratives ~a1*needsecurity+gr_1
  intMainstream ~ a2*needsecurity+gr_1
  needsecurity ~ a3*gr_1+ssec.tsec+traitneedsecurity

  ind1 := a1*b1
  ind2 := a2*b2
  ind3 := a3*a1
  ind4 := a3*a2
  
  intMicronarratives~~intMainstream


'

int_talt.fit <- sem(int_talt.model, data = dmcforsem, estimator = "ML", 
                    missing = "FIML", se = "bootstrap",bootstrap = 5000L, 
                    parallel ="multicore", verbose= T)
summary(int_talt.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci = T)
estimates_int_talt.fit <- parameterEstimates(int_talt.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,zstat = TRUE, pvalue = TRUE, output = "data.frame")
mit <- modificationindices(int_talt.fit)
View(mit)
View(estimates_int_talt.fit)
write.csv(estimates_int_talt.fit, "estimates_int_talt.fit.csv")
write.csv(mit, "mit.csv")


#Trust - agreement security

set.seed(545)

talt.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedsecurity=~ security_1+security_2+security_3
  #ssectsec=~security_freedom_1.security_1+security_freedom_2.security_2+security_freedom_3.security_3

  institution_trust_5 ~ b1*Micronarratives + b2*Mainstream +gr_1 +needsecurity
  Micronarratives ~a1*needsecurity+gr_1
  Mainstream ~ a2*needsecurity+gr_1
  needsecurity ~ a3*gr_1+ssec.tsec+traitneedsecurity

  ind1 := a1*b1
  ind2 := a2*b2
  ind3 := a3*a1
  ind4 := a3*a2

  Micronarratives~~Mainstream

'


talt.fit <- sem(talt.model, data = dmcforsem, estimator = "ML", 
                missing = "FIML", se = "bootstrap",bootstrap = 5000L, 
                parallel ="multicore", verbose= T)
summary(talt.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci= T)
estimates_talt.fit <- parameterEstimates(talt.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,zstat = T, pvalue = T, output = "data.frame")

View(estimates_talt.fit)
write.csv(estimates_talt.fit, "estimates_talt.fit.csv")



#visualisation####


tm_no_covs <- semptools::drop_nodes(
  semPlotModel(int_talt.fit),
  c("security_freedom_1", "security_freedom_2", "security_freedom_3", "nar5_end",
    "nar6_end", "nar7_end", "nar8_end", "nar1_end", "nar2_end", "nar3_end",
    "nar4_end", "security_1", "security_2", "security_3"))

pfad_layout<- get_layout("","", "Micronarratives", "","",
                         "traitneedsecurity","", "","","institution_trust_5",
                         "stateneedsecurity","","","","",
                         "gr_1","","Mainstream","","",
                         "security_freedom_1", "security_freedom_2", "security_freedom_3", "nar5_end", "",
                         "nar6_end", "nar7_end", "nar8_end", "nar1_end", "nar2_end", 
                         "nar3_end", "nar4_end", "security_1", "security_2", "security_3",
                         rows = 7)


tm <- semPaths(tm_no_covs, what= "std", layout = pfad_layout, residuals = FALSE,
               nCharNodes = 0, fade= FALSE, sizeLat = 9, sizeMan = 9, label.cex = 1.6, edge.label.cex =1,
               intercepts = F, equalizeManifests=F, exoCov = F)
my_label_list <- list(list(node = "traitneedsecurity", to = "(T) Need for \nsecurity"),
                      list(node = "stateneedsecurity", to = "(S) Need for \nsecurity"),
                      list(node = "institution_trust_5", to = "Trust in \nthe government"),
                      list(node = "Micronarratives", to = "Anti-mainstream \nnarratives"),
                      list(node = "Mainstream", to = "Mainstream \nnarratives"),
                      list(node = "gr_1", to = "Group \n(vol=0, mand=1"))


#add statistical significance asteriscs
library(semptools)

tp_pa2 <- mark_sig(tm, int_talt.fit)
tp_pa2 <- change_node_label(tp_pa2, my_label_list)
plot(tp_pa2)
#####





#active media use approach security 

set.seed(545)

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
  
  
  Micronarratives ~stateneedsecurity+traitneedsecurity+active_soc_media+interaction
  Mainstream ~stateneedsecurity+traitneedsecurity+active_soc_media+interaction
  stateneedsecurity~active_soc_media

  institution_trust_5 ~ Micronarratives
  institution_trust_5 ~Mainstream
  Micronarratives ~ gr_1
  Mainstream ~gr_1

  stateneedsecurity ~ gr_1
  institution_trust_5~ gr_1 
  institution_trust_5 ~ stateneedsecurity
  
  stateneedsecurity~~traitneedsecurity
  Micronarratives~~Mainstream
'

amedint.fit <- sem(amedint.model, data = dmcforsem, estimator = "MLM")
summary(amedint.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_amedint.fit <- parameterEstimates(amedint.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                            zstat = FALSE, pvalue = FALSE, output = "data.frame")

write.csv(estimates_amedint.fit, "estimates_amedint.fit.csv")


#active media use agreement security 

set.seed(545)

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
  
  
  Micronarratives ~stateneedsecurity+traitneedsecurity+active_soc_media+interaction
  Mainstream ~stateneedsecurity+traitneedsecurity+active_soc_media+interaction
  active_soc_media ~ stateneedsecurity

  institution_trust_5 ~ Micronarratives
  institution_trust_5 ~Mainstream
  Micronarratives ~ gr_1
  Mainstream ~gr_1

  stateneedsecurity ~ gr_1
  institution_trust_5~ gr_1 
  institution_trust_5 ~ stateneedsecurity
  
   stateneedsecurity~~traitneedsecurity
   Micronarratives~~Mainstream
'

amed.fit <- sem(amed.model, data = dmcforsem, estimator = "MLM")
summary(amed.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_amed.fit <- parameterEstimates(amed.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                         zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_amed.fit)

write.csv(estimates_amed.fit, "estimates_amed.fit.csv")


#visualisation####


am_no_covs <- semptools::drop_nodes(
  semPlotModel(amed.fit),
  c("security_freedom_1", "security_freedom_2", "security_freedom_3", "nar5_end",
    "nar6_end", "nar7_end", "nar8_end", "nar1_end", "nar2_end", "nar3_end",
    "nar4_end", "security_1", "security_2", "security_3", "security_freedom_1.active_soc_media", "security_freedom_2.active_soc_media",  "security_freedom_3.active_soc_media" ))

a_pfad_layout<- get_layout("","","traitneedsecurity", "", "Micronarratives","",
                           "","active_soc_media","", "","","interaction",
                           "","","","","","",
                           "","stateneedsecurity","","","","institution_trust_5",
                           "","gr_1","","","","",
                           "","","","","Mainstream","",
                           "","","","","","",
                           "","ssec1", "ssec2", "ssec3", "narrative_2", "narrative_4", 
                           "","narrative_6","narrative_8","tsec1","tsec2","tsec3", 
                           "","narrative_1","narrative_3", "narrative_5", "narrative_7", "",
                           "","ssec1.med_act","ssec2.med_act","ssec3.med_act", "","",
                           "security_freedom_1.active_soc_media", "security_freedom_2.active_soc_media", "security_freedom_3.active_soc_media", "","", "",
                           rows = 12)


am <- semPaths(am_no_covs, what= "std", layout = a_pfad_layout, residuals = FALSE,
               nCharNodes = 0, fade= FALSE, sizeLat = 8, sizeMan = 6, label.cex = 2, edge.label.cex =1,
               intercepts = F, equalizeManifests=F, exoCov = F)


#add statistical significance asteriscs
library(semptools)
my_label_list <- list(list(node = "traitneedsecurity", to = "(T) Need for \nsecurity"),
                      list(node = "stateneedsecurity", to = "(S) Need for \nsecurity"),
                      list(node = "institution_trust_5", to = "Trust in \nthe government"),
                      list(node = "active_soc_media", to = "Active \nmedia use"),
                      list(node = "interaction", to = "Interaction \nterm"),
                      list(node = "Micronarratives", to = "Anti-mainstream \nnarratives"),
                      list(node = "Mainstream", to = "Mainstream \nnarratives"),
                      list(node = "gr_1", to = "Group \n(vol=0, mand=1)"))


ap_pa2 <- mark_sig(am, amed.fit)
ap_pa2 <- change_node_label(ap_pa2, my_label_list)

plot(ap_pa2)
#####



#FREEDOM
fdmcforsem <- forsem

#double mean centering
library(semTools)
fdmcforsem <- indProd(dmcforsem, var1= c("security_freedom_4", "security_freedom_5", "security_freedom_6"),
                     var2=c("security_4", "security_5", "security_6"),
                     match = T , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 
fdmcforsem <- indProd(forsem, var1= c("sfree"),
                      var2=c("tfree"),
                      match = T , meanC = TRUE ,
                      residualC = FALSE , doubleMC = TRUE) 
#freedom approach base


frintmed.model <- '
  needfreedom =~ security_freedom_4+security_freedom_5+security_freedom_6
  Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedfreedom=~ security_4+security_5+security_6
  #sfreetfree=~security_freedom_4.security_4+security_freedom_5.security_5+security_freedom_6.security_6

  Micronarratives ~b1*needfreedom+gr_1
  Mainstream ~b2*needfreedom+gr_1

  needfreedom ~ a1*gr_1+traitneedfreedom+sfree.tfree

  Mainstream~~Micronarratives
  
  ind1 := a1*b1
  ind2 := a1*b2

'

frintmed.fit <- sem(frintmed.model, data = fdmcforsem, estimator = "ML", 
                    missing = "FIML", se = "bootstrap",
                    bootstrap = 5000L,parallel ="multicore", verbose= T)
summary(frintmed.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)
estimates_frintmed.fit <- parameterEstimates(frintmed.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_frintmed.fit)
write.csv(estimates_frintmed.fit, "estimates_frintmed.fit.csv")


#freedom -- agreement base model


frmed.model <- '
  needfreedom =~ security_freedom_4+security_freedom_5+security_freedom_6
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedfreedom=~ security_4+security_5+security_6
  #sfreetfree=~security_freedom_4.security_4+security_freedom_5.security_5+security_freedom_6.security_6

  Micronarratives ~b1*needfreedom+gr_1
  Mainstream ~b2*needfreedom+gr_1

  needfreedom ~ a1*gr_1+traitneedfreedom+sfree.tfree

  Mainstream~~Micronarratives

  ind1 := a1*b1
  ind2 := a1*b2

'

frmed.fit <- sem(frmed.model, data = fdmcforsem, estimator = "ML", 
                 missing = "FIML", se = "bootstrap",
                 bootstrap = 5000L,parallel ="multicore", verbose= T)
summary(frmed.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)

estimates_frmed.fit <- parameterEstimates(frmed.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_frmed.fit)
estimates_frmed.fit[62:63, ]



#freedom approach trust


frtaltint.model <- '
  needfreedom =~ security_freedom_4+security_freedom_5+security_freedom_6
  Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedfreedom=~ security_4+security_5+security_6
  #sfreetfree=~security_freedom_4.security_4+security_freedom_5.security_5+security_freedom_6.security_6

  institution_trust_5 ~ b1*Micronarratives+b2*Mainstream+gr_1+needfreedom
  Micronarratives ~a1*needfreedom+gr_1
  Mainstream ~ a2*needfreedom+gr_1
  needfreedom ~ a3*gr_1 +sfree.tfree+traitneedfreedom

      
  ind1 := a1*b1
  ind2 := a2*b2
  ind3:= a3*a1
  ind4:= a3*a2
  
  Mainstream~~Micronarratives

'

frtaltint.fit <- sem(frtaltint.model, data = fdmcforsem, estimator = "ML", 
                     missing = "FIML", se = "bootstrap",
                     bootstrap = 5000L,parallel ="multicore", verbose= T)
summary(frtaltint.fit, fit.measures=T, standardized = T, rsquare=TRUE,
       ci=T)
estimates_frtaltint.fit <- parameterEstimates(frtaltint.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                              zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_frtaltint.fit)
write.csv(estimates_frtaltint.fit, "estimates_frtaltint.fit.csv")
```


#freedom agreement trust



frtalt.model <- '
  needfreedom =~ security_freedom_4+security_freedom_5+security_freedom_6
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedfreedom=~ security_4+security_5+security_6
  #sfreetfree=~security_freedom_4.security_4+security_freedom_5.security_5+security_freedom_6.security_6

  institution_trust_5 ~ b1*Micronarratives+b2*Mainstream+gr_1+needfreedom
  Micronarratives ~a1*needfreedom+gr_1
  Mainstream ~ a2*needfreedom+gr_1
  needfreedom ~ a3*gr_1 +sfree.tfree+traitneedfreedom

      
  ind1 := a1*b1
  ind2 := a2*b2
  ind3:= a3*a1
  ind4:= a3*a2
  Mainstream~~Micronarratives

'


frtalt.fit <- sem(frtalt.model, data = fdmcforsem, estimator = "ML", 
                  missing = "FIML", se = "bootstrap",
                  bootstrap = 5000L,parallel ="multicore", verbose= T)
summary(frtalt.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)
estimates_frtalt.fit <- parameterEstimates(frtalt.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                           zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_talt.fit)


#freedom approach trust active media use

dmcforsem <- forsem

#double mean centering
library(semTools)
dmcforsem <- indProd(dmcforsem, var1= c("security_freedom_4", "security_freedom_5", "security_freedom_6"),
                     var2=c("active_soc_media"),
                     match = FALSE , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 
frintamed.model <- '
  needfreedom =~ security_freedom_4+security_freedom_5+security_freedom_6
  Mainstream=~Nar_5correct  +nar_6correct  +nar_7correct  +nar_8correct  
  Micronarratives=~nar_1+nar_2+nar_3+ nar_4 
  traitneedfreedom=~ security_4+security_5+security_6
  interaction=~ security_freedom_4.active_soc_media +security_freedom_5.active_soc_media + security_freedom_6.active_soc_media 
  
  
  Micronarratives ~needfreedom+active_soc_media+interaction+gr_1
  Mainstream ~needfreedom+active_soc_media+interaction+gr_1
  active_soc_media ~ needfreedom

  institution_trust_5 ~ Micronarratives
  institution_trust_5 ~Mainstream
  Micronarratives ~ gr_1
  Mainstream ~gr_1

  needfreedom ~ gr_1
  institution_trust_5~ gr_1 
  institution_trust_5 ~ needfreedom
  
  needfreedom~~traitneedfreedom
  Mainstream~~Micronarratives

'

frintamed.fit <- sem(frintamed.model, data = dmcforsem, estimator = "MLM")
summary(frintamed.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_frintamed.fit <- parameterEstimates(frintamed.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                              zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_frintamed.fit)

```


freedom agreement trust active media use


dmcforsem <- forsem

#double mean centering
library(semTools)
dmcforsem <- indProd(dmcforsem, var1= c("security_freedom_4", "security_freedom_5", "security_freedom_6"),
                     var2=c("active_soc_media"),
                     match = FALSE , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 
fragamed.model <- '
  needfreedom =~ security_freedom_4+security_freedom_5+security_freedom_6
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedfreedom=~ security_4+security_5+security_6
  interaction=~ security_freedom_4.active_soc_media +security_freedom_5.active_soc_media + security_freedom_6.active_soc_media 
  
  
  Micronarratives ~needfreedom+traitneedfreedom+active_soc_media+interaction+gr_1
  Mainstream ~needfreedom+traitneedfreedom+active_soc_media+interaction+gr_1
  active_soc_media ~ needfreedom

  institution_trust_5 ~ Micronarratives
  institution_trust_5 ~Mainstream
  Micronarratives ~ gr_1
  Mainstream ~gr_1

  needfreedom ~ gr_1
  institution_trust_5~ gr_1 
  institution_trust_5 ~ needfreedom
  
  needfreedom~~traitneedfreedom
  Mainstream~~Micronarratives

'

fragamed.fit <- sem(fragamed.model, data = dmcforsem, estimator = "MLM")
summary(fragamed.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_fragamed.fit <- parameterEstimates(fragamed.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                             zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_fragamed.fit)

```




#MEDIA VARIABLES

#Moderation active media use likes and dislikes


dmcforsem <- forsem

#double mean centering
library(semTools)
dmcforsem <- indProd(dmcforsem, var1= c("security_freedom_1", "security_freedom_2", "security_freedom_3"),
                     var2=c("active_soc_media"),
                     match = FALSE , meanC = TRUE ,
                     residualC = FALSE , doubleMC = TRUE) 
#both: UNUSED
ldl.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  likeMainstream=~q5_sm_2 +q6_sm_2+q7_sm_2 #+q8_sm_2 
 likeMicronarratives=~q1_sm_2 +q2_sm_2+q3_sm_2#+q4_sm_2 
  dislikeMainstream=~q5_sm_6 +q6_sm_6+q7_sm_6#+q8_sm_6 
  dislikeMicronarratives=~q1_sm_6 +q2_sm_6+q3_sm_6#+q4_sm_6 
  traitneedsecurity=~ security_1+security_2+security_3
    interaction=~ security_freedom_1.active_soc_media +security_freedom_2.active_soc_media + security_freedom_3.active_soc_media 


  
  likeMicronarratives ~a1*needsecurity+traitneedsecurity+gr_1+active_soc_media+interaction
  likeMainstream ~a2*needsecurity+traitneedsecurity+gr_1+active_soc_media+interaction

  dislikeMicronarratives ~a3*needsecurity+traitneedsecurity+gr_1+active_soc_media+interaction
  dislikeMainstream ~a4*needsecurity+traitneedsecurity+gr_1+active_soc_media+interaction

institution_trust_5 ~ b1*likeMicronarratives
institution_trust_5 ~b2*likeMainstream

institution_trust_5 ~ b3*dislikeMicronarratives
institution_trust_5 ~b4*dislikeMainstream

needsecurity ~ gr_1
institution_trust_5~ gr_1
institution_trust_5 ~ needsecurity

likeMainstream~~dislikeMainstream
likeMainstream~~dislikeMicronarratives
likeMicronarratives~~dislikeMicronarratives
likeMicronarratives~~dislikeMainstream

q3_sm_2~~q3_sm_6
q1_sm_2~~q1_sm_6
q7_sm_2~~q7_sm_6
q6_sm_2~~q6_sm_6
q5_sm_2~~q5_sm_6
q2_sm_2~~q2_sm_6
#q4_sm_2~~q4_sm_6
#q8_sm_2~~q8_sm_6




ind1 := a1*b1
ind2 := a2*b2
ind3 := a3*b3
ind4 := a4*b4

'

ldl.fit <- sem(ldl.model, data = dmcforsem, estimator = "ML", missing = "FIML", se = "bootstrap",bootstrap = 500L, parallel ="snow")
summary(ldl.fit, fit.measures=T, standardized = T, rsquare=TRUE)


estimates_ldl.fit <- parameterEstimates(ldl.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_ldl.fit)
mi<- modificationindices(ldl.fit)


#Number of Likes and Dislikes, need for security, active media use


set.seed(545)

ldla.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  likeMainstream=~mainstreamlikes           #q5_sm_2 +q6_sm_2+q7_sm_2 +q8_sm_2 
  likeMicronarratives=~microlikes            #q1_sm_2 +q2_sm_2+q3_sm_2+q4_sm_2 
  dislikeMainstream=~mainstreamdislikes     #q5_sm_6 +q6_sm_6+q7_sm_6#+q8_sm_6 
  dislikeMicronarratives=~microdislikes     #q1_sm_6 +q2_sm_6+q3_sm_6+q4_sm_6 
  traitneedsecurity=~ security_1+security_2+security_3


  
  likeMicronarratives ~a1*needsecurity+gr_1+c1*active_soc_media
  likeMainstream ~a2*needsecurity+gr_1+c2*active_soc_media

  dislikeMicronarratives ~a3*needsecurity+gr_1+c3*active_soc_media
  dislikeMainstream ~a4*needsecurity+gr_1+c4*active_soc_media

  institution_trust_5 ~ b1*likeMicronarratives
  institution_trust_5 ~b2*likeMainstream
  
  institution_trust_5 ~ b3*dislikeMicronarratives
  institution_trust_5 ~b4*dislikeMainstream
  
  needsecurity ~ gr_1 +ssec.tsec+traitneedsecurity
  institution_trust_5~ gr_1
  institution_trust_5 ~ needsecurity +active_soc_media

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

ldla.fit <- sem(ldla.model, data = dmcforsem, estimator = "ML", 
                missing = "FIML", se = "bootstrap",bootstrap = 5000L, 
                parallel ="multicore", verbose= T)
summary(ldla.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


estimates_ldla.fit <- parameterEstimates(ldla.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,zstat = FALSE, pvalue = FALSE, output = "data.frame")
write.csv(estimates_ldla.fit, "estimates_ldla.fit.csv")

View(estimates_ldla.fit)
mi<- modificationindices(ldla.fit)


#visualisation

sm_no_covs <- semptools::drop_nodes(
  semPlotModel(ldla.fit),
  c("security_freedom_1","security_freedom_2","security_freedom_3",
    "q4_sm_2" ,"q5_sm_2" ,"q6_sm_2","q7_sm_2" ,"q7_sm_2" ,"q8_sm_2",
    "q1_sm_2" ,"q2_sm_2","q3_sm_2",
    "q4_sm_6" ,"q5_sm_6" ,"q6_sm_6","q7_sm_6",
    "q1_sm_6","q2_sm_6","q3_sm_6",
    "security_1","security_2","security_3", "traitneedsecurity", "gr_1"))
aaaaa<- semPlotModel(ldla.fit)
s_pfad_layout <- get_layout(
  "","","likeMicronarratives","","","",
  "traitneedsecurity","","", "", "","",
  "needsecurity","","","dislikeMicronarratives","","",
  "","","","","","institution_trust_5",
  "","","","dislikeMainstream","","",
  "active_soc_media","","","","","",
  "","","likeMainstream","","","q8_sm_2",
  "security_freedom_1","security_freedom_2","security_freedom_3","q5_sm_2" ,"q6_sm_2","q7_sm_2" ,
  "q1_sm_2" ,"q2_sm_2","q3_sm_2","q5_sm_6" ,"q6_sm_6","q7_sm_6",
  "q1_sm_6","q2_sm_6","q3_sm_6","security_1","security_2","security_3",
  rows = 10)




sm <- semPlot::semPaths(sm_no_covs, what= "std", layout = s_pfad_layout, residuals = FALSE,  nCharNodes = 0, fade= FALSE, sizeLat = 8,sizeMan = 6, label.cex = 1.8, edge.label.cex =1,intercepts = F, equalizeManifests=F, exoCov = F, covAtResiduals=F)



#add statistical significance asterisks
library(semptools)
my_label_list <- list(#list(node = "traitneedsecurity", to = "(T) Need for \nsecurity"),
  list(node = "needsecurity", to = "(S) Need for \nsecurity"),
  list(node = "institution_trust_5", to = "Trust in \nthe government"),
  list(node = "likeMainstream", to = "Likes on \nMainstream \nPosts"),
  list(node = "dislikeMainstream", to = "Dislikes on \nMainstream \nPosts"),
  list(node = "likeMicronarratives", to = "Likes on \nAnti-mainstream \nPosts"),
  list(node = "dislikeMicronarratives", to = "Dislikes on \nAnti-mainstream \nPosts"),
  # list(node = "gr_1", to = "Group \n(vol=0, mand=1)"),
  list(node = "active_soc_media", to = "Active Media\n Use"))


sp_pa2 <- mark_sig(sm, ldla.fit)
sp_pa2 <- change_node_label(sp_pa2, my_label_list)
plot(sp_pa2)



#Number of Likes and Dislikes, need for freedom, active media use

set.seed(545)
#both
fldl.model <- '
  needfreedom =~ security_freedom_4+security_freedom_5+security_freedom_6
  likeMainstream=~mainstreamlikes           #q5_sm_2 +q6_sm_2+q7_sm_2 +q8_sm_2 
  likeMicronarratives=~microlikes            #q1_sm_2 +q2_sm_2+q3_sm_2+q4_sm_2 
  dislikeMainstream=~mainstreamdislikes     #q5_sm_6 +q6_sm_6+q7_sm_6#+q8_sm_6 
  dislikeMicronarratives=~microdislikes     #q1_sm_6 +q2_sm_6+q3_sm_6+q4_sm_6 
  traitneedfreedom=~ security_4+security_5+security_6

  
    
  likeMicronarratives ~a1*needfreedom+gr_1+c1*active_soc_media
  likeMainstream ~a2*needfreedom+gr_1+c2*active_soc_media

  dislikeMicronarratives ~a3*needfreedom+gr_1+c3*active_soc_media
  dislikeMainstream ~a4*needfreedom+gr_1+c4*active_soc_media

  institution_trust_5 ~ b1*likeMicronarratives
  institution_trust_5 ~b2*likeMainstream

  institution_trust_5 ~ b3*dislikeMicronarratives
  institution_trust_5 ~b4*dislikeMainstream
  
  needfreedom ~ gr_1+sfree.tfree+traitneedfreedom
  institution_trust_5~ gr_1
  institution_trust_5 ~ needfreedom+active_soc_media
  
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

fldl.fit <- sem(fldl.model, data = fdmcforsem, estimator = "ML", 
                missing = "FIML", se = "bootstrap",bootstrap = 5000L, 
                parallel ="multicore", verbose= T)
summary(fldl.fit, fit.measures=T, standardized = T, rsquare=TRUE, ci=T)


estimates_fldl.fit <- parameterEstimates(fldl.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,zstat = FALSE, pvalue = FALSE, output = "data.frame")
write.csv(estimates_fldl.fit, "estimates_fldl.fit.csv")


View(estimates_fldl.fit)

















































