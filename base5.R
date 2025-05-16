


####analyses

##anova
#1 obligatory
#2 voluntary
#3 control

#H1: Participants in the mandatory condition will perceive a higher threat for 
#financial security in comparison to those in the voluntary and control conditions. 

#3 group
groupcom <- aov(ssec ~ gr+tsec, data=mand)
summary(groupcom)
TukeyHSD(groupcom)

#only manipulation
groupcom_man <- aov(ssec ~ gr+tsec, data=onlymanipulation)
summary(groupcom_man)
TukeyHSD(groupcom_man)

#comparisons with control and between the experimental conditions
#are significant

#3 group
groupcom_free <- aov(sfree ~ gr, data=mand)
summary(groupcom_free)
TukeyHSD(groupcom_free)

#only manipulation
groupcom_manfree <- aov(sfree ~ gr+tfree, data=onlymanipulation)
summary(groupcom_manfree)
TukeyHSD(groupcom_manfree)



mand$gr <- ordered(mand$gr, levels = c("1", "2", "3"))


##narratives####

#H2: The perceived threat to financial security after the manipulation will 
#mediate the link between the type of policy and the choice of narratives. 
#Meaning participants in the mandatory condition will perceive a higher threat to 
#their financial security and will thereby choose micro narratives more often than
#those in the voluntary condition.

#anova + pairwise comparisons
mainst <- aov(mainstream ~ gr, data=onlymanipulation)
summary(mainst)
TukeyHSD(mainst)

micron <- aov(micronarratives ~ gr, data=onlymanipulation)
summary(micron)
TukeyHSD(micron)
#no difference between experimental and control groups 





#trajectory of need for security values
list(mand[mand$gr==3,]$ssec)

library(plyr)
legend_title <- "Group"
cdat <- ddply(mand, "gr", summarise, rating.mean=mean(ssec))
cdat
ggplot(mand, aes(x=ssec, color=gr)) + geom_density()+
  labs(color = "Group")+
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=gr),linetype="dashed", linewidth=1)+
  xlab("Need for Security (State)")+
  scale_color_manual(labels = c("mandatory", "voluntary", "control"), values = c( "red","blue","lightgreen")) 


#H5: The perceived effectiveness of the proposed policy will not differ between 
#the mandatory and the voluntary conditions.
a <- aov(man_eff~gr, data=onlymanipulation)
summary(a)
TukeyHSD(a)
#mandatory policy seen as more effective

library(plyr)
legend_title <- "Group"
cdat <- ddply(onlymanipulation, "gr", summarise, rating.mean=mean(man_eff))
cdat
ggplot(onlymanipulation, aes(x=man_eff, color=gr)) + geom_density()+
  labs(color = "Group")+
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=gr),linetype="dashed", linewidth=1)+
  xlab("Need for Security (State)")+
  scale_color_manual(labels = c("voluntary", "mandatory", "control"), values = c( "red","lightgreen")) 

with(onlymanipulation, aggregate(man_acc ~ gr, FUN = mean))

#H6: Participants in the mandatory condition will perceive the policy to be less 
#acceptable in comparison to those in the voluntary and control conditions.

b <- aov(man_acc~gr, data=onlymanipulation)
summary(b)
TukeyHSD(b)


library(plyr)
legend_title <- "Group"
cdat <- ddply(onlymanipulation, "gr", summarise, rating.mean=mean(man_acc))
cdat
ggplot(onlymanipulation, aes(x=man_acc, color=gr)) + geom_density()+
  labs(color = "Group")+
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=gr),linetype="dashed", linewidth=1)+
  xlab("Need for Security (State)")+
  scale_color_manual(labels = c("voluntary", "mandatory", "control"), values = c( "red","lightgreen")) 



#(exploratory) H3:The strength of the relationship between trust in 
#institutions and perceived threat to security will vary by experimental group.
install.packages("fastDummies")

# Load the library
library(fastDummies)


# Create dummy variable
onlymanipulation <- fastDummies::dummy_cols(onlymanipulation, 
                               select_columns = "gr")
mand <- dummy_cols(mand, select_columns = "gr")

#1- Science
#2 - Public media
#3 - Ppl on social media
#4 -EU
#5 - Poland


library(ggeffects)

#two group
m2 <- lm(ssec~ gr_1*institution_trust_4+tsec, data=onlymanipulation)
summary(m2)
ggpredict(m2, terms = c("institution_trust_4[1:7 by=0.1]", "gr_1")) |> plot()

#non-sig
m2.1 <- lm(ssec~ gr_1*institution_trust_5+tsec, data=onlymanipulation)
summary(m2.1)
#almost

ggpredict(m2.1, terms = c("institution_trust_5[1:7 by=0.1]", "gr_1")) |> plot()

#(exploratory) H4: Active media use will moderate the relationship between 
#perceived threat to security and the choice of micro and mainstream narratives.

m3 <- lm(micronarratives~ ssec*active_soc_media+tsec, data=onlymanipulation)
summary(m3)
ggpredict(m3, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()
#non-sig

m3 <- lm(mainstream~ ssec*active_soc_media+tsec, data=onlymanipulation)
summary(m3)
ggpredict(m3, terms = c("ssec[1:7 by=0.1]", "active_soc_media[1:6 by=2]")) |> plot()

#non-sig
#ssec mildly significant (when interaction removed active media use almost significant)
#trait security is significant



#H2: The perceived threat to financial security after the manipulation will 
#mediate the link between the type of policy and the choice of narratives. 
#Meaning participants in the mandatory condition will perceive a higher threat to 
#their financial security and will thereby choose micro narratives more often than
#those in the voluntary condition.

#direct
path_a <- lm(mainstream ~ gr_2, data=onlymanipulation)
summary(path_a)

path_b_c <- lm(mainstream ~ gr_2+ssec, data=onlymanipulation)
summary(path_b_c)

install.packages("mediation")
library(mediation)

results <- mediate(path_a, path_b_c, 
                   treat = "gr_2", mediator = "ssec", 
                   boot = TRUE)
summary(results)




#direct
path_a1 <- lm(micronarratives ~ gr_1, data=onlymanipulation)
summary(path_a1)

path_b1_c1 <- lm(micronarratives ~ gr_1+ssec, data=onlymanipulation)
summary(path_b1_c1)

install.packages("mediation")
library(mediation)

results <- mediate(path_a1, path_b1_c1, 
                   treat = "gr_1", mediator = "ssec", 
                   boot = TRUE)
summary(results)

###SEM#####

  
  
#with dummy variable 


mess.model <- '
  security =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitsecurity=~ security_1+security_2+security_3


'
mess.fit <- sem(mess.model, data = forsem, estimator = "MLR", missing = "FIML",
             group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(mess.fit, fit.measures=T, standardized = T)


semPlot::semPaths(mess.fit,  intercepts = F, residuals = F, what = "std", layout="tree",
                  rotation=1)
?semPaths()



med.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedsecurity=~ security_1+security_2+security_3

  Micronarratives ~b1*needsecurity+traitneedsecurity
  Mainstream ~b2*needsecurity+traitneedsecurity


  needsecurity ~ a1*gr_2
  Micronarratives ~ gr_2
  Mainstream ~gr_2
  
ind1 := a1*b1
ind2 := a1*b2

'

med.fit <- sem(med.model, data = forsem, estimator = "ML", missing = "FIML",
               se = "bootstrap",bootstrap = 500L, parallel ="snow")
summary(med.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_med.fit <- parameterEstimates(med.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                            zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_med.fit)
estimates_med.fit[61:62, ]




pfad_layout<- get_layout("","traitsecurity", "", "","Micronarratives",
                         "","", "","","",
                         "gr_2","","","","",
                         "","","","","Mainstream",
                         "","security","","","",
                         rows = 5)

tidySEM::graph_sem(model = m.fit_final, layout = pfad_layout) 


library(knitr)


table_fit <- matrix(NA, nrow = 5, ncol = 9)
colnames(table_fit) = c("Model", "X2", "df", "CFI", "RMSEA", "SRMR", "X2 Diff",
                        "Pr(>X2)", "AIC")
table_fit[1, ] <- c("Overall Model", round(fitmeasures(m.fit, 
                                                       c("chisq", "df", "cfi",
                                                         "rmsea", "srmr")),3),
                    NA, NA, round((a[1,2])))

table_fit[2, ] <- c("Constrain all ~ gr", round(fitmeasures(m.fit_a, 
                                                            c("chisq", "df", "cfi",
                                                              "rmsea", "srmr")),3),
                    round((a[2,5]),3), round((a[2,7]),3), round((a[2,2])))
table_fit[3, ] <- c("Constrain ssec~gr", round(fitmeasures(m.fit_b, 
                                                           c("chisq", "df", "cfi",
                                                             "rmsea", "srmr")),3),
                    round((b[2,5]),3), round((b[2,7]),3),round((b[2,2])))
table_fit[4, ] <- c("Constrain Micro~gr", round(fitmeasures(m.fit_c, 
                                                            c("chisq", "df", "cfi",
                                                              "rmsea", "srmr")),3),
                    round((c[2,5]),3), round((c[2,7]),3),round((c[2,2])))
table_fit[5, ] <- c("Constrain mainstream~gr", round(fitmeasures(m.fit_d, 
                                                                 c("chisq", "df", "cfi",
                                                                   "rmsea", "srmr", "")),3), 
                    round((d[2,5]),3), round((d[2,7]),3), round((d[2,2])))


kable(table_fit)



t.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedsecurity=~ security_1+security_2+security_3

  Micronarratives ~needsecurity+traitneedsecurity+institution_trust_5
  Mainstream ~needsecurity+traitneedsecurity+institution_trust_5


  needsecurity ~ gr_2
  institution_trust_5~ gr_2 
  institution_trust_5 ~ needsecurity
  

'


t.fit <- sem(t.model, data = forsem, estimator = "MLR", missing = "FIML",
             group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(t.fit, fit.measures=T, standardized = T)


pfad_layout<- get_layout("","traitneedsecurity", "", "","Micronarratives",
                         "","", "","","",
                         "gr_2","institution_trust_5","","","",
                         "","","","","Mainstream",
                         "needsecurity","","","","",
                         rows = 5)

tidySEM::graph_sem(model = t.fit, layout = pfad_layout) 




tmed.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedsecurity=~ security_1+security_2+security_3

  Micronarratives ~needsecurity+traitneedsecurity+b1*institution_trust_5
  Mainstream ~needsecurity+traitneedsecurity+b2*institution_trust_5


  needsecurity ~ gr_2
  institution_trust_5~ gr_2 
  institution_trust_5 ~ a1*needsecurity
      
ind1 := a1*b1
ind2 := a1*b2

'


tmed.fit <- sem(tmed.model, data = forsem, estimator = "ML", missing = "FIML",
                se = "bootstrap",bootstrap = 500L, parallel ="snow")
summary(tmed.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_tmed.fit <- parameterEstimates(tmed.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                        zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_tmed.fit)




estimates_tmed.fit[65:66, ]



talt.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedsecurity=~ security_1+security_2+security_3

  institution_trust_5 ~ b1*Micronarratives
  institution_trust_5 ~b2*Mainstream
  Micronarratives ~a1*needsecurity+traitneedsecurity
  Mainstream ~ a2*needsecurity+traitneedsecurity


  needsecurity ~ gr_2
  institution_trust_5~ gr_2 
  institution_trust_5 ~ needsecurity
      
ind1 := a1*b1
ind2 := a2*b2

'


talt.fit <- sem(talt.model, data = forsem, estimator = "ML", missing = "FIML",
                se = "bootstrap",bootstrap = 500L, parallel ="snow")
summary(talt.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_talt.fit <- parameterEstimates(talt.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                         zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_talt.fit)




estimates_talt.fit[64:65, ]



pfad_layout<- get_layout("","traitneedsecurity", "Micronarratives", "","",
                         "","", "","","",
                         "gr_2","","","","institution_trust_5",
                         "","","","Mainstream","",
                         "needsecurity","","","","",
                         rows = 5)

tidySEM::graph_sem(model = talt.fit, layout = pfad_layout) 




table_fit <- matrix(NA, nrow = 5, ncol = 9)
colnames(table_fit) = c("Model", "X2", "df", "CFI", "RMSEA", "SRMR", "X2 Diff",
                        "Pr(>X2)", "AIC")
table_fit[1, ] <- c("Overall Model", round(fitmeasures(t.fit, 
                                                       c("chisq", "df", "cfi",
                                                         "rmsea", "srmr")),3),
                    NA, NA, round((a[1,2])))

table_fit[2, ] <- c("Micronarratives ~0*institution_trust_5", round(fitmeasures(t.fit1, 
                                                            c("chisq", "df", "cfi",
                                                              "rmsea", "srmr")),3),
                    round((t1[2,5]),3), round((t1[2,7]),3), round((t1[2,2])))
table_fit[3, ] <- c("Mainstream ~0*institution_trust_5", round(fitmeasures(t.fit3, 
                                                            c("chisq", "df", "cfi",
                                                              "rmsea", "srmr")),3),
                    round((t3[2,5]),3), round((t3[2,7]),3),round((t3[2,2])))
table_fit[4, ] <- c("institution_trust_5~ 0*gr_2 ", round(fitmeasures(t.fit4, 
                                                                 c("chisq", "df", "cfi",
                                                                   "rmsea", "srmr", "")),3), 
                    round((t4[2,5]),3), round((t4[2,7]),3), round((t4[2,2])))
table_fit[5, ] <- c("institution_trust_5 ~ 0*needsecurity", round(fitmeasures(t.fit5, 
                                                                 c("chisq", "df", "cfi",
                                                                   "rmsea", "srmr", "")),3), 
                    round((t5[2,5]),3), round((t5[2,7]),3), round((t5[2,2])))

kable(table_fit)











pfad_layout<- get_layout("","traitsecurity", "", "","Micronarratives",
                         "","", "","","",
                         "gr_2","","","","",
                         "","","","","Mainstream",
                         "institution_trust_5","security","","","",
                         rows = 5)

tidySEM::graph_sem(model = t.fit, layout = pfad_layout) 





#active media use

amed.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedsecurity=~ security_1+security_2+security_3

  
  Micronarratives ~needsecurity+traitneedsecurity+institution_trust_5+b1*active_soc_media
  Mainstream ~needsecurity+traitneedsecurity+institution_trust_5+b2*active_soc_media
  active_soc_media ~ a1*needsecurity

  needsecurity ~ gr_2
  institution_trust_5~ gr_2 
  institution_trust_5 ~ needsecurity
      
ind1 := a1*b1
ind2 := a1*b2

'


amed.fit <- sem(amed.model, data = forsem, estimator = "ML", missing = "FIML",
                se = "bootstrap",bootstrap = 500L, parallel ="snow")
summary(amed.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_amed.fit <- parameterEstimates(amed.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                         zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_amed.fit)




estimates_amed.fit[70:71, ]



pfad_layout<- get_layout("","traitneedsecurity", "", "","Micronarratives",
                         "","", "","","",
                         "gr_2","institution_trust_5","","active_soc_media","",
                         "","","","","",
                         "needsecurity","","","","Mainstream",
                         rows = 5)

tidySEM::graph_sem(model = a.fit, layout = pfad_layout) 





#passive media use

p.model <- '
     security =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitsecurity=~ security_1+security_2+security_3

  Micronarratives ~security+traitsecurity+passive_soc_media
  Mainstream ~security+traitsecurity+passive_soc_media
  passive_soc_media ~ security
    passive_soc_media ~ traitsecurity




  security ~ gr_2
  security ~ institution_trust_5
  gr_2 ~ institution_trust_5
'
p.fit <- sem(p.model, data = forsem, estimator = "MLR", missing = "FIML",
             group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(p.fit, fit.measures=T, standardized = T)





pfad_layout<- get_layout("","traitsecurity", "", "","Micronarratives",
                         "","", "","","",
                         "gr_2","passive_soc_media","","","",
                         "","","","","Mainstream",
                         "trust5","security","","","",
                         rows = 5)

tidySEM::graph_sem(model = a.fit, layout = pfad_layout) 




#further analyses

cor(aa$antim_narr_intr, aa$antim_narrat_endors_avg, use = "pairwise.complete.obs")
#0.6361388

cor(aa$main_narrat_endors_avg, aa$Mainstream_Interest_avg, use = "pairwise.complete.obs")
#0.7389155

cor(aa$antim_narr_intr, aa$Mainstream_Interest_avg, use = "pairwise.complete.obs")
#0.4904767

cor(aa$main_narrat_endors_avg, aa$antim_narrat_endors_avg, use = "pairwise.complete.obs")
#0.0274881

#interest might not be two distinct factors between micro and main




cor(aa$antim_endorsement_soc_media, aa$antim_narrat_endors_avg, use = "pairwise.complete.obs")
#-0.4555952
cor(aa$main_narrat_endors_avg, aa$main_endorsement_soc_media, use = "pairwise.complete.obs")
#0.5696791

cor(aa$antim_endorsement_soc_media, aa$main_endorsement_soc_media, use = "pairwise.complete.obs")
# 0.05329129







# testing with social media variables

microlikes <- subset(forsem[,c("q1_sm_2","q2_sm_2","q3_sm_2","q4_sm_2")])
forsem$microlikes <- apply(microlikes, 1, sum, na.rm=T)

mainstreamlikes <- subset(forsem[,c("q5_sm_2","q6_sm_2","q7_sm_2","q8_sm_2")])
forsem$mainstreamlikes <- apply(mainstreamlikes, 1, sum, na.rm=T)

microdislikes <- subset(forsem[,c("q1_sm_6","q2_sm_6","q3_sm_6","q4_sm_6")])
forsem$microdislikes <- apply(microdislikes, 1, sum, na.rm=T)

mainstreamdislikes <- subset(forsem[,c("q5_sm_6","q6_sm_6","q7_sm_6","q8_sm_6")])
forsem$mainstreamdislikes <- apply(mainstreamdislikes, 1, sum, na.rm=T)

microshares <- subset(forsem[,c("q1_sm_3","q2_sm_3","q3_sm_3","q4_sm_3")])
forsem$microshares <- apply(microshares, 1, sum, na.rm=T)

mainstreamshares <- subset(forsem[,c("q5_sm_3","q6_sm_3","q7_sm_3","q8_sm_3")])
forsem$mainstreamshares <- apply(mainstreamshares, 1, sum, na.rm=T)

microcomment <- subset(forsem[,c("q1_sm_4","q2_sm_4","q3_sm_4","q4_sm_4")])
forsem$microcomment <- apply(microcomment, 1, sum, na.rm=T)

mainstreamcomment <- subset(forsem[,c("q5_sm_4","q6_sm_4","q7_sm_4","q8_sm_4")])
forsem$mainstreamcomment <- apply(mainstreamcomment, 1, sum, na.rm=T)

microhash <- subset(forsem[,c("q1_sm_5","q2_sm_5","q3_sm_5","q4_sm_5")])
forsem$microhash <- apply(microhash, 1, sum, na.rm=T)

mainstreamhash <- subset(forsem[,c("q5_sm_5","q6_sm_5","q7_sm_5","q8_sm_5")])
forsem$mainstreamhash <- apply(mainstreamhash, 1, sum, na.rm=T)




forsem1 <- forsem[, c(2:39, 52:86, 91:150, 180:187, 192:200, 204:213 )]
forsem1_n <- mutate_all(forsem1, function(x) as.integer(as.character(x)))
# centering with 'scale()'
center_scale <- function(x) {
  scale(x, center= T, scale = apply(x, 2, sd, na.rm = TRUE))
}
?scale
# apply it


forsem_s<- center_scale(forsem1_n)
forsem_s <- data.frame(forsem_s)
forsem_s$gr_1 <- forsem$gr_1
forsem_s$gr_2 <- forsem$gr_2


#likes (very small significant effect)
soc.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~q5_sm_2 +q6_sm_2+q7_sm_2+q8_sm_2 
  Micronarratives=~q1_sm_2 +q2_sm_2+q3_sm_2+q4_sm_2
  traitneedsecurity=~ security_1+security_2+security_3

  
  Micronarratives ~needsecurity+traitneedsecurity+gr_2
  Mainstream ~needsecurity+traitneedsecurity+gr_2


institution_trust_5 ~ Micronarratives
institution_trust_5 ~Mainstream
Micronarratives ~needsecurity+traitneedsecurity
Mainstream ~ needsecurity+traitneedsecurity


needsecurity ~ gr_2
institution_trust_5~ gr_2 
institution_trust_5 ~ needsecurity
      

'

soc.fit <- sem(soc.model, data = forsem, estimator = "ML")
summary(soc.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_amed.fit <- parameterEstimates(amed.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                         zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_amed.fit)

pfad_layout<- get_layout("","", "", "Micronarratives","",
                         "gr_2","", "","","institution_trust_5",
                         "","","","","",
                         "","","","Mainstream","",
                         "","needsecurity","","","",
                         rows = 5)


tidySEM::graph_sem(model = soc.fit, layout = pfad_layout) 


#dislikes 
soc1.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~q5_sm_6 +q6_sm_6+q7_sm_6+q8_sm_6 
  Micronarratives=~q1_sm_6 +q2_sm_6+q3_sm_6+q4_sm_6
  traitneedsecurity=~ security_1+security_2+security_3

  
  Micronarratives ~needsecurity+traitneedsecurity+gr_2
  Mainstream ~needsecurity+traitneedsecurity+gr_2


institution_trust_5 ~ Micronarratives
institution_trust_5 ~Mainstream
Micronarratives ~needsecurity+traitneedsecurity
Mainstream ~ needsecurity+traitneedsecurity


needsecurity ~ gr_2
institution_trust_5~ gr_2 
institution_trust_5 ~ needsecurity

'


soc1.fit <- sem(soc1.model, data = forsem, estimator = "ML")
summary(soc1.fit, fit.measures=T, standardized = T, rsquare=TRUE)



pfad_layout<- get_layout("","", "", "Micronarratives","",
                         "gr_2","", "","","institution_trust_5",
                         "","","","","",
                         "","","","Mainstream","",
                         "","needsecurity","","","",
                         rows = 5)



tidySEM::graph_sem(model = soc.fit, layout = pfad_layout) 

#both
ldl.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  likeMainstream=~q5_sm_2 +q6_sm_2+q7_sm_2 
 likeMicronarratives=~q1_sm_2 +q2_sm_2+q3_sm_2
  dislikeMainstream=~q5_sm_6 +q6_sm_6+q7_sm_6
  dislikeMicronarratives=~q1_sm_6 +q2_sm_6+q3_sm_6
  traitneedsecurity=~ security_1+security_2+security_3

  
  likeMicronarratives ~needsecurity+traitneedsecurity+gr_2
  likeMainstream ~needsecurity+traitneedsecurity+gr_2

  dislikeMicronarratives ~needsecurity+traitneedsecurity+gr_2
  dislikeMainstream ~needsecurity+traitneedsecurity+gr_2

institution_trust_5 ~ likeMicronarratives
institution_trust_5 ~likeMainstream
likeMicronarratives ~needsecurity+traitneedsecurity
likeMainstream ~ needsecurity+traitneedsecurity

institution_trust_5 ~ dislikeMicronarratives
institution_trust_5 ~dislikeMainstream
dislikeMicronarratives ~needsecurity+traitneedsecurity
dislikeMainstream ~ needsecurity+traitneedsecurity

needsecurity ~ gr_2
institution_trust_5~ gr_2 
institution_trust_5 ~ needsecurity
      dislikeMicronarratives~~likeMicronarratives
dislikeMainstream~~likeMainstream
      dislikeMicronarratives~~likeMainstream
dislikeMainstream~~likeMicronarratives

'

ldl.fit <- sem(ldl.model, data = forsem, estimator = "ML")
summary(ldl.fit, fit.measures=T, standardized = T, rsquare=TRUE)



pfad_layout<- get_layout("","", "", "likeMicronarratives","",
                         "","", "","","",
                         "","","","","institution_trust_5",
                         "gr_2","needsecurity","","dislikeMicronarratives","",
                         "","","","dislikeMainstream","",
                         "","","","","",
                         "","","","likeMainstream","",
                         "","","","","",
                         rows = 8)



tidySEM::graph_sem(model = ldl.fit, layout = pfad_layout) 




#both
gdd.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  traitneedsecurity=~ security_1+security_2+security_3

  
  antim_endorsement_soc_media ~needsecurity+traitneedsecurity+gr_2
  main_endorsement_soc_media ~needsecurity+traitneedsecurity+gr_2

 institution_trust_5 ~ antim_endorsement_soc_media
institution_trust_5 ~main_endorsement_soc_media

needsecurity ~ gr_2
institution_trust_5~ gr_2 
institution_trust_5 ~ needsecurity
      antim_endorsement_soc_media~~main_endorsement_soc_media

'

gdd.fit <- sem(gdd.model, data = forsem, estimator = "ML")
summary(gdd.fit, fit.measures=T, standardized = T, rsquare=TRUE)



pfad_layout<- get_layout("","", "", "antim_endorsement_soc_media","",
                         "","", "","","",
                         "","","","","institution_trust_5",
                         "gr_2","","needsecurity","","",
                         "","","","","",
                         "","","","main_endorsement_soc_media","",
                         "","","","","",
                         "","","","","",
                         rows = 8)

tidySEM::graph_sem(model = gdd.fit, layout = pfad_layout) 



#both
all.model <- '
  #measurement model
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  likeMainstream=~q5_sm_2 +q6_sm_2+q7_sm_2 
  likeMicronarratives=~q1_sm_2 +q2_sm_2+q3_sm_2
  dislikeMainstream=~q5_sm_6 +q6_sm_6+q7_sm_6
  dislikeMicronarratives=~q1_sm_6 +q2_sm_6+q3_sm_6
  traitneedsecurity=~ security_1+security_2+security_3

  #regressions
  likeMicronarratives ~needsecurity+traitneedsecurity+gr_2
  likeMainstream ~needsecurity+traitneedsecurity+gr_2

  dislikeMicronarratives ~needsecurity+traitneedsecurity+gr_2
  dislikeMainstream ~needsecurity+traitneedsecurity+gr_2

  institution_trust_5 ~ Micronarratives
  institution_trust_5 ~Mainstream
  likeMicronarratives ~needsecurity+traitneedsecurity
  likeMainstream ~ needsecurity+traitneedsecurity
  
  institution_trust_5 ~ dislikeMicronarratives
  institution_trust_5 ~dislikeMainstream
  dislikeMicronarratives ~needsecurity+traitneedsecurity
  dislikeMainstream ~ needsecurity+traitneedsecurity
  
  needsecurity ~ gr_2
  institution_trust_5~ gr_2 
  institution_trust_5 ~ needsecurity
  
  
  #covariances
  dislikeMicronarratives~~likeMicronarratives
  dislikeMainstream~~likeMainstream
  dislikeMicronarratives~~likeMainstream
  dislikeMainstream~~likeMicronarratives

'

ldl.fit <- sem(ldl.model, data = forsem, estimator = "ML")
summary(ldl.fit, fit.measures=T, standardized = T, rsquare=TRUE)





#both
rm.model <- '
  #measurement model
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  likeMainstream=~ mainstreamlikes 
  likeMicronarratives=~microlikes                
  dislikeMainstream=~mainstreamdislikes 
  dislikeMicronarratives=~microdislikes             
  traitneedsecurity=~ security_1+security_2+security_3

  #regressions
  likeMicronarratives ~needsecurity+traitneedsecurity+gr_2
  likeMainstream ~needsecurity+traitneedsecurity+gr_2

  dislikeMicronarratives ~needsecurity+traitneedsecurity+gr_2
  dislikeMainstream ~needsecurity+traitneedsecurity+gr_2

  institution_trust_5 ~ likeMicronarratives
  institution_trust_5 ~likeMainstream
  likeMicronarratives ~needsecurity+traitneedsecurity
  likeMainstream ~ needsecurity+traitneedsecurity
  
  institution_trust_5 ~ dislikeMicronarratives
  institution_trust_5 ~dislikeMainstream
  dislikeMicronarratives ~needsecurity+traitneedsecurity
  dislikeMainstream ~ needsecurity+traitneedsecurity
  
  needsecurity ~ gr_2
  institution_trust_5~ gr_2 
  institution_trust_5 ~ needsecurity
  
  
  #covariances
  dislikeMicronarratives~~likeMicronarratives
  dislikeMainstream~~likeMainstream
  dislikeMicronarratives~~likeMainstream
  dislikeMainstream~~likeMicronarratives
  likeMainstream~~likeMicronarratives


'

rm.fit <- sem(rm.model, data = forsem_s, estimator = "ML")
summary(rm.fit, fit.measures=T, standardized = T, rsquare=TRUE)




pfad_layout<- get_layout("gr_2","", "likeMicronarratives", "","",
                         "","", "","","",
                         "","","","","institution_trust_5",
                         "needsecurity","","","","",
                         "","","","","",
                         "","","","","",
                         "","","likeMainstream","","",
                         "","","","","",
                         rows = 8)


tidySEM::graph_sem(model = rm.fit, layout = pfad_layout) 

#####single factor solution#####
mand1 <- mand
mand1 <- mand %>% 
  mutate_at(c("nar1_end","nar2_end", "nar3_end", "nar4_end" ), 
            list(~ recode(., `1`=7, `2`=6, `3`=5, `4`=4,`5`=3, `6`=2, `7`=1)))

mand1$narratives


narratives <- subset(mand[,c(70,72,74,76,78,80,82,84)])
cor(narratives, use = "pairwise.complete.obs" )

narratives <- apply(micronarratives, 1, mean, na.rm=T)


##narratives####
#perform EFA
narr <- subset(mand1[,c(70,72,74,76,78,80,82,84)])
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


#still recommends 2 factors######




#invariance granted
#####ignore for sanity####

write.csv(mand, file = "mand.csv")













#H5: The perceived effectiveness of the proposed policy will not differ between 
#the mandatory and the voluntary conditions.
a <- aov(man_eff~gr, data=onlymanipulation)
summary(a)
TukeyHSD(a)
#mandatory policy seen as more effective

library(plyr)
legend_title <- "Group"
cdat <- ddply(onlymanipulation, "gr", summarise, rating.mean=mean(man_eff))
cdat
ggplot(onlymanipulation, aes(x=man_eff, color=gr)) + geom_density()+
  labs(color = "Group")+
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=gr),linetype="dashed", linewidth=1)+
  xlab("Need for Security (State)")+
  scale_color_manual(labels = c("voluntary", "mandatory", "control"), values = c( "red","lightgreen")) 

with(onlymanipulation, aggregate(man_acc ~ gr, FUN = mean))

#H6: Participants in the mandatory condition will perceive the policy to be less 
#acceptable in comparison to those in the voluntary and control conditions.

b <- aov(man_acc~gr, data=onlymanipulation)
summary(b)
TukeyHSD(b)
















