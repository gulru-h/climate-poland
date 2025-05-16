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

aa <- read_sav("corrected_dataset_preliminary_cleaned.sav")
raw <- read.csv("corrected_dataset_preliminary_cleaned.csv", sep = ";")

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

clean <- clean %>% mutate_if(is.character, as.numeric)

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

#acceptability assessed with one item

#save manipulation indicator as a factor
class(clean$gr)
clean$gr <- as.factor(clean$gr)
#1 control
#2 voluntary
#3 obligatory

mand<-clean
##########DO NOT TOUCH#######

###descriptives 

mand$political <- as.factor(mand$political)

##descriptives

table1 <-
  mand |> 
  gtsummary::tbl_summary(include = c(age, gender, SES),
                         by= NULL,
                         statistic = list(gtsummary::all_continuous() ~ "{mean} ({p25}, {p75})",
                                          gtsummary::all_categorical() ~"{n} ({p}%)"),)

table1 <-
  mand |> 
  gtsummary::tbl_summary(include = c(pol),
                         by= NULL,
                         statistic = list(gtsummary::all_continuous() ~ "{median} ({p25}, {p75})",
                                          gtsummary::all_categorical() ~"{n} ({p}%)"),)



#check if anyone in the sample already had exclusively climate friendly heaters 
table(mand[ , mand$heiz1==1 & mand[, c(40:45)] == 0]$heiz1)


mand$heiz1[mand$heiz1==2 & mand[, c(40:44)] == 0]

##correlations
#method1
cors <- subset(mand[,c(9:12, 61, 64:69 )])
cors1 <- cors[mand$gr==1, ]
cors2 <- cors[mand$gr==2, ]
cors3 <- cors[mand$gr==3, ]

M <- cor(cors)
M1<- cor(cors1)
M2<- cor(cors2)
M3<- cor(cors3)
corrplot(M1, method="number")
corrplot(M2, method="number")
corrplot(M3, method="number")

?corrplot

colnames(M) <- c("Left Wing Media", "Right Wing Media",
                 "Social Media", "Active media use",
                 "SES", "Financial Situation",
                 "(T)Need for Security", "(T)Need for Freedom", 
                 "(S)Need for Security", "(S)Need for Freedom",
                 "Acceptability")
rownames(M) <- c("Left Wing Media", "Right Wing Media",
                 "Social Media", "Active media use",
                 "SES", "Financial Situation",
                 "(T)Need for Security", "(T)Need for Freedom", 
                 "(S)Need for Security", "(S)Need for Freedom",
                 "Acceptability")


PLO<- corrplot(M, method="number", )

#method 2

df.corr <- psych::corr.test(cors, adjust = "none")

inter_corr_r <- df.corr$stars
inter_corr_r <- cbind(var = rownames(inter_corr_r), inter_corr_r)
inter_corr_r <- as.data.frame(inter_corr_r)

ft.corr <- flextable(inter_corr_r) |> 
  set_header_labels(var = "")
ft.corr


#method3

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = cors, mapping = mapping) + 
    geom_point(size= .1) + 
    theme(strip.text.x = element_text(size = .2),
          strip.text.y = element_text(size = .2)) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = cors, mapping = mapping) + 
    geom_point(size=.1) + 
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

g = ggpairs(cors, lower = list(continuous = my_fn),
            upper = list(continuous = wrap(ggally_cor, alignPercent = 0.8, digits=2)),
            columnLabels = c("Left Wing Media", "Right Wing Media",
                             "Social Media", "Active media use",
                             "SES", "Financial Situation",
                             "Trait Security", "Trait Freedom", 
                             "State Security", "State Freedom",
                             "Acceptability"
                             
            )) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g




####analyses
##anova
#1 control
#2 voluntary
#3 obligatory

#H1: Participants in the mandatory condition will perceive a higher threat for 
#financial security in comparison to those in the voluntary and control conditions. 

#3 group
groupcom <- aov(ssec ~ gr, data=mand)
summary(groupcom)
TukeyHSD(groupcom)

#only manipulation
groupcom_man <- aov(ssec ~ gr+tsec, data=onlymanipulation)
summary(groupcom_man)
TukeyHSD(groupcom_man)

#comparisons with control and between the expreminemtal conditions
#are significant

#3 group
groupcom_free <- aov(sfree ~ gr, data=mand)
summary(groupcom_free)
TukeyHSD(groupcom_free)

#only manipulation
groupcom_manfree <- aov(sfree ~ gr, data=onlymanipulation)
summary(groupcom_manfree)
TukeyHSD(groupcom_manfree)

##############pairwise comparisons
gtsummary::theme_gtsummary_mean_sd()

# function to add pairwise copmarisons to `tbl_summary()`
add_stat_pairwise <- function(data, variable, by, ...) {
  # calculate pairwise p-values
  pw <- pairwise.t.test(data[[variable]], data[[by]], p.adj = "none")
  
  # convert p-values to list
  index <- 0L
  p.value.list <- list()
  for (i in seq_len(nrow(pw$p.value))) {
    for (j in seq_len(nrow(pw$p.value))) {
      index <- index + 1L
      
      p.value.list[[index]] <- 
        c(pw$p.value[i, j]) %>%
        setNames(glue::glue("**{colnames(pw$p.value)[j]} vs. {rownames(pw$p.value)[i]}**"))
    }
  }
  
  # convert list to data frame
  p.value.list %>% 
    unlist() %>%
    purrr::discard(is.na) %>%
    t() %>%
    as.data.frame() %>%
    # formatting/roundign p-values
    dplyr::mutate(dplyr::across(everything(), gtsummary::style_pvalue))
}

mand %>%
  select(ssec, gr, tsec) %>%
  gtsummary::tbl_summary(by = gr, missing = "no") %>%
  # add pariwaise p-values
  gtsummary::add_stat(everything() ~ add_stat_pairwise) %>%
  print()



#anova results 
tbl <- 
  mand %>%
  select(ssec, gr, tsec) %>%
  gtsummary::tbl_summary(
    by = gr, 
    missing = "no"
  ) %>%
  gtsummary::add_p(gtsummary::all_continuous() ~ "oneway.test") %>%
  # add a header (which also unhides a hidden column)
  gtsummary::modify_header(statistic ~ "**Test Statistic**") %>%
  # add a function to format the column
  gtsummary::modify_fmt_fun(statistic ~ gtsummary::style_sigfig)
tbl |>
  gtsummary::modify_header(label = "**Variable**", p.value = "**P**") 


#make the grouping variable ordered
mand$gr <- ordered(mand$gr, levels = c("1", "2", "3"))



##narratives
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

micronarratives <- subset(mand[,c(70,72,74,76)])
mand$micronarratives <- apply(micronarratives, 1, mean, na.rm=T)

mainstream <- subset(mand[,c(78,80,82,84)])
mand$mainstream <- apply(mainstream, 1, mean, na.rm=T)

#H2: The perceived threat to financial security after the manipulation will 
#mediate the link between the type of policy and the choice of narratives. 
#Meaning participants in the mandatory condition will perceive a higher threat to 
#their financial security and will thereby choose micro narratives more often than
#those in the voluntary condition.

#anova + pairwise comparisons
mainst <- aov(mainstream ~ gr, data=mand)
summary(mainst)
TukeyHSD(mainst)

micron <- aov(micronarratives ~ gr, data=mand)
summary(micron)
TukeyHSD(micron)
#no difference between experimental and control groups 

#create a manipulation only group without the control group
rm(onlymanipulation)
onlymanipulation <- mand[mand$gr ==2|mand$gr ==3 , ]

#create the two levels 
onlymanipulation$gr <- ordered(onlymanipulation$gr, levels = c("2", "3"))

#not working
tbl <- 
  onlymanipulation %>%
  gtsummary::select(man_eff, gr) %>%
  gtsummary::tbl_summary(
    by = gr, 
    missing = "no"
  ) %>%
  gtsummary::add_p(gtsummary::all_continuous() ~ "oneway.test") %>%
  # add a header (which also unhides a hidden column)
  gtsummary::modify_header(statistic ~ "**Test Statistic**") %>%
  # add a function to format the column
  gtsummary::modify_fmt_fun(gtsummary::statistic ~ gtsummary::style_sigfig)
tbl |>
  gtsummary::modify_header(label = "**Variable**", p.value = "**P**") 



##############pairwise comparisons

gtsummary::theme_gtsummary_mean_sd()

# function to add pairwise comparisons to `tbl_summary()`
add_stat_pairwise <- function(data, variable, by, ...) {
  # calculate pairwise p-values
  pw <- pairwise.t.test(data[[variable]], data[[by]], p.adj = "none")
  
  # convert p-values to list
  index <- 0L
  p.value.list <- list()
  for (i in seq_len(nrow(pw$p.value))) {
    for (j in seq_len(nrow(pw$p.value))) {
      index <- index + 1L
      
      p.value.list[[index]] <- 
        c(pw$p.value[i, j]) %>%
        setNames(glue::glue("**{colnames(pw$p.value)[j]} vs. {rownames(pw$p.value)[i]}**"))
    }
  }
  
  # convert list to data frame
  p.value.list %>% 
    unlist() %>%
    purrr::discard(is.na) %>%
    t() %>%
    as.data.frame() %>%
    # formatting/roundign p-values
    dplyr::mutate(dplyr::across(everything(), gtsummary::style_pvalue))
}

onlymanipulation %>%
  select(micronarratives, gr) %>%
  gtsummary::tbl_summary(by = gr, missing = "no") %>%
  # add pariwaise p-values
  gtsummary::add_stat(everything() ~ add_stat_pairwise) %>%
  print()

?as_kable()
??ddply

tbl <- 
  onlymanipulation %>%
  select(mainstream, gr) %>%
  gtsummary::tbl_summary(
    by = gr, 
    missing = "no"
  ) %>%
  gtsummary::add_p(gtsummary::all_continuous() ~ "oneway.test") %>%
  # add a header (which also unhides a hidden column)
  gtsummary::modify_header(statistic ~ "**Test Statistic**") %>%
  # add a function to format the column
  gtsummary::modify_fmt_fun(statistic ~ gtsummary::style_sigfig)
tbl |>
  gtsummary::modify_header(label = "**Variable**", p.value = "**P**") 


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
  scale_color_manual(labels = c("voluntary", "mandatory", "control"), values = c("blue", "red","lightgreen")) 


#H5: The perceived effectiveness of the proposed policy will not differ between 
#the mandatory and the voluntary conditions.
a <- aov(man_eff~gr, data=onlymanipulation)
summary(a)
TukeyHSD(a)
#mandatory policy seen as more effective

#H6: Participants in the mandatory condition will perceive the policy to be less 
#acceptable in comparison to those in the voluntary and control conditions.

b <- aov(man_acc~gr, data=mand)
summary(b)
TukeyHSD(b)
rm(m.model0)


#(exploratory) H3:The strength of the relationship between trust in 
#institutions and perceived threat to security will vary by experimental group.
install.packages("fastDummies")

# Load the library
library(fastDummies)


# Create dummy variable
onlymanipulation <- dummy_cols(onlymanipulation, 
                   select_columns = "gr")
mand <- dummy_cols(mand, select_columns = "gr")

#1- Science
#2 - Public media
#3 - Ppl on social media
#4 -EU
#5 - Poland

#for trust i used a regression 
m1 <- glm(ssec~ gr_2*institution_trust_4+gr_3*institution_trust_4+tsec, data=mand)
summary(m1)
#interesting results here
m1.1 <- glm(ssec~ gr_2*institution_trust_5+gr_3*institution_trust_5+tsec, data=mand)
summary(m1.1)

#two group
m2 <- glm(ssec~ gr_2*institution_trust_4+tsec, data=onlymanipulation)
summary(m2)
#non-sig
m2.1 <- glm(ssec~ gr_2*institution_trust_5+tsec, data=onlymanipulation)
summary(m2.1)
#almost

#(exploratory) H4: Active media use will moderate the relationship between 
#perceived threat to security and the choice of micro and mainstream narratives.

m3 <- glm(micronarratives~ ssec*active_soc_media+tsec, data=onlymanipulation)
summary(m2)
#non-sig

m3 <- glm(mainstream~ ssec*active_soc_media+tsec, data=onlymanipulation)
summary(m3)
#non-sig
#ssec mildly significant (when interaction removed active media use almost significant)
#trait security is significant

###SEM

aa<-onlymanipulation[rowSums(is.na(onlymanipulation)) != ncol(onlymanipulation), ]

m.model0 <- '
  security =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitsecurity=~ security_1+security_2+security_3


  Micronarratives ~security+traitsecurity
  Mainstream ~security+traitsecurity


'
m.fit0 <- sem(m.model0, data = aa, estimator = "MLR", missing = "FIML", group = "gr", group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(m.fit0, fit.measures=T, standardized = T)

mi <- modificationindices(test_fit)
mi[mi$mi>10,] 


m.model1 <- '
  security =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitsecurity=~ security_1+security_2+security_3


  Micronarratives ~c(a,a)*security+traitsecurity
  Mainstream ~security+traitsecurity


'
m.fit1 <- sem(m.model1, data = aa, estimator = "MLR", missing = "FIML", group = "gr", group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(m.fit1, fit.measures=T, standardized = T)

a <- anova(m.fit0,m.fit1)
#sign (better when not constrained)


m.model2 <- '
   security =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitsecurity=~ security_1+security_2+security_3



  Micronarratives ~security+c(a,a)*traitsecurity
  Mainstream ~security+traitsecurity


'
m.fit2 <- sem(m.model2, data = aa, estimator = "MLR", missing = "FIML", group = "gr", group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(m.fit2, fit.measures=T, standardized = T)

b <- anova(m.fit0,m.fit2)
#non-sig


m.model3 <- '
  security =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitsecurity=~ security_1+security_2+security_3


  Micronarratives ~security+traitsecurity
  Mainstream ~c(a,a)*security+traitsecurity

'
m.fit3 <- sem(m.model3, data = aa, estimator = "MLR", missing = "FIML", group = "gr", group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(m.fit3, fit.measures=T, standardized = T)

c <- anova(m.fit0,m.fit3)
#non-sig


m.model4 <- '
  security =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitsecurity=~ security_1+security_2+security_3



  Micronarratives ~security+traitsecurity
  Mainstream ~security+c(a,a)*traitsecurity

'
m.fit4 <- sem(m.model4, data = aa, estimator = "MLR", missing = "FIML", group = "gr", group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(m.fit4, fit.measures=T, standardized = T)

d <- anova(m.fit0,m.fit4)

m.model5 <- '
  security =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitsecurity=~ security_1+security_2+security_3



  Micronarratives ~security+c(a,a)*traitsecurity
  Mainstream ~security+c(a,a)*traitsecurity

'
m.fit5 <- sem(m.model5, data = aa, estimator = "MLR", missing = "FIML", group = "gr", group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(m.fit5, fit.measures=T, standardized = T)

d <- anova(m.fit0,m.fit5)

#non-sig
a$`Chisq diff`

library(knitr)
table_fit <- matrix(NA, nrow = 5, ncol = 8)
colnames(table_fit) = c("Model", "X2", "df", "CFI", "RMSEA", "SRMR", "X2 Diff",
                        "Pr(>X2)")
table_fit[1, ] <- c("Overall Model", round(fitmeasures(m.fit0, 
                                                       c("chisq", "df", "cfi",
                                                         "rmsea", "srmr")),3),
                    NA, NA)

table_fit[2, ] <- c("Constrain Min~Ssec", round(fitmeasures(m.fit1, 
                                                            c("chisq", "df", "cfi",
                                                              "rmsea", "srmr")),3),
                    round((a[2,5]),3), round((a[2,7]),3))
table_fit[3, ] <- c("Constrain Min~Tsec", round(fitmeasures(m.fit2, 
                                                            c("chisq", "df", "cfi",
                                                              "rmsea", "srmr")),3),
                    round((b[2,5]),3), round((b[2,7]),3))
table_fit[4, ] <- c("Constrain Man~Ssec", round(fitmeasures(m.fit3, 
                                                            c("chisq", "df", "cfi",
                                                              "rmsea", "srmr")),3),
                    round((c[2,5]),3), round((c[2,7]),3))
table_fit[5, ] <- c("Constrain Man~Tsec", round(fitmeasures(m.fit4, 
                                                            c("chisq", "df", "cfi",
                                                              "rmsea", "srmr", "")),3), 
                    round((d[2,5]),3), round((d[2,7]),3))

kable(table_fit)
library(semPlot)
sem_1 <- semPaths(m.fit0, intercepts = F, residuals = F, what = "std")

pfad_layout<- get_layout("traitsecurity", "", "","Micronarratives",
                         "", "","","",
                         "security","","","",
                         "","","Mainstream","",
                         rows = 4)

tidySEM::graph_sem(model = m.fit0, layout = pfad_layout) 





###with all 3 groups######

m.model03 <- '
  security =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+ narrative_7 
  traitsecurity=~ tsec1+tsec2+tsec3


  Micronarratives ~security+traitsecurity
  Mainstream ~security+traitsecurity


'
m.fit03 <- sem(m.model03, data = mand, estimator = "MLR", missing = "FIML", group = "gr", group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(m.fit03, fit.measures=T, standardized = T)

m.model13 <- '
  security =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5 +narrative_7 
  traitsecurity=~ tsec1+tsec2+tsec3


  Micronarratives ~c(a,a,a)*security+traitsecurity
  Mainstream ~security+traitsecurity


'
m.fit13 <- sem(m.model13, data = mand, estimator = "MLR", missing = "FIML", group = "gr", group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(m.fit13, fit.measures=T, standardized = T)

a3 <- anova(m.fit03,m.fit13)
#sig


m.model23 <- '
  security =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+narrative_7 
  traitsecurity=~ tsec1+tsec2+tsec3


  Micronarratives ~security+c(a,a,a)*traitsecurity
  Mainstream ~security+traitsecurity


'
m.fit23 <- sem(m.model23, data = mand, estimator = "MLR", missing = "FIML", group = "gr", group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(m.fit23, fit.measures=T, standardized = T)

b3 <- anova(m.fit03,m.fit23)
#non-sig


m.model33 <- '
  security =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5+narrative_7 
  traitsecurity=~ tsec1+tsec2+tsec3


  Micronarratives ~security+traitsecurity
  Mainstream ~c(a,a,a)*security+traitsecurity

'
m.fit33 <- sem(m.model33, data = mand, estimator = "MLR", missing = "FIML", group = "gr", group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(m.fit33, fit.measures=T, standardized = T)

c3 <- anova(m.fit03,m.fit33)
#non-sig


m.model43 <- '
  security =~ ssec1+ssec2+ssec3
  Mainstream=~narrative_2  +narrative_4  +narrative_6  +narrative_8  
  Micronarratives=~narrative_1+narrative_3+narrative_5  +narrative_7 
  traitsecurity=~ tsec1+tsec2+tsec3


  Micronarratives ~security+traitsecurity
  Mainstream ~security+c(a,a,a)*traitsecurity

'
m.fit43 <- sem(m.model43, data = mand, estimator = "MLR", missing = "FIML", group = "gr", group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(m.fit43, fit.measures=T, standardized = T)

d3 <- anova(m.fit03,m.fit43)
#non-sig


#nice little table

table_fit <- matrix(NA, nrow = 5, ncol = 8)
colnames(table_fit) = c("Model", "X2", "df", "CFI", "RMSEA", "SRMR", "X2 Diff",
                        "Pr(>X2)")
table_fit[1, ] <- c("Overall Model", round(fitmeasures(m.fit03, 
                                                       c("chisq", "df", "cfi",
                                                         "rmsea", "srmr")),3),
                    NA, NA)

table_fit[2, ] <- c("Constrain Min~Ssec", round(fitmeasures(m.fit13, 
                                                            c("chisq", "df", "cfi",
                                                              "rmsea", "srmr")),3),
                    round((a3[2,5]),3), round((a3[2,7]),3))
table_fit[3, ] <- c("Constrain Min~Tsec", round(fitmeasures(m.fit23, 
                                                            c("chisq", "df", "cfi",
                                                              "rmsea", "srmr")),3),
                    round((b3[2,5]),3), round((b3[2,7]),3))
table_fit[4, ] <- c("Constrain Man~Ssec", round(fitmeasures(m.fit33, 
                                                            c("chisq", "df", "cfi",
                                                              "rmsea", "srmr")),3),
                    round((c3[2,5]),3), round((c3[2,7]),3))
table_fit[5, ] <- c("Constrain Man~Tsec", round(fitmeasures(m.fit43, 
                                                            c("chisq", "df", "cfi",
                                                              "rmsea", "srmr", "")),3), 
                    round((d3[2,5]),3), round((d3[2,7]),3))


kable(table_fit)

###with all 3 groups######

#adding trust


m.model02 <- '
  security =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitsecurity=~ security_1+security_2+security_3

  Micronarratives ~security+traitsecurity
  Mainstream ~security+traitsecurity
  security ~ institution_trust_5

'
m.fit02 <- sem(m.model02, data = aa, estimator = "MLR", missing = "FIML", group = "gr", group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(m.fit02, fit.measures=T, standardized = T)



m.model12 <- '
  security =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitsecurity=~ security_1+security_2+security_3



  Micronarratives ~security+traitsecurity
  Mainstream ~security+traitsecurity
  security ~ c(a,a)*institution_trust_5

'
m.fit12 <- sem(m.model12, data = aa, estimator = "MLR", missing = "FIML", group = "gr", group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(m.fit12, fit.measures=T, standardized = T)


a2<- anova(m.fit02, m.fit12)

library(semTools)

#adding active media use
#(exploratory) H4: Active media use will moderate the relationship between 
#perceived threat to security and the choice of micro and mainstream narratives.

aa <- semTools::indProd (aa , var1 = c("security_freedom_1", "security_freedom_2", "security_freedom_3"),
                       var2 = c("active_soc_media"),
                       match = FALSE , meanC = TRUE ,
                       residualC = FALSE , doubleMC = TRUE) 

m.model04 <- '
  security =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitsecurity=~ security_1+security_2+security_3
  int =~ security_freedom_1.active_soc_media+security_freedom_2.active_soc_media+security_freedom_3.active_soc_media
  

  Micronarratives ~security+traitsecurity+int
  Mainstream ~security+traitsecurity+int
  security ~ institution_trust_5

'
m.fit04 <- sem(m.model04, data = aa, estimator = "MLR", missing = "FIML", group = "gr", group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(m.fit04, fit.measures=T, standardized = T)
#interaction not significant in either group 


pfad_layout<- get_layout("traitsecurity", "", "","Micronarratives",
                         "", "","","active_soc_media",
                         "security","","","",
                         "institution_trust_5","","Mainstream","",
                         rows = 4)

tidySEM::graph_sem(model = m.fit04, layout = pfad_layout) 






table_fit1 <- matrix(NA, nrow = 2, ncol = 8)
colnames(table_fit1) = c("Model", "X2", "df", "CFI", "RMSEA", "SRMR", "X2 Diff",
                         "Pr(>X2)")
table_fit1[1, ] <- c("Overall Model", round(fitmeasures(m.fit03, 
                                                        c("chisq", "df", "cfi",
                                                          "rmsea", "srmr")),3),
                     NA, NA)

table_fit1[2, ] <- c("Constrain Min~Ssec", round(fitmeasures(m.fit13, 
                                                             c("chisq", "df", "cfi",
                                                               "rmsea", "srmr")),3),
                     round((a3[2,5]),3), round((a3[2,7]),3))


kable(table_fit1)



#with dummy

m.modeld1 <- '
  security =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitsecurity=~ security_1+security_2+security_3


  Micronarratives ~security+traitsecurity*gr_2
  Mainstream~security+traitsecurity*gr_2
  security ~ institution_trust_5

'
m.fitd1 <- sem(m.modeld1, data = aa, ordered = "gr_2", group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(m.fitd1, fit.measures=T, standardized = T)




















#checking fit of model
# configural invariance
fit1 <- cfa(m.model1, data = onlymanipulation, group = "gr")

# weak invariance
fit2 <- cfa(m.model1, data = onlymanipulation, group = "gr",
            group.equal = "loadings")

# strong invariance
fit3 <- cfa(m.model1, data = onlymanipulation, group = "gr",
            group.equal = c("intercepts", "loadings"))

fit4 <- cfa(m.model1, data = onlymanipulation, group = "gr",
            group.equal = c("intercepts", "loadings", "regressions"))


# model comparison tests
lavTestLRT(fit1, fit2, fit3, fit4)

#invariance granted


write.csv(mand, file = "mand.csv")

#######codes###### (for bilendi--irrelevant otherwise)

codes <- raw$p_0001

codes <- rawcodes <- as.character(codes)
write.csv(codes, "codes.csv")
codes

codes_after <- mand$p_0001

codes_after <- as.character(codes_after)
write.csv(codes_after, "codes_after.csv")


