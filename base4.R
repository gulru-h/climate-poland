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
mand$political <- as.character(mand$sss_1)


##descriptives

table1 <-
  mand |> 
  gtsummary::tbl_summary(include = c(age_5, gender, sss_1),
                         by= NULL,
                         statistic = list(gtsummary::all_continuous() ~ "{mean} ({p25}, {p75})",
                                          gtsummary::all_categorical() ~"{n} ({p}%)"),)

table2 <-
  mand |> 
  gtsummary::tbl_summary(include = c(political),
                         by= NULL,
                         statistic = list(gtsummary::all_continuous() ~ "{median} ({p25}, {p75})",
                                          gtsummary::all_categorical() ~"{n} ({p}%)"),)


##correlations######
#method1
cors <- subset(mand[,c(16, 15, 28:30, 150, 193:202, 145:149 )])
cors1 <- cors[mand$gr==1, ]
cors2 <- cors[mand$gr==2, ]
cors3 <- cors[mand$gr==3, ]



# centering with 'scale()'
center_scale <- function(x) {
  scale(x, center= T, scale = FALSE)
}

# apply it
cors_c <- center_scale(cors)

M <- cor(cors_c, use= "pairwise.complete.obs")
M1<- cor(cors1)
M2<- cor(cors2)
M3<- cor(cors3)
corrplot(M1, method="number")
corrplot(M2, method="number")
corrplot(M3, method="number")
corrplot(M, method="number")

?corrplot

colnames(M) <- c( "Left Wing Media", "Right Wing Media","Active Social Media Use", "Passive Social Media Use",
                 "EU Attitudes", "SES","Financial Burden","Effectiveness","Acceptability", "Financial Situation",
                 "(T)Need for Security", "(T)Need for Freedom", 
                 "(S)Need for Security", "(S)Need for Freedom","micronarratives",
                 "mainstream", "Science", "Popular media", "Others on Social Media",
                 "EU", "Poland")

rownames(M) <- c("Left Wing Media", "Right Wing Media","Active Social Media Use", "Passive Social Media Use",
                 "EU Attitudes", "SES","Financial Burden","Effectiveness","Acceptability", "Financial Situation",
                 "(T)Need for Security", "(T)Need for Freedom", 
                 "(S)Need for Security", "(S)Need for Freedom","micronarratives",
                 "mainstream", "Science", "Popular media", "Others on Social Media",
                 "EU", "Poland")


PLO<- corrplot(M, method="number", )

#method3

# centering with 'scale()'
center_scale <- function(x) {
  scale(x, center= T, scale = T)
}

# apply it
cors_s <- center_scale(cors)




my_fn <- function(data, mapping, ...){
  p <- ggplot(data = cors_s, mapping = mapping) + 
    geom_point(size= .1) + 
    theme(strip.text.x = element_text(size = .2),
          strip.text.y = element_text(size = .2)) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = cors_s, mapping = mapping) + 
    geom_point(size=.1) + 
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

g = ggpairs(cors_s, lower = list(continuous = my_fn),
            upper = list(continuous = wrap(ggally_cor, alignPercent = 0.8, digits=2)),
            columnLabels = c("Left Wing Media", "Right Wing Media","Active Social Media Use", "Passive Social Media Use",
                             "EU Attitudes", "SES","Financial Burden","Effectiveness","Acceptability", "Financial Situation",
                             "(T)Need for Security", "(T)Need for Freedom", 
                             "(S)Need for Security", "(S)Need for Freedom","micronarratives",
                             "mainstream", "Science", "Popular media", "Others on Social Media",
                             "EU", "Poland"
            )) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g


library(PerformanceAnalytics)
chart.Correlation(cors, histogram=TRUE, pch=19)

range(cors$finan, na.rm=T)

##correlations######


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

micronarratives <- subset(mand[,c(70,72,74,76)])
mand$micronarratives <- apply(micronarratives, 1, mean, na.rm=T)

mainstream <- subset(mand[,c(78,80,82,84)])
mand$mainstream <- apply(mainstream, 1, mean, na.rm=T)

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

#create a manipulation only group without the control group
rm(onlymanipulation)
onlymanipulation <- mand[mand$gr ==1|mand$gr ==2 , ]

#create the two levels 
onlymanipulation$gr <- ordered(onlymanipulation$gr, levels = c("1", "2"))

#not working
tbl <- 
  onlymanipulation %>%
  gtsummary::select(mainstream, gr) %>%
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

forsem <- onlymanipulation

which(is.na(forsem$gr))

forsem <- forsem[-c(123:125, 187, 188, 262,263), ]


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

  
m.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedsecurity=~ security_1+security_2+security_3

  Micronarratives ~needsecurity+traitneedsecurity
  Mainstream ~needsecurity+traitneedsecurity


  needsecurity ~ gr_2
  Micronarratives ~ gr_2
  Mainstream ~gr_2
  

'

m.fit <- sem(m.model, data = forsem, estimator = "MLR", missing = "FIML",
             group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(m.fit, fit.measures=T, standardized = T)

#, missing = "FIML",
#               se = "bootstrap",bootstrap = 500L, parallel ="snow"


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

med.fit <- sem(med.model, data = forsem, estimator = "ML")
summary(med.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_med.fit <- parameterEstimates(med.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                            zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_med.fit)
estimates_med.fit[42:43, ]




?sem()


free.model <- '
  needfreedom =~ security_freedom_4+security_freedom_5+security_freedom_6
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedfreedom=~ security_4+security_5+security_6

  Micronarratives ~b1*needfreedom+traitneedfreedom
  Mainstream ~b2*needfreedom+traitneedfreedom


  needfreedom ~ a1*gr_2
  Micronarratives ~ gr_2
  Mainstream ~gr_2
  
ind1 := a1*b1
ind2 := a1*b2

'

free.fit <- sem(free.model, data = forsem, estimator = "ML")
summary(free.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_free.fit <- parameterEstimates(free.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                        zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_free.fit)
estimates_free.fit[42:43, ]


pfad_layout<- get_layout("","traitneedsecurity", "", "","Micronarratives",
                         "","", "","","",
                         "gr_2","","","","",
                         "","","","","Mainstream",
                         "","needsecurity","","","",
                         rows = 5)

tidySEM::graph_sem(model = med.fit, layout = pfad_layout) 



pfad_layout<- get_layout("","traitneedfreedom", "", "","Micronarratives",
                         "","", "","","",
                         "gr_2","","","","",
                         "","","","","Mainstream",
                         "","needfreedom","","","",
                         rows = 5)

tidySEM::graph_sem(model = free.fit, layout = pfad_layout) 


nonnest2::vuongtest(free.fit, combined.fit)
nonnest2::vuongtest(med.fit, combined.fit)


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





combined.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  needfreedom =~ security_freedom_4+security_freedom_5+security_freedom_6
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedfreedom=~ security_4+security_5+security_6
  traitneedsecurity=~ security_1+security_2+security_3

  Micronarratives ~needfreedom+traitneedfreedom+needsecurity+traitneedsecurity
  Mainstream ~needfreedom+traitneedfreedom+needsecurity+traitneedsecurity


  needfreedom ~ gr_2
  needsecurity ~ gr_2

  Micronarratives ~ gr_2
  Mainstream ~gr_2
  needfreedom ~~ needsecurity
  traitneedfreedom ~~traitneedsecurity

'

combined.fit <- sem(combined.model, data = forsem, estimator = "ML")
summary(combined.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_combined.fit <- parameterEstimates(combined.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                         zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_combined.fit)
estimates_combined.fit[42:43, ]




pfad_layout<- get_layout("","traitneedfreedom", "", "","Micronarratives",
                         "","traitneedsecurity", "","","",
                         "gr_2","","","","",
                         "","needsecurity","","","Mainstream",
                         "","needfreedom","","","",
                         rows = 5)

tidySEM::graph_sem(model = combined.fit, layout = pfad_layout) 



cor(forsem$ssec, forsem$sfree)
#0.7989506


a <-lm(micronarratives ~ ssec + sfree + institution_trust_5, data=forsem)
vif(a)















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


talt.fit <- sem(talt.model, data = forsem, estimator = "ML")
summary(talt.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_talt.fit <- parameterEstimates(talt.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                         zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_talt.fit)



pfad_layout<- get_layout("","", "Micronarratives", "","",
                         "","traitneedsecurity", "","","",
                         "gr_2","","","","institution_trust_5",
                         "","","","Mainstream","",
                         "needsecurity","","","","",
                         rows = 5)

tidySEM::graph_sem(model = talt.fit, layout = pfad_layout) 



tfree.model <- '
  needfreedom =~ security_freedom_4+security_freedom_5+security_freedom_6
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedfreedom=~ security_4+security_5+security_6

  institution_trust_5 ~ b1*Micronarratives
  institution_trust_5 ~b2*Mainstream
  Micronarratives ~a1*needfreedom+traitneedfreedom
  Mainstream ~ a2*needfreedom+traitneedfreedom


  needfreedom ~ gr_2
  institution_trust_5~ gr_2 
  institution_trust_5 ~ needfreedom
      
ind1 := a1*b1
ind2 := a2*b2


'

tfree.fit <- sem(tfree.model, data = forsem, estimator = "ML")
summary(tfree.fit, fit.measures=T, standardized = T, rsquare=TRUE)
estimates_tfree.fit <- parameterEstimates(tfree.fit, standardized=TRUE, boot.ci.type="perc", level=0.95,
                                         zstat = FALSE, pvalue = FALSE, output = "data.frame")

View(estimates_free.fit)
estimates_free.fit[42:43, ]



estimates_talt.fit[64:65, ]



pfad_layout<- get_layout("","needsecurity", "Micronarratives", "","",
                         "","", "","","",
                         "gr_2","","","","institution_trust_5",
                         "","","","Mainstream","",
                         "needfreedom","","","","",
                         rows = 5)

tidySEM::graph_sem(model = combined.fit, layout = pfad_layout) 



combined.model <- '
  needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  needfreedom =~ security_freedom_4+security_freedom_5+security_freedom_6
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedfreedom=~ security_4+security_5+security_6
  traitneedsecurity=~ security_1+security_2+security_3

  Micronarratives ~needfreedom+traitneedfreedom+needsecurity+traitneedsecurity
  Mainstream ~needfreedom+traitneedfreedom+needsecurity+traitneedsecurity


  needfreedom ~ gr_2
  needsecurity ~ gr_2

  Micronarratives ~ gr_2
  Mainstream ~gr_2
  needfreedom ~~ needsecurity
  traitneedfreedom ~~traitneedsecurity
  
  
  institution_trust_5 ~ Micronarratives
  institution_trust_5 ~Mainstream
  Micronarratives ~needfreedom+traitneedfreedom
  Mainstream ~ needfreedom+traitneedfreedom


  needfreedom ~ gr_2
  institution_trust_5~ gr_2 
  institution_trust_5 ~ needfreedom
  
  
  Micronarratives ~needsecurity+traitneedsecurity
  Mainstream ~ needsecurity+traitneedsecurity


  needsecurity ~ gr_2
  institution_trust_5~ gr_2 
  institution_trust_5 ~ needsecurity

'

combined.fit <- sem(combined.model, data = forsem, estimator = "ML")
summary(combined.fit, fit.measures=T, standardized = T, rsquare=TRUE)

nonnest2::vuongtest(talt.fit, combined.fit)

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

a.model <- '
     needsecurity =~ security_freedom_1+security_freedom_2+security_freedom_3
  Mainstream=~nar5_end  +nar6_end  +nar7_end  +nar8_end  
  Micronarratives=~nar1_end+nar2_end+nar3_end+ nar4_end 
  traitneedsecurity=~ security_1+security_2+security_3

  
  Micronarratives ~needsecurity+traitneedsecurity+institution_trust_5+active_soc_media
  Mainstream ~needsecurity+traitneedsecurity+institution_trust_5+active_soc_media
  active_soc_media ~ needsecurity

  needsecurity ~ gr_2
  institution_trust_5~ gr_2 
  institution_trust_5 ~ needsecurity

'
a.fit <- sem(a.model, data = forsem, estimator = "MLR", missing = "FIML",
             group.equal = c("loadings", "intercepts", "means", "residuals", "residual.covariances", "lv.variances", "lv.covariances"))
summary(a.fit, fit.measures=T, standardized = T)





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













#single factor solution
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


#still recommends 2 factors





#subsetting and building means 


#checking fit of model
# configural invariance
fit1 <- cfa(m.model0, data = forsem, group = "gr")

# weak invariance
fit2 <- cfa(m.model0, data = forsem, group = "gr",
            group.equal = "loadings")

# strong invariance
fit3 <- cfa(m.model0, data = forsem, group = "gr",
            group.equal = c("intercepts", "loadings"))

fit4 <- cfa(m.model0, data = forsem, group = "gr",
            group.equal = c("intercepts", "loadings", "regressions"))


# model comparison tests
lavTestLRT(fit1, fit2, fit3, fit4)

#invariance granted
#####ignore for sanity####

write.csv(mand, file = "mand.csv")

#######codes###### (for bilendi--irrelevant otherwise)

codes <- raw$p_0001

codes <- rawcodes <- as.character(codes)
write.csv(codes, "codes.csv")
codes

codes_after <- mand$p_0001

codes_after <- as.character(codes_after)
write.csv(codes_after, "codes_after.csv")


