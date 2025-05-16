##descriptives


sd(mand$age_5, na.rm=T)

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

#Koalicja Obywatelska  (1) 
#Konfederacja Wolność i Niepodległość (Konfederacja, KORWiN, Ruch Narodowy)  (2) 
#Nowa Lewica  (3) 
#Prawo i Sprawiedliwość  (4) 
#Trzecia Droga (Polska 2050, Polskie Stronnictwo Ludowe)  (5) 
#Inna   (6) __________________________________________________
#Nie głosował(a)bym  (7) 



##correlations######
#method1
cors <- subset(mand[,c(16, 15, 28:30, 150, 193:202, 145:149)])
corssem <- subset(forsem[,c(16, 15, 28:30, 150,185:187,  193:202, 204:213)])

cors1 <- cors[mand$gr==1, ]
cors2 <- cors[mand$gr==2, ]
cors3 <- cors[mand$gr==3, ]



# centering with 'scale()'
center_scale <- function(x) {
  scale(x, center= T, scale = T)
}

# apply it
cors_c <- center_scale(cors)
cors_csem <- center_scale(corssem)

M <- cor(cors_csem, use= "pairwise.complete.obs")
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





my_fn <- function(data, mapping, ...){
  p <- ggplot(data = cors_csem , mapping = mapping) + 
    geom_point(size= .1) + 
    theme(strip.text.x = element_text(size = .2),
          strip.text.y = element_text(size = .2)) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = cors_csem , mapping = mapping) + 
    geom_point(size=.1) + 
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}
library(GGally)
g = ggpairs(cors_csem , lower = list(continuous = my_fn),
            upper = list(continuous = wrap(ggally_cor, alignPercent = 0.8, digits=2))
            
) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g

library(PerformanceAnalytics)
chart.Correlation(cors, histogram=TRUE, pch=19)

range(cors$finan, na.rm=T)

##correlations######



cors <- subset(mand[,c(159, 16,15, 14, 28:29, 196,193, 150, 197:200, 195, 201:204, 149)])
cors1 <- cors[mand$c_0001==1, ]
cors2 <- cors[mand$c_0001==2, ]
cors3 <- cors[mand$c_0001==3, ]

corssmall <- subset(onlymanipulation[,c(159, 16,15, 14, 28:29, 196,193, 150, 197:200, 195, 201:204, 149)])

displaynames<- c("gr", "Left Wing Media", "Right Wing Media",
                 "Social Media", "Active media use", "Passive media use",
                 "Financial Burden",
                 "SES", "Financial Situation",
                 "(T)Need for Security", "(T)Need for Freedom", 
                 "(S)Need for Security", "(S)Need for Freedom",
                 "Acceptability","Anti-mainstream narratives",
                 "Mainstream Narratives", "Anti-mainstream n. (Aggreement)",
                 "Mainstream n. (Aggreement)","Trust in the government")


names(onlymanipulation)


#visualisation
colnames(corssmall) <- displaynames

# Split data by group
grouped_data <- split(corssmall, corssmall$gr)

# Create correlation matrix for each group
cor_matrices <- lapply(grouped_data, function(grouped_df) {
  cor(grouped_df[, 2:18])  # Exclude the grouping column
})

# View the result
cor_matrices


library(ggplot2)
library(ggcorrplot)

# Compute and plot for each group
for (gr in names(grouped_data)) {
  cor_mat <- cor(grouped_data[[gr]][, 2:18])
  
  print(ggcorrplot(cor_mat, 
                   lab = TRUE, 
                   title = paste("Correlation - Group", gr)))
}



#install.packages("gridExtra")
library(gridExtra)

plots <- lapply(names(grouped_data), function(gr) {
  cor_mat <- cor(grouped_data[[gr]][, 2:16])
  ggcorrplot(cor_mat, lab = TRUE, title = paste("Group", gr))
})

# Arrange side by side
do.call(gridExtra::grid.arrange, c(plots, ncol = length(plots)))


#ungrouped
p.mat<-cor_pmat(cor_mat)
ggcorrplot(cor_mat, lab= T, type = "upper")














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










