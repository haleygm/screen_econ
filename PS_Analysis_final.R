#Cites Used
# #https://journals.sfu.ca/jmde/index.php/jmde_1/article/view/431 
# https://sejdemyr.github.io/r-tutorials/statistics/tutorial8.html

#Import Libraries
library(tidyverse)
library(readxl)
library(writexl)
library(MatchIt)
library(optmatch)
library(twang)
library(scales)
library(RItools)
library(foreign)
library(readstata13)
library(Hmisc)
library(scales)
library(Matching)
library(rbounds)
library(stargazer)
library(hrbrthemes)
library(mfx)
library(margins)
library(grid)
library(gridExtra)
library(psych)

# Import Data 
data <- as.data.frame(read.dta13("fd.dta"))
length(which(data$Remake == 1)) #total number of remakes: 93
data$Sal_R2 <- data$Sal_R^2 #Quadratic for salience*remake interaction 

####Pre-Analysis: Non-matched Data  ####
#Examining the Outcomes of Interest (ROI) mean & SD 
data %>% 
  dplyr::group_by(Remake) %>% 
  dplyr::summarise(n_movie = n(), 
            mean_ROI1 = mean(ROI_BFI), 
            sd_ROI1 = sd(ROI_BFI), 
            mean_ROI2 = mean(ROI_V), 
            sd_ROI2 = sd(ROI_V), 
            mean_ROI3 = mean(ROI_MKT), 
            sd_ROI3 = sd(ROI_MKT))

with(data, t.test(ROI_BFI~Remake))
with(data, t.test(ROI_V~Remake))
with(data, t.test(ROI_MKT~Remake)) #Note, no statisitcally significant difference in means for this measure of ROI 

rem <- data$Remake ==1
covar1 <- data[,c( 7, 9, 10, 11, 12, 16, 19, 20, 21, 22, 33)]
std.diff <- apply(covar1, 2, function(x) 100*(mean(x[rem]-mean(x[!rem]))/(sqrt(0.5*var(x[rem] + var(x[!rem]))))))
abs(std.diff)

xBalance(Remake ~  y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9 + y10 + y11 + 
                  y12 + y13 + y14 + y15+ y16 + y17 + y18 + y19 + y20 + y21 + 
                  y22 + y23 + y24 + y25 + y26 + y27 + y28 + y29 + y30 + y31 + 
                  y32 + y33 + y34 + y35 + y36 + y37 + y38 + y39 + y40 + y41 +
                  y42 + y43 + y44 + y45 + y46 + y47 + y48 + y49 + y50 + y51 + 
                  y52 + y53 + y54 + y55 + y56 + y57 + y58 + y59 + y60 + y61 +
                  y62 + y63 + y64 + y65 + y66 + y67 + y68 + y69 + y70 + y71 +
                  y72 + y73 + y74 + y75 + y76 + y77 + y78 + y79 + y80 + y81 + y82 + 
                  Action + Animation + Family + G + PG + `PG-13` + Warner + Universal +
                  Budget + Series,  data = data,  report = c("chisquare.test"))

#Examining Pre-Treatment Covariates 
covar <- c('Action', 'Family', 'G', 'PG', 'PG-13', 'R', 'Runtime', 'Disney', 'Universal', 'Warner', 'Budget', 'Series')
means <- data %>% 
  dplyr::group_by(Remake) %>% 
  dplyr::select(one_of(covar)) %>%
  dplyr::summarise_all(funs(mean(., na.rm = T)))
means

sd <- data %>% 
  dplyr::group_by(Remake) %>% 
  dplyr::select(one_of(covar)) %>%
  dplyr::summarise_all(funs(sd(., na.rm = T)))
sd

lapply(covar, function(f){
  t.test(data[,f] ~ data$Remake)
})

#Pre-Treatment Regressions
pre_m1 <- lm(ROI_BFI ~ Remake  + Salience + Action + Family + G + PG + `PG-13` + Warner + Universal + Avg_Rating, data = data)
summary(pre_m1)
pre_m2 <- lm(ROI_BFI ~ Remake  + Salience + Action + Family + G + PG + `PG-13` + Warner + Universal + Avg_Rating, data = data)
summary(pre_m2)
pre_m3 <- lm(ROI_BFI ~ Remake  + Salience + Action + Family + G + PG + `PG-13` + Warner + Universal + Avg_Rating, data = data)
summary(pre_m3)

#### Propensity Scores and Common  Support####
  #Estimate PS
ps <- glm(Remake ~ Action  + Family + G + PG + `PG-13` + Warner + Universal + Budget + Series,  
            data = data, family = binomial())
summary(ps)
ps1 = vcovHC(ps, type = "HC1" )
ps1r = coeftest(ps, vcov = ps1)
ps1r
ps1fx <- logitmfx(Remake ~ Action  + Family + G + PG + `PG-13` + Warner + Universal + Budget + Series,  
         data = data, atmean = F, robust = T)
ps1fx
log_ps1fx <- ps1fx$mfxest[,1]
log_se_ps1fx <- ps1fx$mfxest[,2]

  #Append PS 
data$psvalue <- predict(ps, type = "response")
# Histogram for Common Support (looks relatively fine)
nrow(data[which(data$psvalue == 1), ]) #Pr(Remake = 1| Covariates) < 1 for all X (meets key requirement)

histbackback(split(data$psvalue, data$Remake), main = "Propensity Score Before Matching", xlab = c("control", "treatment"), probability = T)

labs <- paste(c("Remake", "Original"))
csplot <- data %>% 
  mutate(Remake = ifelse(Remake == 1, labs[1], labs[2])) %>% 
  ggplot(aes(x = psvalue, y = ..density.., fill = Remake)) +
  geom_density(color="#e9ecef", alpha=0.6, position = 'identity') + 
  scale_fill_brewer(palette = "Set1") + 
  theme_minimal()
print(csplot + labs(title = 'Propensity Score Before Matching', x = 'Propensity Score', y = 'Density', fill = 'Actual Movie Status'))


#### Matching ####
data_nomiss <- data %>% 
  na.omit()

### NN 1:1 ###
{m.nn1 <- matchit(Remake ~ Action + Family + G + PG + `PG-13` + Warner + Universal + Budget + Series, data = data_nomiss, method = "nearest")
summary(m.nn1)
plot(m.nn1, type = "hist")
data_match1 <- match.data(m.nn1) 

histbackback(split(data_match1$psvalue, data_match1$Remake), main = "Propensity Score After Matching", xlab = c("control", "treatment"), probability = T)

data_match1 %>% 
  mutate(Remake = ifelse(Remake == 1, labs[1], labs[2])) %>% 
  ggplot(aes(x = psvalue, y = ..density.., color = Remake)) +
  geom_histogram(fill = 'white') + 
  scale_fill_brewer(palette = "Set1") + 
  theme_minimal()

rem2 <- data_match1$Remake ==1
covar2 <- data_match1[,c( 7, 9, 10, 11, 12, 16, 19, 20, 21, 22, 33)]
std.diff2 <- apply(covar2, 2, function(x) 100*(mean(x[rem2]-mean(x[!rem2]))/(sqrt(0.5*var(x[rem2] + var(x[!rem2]))))))
abs(std.diff2)

xBalance(Remake ~  y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9 + y10 + y11 + 
           y12 + y13 + y14 + y15+ y16 + y17 + y18 + y19 + y20 + y21 + 
           y22 + y23 + y24 + y25 + y26 + y27 + y28 + y29 + y30 + y31 + 
           y32 + y33 + y34 + y35 + y36 + y37 + y38 + y39 + y40 + y41 +
           y42 + y43 + y44 + y45 + y46 + y47 + y48 + y49 + y50 + y51 + 
           y52 + y53 + y54 + y55 + y56 + y57 + y58 + y59 + y60 + y61 +
           y62 + y63 + y64 + y65 + y66 + y67 + y68 + y69 + y70 + y71 +
           y72 + y73 + y74 + y75 + y76 + y77 + y78 + y79 + y80 + y81 + y82 + 
           Action + Animation + Family + G + PG + `PG-13` + Warner + Universal +
           Budget + Series,  data = data_match1,  report = c("chisquare.test"))
}
### NN 2:1 ### Prefered Matching Method 
{m.nn2 <- matchit(Remake ~ Action + Family + G + PG + `PG-13` + Warner + Universal + Budget + Series, data = data_nomiss, method = "nearest", ratio = 2)
summary(m.nn2)
plot(m.nn2, type = "hist")
data_match2 <- match.data(m.nn2)
names(summary(m.nn2))
pbimp <- summary(m.nn2)$reduction

# Matching checks
histbackback(split(data_match2$psvalue, data_match2$Remake), main = "Propensity Score After Matching", xlab = c("control", "treatment"), probability = T)
csplot2 <- data_match2 %>% 
  mutate(Remake = ifelse(Remake == 1, labs[1], labs[2])) %>% 
  ggplot(aes(x = psvalue, y = ..density.., fill = Remake)) +
  geom_density(color="#e9ecef", alpha=0.6, position = 'identity') + 
  scale_fill_brewer(palette = "Set1") + 
  theme_minimal()
print(csplot2 + labs(title = 'Propensity Score Before Matching', x = 'Propensity Score', y = 'Density', fill = 'Actual Movie Status'))


rem3 <- data_match2$Remake ==1
covar3 <- data_match2[,c( 7, 9, 10, 11, 12, 16, 19, 20, 21, 22, 33)]
std.diff3 <- apply(covar3, 2, function(x) 100*(mean(x[rem3]-mean(x[!rem3]))/(sqrt(0.5*var(x[rem3] + var(x[!rem3]))))))
abs(std.diff3)

xBalance(Remake ~  y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9 + y10 + y11 + 
           y12 + y13 + y14 + y15+ y16 + y17 + y18 + y19 + y20 + y21 + 
           y22 + y23 + y24 + y25 + y26 + y27 + y28 + y29 + y30 + y31 + 
           y32 + y33 + y34 + y35 + y36 + y37 + y38 + y39 + y40 + y41 +
           y42 + y43 + y44 + y45 + y46 + y47 + y48 + y49 + y50 + y51 + 
           y52 + y53 + y54 + y55 + y56 + y57 + y58 + y59 + y60 + y61 +
           y62 + y63 + y64 + y65 + y66 + y67 + y68 + y69 + y70 + y71 +
           y72 + y73 + y74 + y75 + y76 + y77 + y78 + y79 + y80 + y81 + y82 + 
           Action + Animation + Family + G + PG + `PG-13` + Warner + Universal +
           Budget + Series,  data = data_match2,  report = c("chisquare.test"))
means2 <- data_match2 %>% 
  dplyr::group_by(Remake) %>% 
  dplyr::select(one_of(covar)) %>%
  dplyr::summarise_all(funs(mean(., na.rm = T)))
means2

sd2 <- data_match2 %>% 
  dplyr::group_by(Remake) %>% 
  dplyr::select(one_of(covar)) %>%
  dplyr::summarise_all(funs(sd(., na.rm = T)))
sd2


lapply(covar, function(f){
  t.test(data_match2[,f] ~ data_match2$Remake)
})

psa <- glm(Remake ~ Action  + Family + G + PG + `PG-13` + Warner + Universal + Budget + Series,  
           data = data_match2, family = binomial())
summary(psa)
ps2 = vcovHC(psa, type = "HC1" )
ps2r = coeftest(psa, vcov = ps2)
ps2r
ps2fx <- logitmfx(Remake ~ Action  + Family + G + PG + `PG-13` + Warner + Universal + Budget + Series,  
                  data = data_match2, atmean = F, robust = T)
ps2fx
log_ps2fx <- ps2fx$mfxest[,1]
log_se_ps2fx <- ps2fx$mfxest[,2]
# Note, I prefer the 2:1 matching. Better overlap, higher P-value, reduction in variance
# Also likely, bias did  not increase much 
}

#### Sensitivity Analysis ####  (Ignore, I  don't actually know what it  means)
Y1 <- data$ROI_BFI
Y2 <- data$ROI_V
Y3 <- data$ROI_MKT
Tr <- data$Remake
ps1 <- glm(Remake ~ Action  + Family + G + PG + `PG-13` + Warner + Universal + Budget + Series,  
          data = data, family = binomial())

Match1 <- Match(Y = Y1, Tr = Tr, X = ps1$fitted, replace = F)
psens(Match1, Gamma = 2, GammaInc = 0.1) #Gamma switch at 1.4
Match2 <- Match(Y = Y2, Tr = Tr, X = ps1$fitted, replace = F)
psens(Match2, Gamma = 2, GammaInc = 0.1) #Gamma switch at 1
Match3 <- Match(Y = Y3, Tr = Tr, X = ps1$fitted, replace = F)
psens(Match3, Gamma = 2, GammaInc = 0.1) #Gamma switch at 1.4



#### OLS #### 
# NN1 OLS
{
  m1.1 <- lm(ROI_BFI ~ Remake + Salience  + Family + G + PG + `PG-13` + Warner + Universal +
               Avg_Rating, data = data_match1)
  summary(m1.1)
  hc_m1.1 = vcovHC(m1.1, type = "HC1" )
  m1.1r = coeftest(m1.1, vcov = hc_m1.1)
  m1.1r
  
  m2.1 <- lm(ROI_V ~ Remake + Salience  + Family + G + PG + `PG-13` + Warner + Universal +
               Avg_Rating, data = data_match1)
  summary(m1.1)
  hc_m2.1 = vcovHC(m2.1, type = "HC1" )
  m2.1r = coeftest(m2.1, vcov = hc_m2.1)
  m2.1r
  
  m3.1 <- lm(ROI_MKT ~ Remake + Salience  + Family + G + PG + `PG-13` + Warner + Universal +
               Avg_Rating, data = data_match1)
  summary(m3.1)
  hc_m3.1 = vcovHC(m3.1, type = "HC1" )
  m3.1r = coeftest(m3.1, vcov = hc_m3.1)
  m3.1r
  
}
# ROI_BFI 
m1.2 <- lm(ROI_BFI ~ Remake + Salience  + Family + G + PG + `PG-13` + Warner + Universal +
             Avg_Rating, data = data_match2)
summary(m1.2)
hc_m1.2 = vcovHC(m1.2, type = "HC1" )
m1.2r = coeftest(m1.2, vcov = hc_m1.2)
m1.2r

# ROI_V 
m2.2 <- lm(ROI_V ~ Remake + Salience  + Family + G + PG + `PG-13` + Warner + Universal +
             Avg_Rating, data = data_match2)
summary(m1.2)
hc_m2.2 = vcovHC(m2.2, type = "HC1" )
m2.2r = coeftest(m2.2, vcov = hc_m2.2)
m2.2r

# ROI_MKT 
m3.2 <- lm(ROI_MKT ~ Remake + Salience  + Family + G + PG + `PG-13` + Warner + Universal +
             Avg_Rating, data = data_match2)
summary(m3.2)
hc_m3.2 = vcovHC(m3.2, type = "HC1" )
m3.2r = coeftest(m3.2, vcov = hc_m3.2)
m3.2r

### Robustness #### 
ms1 <- lm(ROI_BFI ~ Remake + Salience  + Sal2 + Family + G + PG + `PG-13` + Warner + Universal +
             Avg_Rating, data = data_match2)
summary(ms1)
hc_ms1 = vcovHC(ms1, type = "HC1" )
ms1r = coeftest(ms1, vcov = hc_ms1)
ms1r

ms2 <- lm(ROI_V ~ Remake + Salience  + Sal2 + Family + G + PG + `PG-13` + Warner + Universal +
             Avg_Rating, data = data_match2)
summary(ms2)
hc_ms2 = vcovHC(ms2,  type = "HC1" )
ms2r = coeftest(ms2, vcov = hc_ms2)
ms2r

ms3 <- lm(ROI_MKT ~ Remake + Salience + Sal2 + Family + G + PG + `PG-13` + Warner + Universal +
             Avg_Rating, data = data_match2)
summary(ms3)
hc_ms3 = vcovHC(ms3, type = "HC1" )
ms3r = coeftest(ms3, vcov = hc_ms3)
ms3r


#### Export to Latex ####
stargazer(m1.2r, m2.2r, m3.2r,  title = "Movie Remake and ROI Models", 
          dep.var.caption = "ROI", 
          align = T, no.space = T, digits = 4,
          column.labels= c("BFI Method", "Vogel's Method", "Non-constant P\&A"), 
          covariate.labels = c("Remake", "Salience*Remake", "Family", "G", "PG", "PG-13", 
                               "Warner", "Universal", "Average Rating"),
          notes.label = "Robust Standard Errors"
)


stargazer(ms1r, ms2r, ms3r,  title = "Movie Remake and ROI Models with Nonconstant Salience Effect", 
          dep.var.caption = "ROI", 
          align = T, no.space = T, digits = 4,
          column.labels= c("BFI Method", "Vogel's Method", "Non-constant P\&A"), 
          covariate.labels = c("Remake", "Salience*Remake", "(Salience*Remake)^2", "Family", "G", "PG", "PG-13", 
                               "Warner", "Universal", "Average Rating"),
          notes.label = "Robust Standard Errors"
)

stargazer(ps1r, ps2r, title = 'Marginal Effects for Logistic Regression Before and After Matching', 
          dep.var.caption = 'Treatment Status: Remake', 
          align = T, no.space = T, digits = 4, 
          coef = list(c(NA, log_ps1fx), c(NA, log_ps2fx)),
          se = list(c(NA, log_se_ps1fx), c(NA, log_se_ps2fx)),
          column.labels = c('Before Matching', 'After Matching'),
          covariate.labels = c('Action', 'Family', 'G', 'PG', 'PG-13', 'Warner', 'Universal', 'Budget', 'Series'), 
          notes.label = 'Robust Standard Errors')

stargazer(pbimp,  title = "Percent Balance Improvement", 
          align = T, no.space = T, digits = 4,
         covariate.labels = c("Distance", "Action", 
                               "Family", "G", "PG", "PG-13", "Warner", "Universal", "Budget", 'Series')
)

# Final Plots 
print(csplot + labs(title = 'PS Before Matching', x = 'Propensity Score', y = 'Density', fill = 'Actual Movie Status'))
print(csplot2 + labs(title = 'PS After Matching', x = 'Propensity Score', y = 'Density', fill = 'Actual Movie Status'))


