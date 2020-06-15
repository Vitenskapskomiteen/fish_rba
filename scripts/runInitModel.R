# Quick script to calculate individual level intake of
# compounds given in excel sheet @sharepoint..

library(readxl)
library(dplyr)

dir(org::project$raw)

inds <- read_excel(paste0(org::project$raw,'/N3 alder og vekt_også etter kjønn_280619.xlsx'))
freqs <- read_excel(paste0(org::project$raw,'/Tendensskjema kjørt i syntax_overført fra SPSS_060819.xlsx'))
day1_raw <- read_excel(paste0(org::project$raw,'/N3_1787_dag1_MED kosttilskudd_AE18 splittet tilb ingred_060320_til spss.xlsx'))
day2_raw <- read_excel(paste0(org::project$raw,'/N3_1787_dag2_MED kosttilskudd_AE18 splittet tilb ingred_060320_til spss.xlsx'))
concs <- read_excel(paste0(org::project$raw,'/alle matvarer AE18 med næringsstoffer og energi 190320MHC.xlsx'),skip=5,
                    col_type=c('numeric','text',rep('numeric',8)))

units <- read_excel(paste0(org::project$raw,'/alle matvarer AE18 med næringsstoffer og energi 190320MHC.xlsx'),
                    range="A6:H7")

# Extracting the individual covariates et al.
dataNK3 <- data.frame(ind = rep(as.factor(inds$`Id nr`),2),
                      sex = rep(as.factor(ifelse(inds$Kjønn==1,'male','female')),2),
                      wgt = rep(as.numeric(inds$Vekt_imputert_etter_kjønn),2),
                      age = rep(inds$Alder,2),
                      day = rep(c('a','b'),each=1787),
                      educ = rep(ifelse(inds$Utdann1 %in% c(5,6),'H','L'),2))

# The single foods (not recipes etc) have a code staring with V as a name in
# the daily intake (day1 & day2). These should correspond to the foods
# with concentrations given in the column concs[,1].
# If we have a pipeline for this, we could ideally just input another column 
# in the concs sheet and rerun for a given column number.

filtersupps <- TRUE # If true, then the calculations remove supplements,
# assumed to be all V-codes 97XX and 97XXX

# Removing all non- V-code entries.
day1 <- day1_raw[,which(regexpr("[[:digit:]]+",colnames(day1_raw))!=-1 & substr(colnames(day1_raw),1,1)=="V")]#starts_with("V"))
day2 <- day2_raw[,which(regexpr("[[:digit:]]+",colnames(day2_raw))!=-1 & substr(colnames(day2_raw),1,1)=="V")]

if (filtersupps){
  colnames(day1)[which(concarr_day1 %in% c(81197,9701:9799,97001:97999))]
  # Also 81197 which is not in the concentration array.
  day1 <- day1[,-which(concarr_day1 %in% c(81197,9701:9799,97001:97999))]
  day2 <- day2[,-which(concarr_day2 %in% c(81197,9701:9799,97001:97999))]
}

# Making a concentration array that fits with the different days.
# Could probably pipe this, but I'm new to dplyr
# Linking day1/day2 with entries in the concentration matrix. Matching V-codes
concarr_day1 <- sapply(1:dim(day1)[2],function(ii){
  as.numeric(regmatches(colnames(day1)[ii],regexpr("[[:digit:]]+",colnames(day1)[ii])))})
inxd1 <- (sapply(1:length(concarr_day1),function(ii){which(concarr_day1[ii] == concs[,1])}))
inxd1 <- unlist(ifelse(inxd1>0,inxd1,NA)) # Some are not there
table(is.na(inxd1))
# THere is one Vcode missing for now, remove the whole entry and rerun lines above.
# day1 <- day1[,-1336]

concarr_day2 <- sapply(1:dim(day2)[2],function(ii){
  as.numeric(regmatches(colnames(day2)[ii],regexpr("[[:digit:]]+",colnames(day2)[ii])))})
inxd2 <- (sapply(1:length(concarr_day2),function(ii){which(concarr_day2[ii] == concs[,1])}))
inxd2 <- unlist(ifelse(inxd2>0,inxd2,NA)) # Some are not there
table(is.na(inxd2))


# Could probably also filter on ny code with 97XXX or 97XX, which are
# supplements. 

# Does it work when setting all NA's to 0
concs[is.na(concs)] = 0;

# Calculating daily intakes 
docalcs = c(3:10)
# for
for (cc in docalcs){
  # All concentrations are given in 100 g of food.
  int_d1 <- concs[[cc]][inxd1] %*% t(as.matrix(day1))/100
  int_d2 <- concs[[cc]][inxd2] %*% t(as.matrix(day2))/100
  
  # dataNK3[[colnames(concs)[3]]] = runif(3574)
  dataNK3[[colnames(concs)[cc]]]  = c(int_d1,int_d2);
}



myf <- function(ii){c(mean(ii),sd(ii),quantile(ii,c(0.25,0.5,0.75,0.95,.975)))}
oims <- t(sapply(unique(dataNK3$ind),function(ii){
  colMeans(dataNK3[which(dataNK3$ind==ii),7:14])}))

stats <- data.frame(rep(NA,7),row.names=c('mean','sd','p25','p50','p75','p95','p975'))
for (ii in 1:8){
  stats[[colnames(oims)[ii]]] <- myf(oims[,ii])
}

stats_male <- data.frame(rep(NA,7),row.names=c('mean','sd','p25','p50','p75','p95','p975'))
for (ii in 1:8){
  stats_male[[colnames(oims)[ii]]] <- myf(oims[which(dataNK3$sex[1:1787]=='male'),ii])
}
stats_female <- data.frame(rep(NA,7),row.names=c('mean','sd','p25','p50','p75','p95','p975'))
for (ii in 1:8){
  stats_female[[colnames(oims)[ii]]] <- myf(oims[which(dataNK3$sex[1:1787]=='female'),ii])
}
# These checks out for the Iodine Oims in the report.

## We make the transformations the next step.
# Box-Cox transform, optimized by Shapiro-Wilk normality statistic;
par(mfrow=c(2,1),mar=c(3,2,2,2))
tmpint <- dataNK3$intake
tmpint <- tmpint[tmpint>0]
Lams <- seq(0.00001,3,length.out=201)
Ws <- sapply(Lams,function(ii){
  shapiro.test(((tmpint^ii)-1)/(ii))$statistic})
plot(Lams,Ws,type="l")

# Optimizer to get best Box-Cox transform parameter.
myopt <- function(ii){-shapiro.test(((tmpint^ii)-1)/(ii))$statistic}
f1 <- optim(0.2,myopt,method='Brent',lower=1e-8,upper=10)
f1$par
hist(((tmpint^(f1$par)-1)/(f1$par)))

dataNK3$transIntake <- ((dataNK3$intake^(f1$par)-1)/(f1$par))
dataNK3$transIntake <- log(dataNK3$intake)
dataNK3$transIntake[dataNK3$intake==0] = NA
library(MCMCglmm)
fm1 <- MCMCglmm(transIntake~age+sex+educ,
                random = ~idh(sex):ind ,
                rcov  = ~ units,
                family = c("gaussian"),
                data = dataNK3)

## 
# Can we make a generic function that generates a simulation function
# from a MCMCglmm fit?
# First we can make a function to sample gixen an X (i.e. age, sex, educ)
smpfun1 <- function(X,mcmcfit,n=10){
  # X is intercept, age, sex, educlevel
  xix <- sample(dim(mcmcfit$Sol)[1],1);
  y_hat <- sum(mcmcfit$Sol[xix,]*X)+rnorm(1,mean=0,sd = mcmcfit$VCV[xix,1+X[3]])
  y_out <- y_hat + rnorm(n,mean=0,sd = mcmcfit$VCV[xix,3])
  return(y_out)
}


oims <- sapply(unique(dataNK3$ind),function(ii){
  mean(dataNK3[which(dataNK3$ind==ii),]$intake)})
quantile(oims[which(dataNK3$sex[1:1787]=='male')]/100,c(0.25,0.5,.75,.95,0.975))
quantile(oims[which(dataNK3$sex[1:1787]=='female')]/100,c(0.25,0.5,.75,.95,0.975))

samp <- replicate(1e5,smpfun1(c(1,runif(1,min=20,max=70),round(runif(1)),1),fm1,n=250))
# a = (y^b - 1) / b
# ==> exp(log(a*b + 1)/(b)
samp_bt <- exp(log(samp*f1$par + 1)/f1$par)

samp_bt <- exp(samp)
plot(density(colMeans(samp_bt/100)),main=paste0('Simulated ',colnames(concs)[colinx]))
rug(dataNK3$intake/100)


plot(t(samp_bt))
plot(int_d1,int_d2)








#### ==== ####
as.numeric(regmatches(colnames(day1)[ii],regexpr("[[:digit:]]+",colnames(day1)[ii])))
# 'tis weird. some entries are not NA nor NULL, but disappear with unlisting  
# as.numeric(regmatches(colnames(day1),regexpr("[[:digit:]]+",colnames(day1))))[10] %in% concs[-c(1),1]

length(unlist(inxd1[1331:1340]))
colnames(day1)[1336]
