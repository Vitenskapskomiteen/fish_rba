# # Quick script to calculate individual level intake of
# # compounds given in excel sheet @sharepoint 
# # and output simple figures with table...
# 
# library(readxl)
# library(dplyr)
# 
# dir(org::project$raw)
# 
# inds <- read_excel(paste0(org::project$raw,'/N3 alder og vekt_også etter kjønn_280619.xlsx'))
# freqs <- read_excel(paste0(org::project$raw,'/Tendensskjema kjørt i syntax_overført fra SPSS_060819.xlsx'))
# day1_raw <- read_excel(paste0(org::project$raw,'/N3_1787_dag1_MED kosttilskudd_AE18 splittet tilb ingred_060320_til spss.xlsx'))
# day2_raw <- read_excel(paste0(org::project$raw,'/N3_1787_dag2_MED kosttilskudd_AE18 splittet tilb ingred_060320_til spss.xlsx'))
# concs <- read_excel(paste0(org::project$raw,'/alle matvarer AE18 med næringsstoffer og energi 190320MHC.xlsx'),skip=5,
#                     col_type=c('numeric','text',rep('numeric',8)))
# 
# units <- read_excel(paste0(org::project$raw,'/alle matvarer AE18 med næringsstoffer og energi 190320MHC.xlsx'),
#                     range="C6:J7")
# 
# # Extracting the individual covariates et al.
# dataNK3 <- data.frame(ind = rep(as.factor(inds$`Id nr`),2),
#                       sex = rep(as.factor(ifelse(inds$Kjønn==1,'male','female')),2),
#                       wgt = rep(as.numeric(inds$Vekt_imputert_etter_kjønn),2),
#                       age = rep(inds$Alder,2),
#                       day = rep(c('a','b'),each=1787),
#                       educ = rep(ifelse(inds$Utdann1 %in% c(5,6),'H','L'),2))
# 
# # The single foods (not recipes etc) have a code staring with V as a name in
# # the daily intake (day1 & day2). These should correspond to the foods
# # with concentrations given in the column concs[,1].
# # If we have a pipeline for this, we could ideally just input another column 
# # in the concs sheet and rerun for a given column number.
# 
# filtersupps <- TRUE # If true, then the calculations remove supplements,
# # assumed to be all V-codes 97XX and 97XXX
# 
# # Removing all non- V-code entries.
# day1 <- day1_raw[,which(regexpr("[[:digit:]]+",colnames(day1_raw))!=-1 & substr(colnames(day1_raw),1,1)=="V")]#starts_with("V"))
# day2 <- day2_raw[,which(regexpr("[[:digit:]]+",colnames(day2_raw))!=-1 & substr(colnames(day2_raw),1,1)=="V")]
# 
# if (filtersupps){
#   colnames(day1)[which(concarr_day1 %in% c(81197,9701:9799,97001:97999))]
#   # Also 81197 which is not in the concentration array.
#   day1 <- day1[,-which(concarr_day1 %in% c(81197,9701:9799,97001:97999))]
#   day2 <- day2[,-which(concarr_day2 %in% c(81197,9701:9799,97001:97999))]
# }
# 
# # Making a concentration array that fits with the different days.
# # Could probably pipe this, but I'm new to dplyr
# # Linking day1/day2 with entries in the concentration matrix. Matching V-codes
# concarr_day1 <- sapply(1:dim(day1)[2],function(ii){
#   as.numeric(regmatches(colnames(day1)[ii],regexpr("[[:digit:]]+",colnames(day1)[ii])))})
# inxd1 <- (sapply(1:length(concarr_day1),function(ii){which(concarr_day1[ii] == concs[,1])}))
# inxd1 <- unlist(ifelse(inxd1>0,inxd1,NA)) # Some are not there
# table(is.na(inxd1))
# # THere is one Vcode missing for now, remove the whole entry and rerun lines above.
# # day1 <- day1[,-1336]
# 
# concarr_day2 <- sapply(1:dim(day2)[2],function(ii){
#   as.numeric(regmatches(colnames(day2)[ii],regexpr("[[:digit:]]+",colnames(day2)[ii])))})
# inxd2 <- (sapply(1:length(concarr_day2),function(ii){which(concarr_day2[ii] == concs[,1])}))
# inxd2 <- unlist(ifelse(inxd2>0,inxd2,NA)) # Some are not there
# table(is.na(inxd2))
# 
# 
# # Could probably also filter on ny code with 97XXX or 97XX, which are
# # supplements. 
# 
# # Does it work when setting all NA's to 0
# concs[is.na(concs)] = 0;
# 
# # Calculating daily intakes 
# docalcs = c(3:10)
# # for
# for (cc in docalcs){
#   # All concentrations are given in 100 g of food.
#   int_d1 <- concs[[cc]][inxd1] %*% t(as.matrix(day1))/100
#   int_d2 <- concs[[cc]][inxd2] %*% t(as.matrix(day2))/100
#   
#   # dataNK3[[colnames(concs)[3]]] = runif(3574)
#   dataNK3[[colnames(concs)[cc]]]  = c(int_d1,int_d2);
# }
# 


myf <- function(ii){c(mean(ii),sd(ii),quantile(ii,c(0.25,0.5,0.75,0.95,.975)))}
oims <- data.frame(t(sapply(unique(dataNK3$ind),function(ii){
  colMeans(dataNK3[which(dataNK3$ind==ii),7:14])})))
oims$sex = dataNK3$sex[1:1787]

par(mfrow=c(2,4))
for (ii in 1:8){
  
  plot(density(oims[oims$sex=='female',ii],from=0),col='red',main=colnames(oims)[ii],
       xlab=paste0('Daily intakes [',units[[ii]],'/day]'),ylab='',lwd=3)
  lines(density(oims[oims$sex=='male',ii],from=0),col='blue',lwd=3)
  rug(oims[oims$sex=='female',ii],col='red')
  rug(oims[oims$sex=='male',ii],col='blue')
}

stats <- data.frame(rep(NA,7),row.names=c('mean','sd','p25','p50','p75','p95','p975'))
for (ii in 1:8){
  stats[[colnames(oims)[ii]]] <- myf(oims[,ii])
}
stats <- stats[,-1]

stats_male <- data.frame(rep(NA,7),row.names=c('mean','sd','p25','p50','p75','p95','p975'))
for (ii in 1:8){
  stats_male[[colnames(oims)[ii]]] <- myf(oims[which(dataNK3$sex[1:1787]=='male'),ii])
}
stats_male <- stats_male[,-1]
stats_female <- data.frame(rep(NA,7),row.names=c('mean','sd','p25','p50','p75','p95','p975'))
for (ii in 1:8){
  stats_female[[colnames(oims)[ii]]] <- myf(oims[which(dataNK3$sex[1:1787]=='female'),ii])
}
stats_female <- stats_female[,-1]
# These checks out for the Iodine Oims in the report.

write.table(stats,file=paste0(org::project$results_today,"/oims_all.csv"),sep=";",dec=",")
write.table(stats_male,file=paste0(org::project$results_today,"/oims_males.csv"),sep=";",dec=",")
write.table(stats_female,file=paste0(org::project$results_today,"/oims_females.csv"),sep=";",dec=",")

png(paste0(org::project$results_today,'/oims_distr.png'),res=200,width=1200,height=1200,pointsize = 10)
par(mfrow=c(1,1))
for (ii in 1:8){
  png(paste0(org::project$results_today,'/oims_distr',colnames(oims)[ii],'.png'),res=200,width=1200,height=1200,pointsize = 10)
  
  plot(density(oims[oims$sex=='female',ii],from=0),col='red',main=colnames(oims)[ii],
       xlab=paste0('Daily intakes [',units[[ii]],'/day]'),ylab='',lwd=3)
  lines(density(oims[oims$sex=='male',ii],from=0),col='blue',lwd=3)
  rug(oims[oims$sex=='female',ii],col='red')
  rug(oims[oims$sex=='male',ii],col='blue')
}

dev.off()
