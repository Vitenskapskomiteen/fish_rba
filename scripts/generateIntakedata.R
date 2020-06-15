

# Quick function to read in the intake-data in the 
# $raw folder. Now constructed to read in Norkost 3, 
# and calculate all intakes from the excel sheet with 
# concentrations. Could easily be generalized to take filenames for different dietary intake surveys.

# library(readxl)
# library(dplyr)

# dir(org::project$raw)
generateIntakedata <- function(remove_supplements=T,docalcs = c(3),missing_V = c(81197)){
  # If remove_supplements, all codes 97XX and 97XXX are removed.
  inds <- read_excel(paste0(org::project$raw,'/N3 alder og vekt_ogsaa etter kjoenn_280619.xlsx'))
  # freqs <- read_excel(paste0(org::project$raw,'/Tendensskjema kjoert i syntax_overfoert fra SPSS_060819.xlsx'))
  day1_raw <- read_excel(paste0(org::project$raw,'/N3_1787_dag1_MED kosttilskudd_AE18 splittet tilb ingred_060320_til spss.xlsx'))
  day2_raw <- read_excel(paste0(org::project$raw,'/N3_1787_dag2_MED kosttilskudd_AE18 splittet tilb ingred_060320_til spss.xlsx'))
  tmpcolno <- dim(read_excel(paste0(org::project$raw,'/alle matvarer AE18 med naeringsstoffer og energi 190320MHC.xlsx'),skip=5))[2]
  concs <- read_excel(paste0(org::project$raw,'/alle matvarer AE18 med naeringsstoffer og energi 190320MHC.xlsx'),skip=5,
                      col_type=c('numeric','text',rep('numeric',tmpcolno-2)))
  
  units <- read_excel(paste0(org::project$raw,'/alle matvarer AE18 med naeringsstoffer og energi 190320MHC.xlsx'),
                      range="A6:H7")
  
  # Extracting the individual covariates et al.
  dataNK3 <- data.frame(ind = rep(as.factor(inds$`Id nr`),2),
                        sex = rep(as.factor(ifelse(inds$Kjoenn==1,'male','female')),2),
                        wgt = rep(as.numeric(inds$Vekt_imputert_etter_kjoenn),2),
                        age = rep(inds$Alder,2),
                        day = rep(c('a','b'),each=1787),
                        educ = rep(ifelse(inds$Utdann1 %in% c(5,6),'H','L'),2))
  
  # The single foods (not recipes etc) have a code staring with V as a name in
  # the daily intake (day1 & day2). These should correspond to the foods
  # with concentrations given in the column concs[,1].
  # If we have a pipeline for this, we could ideally just input another column 
  # in the concs sheet and rerun for a given column number.
  
  filtersupps <- remove_supplements; # If true, then the calculations remove supplements,
  # assumed to be all V-codes 97XX and 97XXX
  
  # Removing all non- V-code entries.
  day1 <- day1_raw[,which(regexpr("[[:digit:]]+",colnames(day1_raw))!=-1 & substr(colnames(day1_raw),1,1)=="V")]#starts_with("V"))
  day2 <- day2_raw[,which(regexpr("[[:digit:]]+",colnames(day2_raw))!=-1 & substr(colnames(day2_raw),1,1)=="V")]
  
  
  # Making a concentration array that fits with the different days.
  # Could probably pipe this, but I'm new to dplyr
  # Linking day1/day2 with entries in the concentration matrix. Matching V-codes
  concarr_day1 <- sapply(1:dim(day1)[2],function(ii){
    as.numeric(regmatches(colnames(day1)[ii],regexpr("[[:digit:]]+",colnames(day1)[ii])))})
  inxd1 <- (sapply(1:length(concarr_day1),function(ii){which(concarr_day1[ii] == concs[,1])}))
  inxd1 <- unlist(ifelse(inxd1>0,inxd1,NA)) # Some are not there
  # table(is.na(inxd1))
  # THere is one Vcode missing for now, remove the whole entry and rerun lines above.
  # day1 <- day1[,-1336]
  
  concarr_day2 <- sapply(1:dim(day2)[2],function(ii){
    as.numeric(regmatches(colnames(day2)[ii],regexpr("[[:digit:]]+",colnames(day2)[ii])))})
  inxd2 <- (sapply(1:length(concarr_day2),function(ii){which(concarr_day2[ii] == concs[,1])}))
  inxd2 <- unlist(ifelse(inxd2>0,inxd2,NA)) # Some are not there
  # table(is.na(inxd2))
  
  # Does it work when setting all NA's to 0
  concs[is.na(concs)] = 0;
  if (filtersupps){
    # colnames(day1)[which(concarr_day1 %in% c(81197,9701:9799,97001:97999))]
    # Also 81197 which is not in the concentration array.
    day1 <- day1[,-which(concarr_day1 %in% c(missing_V,9701:9799,97001:97999))]
    day2 <- day2[,-which(concarr_day2 %in% c(missing_V,9701:9799,97001:97999))]
    concarr_day1 <- sapply(1:dim(day1)[2],function(ii){
      as.numeric(regmatches(colnames(day1)[ii],regexpr("[[:digit:]]+",colnames(day1)[ii])))})
    inxd1 <- (sapply(1:length(concarr_day1),function(ii){which(concarr_day1[ii] == concs[,1])}))
    inxd1 <- unlist(ifelse(inxd1>0,inxd1,NA)) # Some are not there
    concarr_day2 <- sapply(1:dim(day2)[2],function(ii){
      as.numeric(regmatches(colnames(day2)[ii],regexpr("[[:digit:]]+",colnames(day2)[ii])))})
    inxd2 <- (sapply(1:length(concarr_day2),function(ii){which(concarr_day2[ii] == concs[,1])}))
    inxd2 <- unlist(ifelse(inxd2>0,inxd2,NA)) # Some are not there
  } else {
    # colnames(day1)[which(concarr_day1 %in% c(missing_V))]
    # Also 81197 which is not in the concentration array.
    if (sum(concarr_day1 %in% c(missing_V))){
      day1 <- day1[,-which(concarr_day1 %in% c(missing_V))]
      concarr_day1 <- sapply(1:dim(day1)[2],function(ii){
        as.numeric(regmatches(colnames(day1)[ii],regexpr("[[:digit:]]+",colnames(day1)[ii])))})
      inxd1 <- (sapply(1:length(concarr_day1),function(ii){which(concarr_day1[ii] == concs[,1])}))
      inxd1 <- unlist(ifelse(inxd1>0,inxd1,NA)) # Some are not there
    }
    if (sum(concarr_day2 %in% c(missing_V))){
      day2 <- day2[,-which(concarr_day2 %in% c(missing_V))]
      concarr_day2 <- sapply(1:dim(day2)[2],function(ii){
        as.numeric(regmatches(colnames(day2)[ii],regexpr("[[:digit:]]+",colnames(day2)[ii])))})
      inxd2 <- (sapply(1:length(concarr_day2),function(ii){which(concarr_day2[ii] == concs[,1])}))
      inxd2 <- unlist(ifelse(inxd2>0,inxd2,NA)) # Some are not there
    }    
    }
  
  # Calculating daily intakes 
  
  # for
  for (cc in docalcs){
    # All concentrations are given in 100 g of food.
    int_d1 <- concs[[cc]][inxd1] %*% t(as.matrix(day1))/100
    int_d2 <- concs[[cc]][inxd2] %*% t(as.matrix(day2))/100
    
    # dataNK3[[colnames(concs)[3]]] = runif(3574)
    dataNK3[[colnames(concs)[cc]]]  = c(int_d1,int_d2);
  }
  
  return(dataNK3)
}