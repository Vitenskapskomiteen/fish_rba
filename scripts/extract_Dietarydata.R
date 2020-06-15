# THis is not used. Use the one in the RBS SharepointDriveFolder.




#UK 9
day1_raw <- read_excel(paste0(org::project$raw,'/9år_alle matvarer_636_DAG1_250520_til spss.xlsx'))
day2_raw <- read_excel(paste0(org::project$raw,'/9år_n636_DAG2_alle matvarer_AE18_190520_til spss.xlsx'))
day3_raw <- read_excel(paste0(org::project$raw,'/9år_n636_DAG3_alle matvarer_AE18_190520_til spss.xlsx'))
day4_raw <- read_excel(paste0(org::project$raw,'/9år_n636_DAG4_alle matvarer_AE18_190520_til spss.xlsx'))

cbind(day1_raw$`Nr NA`,day2_raw$`Nr NA`,
      day3_raw$`Nr NA`,day4_raw$`Nr NA`)
tmpind = day1_raw$`Nr NA`
day1 <- day1_raw[,which(regexpr("[[:digit:]]+",colnames(day1_raw))!=-1 & substr(colnames(day1_raw),1,1)=="V")]#starts_with("V"))
day2 <- day2_raw[,which(regexpr("[[:digit:]]+",colnames(day2_raw))!=-1 & substr(colnames(day2_raw),1,1)=="V")]
day3 <- day3_raw[,which(regexpr("[[:digit:]]+",colnames(day3_raw))!=-1 & substr(colnames(day3_raw),1,1)=="V")]
day4 <- day4_raw[,which(regexpr("[[:digit:]]+",colnames(day4_raw))!=-1 & substr(colnames(day4_raw),1,1)=="V")]



j <- list(day1,day2,day3,day4)
ds <- c('a','b','c','d')
bigday1 = data.frame()
for (dd in 1:4){
  day = j[[dd]]
  dayrw = day1_raw;
  kbs_nos <- sapply(1:dim(day)[2],function(ii){as.numeric(regmatches(colnames(day)[ii],regexpr("[[:digit:]]+",colnames(day)[ii])))})
  kbs_names <- colnames(day)

  for (ii in 1:dim(day)[1]){
    nt <- sum(day[ii,]>0)
      
    tmp <-  data.frame(ind = as.factor(rep(tmpind[ii],nt)),
                 day = as.factor(rep(ds[dd],nt)),
                 kbs_code = as.factor(kbs_nos[which(day[ii,]>0)]),
                 kbs_name = kbs_names[which(day[ii,]>0)],
                 amounts = as.numeric(unname(unlist(day[ii,which(day[ii,]>0)]))))
    bigday1 <- rbind(bigday1,tmp)
  }
}

# This is not the same size as lst time, why?
day1_raw %>% filter(`Nr NA`==1001) 

tmp <- bigday1 %>% group_by(day,ind) %>% summarise(sum(amounts))
plot(tmp$`sum(amounts)`,
     c(day1_raw$`TOTALT NA`,
       day2_raw$`TOTALT NA`,
       day3_raw$`TOTALT NA`,
       day4_raw$`TOTALT NA`[day4_raw$`TOTALT NA`>0]))
abline(0,1)
# Ok, these are correct.
# How do they compare with the old uk9data

tmp2 <- uk9data %>% group_by(day,ind) %>% summarise(sum(amounts))
plot(tmp$`sum(amounts)`,tmp2$`sum(amounts)`)
plot(1:2537,tmp2$`sum(amounts)`)
points(1:2537,c(day1_raw$`TOTALT NA`,                           day2_raw$`TOTALT NA`,                           day3_raw$`TOTALT NA`,                           day4_raw$`TOTALT NA`[day4_raw$`TOTALT NA`>0]),
       col=rgb(0.4,0.4,0.8,0.3))
# Shall we assume the old was somewhow wrong? Yes. The newly readin are legit.

bigday1 %>% filter(ind==1001) %>% filter(day=='a') %>% select(amounts)
uk9data %>% filter(ind==1001) %>% filter(day=='a') %>% select(amounts)

uk9data <- bigday1
save(uk9data,
     file='C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/cleandata/uk9.RData')


## Doing 16yo as check for old read-in.


library(readxl)
#UK 13
day1_raw <- read_excel(paste0(org::project$raw,'/13år_n636_DAG1_alle matvarer_AE18_190520_til spss.xlsx'))
day2_raw <- read_excel(paste0(org::project$raw,'/13år_n636_DAG2_alle matvarer_AE18_190520_til spss.xlsx'))
day3_raw <- read_excel(paste0(org::project$raw,'/13år_n636_DAG3_alle matvarer_AE18_190520_til spss.xlsx'))
day4_raw <- read_excel(paste0(org::project$raw,'/13år_n636_DAG4_alle matvarer_AE18_190520_til spss.xlsx'))

matplot(cbind(day1_raw$Nr,day2_raw$Nr,
      day3_raw$Nr,day4_raw$Nr),pch=1)
tmpind = day1_raw$Nr
day1 <- day1_raw[,which(regexpr("[[:digit:]]+",colnames(day1_raw))!=-1 & substr(colnames(day1_raw),1,1)=="V")]#starts_with("V"))
day2 <- day2_raw[,which(regexpr("[[:digit:]]+",colnames(day2_raw))!=-1 & substr(colnames(day2_raw),1,1)=="V")]
day3 <- day3_raw[,which(regexpr("[[:digit:]]+",colnames(day3_raw))!=-1 & substr(colnames(day3_raw),1,1)=="V")]
day4 <- day4_raw[,which(regexpr("[[:digit:]]+",colnames(day4_raw))!=-1 & substr(colnames(day4_raw),1,1)=="V")]



j <- list(day1,day2,day3,day4)
ds <- c('a','b','c','d')
bigday1 = data.frame()
for (dd in 1:4){
  day = j[[dd]]
  dayrw = day1_raw;
  kbs_nos <- sapply(1:dim(day)[2],function(ii){as.numeric(regmatches(colnames(day)[ii],regexpr("[[:digit:]]+",colnames(day)[ii])))})
  kbs_names <- colnames(day)
  
  for (ii in 1:dim(day)[1]){
    nt <- sum(day[ii,]>0)
    
    tmp <-  data.frame(ind = as.factor(rep(tmpind[ii],nt)),
                       day = as.factor(rep(ds[dd],nt)),
                       kbs_code = as.factor(kbs_nos[which(day[ii,]>0)]),
                       kbs_name = kbs_names[which(day[ii,]>0)],
                       amounts = as.numeric(unname(unlist(day[ii,which(day[ii,]>0)]))))
    bigday1 <- rbind(bigday1,tmp)
  }
}


tmp <- bigday1 %>% group_by(day,ind) %>% summarise(sum(amounts))
tmp2 <- uk13data %>% group_by(day,ind) %>% summarise(sum(amounts))

plot(tmp$`sum(amounts)`[1:1000],tmp2$`sum(amounts)`[1:1000])
# Some are off by something else than a factor of 4...
# No, they are probably just sorted differently....

uk13data <- bigday1
save(uk13data,
     file='C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/cleandata/uk13.RData')


## ====
#UK 4, these should not have changed.
day1_raw <- read_excel(paste0(org::project$raw,'/4år_399_MED kosttiskudd_dag1_090320_til spss.xlsx'))
day2_raw <- read_excel(paste0(org::project$raw,'/4år_399_MED kosttiskudd_dag2_090320 til spss.xlsx'))
day3_raw <- read_excel(paste0(org::project$raw,'/4år_399_MED kosttiskudd_dag3_090320 til spss.xlsx'))
day4_raw <- read_excel(paste0(org::project$raw,'/4år_399_MED kosttiskudd_dag4_090320 til spss.xlsx'))

matplot(cbind(day1_raw$ID,day2_raw$ID,
              day3_raw$ID,day4_raw$`ID NA`),pch=1)
tmpind = day1_raw$ID
day1 <- day1_raw[,which(regexpr("[[:digit:]]+",colnames(day1_raw))!=-1 & substr(colnames(day1_raw),1,1)=="V")]#starts_with("V"))
day2 <- day2_raw[,which(regexpr("[[:digit:]]+",colnames(day2_raw))!=-1 & substr(colnames(day2_raw),1,1)=="V")]
day3 <- day3_raw[,which(regexpr("[[:digit:]]+",colnames(day3_raw))!=-1 & substr(colnames(day3_raw),1,1)=="V")]
day4 <- day4_raw[,which(regexpr("[[:digit:]]+",colnames(day4_raw))!=-1 & substr(colnames(day4_raw),1,1)=="V")]



j <- list(day1,day2,day3,day4)
ds <- c('a','b','c','d')
bigday1 = data.frame()
for (dd in 1:4){
  day = j[[dd]]
  dayrw = day1_raw;
  kbs_nos <- sapply(1:dim(day)[2],function(ii){as.numeric(regmatches(colnames(day)[ii],regexpr("[[:digit:]]+",colnames(day)[ii])))})
  kbs_names <- colnames(day)
  
  for (ii in 1:dim(day)[1]){
    nt <- sum(day[ii,]>0)
    
    tmp <-  data.frame(ind = as.factor(rep(tmpind[ii],nt)),
                       day = as.factor(rep(ds[dd],nt)),
                       kbs_code = as.factor(kbs_nos[which(day[ii,]>0)]),
                       kbs_name = kbs_names[which(day[ii,]>0)],
                       amounts = as.numeric(unname(unlist(day[ii,which(day[ii,]>0)]))))
    bigday1 <- rbind(bigday1,tmp)
  }
}


tmp <- bigday1 %>% group_by(day,ind) %>% summarise(sum(amounts))
tmp2 <- uk4data %>% group_by(day,ind) %>% summarise(sum(amounts))
plot(tmp$`sum(amounts)`[1:1000],tmp2$`sum(amounts)`[1:1000])
# Også her er det noen forskjeller....
tix = sample(1:399,10)
day1_raw[tix,c(1:2)]


sort(unlist(day1_raw[tix,c(2)]))
sort(unlist(bigday1 %>% filter(ind %in% day1_raw$ID[tix], day=='a') %>% group_by(ind) %>% summarise(sum(amounts)) %>% select(`sum(amounts)`)))
# Theyre OK.
tmp$`sum(amounts)`[tix]
tmp2$`sum(amounts)`[tix]

# This is really weird, and it's not correct. We get >0 intake
# when there is none (ind==11421, day 1), 
# and both positive and negative deviations. Here we are
# comparing within each calc (so not between old and new excel sheets)
# Perhaps there's a hick-up, when there are no intakes? 

## SORTED out, they are not listed/sorted in the same way. Checksum;
sum(uk4data$amounts)
sum(bigday1$amounts)
# Identical

uk4data <- bigday1
save(uk4data,
     file='C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/cleandata/uk4.RData')



# Resaving dietaryData with individual covariates etc. Norkost has 
# been checked before.
rm(list=ls())
load(file='C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/cleandata/dietarydata.RData')
load(file='C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/cleandata/nk3.RData')
load(file='C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/cleandata/uk4.RData')
load(file='C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/cleandata/uk9.RData')
load(file='C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/cleandata/uk13.RData')

save(nk3data,indsNK3,file='C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/cleandata/nk3.RData')
save(uk4data,indsUK4,file='C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/cleandata/uk4.RData')
save(uk9data,indsUK9,file='C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/cleandata/uk9.RData')
save(uk13data,indsUK13,file='C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/cleandata/uk13.RData')
save(concentrations,file= 'C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/cleandata/concentrations.RData')
save.image('C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/cleandata/dietarydata.RData')