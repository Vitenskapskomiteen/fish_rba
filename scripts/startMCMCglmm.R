
# Script to import Norkost dietary data, run a simple model of
# fish-consumption on the daily data, and compare with the frequency
# questionnaire.




# For the frequencies reported, can we model them?

library(readxl)

dir(org::project$raw)

inds <- read_excel(paste0(org::project$raw,'/N3 alder og vekt_også etter kjønn_280619.xlsx'))
freqs <- read_excel(paste0(org::project$raw,'/Tendensskjema kjørt i syntax_overført fra SPSS_060819.xlsx'))
day1 <- read_excel(paste0(org::project$raw,'/N3_1787_dag1_MED kosttilskudd_AE18 splittet tilb ingred_060320_til spss.xlsx'))
day2 <- read_excel(paste0(org::project$raw,'/N3_1787_dag2_MED kosttilskudd_AE18 splittet tilb ingred_060320_til spss.xlsx'))

table(freqs$Fiskekak_sp116)

# FInd categories, and not specific foods:
colnames(day1)[which(substr(colnames(day1),1,1) !='V')]
# I guess we should compare 
# FISKFisk,skalldyr (which is with shellfish) [130]
# FIS_MHFisk,mager+halvfet - 131
# FISK_FFisk,fet           - 140
# Fisk_PFisk,produkter     - 150
# FISKPAFiskepålegg        - 161
# SUSHISushi               - 163
coltx <- c('FISKFisk,skalldyr','FIS_MHFisk,mager+halvfet',
           'FISK_FFisk,fet','FISK_PFisk,produkter',
           'FISKPAFiskepålegg','SUSHISushi')
ixd1 <- which(colnames(day1) %in% coltx)
ixd2 <- which(colnames(day2) %in% coltx)
#
sum(day1$`FISKFisk,skalldyr` == day1$FISKPAFiskepålegg)
sum(day2$`FISKFisk,skalldyr` == day1$FISKPAFiskepålegg)
# So for many (D1: 1223, D2: 868) spread is the only intake of fish.


dataF_NK3 <- data.frame(ind = rep(as.factor(inds$`Id nr`),2),
                        sex = rep(as.factor(ifelse(inds$Kjønn==1,'male','female')),2),
                        wgt = rep(as.numeric(inds$Vekt_imputert_etter_kjønn),2),
                        age = rep(inds$Alder,2),
                        day = rep(c('a','b'),each=1787),fiskk = NA)

tmpfisk <- sapply(dataF_NK3$ind, function(ii){ifelse(ii %in% freqs$id_nummer,freqs[which(ii==freqs$id_nummer),]$Fiskekak_sp116,NA)})
# How are these frequencies added?

tmpfiska <- t(sapply(inds$`Id nr`,function(ii){as.numeric(day1[which(ii == day1$ID),ixd1])}))
tmpfiskb <- t(sapply(inds$`Id nr`,function(ii){as.numeric(day2[which(ii == day2$ID),ixd2])}))
tmp <- data.frame(rbind(tmpfiska,tmpfiskb))
colnames(tmp) <- coltx

dataNK3 <- as.data.frame(cbind(dataF_NK3,tmp))

oimsf <- sapply(unique(dataNK3$ind),function(ii){mean(dataNK3[which(dataNK3$ind==ii),6])})
# 630 inds eat NO fish. 

# Box-Cox transform, optimized by Shapiro-Wilk normality statistic;
par(mfrow=c(2,1),mar=c(3,2,2,2))
tmpint <- dataNK3$`FISKFisk,skalldyr`
tmpint <- tmpint[tmpint>0]
Lams <- seq(0.00001,3,length.out=201)
Ws <- sapply(Lams,function(ii){
  shapiro.test(((tmpint^ii)-1)/(ii))$statistic})
plot(Lams,Ws,type="l")


tmpint <- dataNK3$`FISKFisk,skalldyr`
tmpint <- tmpint[tmpint>0] #eats fish at all=?

# Optimizer to get best Box-Cox transform parameter.
myopt <- function(ii){-shapiro.test(((tmpint^ii)-1)/(ii))$statistic}
f1 <- optim(0.2,myopt,method='Brent',lower=1e-8,upper=10)


# Generalized mixed models.
library(MCMCglmm)

# Transforming the fish-intake
dataNK3$BC_fisk <- (dataNK3$`FISKFisk,skalldyr`^f1$par - 1)/(f1$par)
# Setting NA if 0
dataNK3$BC_fisk[which(dataNK3$`FISKFisk,skalldyr`==0)] = NA
# Generating categorical variable, eats fish or not.
dataNK3$eatsfish <- ifelse(dataNK3$`FISKFisk,skalldyr`==0,0,1)

# Eats a full fish meal (not sure if this is thinking correctly;
# if the total fish intake is not equal to the fish as spread on bread, then a full meal has been eaten)
dataNK3$eatsfishmeal <- ifelse(dataNK3$`FISKFisk,skalldyr`==
                                 dataNK3$FISKPAFiskepålegg,0,1)


## == Model 1 == 
# Eats fish (outside breadspread) at all. A categorical model
f1 <- MCMCglmm(eatsfishmeal ~age + sex,
               random =~ind,
               data = dataNK3,
               family = 'categorical')
# Didn't work so well it seems...


# Mixed model, predict IF meal and How big meal at once
fm1 <- MCMCglmm(cbind(BC_fisk,eatsfish)~trait*age+trait*sex,
                random = ~us(trait):ind ,
                rcov  = ~ idh(trait):units,
                family = c("gaussian","categorical"),
                data = dataNK3)

prio2 = list(R = list(V = diag(2)/10,nu=3,fix=2),
             G = list(G1 = list(V= diag(2)/1   , nu=3)))
fm2 <- MCMCglmm(cbind(BC_fisk,eatsfish)~trait*age+trait*sex,
                random = ~us(trait):ind ,
                rcov  = ~ idh(trait):units,
                prior = prio2,
                family = c("gaussian","categorical"),
                data = dataNK3,nitt=25000,burnin=12000)


# Will these ind-RE's lead to some simulated individual NEVER
# eating fish? IT should.
priox2 = list(R = list(V = diag(1),nu=3,fix=1),
              G = list(G1 = list(V= diag(1)/1   , nu=3)))
priox2b = list(R = list(V = diag(1),nu=3,fix=1),
               G = list(G1 = list(V= diag(1)/1   , nu=3),
                        G2 = list(V= diag(1)/1   , nu=3)))

ft2 <- MCMCglmm(eatsfishmeal~age+sex,
                random = ~ind ,
                rcov  = ~ units,
                prior = priox2,
                family = c("categorical"),
                data = dataNK3,nitt=25000,burnin=12000)
ft2b <- MCMCglmm(eatsfishmeal~age+sex,
                 random = ~ idh(at.level(sex,1)):ind + idh(at.level(sex,2)):ind ,
                 rcov  = ~ units,
                 prior = priox2b,
                 family = c("categorical"),
                 data = dataNK3,nitt=25000,burnin=12000)
# Categorical is a logit link, i.e. for p(x) = exp(x)/(exp(x)+1)


RealEats <- rle(sort(sapply(unique(dataNK3$ind),function(ii){sum(dataNK3[which(dataNK3$ind==ii),]$eatsfish>0)})))

# Trying to replicate data from the frequency questionnaire with the
# 24 hr recall model.
BigTab34 = array(NA,dim=c(100,3))
for (jj in 1:100){
n = 1787;
n_days = 30
ix <- sample(dim(ft2$Sol)[1],n,replace=T)
s_age = sample(dataNK3$age,n,replace=T)
# sample(18:29,n,replace=T)#
s_sex = sample(dataNK3$sex,n,replace=T)=='male'
# 20;

xs <- colSums(sapply(1:length(ix),function(ii){ft2$Sol[ix[ii],] * c(1,s_age[ii],s_sex[ii])}))
re <- rnorm(length(ix),0,sqrt(ft2$VCV[ix,1]))
rc <- t(sapply(ix,function(ii){rnorm(n_days,0,sqrt(ft2$VCV[ii,]))}))
xds <- matrix(rep(xs+re,n_days),ncol=n_days)+rc
ps <- exp(xds)/(exp(xds)+1)
eats <- matrix(runif(length(ps)),nrow=NROW(ps))<ps

BigTab34[jj,] <- c(sum(rowSums(eats[,1:30]) <4)/n,
  sum(rowSums(eats[,1:7]) %in% c(1,2))/n,
  sum(rowSums(eats[,1:7]) > 2)/n)
}

round(100*apply(BigTab34,2,range))/100



 sum(eats==0)/n
 sum(eats %in% c(1,2))/n
 sum(eats>2)/n

n = 10000;
bigeats = array(NA,c(100,3)) # no fish, 1 day fish, 2 or more days figh
for (n_days in 2:100){
  ix <- sample(dim(ft2$Sol)[1],n,replace=T)
  s_age = sample(18:29,n,replace=T)#sample(dataNK3$age,n,replace=T)
  s_sex = sample(dataNK3$sex,n,replace=T)=='male'
  # 20;
  
  xs <- colSums(sapply(1:length(ix),
                       function(ii){ft2$Sol[ix[ii],] * c(1,s_age[ii],s_sex[ii])}))
  re <- rnorm(length(ix),0,sqrt(ft2$VCV[ix,1]))
  rc <- t(sapply(ix,function(ii){rnorm(n_days,0,sqrt(ft2$VCV[ii,]))}))
  xds <- matrix(rep(xs+re,n_days),ncol=n_days)+rc
  ps <- exp(xds)/(exp(xds)+1)
  eats <- rowSums(matrix(runif(length(ps)),nrow=NROW(ps))<ps)
  
  bigeats[n_days,1] = sum(eats==0)/n
  bigeats[n_days,2] = sum(eats %in% c(1,2))/n
  bigeats[n_days,3] = sum(eats>2)/n
}

rle(sort(eats))
hist(eats)

# these simulated data fits well.
rle(sort(eats))$lengths/n
RealEats$lengths/1787

eat <- (matrix(runif(length(ps)),nrow=NROW(ps))<ps)
# sapply(2:dim(eat)[2],function(ii){rowSums(eat[,1:ii])>2}7
# Summary in Norkost report: Tabell 35:
# Sjelden <4 ganger/mnd
sum(rowSums(eat[,1:30])<4)/dim(eat)[1] # At 14% i the table
# 1-2 /week
sum(rowSums(eat[,11:17]) %in% c(1,2))/dim(eat)[1] # at 60% in the table
# 3 or more / week
sum(rowSums(eat[,1:7]) > 3)/dim(eat)[1] # at 26 % in the table



dataNK3$f1 <- (dataNK3$`FISKFisk,skalldyr`-dataNK3$FISKPAFiskepålegg)
dataNK3$f2 <- ifelse(dataNK3$f1==0,0,1)
# OK, so can we model both the categorical (eatsfishmeal) and
# size of fishmeal?
ft3 <- MCMCglmm(log())

#


f1 <- MCMCglmm(fiskk~sex+age,data=datafishNK3,family='ordinal',nitt = 25000)

datafishNK3$fiskk2 = datafishNK3$fiskk
datafishNK3$fiskk2[datafishNK3$fiskk2 %in% c(2,3)] = 2
datafishNK3$fiskk2[datafishNK3$fiskk2 %in% c(4,5)] = 3
datafishNK3$fiskk2[datafishNK3$fiskk2 %in% c(6,7,8)] = 4
f2 <- MCMCglmm(fiskk2~age+wgt,data=datafishNK3,family='ordinal',nitt = 50000)





# Doesn't seem like simulate does the trick....
n_sim = 10;
n_day = 2;
newdata <- data.frame(age = rep(round(runif(n_sim,min=20,max=80)),n_day),
                      sex = rep(sample(c('female','male'),n_sim,replace=T),n_day),
                      dat = rep(1:n_day,n_sim),
                      eatsfishmeal = rep(NA,n_sim*n_day))
tmp1 <- simulate(ft2b,newdata=newdata)


tmp2 <- simulate(ft2)