
rm(list=ls())
library(MCMCglmm)
library(org) #
library(dplyr)
library(readxl)
# See here for the idea behind this structure, not sure if it's exactly
# what we need, but lets try.
# https://folkehelseinstituttet.github.io/org/articles/intro.html
org::initialize_project(
  home = "C:/Users/JSTA/Documents/R_workingDir/Gits/fish_rba",
  results = "C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/resultater",
  raw = "C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/rawdata",
  cleandata = "C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/cleandata",
  create_folders = TRUE
)






load(paste0(org::project$cleandata,"/StartClean.RData"))
Fish_groups

concs$fatfish = ifelse(concs$kbs_code %in% Fish_groups$fatfish,1,0)
concs$leanfish = ifelse(concs$kbs_code %in% Fish_groups$leanfish,1,0)

# Could also make a 'standard lean' and 'standard fat' fish as a weighted mean
# of intakes for each concentration?
#

# Concs UNITS:
# Energi	Omega3	VitD	B12	Se	I	  C20:5n-3	C22:6n-3	Fisk
# kJ	    g  	    mcg	  mcg	mcg	mcg	g	        g	        g
# UL and EAR/LowerIntake (LI). From NNR 2012,  p42
# VitD: 2.5µg/day (LI), 7.5 µg/day (AR), 100 (UL:NNR,VKM, EFSA)
# B12:  1 µg/day (LI),  1.4 µg/day (AR), 2000 (UL: VKM), 
# Iod:  70 µg(day) (LI), 100 µg/day (AR), 600 (UL:NNR, EFSA, VKM)
# Selenium: 20µg/day (LI), 30 µg/day (AR, female) 35 µg/day (male), 300 (UL:NNR)



ae18 %>% filter(hlev1_name == 'TILSKUDD Kosttilskudd 8')
keep_codes   <- ae18 %>% filter(hlev1 != 18) %>% select(kbs_code)

# 3. Do a calculation for the observed individual means for Norkost 3 (nk3data)

intake_inds <-   nk3data %>% # Take the dietary intake tibble
  inner_join(.,concs,by='kbs_code') %>%  # join it with the concentrations to make a new column
  filter(kbs_code %in% keep_codes$kbs_code) %>% # filter out only the codes we want (i.e. only fish or remove supplements)
  mutate(tmpiodine = amounts_cor*I,tmpvitd=amounts_cor*VitD,tmpse=amounts_cor*Se,
         tmpb12=amounts_cor*B12,tmpomega3=amounts_cor*`Omega 3`,tmpEPA=amounts_cor*`C20:5n-3`,
         tmpDHA=amounts_cor*`C22:6n-3`,tmpEnergy = amounts_cor*Energi,tmpsum = amounts,
         tmpfat = amounts_cor*fatfish,tmplean = amounts_cor*leanfish) %>%
  group_by(day,ind) %>% # group intakes by day and individuals and
  summarise(Iod = sum(tmpiodine,na.rm=T),
            Se  = sum(tmpse,na.rm=T),
            B12 = sum(tmpb12,na.rm=T),
            Omega3 = sum(tmpomega3,na.rm=T),
            VitD = sum(tmpvitd,na.rm=T),
            EPA = sum(tmpEPA,na.rm=T),
            DHA = sum(tmpDHA,na.rm=T),
            Ene = sum(tmpEnergy,na.rm=T),
            Grm = sum(tmpsum,na.rm=T),
            ffish = sum(tmpfat,na.rm=T),
            lfish = sum(tmplean,na.rm=T))


NK3_daily = full_join(intake_inds,indsNK3,by='ind')
# Replaced 0 for nutrients with NA. These
NK3_daily <- NK3_daily %>% mutate_at(c(3:9),function(ii){ifelse(ii==0,NA,ii)})

# Replacing 0's in the nutrients with NA


par(mfrow=c(2,4))
sapply(2:9,function(ii){
  intake_oims %>% pull(names(intake_oims)[ii]) %>% 
    log %>% density(na.rm=T) %>% 
    plot(xlab='',ylab='',main=names(intake_oims)[ii],xaxt='n',yaxt='n');
  axis(1,at=log(c(0.01,0.05,.1,.5,1,5,10,50,100,500,1000)),labels=c(0.01,0.05,.1,.5,1,5,10,50,100,500,1000))})

par(mfrow=c(2,4))
sapply(2:9,function(ii){
  qqnorm(intake_oims %>% pull(names(intake_oims)[ii]) %>% log,main=names(intake_oims)[ii])
  qqline(intake_oims %>% pull(names(intake_oims)[ii]) %>% log)})

# Looping over mcmc-models. These are with no fixed effects, and sexes mixed.
# These models then do not correct for bias due to lack of representativeness in the
# intakes. Thus no correction for age, sex or level of education. 
do <- c(3:10)
tmp <- colnames(NK3_daily)[do]




# VitD: 2.5µg/day (LI), 7.5 µg/day (AR), 100 (UL:NNR,VKM, EFSA)
# B12:  1 µg/day (LI),  1.4 µg/day (AR), 2000 (UL: VKM), 
# Iod:  70 µg(day) (LI), 100 µg/day (AR), 600 (UL:NNR, EFSA, VKM)
# Selenium: 20µg/day (LI), 30 µg/day (AR, female) 35 µg/day (male), 300 (UL:NNR)
ears <- c(100,30,1.4,NA,7.5,NA,NA,NA)
uls  <- c(600,300,2000,NA,100,NA,NA,NA)
unit_tmp <- c('µg','µg','µg','g','µg','g','g','kJ')
# Models ff: sex/age/educ
# rnd idh(sex):ind or not
# rcov idh(sex):units or not
# 2*2*2*2*2 = 32 models. For each nutrient.
Fes <- c("log(var)~1",
         "log(var)~sex",
         "log(var)~sex+age",
         "log(var)~sex+educ",
         "log(var)~sex+educ+age",
         "log(var)~age",
         "log(var)~age+educ",
         "log(var)~educ")
Res <- c("~ind","~idh(sex):ind")
Rcs <- c("~units","~idh(sex):units")
ffs <- expand.grid(1:length(Fes),1:length(Res),1:length(Rcs))

fts <- lapply(do,function(ii){
  lapply(1:dim(ffs)[1],function(jj){
    MCMCglmm(as.formula(gsub("var",as.name(colnames(NK3_daily)[ii]),Fes[ffs[jj,1]])),
                                       random=as.formula(Res[ffs[jj,2]]),
             rcov = as.formula(Rcs[ffs[jj,3]]),data=NK3_daily,family="gaussian")})})

# Saving the models
save(fts,file="C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/cleandata/simpleMod8Nutrients.RData")

# Table with 'best' models?
Fes
# Fes[ffs[which.min(sapply(1:dim(ffs)[1],function(ii){fts[[1]][[ii]]$DIC})),1]
which.min(sapply(1:dim(ffs)[1],function(ii){fts[[ll]][[ii]]$DIC}))

sapply(1:8,function(ll){c(Fes[ffs[which.min(sapply(1:dim(ffs)[1],function(ii){fts[[ll]][[ii]]$DIC})),1]],
                          Res[ffs[which.min(sapply(1:dim(ffs)[1],function(ii){fts[[ll]][[ii]]$DIC})),2]],
                          Rcs[ffs[which.min(sapply(1:dim(ffs)[1],function(ii){fts[[ll]][[ii]]$DIC})),3]])})










sapply(1:8,function(ll){c(Fes[ffs[which.min(sapply(1:dim(ffs)[1],function(ii){fts[[ll]][[ii]]$DIC})),1]],
Res[ffs[which.min(sapply(1:dim(ffs)[1],function(ii){fts[[ll]][[ii]]$DIC})),2]],
Rcs[ffs[which.min(sapply(1:dim(ffs)[1],function(ii){fts[[ll]][[ii]]$DIC})),3]])})

fts2 <- lapply(do,function(ii){MCMCglmm(as.formula(paste0('log(',as.name(colnames(NK3_daily)[ii]),') ~1')),
                                        random=~idh(sex):ind,data=NK3_daily,family="gaussian")})
fts2 <- lapply(do,function(ii){MCMCglmm(as.formula(paste0('log(',as.name(colnames(NK3_daily)[ii]),') ~sex')),
                                       random=~idh(sex):ind,data=NK3_daily,family="gaussian")})
fts2 <- lapply(do,function(ii){MCMCglmm(as.formula(paste0('log(',as.name(colnames(NK3_daily)[ii]),') ~sex')),
                                        random=~idh(sex):ind,
                                        rcov = ~idh(sex):units,data=NK3_daily,family="gaussian")})

fts2 <- lapply(do,function(ii){MCMCglmm(as.formula(paste0('log(',as.name(colnames(NK3_daily)[ii]),') ~sex')),
                                        random=~idh(sex):ind,
                                        rcov = ~idh(sex):units,data=NK3_daily,family="gaussian")})
fts2 <- lapply(do,function(ii){MCMCglmm(as.formula(paste0('log(',as.name(colnames(NK3_daily)[ii]),') ~sex')),
                                        random=~idh(sex):ind,data=NK3_daily,family="gaussian")})
fts2 <- lapply(do,function(ii){MCMCglmm(as.formula(paste0('log(',as.name(colnames(NK3_daily)[ii]),') ~sex')),
                                        random=~idh(sex):ind,data=NK3_daily,family="gaussian")})

smd <- lapply(1:length(fts),function(ii){sapply(c(1,2,10,50,100),function(jj){
  drawInt_1(fts[[ii]],n_ind=1787,n_days=jj)})})

par(mfrow=c(2,4))
for (ii in 1:length(fts)){
  plot(density(NK3_daily %>% pull(tmp[[ii]]),from=0,na.rm=T),col='red',main=tmp[ii],yaxt='n',ylab='',
       xlab=paste0(unit_tmp[ii],'/day'))
  rug(NK3_daily %>% pull(tmp[[ii]]),col=rgb(0.3,0.3,0.3,0.3))
  for (jj in 5){
    lines(density(smd[[ii]][,jj],from=0),col=rgb(0.3,.3,.3,.1+jj*.13))
  }
  abline(v=c(ears[ii],uls[ii]),col=rgb(0.8,0.1,0.1,0.3))
  
}


intake_oims <- NK3_daily %>% group_by(ind) %>% summarise(Iod_oim = mean(Iod),
                                                           Se_oim =mean(Se),
                                                           B12_oim=mean(B12),
                                                           Omega3_oim=mean(Omega3),
                                                           VitD_oim=mean(VitD),
                                                           EPA_oim=mean(EPA),
                                                           DHA_oim=mean(DHA),
                                                           Ene_oim=mean(Ene),
                                                           Grm_oim=mean(Grm),
                                                           .groups = "drop_last")

myf <- function(ii,ear,ul){c(quantile(ii,c(0.05,0.5)),'mean' = mean(ii),quantile(ii,0.95),
                             'Above EAR' = round(100*sum(ii>ear)/length(ii)),
                             'Above UL'=round(100*sum(ii>ul)/length(ii)))}
myf(smd[[5]][,5],ear=2.5,ul=100)

mf <- function(ii){quantile(ii,c(0.05,0.5,0.95),na.rm=T)}
js <- intake_oims %>% 
  full_join(.,indsNK3,by='ind') %>%
  group_by(sex) %>%
  summarise_if(is.numeric,mf) %>%
  select(1:7) 
js <- round(100*do.call(rbind,c(js)))/100

# %>%
#   kbl(digits=2) %>%
#   kable_styling()


f1 <- MCMCglmm(log(VitD)~sex+age,
               random=~ind,
               data=as.data.frame(NK3_daily),
               family="gaussian")


f2 <- MCMCglmm(log(VitD)~1,
               random=~ind,
               data=as.data.frame(NK3_daily),
               family="gaussian")


tmp <- sapply(1:n_ind,function(ii){
  mean(exp(rnorm(n_days,0,f2$VCV[ix[ii],2]))*exp(f2$Sol[ix[ii],]+rnorm(1,0,f2$VCV[ix[ii],1])))})



oims <- NK3_daily %>% group_by(ind) %>% summarise(vitD = mean(VitD,na.rm=T))


sm <- drawInt_1(f2,n_ind=1787,n_days=2)
sm1 <- drawInt_1(f2,n_ind=1787,n_days=1)
dev.off()
par(mfrow=c(2,2))
plot(density(oims$vitD,from=0),main='two days')
lines(density((sm),from=0),col='red')
plot(density(log(oims$vitD)))
lines(density(log(sm)),col='red')

plot(density(NK3_daily$VitD,from=0,na.rm=T),main='one day')
lines(density((sm1),from=0),col='red')
plot(density(log(NK3_daily$VitD),na.rm=T))
lines(density(log(sm1)),col='red')



quantile(oims$vitD,c(0,0.05,0.5,0.95,1),na.rm=T)
quantile(sm,c(0,0.05,0.5,0.95,1))

quantile(NK3_daily$VitD,c(0,0.05,0.5,0.95,1),na.rm=T)
quantile(sm1,c(0,0.05,0.5,0.95,1))


matplot(t(sapply(1:30,function(jj){quantile(drawInt_1(f2,n_ind=1787,n_days=jj),c(0.05,0.5,0.95))})),type="l",lty=c(2,1,2))



f3 <- MCMCglmm(log(Iod)~1,
               random=~ind,
               data=as.data.frame(NK3_daily),
               family="gaussian")

smIo <- drawInt_1(f3,n_ind=1e5,n_days=2)
smIo1 <- drawInt_1(f3,n_ind=1e5,n_days=1)
oims <- NK3_daily %>% group_by(ind) %>% summarise(Iod = mean(Iod,na.rm=T))

par(mfrow=c(2,2))
plot(density(oims$Iod,from=0),main='two days')
lines(density((smIo),from=0),col='red')
plot(density(log(oims$Iod)))
lines(density(log(smIo)),col='red')

plot(density(NK3_daily$Iod,from=0,na.rm=T),main='one day')
lines(density((smIo1),from=0),col='red')
plot(density(log(NK3_daily$Iod),na.rm=T))
lines(density(log(smIo1)),col='red')



makemc <- function(jj,var){MCMCglmm(gsub("jj",var,"log(var)~1"),
                                    random=~ind,data=jj,family="gaussian")}


drawInt_1 <- function(mcmcfit,n_ind=100,n_days=2){
  # Basic function do draw intakes as estimated by a MCMCglmm model with NO
  # fixed effects ,i.e. intake~1. Assumed that random effects are at the level of 
  # individuals and residuals are daily.
  ix <- sample(dim(mcmcfit$Sol)[1],n_ind,replace=TRUE) # which pars to sample from
  int_sim <- sapply(1:n_ind,function(ii){
    mean(exp(rnorm(n_days,0,sqrt(mcmcfit$VCV[ix[ii],2])))*exp(mcmcfit$Sol[ix[ii],]+rnorm(1,0,sqrt(mcmcfit$VCV[ix[ii],1]))))})
  return(int_sim)
}


intake_oims <- intake_inds %>% group_by(ind) %>% summarise(Iod_oim = mean(Iod),
                                                           Se_oim =mean(Se),
                                                           B12_oim=mean(B12),
                                                           Omega3_oim=mean(Omega3),
                                                           VitD_oim=mean(VitD),
                                                           EPA_oim=mean(EPA),
                                                           DHA_oim=mean(DHA),
                                                           Ene_oim=mean(Ene),
                                                           Grm_oim=mean(Grm))




par(mfrow=c(2,4))
for (ii in 2:9){
  plot(density(intake_oims %>% pull(ii),from=0),main=colnames(intake_oims)[ii],xlab='Intake',ylab='',yaxt='n')
  rug(intake_oims %>% pull(ii))
}










keep_codes_fat <- setdiff(as.numeric(Fish_groups$fatfish),0)
keep_codes_lean <- setdiff(as.numeric(Fish_groups$leanfish),0)
# ae18 %>% filter(hlev1_name %in% 'FISKFisk,skalldyr9') #%>% pull(kbs_code)
fiskedata <-   nk3data %>% # Take the dietary intake tibble
  filter(kbs_code %in% keep_codes_fat) %>% # filter out only the codes we want (i.e. only fish or remove supplements)
  group_by(day,ind) %>% # group intakes by day and individuals and
  summarise(fishperday_fat = sum(amounts_cor,na.rm=T)) %>% #take the sum, this sums all amounts*conc for each day within each individual
  inner_join(.,indsNK3,by='ind')
# How to also have it output the ones that are 0?
# Perhaps we should have a dummy category with 0 intake?

fiskedata2 <-   nk3data %>% # Take the dietary intake tibble
  filter(kbs_code %in% keep_codes_lean) %>% # filter out only the codes we want (i.e. only fish or remove supplements)
  group_by(day,ind) %>% # group intakes by day and individuals and
  summarise(fishperday_lean = sum(amounts_cor,na.rm=T)) #%>% #take the sum, this sums all amounts*conc for each day within each individual
# inner_join(.,indsNK3,by='ind')
# How to also have it output the ones that are 0?
# Perhaps we should have a dummy category with 0 intake?

# Need to do this in a different way to have all individuals
# in the dataframe, including the ones with no reported fish-intake.
tmp <- expand.grid(ind = levels(nk3data$ind),
                   day = levels(nk3data$day))
# Laking covariates for those not eating...
fiskedat <- tmp %>% full_join(.,fiskedata2,by=c('ind','day'))
tmpfishdata <- fiskedat %>% mutate(eatslean=if_else(is.na(fishperday_lean),0,1)) %>%
  inner_join(.,indsNK3,by='ind')


# This only returns the ones with non-zero fishintake;
fishdata_fl <- tmpfishdata %>% full_join(fiskedat,by=c('ind','day'))

fishdata_fl$eatsfat <- ifelse(is.na(fishdata_fl$fishperday_fat),0,1)

rle(sort(rowSums(fishdata_fl[,c(4,10)])))
sum(fishdata_fl$eatsfat)
sum(fishdata_fl$eatslean)
# 172 inddays 

# fix = 1 makes all fixed. Does 3 make all from 3ff (i.e. 3&4) fixed?
prio1 = list(R = list(V = diag(4),nu=3,fix=c(3)),
             G = list(G1 = list(V= diag(4)/1   , nu=3)))
fiskemod1 <- MCMCglmm(cbind(log(fishperday_lean),log(fishperday_fat),eatslean,eatsfat)~trait*sex+trait*age,
                      random= ~ idh(trait):ind,
                      rcov  = ~ idh(trait):units,
                      family = c('gaussian','gaussian','categorical','categorical'),
                      data = fishdata_fl,
                      prior = prio1,
                      nitt = 25000)

prio2 = list(R = list(V = diag(4),nu=3,fix=c(3)),
             G = list(G1 = list(V= diag(4)/1   , nu=3)))
fiskemod2 <- MCMCglmm(cbind(log(fishperday_lean),log(fishperday_fat),eatslean,eatsfat)~trait*sex+trait*age-1,
                      random= ~ us(trait):ind,
                      rcov  = ~ idh(trait):units,
                      family = c('gaussian','gaussian','categorical','categorical'),
                      data = fishdata_fl,
                      prior = prio1,
                      nitt = 25000)


which(apply(sign(summary(fiskemod2$VCV)$quantiles[,c(1,5)]),1,prod)==1)
# Which of the covariances are sign?
# so there is a covariance in tendency to eat lean and fat
summary(fiskemod2$VCV[,which(apply(sign(summary(fiskemod2$VCV)$quantiles[,c(1,5)]),1,prod)==1)])
# Note to self idh(trait) yields covariance between probability of eating fish and amount. Ignore.



prio1 <- list(R=list(V=diag(2),nu=3,fix=1),
              G = list(G1 = list(V=diag(2),nu=3)))
fiskemod_tmp <- MCMCglmm(cbind(eatslean,eatsfat)~trait*sex+trait*age-1,
                         random= ~ us(trait):ind, 
                         rcov  = ~ idh(trait):units,
                         family = c('categorical','categorical'),
                         data = fishdata_fl,
                         prior = prio1,
                         nitt = 25000)














prio2 = list(R = list(V = diag(1),nu=3,fix=1),
             G = list(G1 = list(V= diag(2)/1   , nu=3)))
fiskemod2 <- MCMCglmm(eatsfish~age,
                      random= ~ idh(sex):ind,
                      rcov  = ~ units,
                      family = c('categorical'),
                      data = fatfishdata,
                      prior = prio2,
                      nitt = 25000)
# Older are more likely to eat fish at any given day. Noe mean 
# difference between the sexes, but males vary more in tendency.
# Fiskemod2 is here a model of the propensity to eat fish or
# not on a single day. Perhaps this is how we should attack this,
# only in terms of frequencies?

# p <- exp(l)/(1+exp(l))

summary(fiskemod2)
ags <- 18:70
colMeans(fiskemod2$Sol)[c(1,3)]

n_ind <- 1000;
ags <- sample(fiskedat$age,20);
n_days <- 7

nsF <- mysmp(ags=sample(fatfishdata$age,100),n_ind=1500,n_days=2,fiskemod2 = fiskemod2)
nsM <- mysmp(ags=sample(fatfishdata$age,100),n_ind=1500,n_days=2,fiskemod2 = fiskemod2,male=T)
par(mfrow=c(1,2))
hist(nsF)
hist(nsM)

rle(sort(tmp[fatfishdata[1:1787,]$sex=='male']))$length/sum(fatfishdata[1:1787,]$sex=='male')
rle(sort(nsM))$length/length(nsM)
rle(sort(tmp[fatfishdata[1:1787,]$sex=='female']))$length/sum(fatfishdata[1:1787,]$sex=='female')
rle(sort(nsF))$length/length(nsF)

plot(rowMeans(ns),type="l")

tmp <- sapply(unique(fatfishdata$ind),function(ii){sum(fatfishdata[fatfishdata$ind==ii,]$eatsfish)})
table(ns)/length(ns)
table(tmp)/length(tmp)

mysmp <- function(ags,n_days=7,n_ind=100,fiskemod2,male=F){
  Nfs <- array(0,dim=c(length(ags),n_ind))
  for (aa in 1:length(ags)){
    l <- colMeans(fiskemod2$Sol)[1] + colMeans(fiskemod2$Sol)[2]*ags[aa] + 
      matrix(rep(rnorm(n_ind,0,sqrt(colMeans(fiskemod2$VCV)[1+1*male])),n_days),byrow=T,nrow = n_days)+
      matrix(replicate(n_ind,rnorm(n_days,0,sqrt(colMeans(fiskemod2$VCV)[3]))),byrow=F,nrow=n_days)
    
    Nfs[aa,] <- colSums(matrix(runif(prod(dim(l))),nrow=NROW(l))<(exp(l)/(1+exp(l))))
  }
  return(Nfs)
}


mysmp2 <- function(ags,n_days=7,n_ind=100,fiskemod2,male=F){
  Nfs <- array(0,dim=c(length(ags),n_ind))
  for (aa in 1:length(ags)){
    ix <- sample(dim(fiskemod2$Sol)[1],n_ind)
    
    l <- matrix(rep(fiskemod2$Sol[ix,1] + fiskemod2$Sol[ix,2]*ags[aa],n_days),byrow=T,ncol=2) + 
      matrix(rep(rnorm(n_ind,0,sqrt(fiskemod2$VCV[ix,1+1*male])),n_days),byrow=T,nrow = n_days)+
      matrix(replicate(n_ind,rnorm(n_days,0,sqrt(fiskemod2$VCV[ix,3]))),byrow=F,nrow=n_days)
    
    Nfs[aa,] <- colSums(matrix(runif(prod(dim(l))),nrow=NROW(l))<(exp(l)/(1+exp(l))))
  }
  return(Nfs)
}
nsF2 <- mysmp2(ags=sample(fatfishdata$age,100),n_ind=10,n_days=2,fiskemod2 = fiskemod2)
nsM2 <- mysmp2(ags=sample(fatfishdata$age,100),n_ind=1500,n_days=2,fiskemod2 = fiskemod2,male=T)
par(mfrow=c(1,2))
hist(nsF)
hist(nsM)
