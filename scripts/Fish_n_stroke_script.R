# Take all fish in Norkost 3 and sum for all inds

rm(list=ls())
library(MCMCglmm)
library(org) #
library(dplyr)
library(readxl)
source('C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/r_code/sampSSB.R')
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


# keep_codes_fat <- setdiff(as.numeric(Fish_groups$fatfish),0)
# keep_codes_lean <- setdiff(as.numeric(Fish_groups$leanfish),0)

# Using these KBS-codes as fish. Sums over all intakes here using the _cor_ values
# which are corrected for weight and content of fish-filet. This might not
# correspond to 'fish' as used in the meta-analysis and should be considered.
# If using 'raw' weights of fish (i.e. 100 grams of fish-cake = 100 gram of fish)
# use = sum(amount,na.rm=T) in the pipeline below.
# keep_codes_all <- setdiff(as.numeric(Fish_groups$fish),c(0,
# ae18 %>% filter(kbs_code %in% Fish_groups$otherseafood) %>% pull(kbs_code) %>% as.numeric))

keep_codes_all <- setdiff(as.numeric(Fish_groups$fish),c(0,as.numeric(Fish_groups$otherseafood)))
fiskedata <-   nk3data %>% # Take the dietary intake tibble
  filter(kbs_code %in% keep_codes_all) %>% # filter out only the codes we want (i.e. only fish or remove supplements)
  group_by(day,ind) %>% # group intakes by day and individuals and
  summarise(fishperday_all = sum(amounts_cor,na.rm=T)) #%>% #take the sum, this sums all amounts*conc for each day within each individual
# in the dataframe, including the ones with no reported fish-intake.
tmp <- expand.grid(ind = levels(nk3data$ind),
                   day = levels(nk3data$day))
# Laking covariates for those not eating...
fiskedat <- tmp %>% full_join(.,fiskedata,by=c('ind','day'))
fishdata <- fiskedat %>% mutate(eatsfish=if_else(is.na(fishperday_all),0,1)) %>%
  inner_join(.,indsNK3,by='ind')


# fishdata %>% group_by(ind) %>% summarize(oim = log10(sum(fishperday_all,na.rm=T))) %>% pull(oim) %>% hist
oims <- fishdata %>% group_by(ind) %>% summarize(oim = sum(fishperday_all,na.rm=T)/2) %>% pull(oim)

# fix = 1 makes all fixed. Does 3 make all from 3ff (i.e. 3&4) fixed?
prio1 = list(R = list(V = diag(2),nu=3,fix=c(2)),
             G = list(G1 = list(V= diag(2)/1   , nu=3)))
fiskemod1 <- MCMCglmm(cbind(log(fishperday_all),eatsfish)~trait*sex+trait*age,
                      random= ~ idh(trait):ind,
                      rcov  = ~ idh(trait):units,
                      family = c('gaussian','categorical'),
                      data = fishdata,
                      prior = prio1,
                      nitt = 25000)

fiskemod2 <- MCMCglmm(cbind(log(fishperday_all),eatsfish)~trait*sex+trait*age,
                      random= ~ us(trait):ind,
                      rcov  = ~ idh(trait):units,
                      family = c('gaussian','categorical'),
                      data = fishdata,
                      prior = prio1,
                      nitt = 25000)
# Seems like people who eat fish more often do not necessarily eat more fish if they eat.
# Do males vary more or less than females in amounts/tendency to eat fish?
# Level of education?
# But it does seem like sex has little impact on whether or not they eat fish

fiskemod3 <- MCMCglmm(cbind(log(fishperday_all),eatsfish)~at.level(trait,1):sex+trait*age,
                      random= ~ idh(trait):ind,
                      rcov  = ~ idh(trait):units,
                      family = c('gaussian','categorical'),
                      data = fishdata,
                      prior = prio1,
                      nitt = 25000)
sampSSB(10)

mysamp <- function(ft,n_ind,n_days,drawnew=T,ages=c(18,70),sex='both'){
  # n_ind = 20;
  # ft <- fiskemod3
  if (drawnew){
  cv <- sampSSB(n_ind,sex=sex,ages=ages)[,c(1,2,4)]
  cv$sexfemale = 1*(cv$sexmale==0) # in this model the female became 1
  } else {
    nkix <- sample(1:1787,n_ind,replace=T)
    cv <- data.frame(`X.Intercept.` = rep(1,n_ind),
                     age = indsNK3$age[nkix],
                     sexfemale = 1*(indsNK3$sex[nkix]=='female'))
      # ft$X[1:1787,c(1,3,4)]
  }
  ix <- sample(dim(ft$Sol)[1],n_ind,replace=TRUE)
  # Eat or not
  cvtmp <- cbind(cv$X.Intercept.,cv$X.Intercept.,cv$age,cv$age)
  lx_tmp <- rowSums(cvtmp * ft$Sol[ix,c(1,2,3,5)]) + rnorm(n_ind,mean=0,sd=sqrt(ft$VCV[ix,2]))
  # Need to add days
  lx <- matrix(rep(lx_tmp,n_days),byrow=F,ncol=n_days) + matrix(rnorm(n_ind*n_days,0,ft$VCV[ix,4]),byrow=F,ncol=n_days)
  
  exp(lx)/(1+exp(lx))> runif(n_ind*n_days)
  
  # Amounts
  cvtmp2 <- cbind(cv$X.Intercept.,cv$age,cv$sexfemale)
  lx2_tmp <- rowSums(cvtmp2 * ft$Sol[ix,c(1,3,4)]) + rnorm(n_ind,mean=0,sd=sqrt(ft$VCV[ix,1]))
  lx2 <- matrix(rep(lx2_tmp,n_days),byrow=F,ncol=n_days) + matrix(rnorm(n_ind*n_days,0,sqrt(ft$VCV[ix,3])),byrow=F,ncol=n_days)
  
  
  eats <- exp(lx)/(1+exp(lx))> runif(n_ind*n_days)
  amounts <- exp(lx2)
  
  return(list(eats=eats,amount=amounts,lat_prob = lx,lat_amnt = lx2,cv=cv))
}

ms <- mysamp(fiskemod3,n_ind=1e4,n_days=150,ages=c(18,49))
dlintak <- rowMeans(ms$eats*ms$amount)
oims <- fishdata %>% group_by(ind) %>% filter(age<50) %>% summarize(oim = sum(fishperday_all,na.rm=T)/2) %>% pull(oim)

hist(rr_norm(dlintak)$mu)
plot(dlintak,rr_norm(dlintak)$mu,ylim=c(0.8,1.1),xlim=c(0,150))
mean(rr_norm(dlintak)$mu)


ages18_70_men<-c(34379,34676,34048,34896,35746,35789,35857,36328,37618,38224,38744,38696,38400,37094,37425,36912,36433,36697,36765,35618,36496,35890,35731,34544,35594,36237,37644,38042,39157,38968,38611,39296,38891,37784,37842,37089,36341,34436,33786,32967,32503,32448,31775,30837,30966,29972,29040,28378,27952,26627,26679,26489,26585)
ages18_70_women<-c(31982,31912,31815,32531,33458,33285,33806,34469,35357,36612,37725,37415,37281,36174,35522,34968,34763,34148,34599,34097,34252,33835,33516,32815,33435,34612,36083,35791,37092,37258,36742,37471,37063,36134,35504,35004,34392,33287,32172,31709,31261,31131,31079,30376,30709,29818,29171,28768,28049,26413,26856,26793,26784)
# In 
# Slag diagnoser fra Hjerte og karregisteret.
# x	x	2015	2016	2017	2018	2019
# Mann	0-49 år	636	634	635	652	651
# Mann	50-69 år	2473	2513	2560	2620	2594
# Mann	70-89 år	3538	3701	3738	3914	4114
# Mann	90+ år	324	332	329	349	361
# Kvinne	0-49 år	422	462	470	445	497
# Kvinne	50-69 år	1359	1332	1330	1301	1381
# Kvinne	70-89 år	3198	3384	3318	3420	3545
# Kvinne	90+ år	814	803	753	798	715

### 18-49 YEARs
# So 18-49 years;
(651+497)/sum(ages18_70_men[1:32]+ages18_70_women[1:32])
(636+634+635+652+651+497+462+470+445+422)/(5*sum(ages18_70_men[1:32]+ages18_70_women[1:32]))*100
(1100.8)/sum(ages18_70_men[1:32]+ages18_70_women[1:32])*100
# Probability of stroke in year 2019 is over all 0.05% for this group.
# So absolute 
mean(rr_norm(dlintak)$mu)
median(rr_norm(dlintak)$mu)
# So the overall relative risk for this group seems to be 93%, compared to if the
# group had 0 fish intake. If everyone increased their fish intake by 20%
mean(rr_norm(dlintak*1.2)$mu)
median(rr_norm(dlintak*1.2)$mu)

plot(sapply(seq(0,1.5,by=0.01),function(ii){mean(dlintak*ii)}),
     sapply(seq(0,1.5,by=0.01),function(ii){mean(rr_norm(dlintak*ii)$mu)}),type="l")
lines(0:100,mnf(0:100),lty=2)
# So this is the overall relative risk with %-wise increases in fish consumpsion
# for all (there is hardly ANY that absolutely never eat fish in the simulated data)
# This implies that the variability in intake makes the RR-curve less steep (compare full with dashed)

# Relative risk is really (prob stroke in group eating fish at x)/(prob strok at 0 fish)
# We have now estimated prob strok at x intake (0.05% from insidence above)
# RR_now --> abs_now / abs_imaginary
# RR_new --> abs_new / abs_imaginary
# abs_new = RR_new * abs_imaginary
# substituting abs_imaginary = abs_now/RR_now
# abs_new = RR_new * abs_now / RR_now
# abs_new = abs_now * (RR_new/RR_now)
# There are some issue here on 'mean' and meaning - we're using the national group
# level absolute risk, and the mean group level relative risk estimated from
# dietary data. This, e.g., ignores that some individuals here have a much higher
# consumption of fish and likely a higher relative risk (and vice versa).
# Here we use the mean of the dose-response curve
# par(mfrow=c(1,2))
layout(matrix(c(1,2),nrow=2),heights=c(1,0.3))
par(mar=c(0,4,3,3))
plot(sapply(seq(0,1.5,by=0.01),function(ii){mean(dlintak*ii)}),
     (651+497)*sapply(seq(0,1.5,by=0.01),function(ii){mean(rr_norm(dlintak*ii)$mu)})/mean(rr_norm(dlintak)$mu),type="l",
     ylab='#strokes',xlab='mean fish intake [g/day]',
     main = 'Estimated number of strokes in adults (18-49) \n in relaton to fish intake',cex.main=0.7,
     xaxt='n',xlim=c(0,150))
abline(v = mean(dlintak))
abline(h=(651+497))
rect(300/7,0,450/7,10000,col=rgb(0.1,0.1,0.1,0.1),border= NA)
grid(col=rgb(0.3,0.3,0.3,0.3))
par(mar=c(3,4,0,3))
plot(density(dlintak,from=0),main='',xlim=c(0,150),yaxt='n')
grid(col=rgb(0.3,0.3,0.3,0.3),ny=0)
abline(v=mean(dlintak))
# oims <- ifelse(is.na(oims),0,oims)


plot(sapply(seq(0,1.5,by=0.01),function(ii){mean(oims*ii)}),
     (651+497)*sapply(seq(0,1.5,by=0.01),function(ii){mean(rr_norm(oims*ii)$mu)})/mean(rr_norm(oims)$mu),type="l",
     ylab='#strokes',xlab='mean fish intake [g/day]',
     main = 'Estimated number of strokes in adults (18-49) \n in relaton to fish intake',cex.main=0.7,
     xaxt='n',xlim=c(0,150))
abline(v = mean(oims))
abline(h=(651+497))
rect(300/7,0,450/7,10000,col=rgb(0.1,0.1,0.1,0.1),border= NA)
grid(col=rgb(0.3,0.3,0.3,0.3))
rug(oims)

## 50-69 yrs
oims2 <- fishdata %>% group_by(ind) %>% filter(age>49) %>% summarize(oim = sum(fishperday_all,na.rm=T)/2) %>% pull(oim)
ms2 <- mysamp(fiskemod3,n_ind=1e4,n_days=150,ages=c(50,69))
dlintak2 <- rowMeans(ms2$eats*ms2$amount)
(2594+1381)/sum(ages18_70_men[33:52]+ages18_70_women[33:52])*100

par(mfrow=c(1,2))
plot(sapply(seq(0,1.5,by=0.01),function(ii){mean(dlintak2*ii)}),
     (2594+1381)*sapply(seq(0,1.5,by=0.01),function(ii){mean(rr_norm(dlintak2*ii)$mu)})/mean(rr_norm(dlintak2)$mu),type="l",
     ylab='#strokes',xlab='mean fish intake [g/day]',
     main = 'Estimated number of strokes in adults (50-69) \n in relaton to fish intake',cex.main=0.7)
abline(v = mean(dlintak2))
abline(h=(2594+1381))
rect(300/7,0,450/7,10000,col=rgb(0.1,0.1,0.1,0.1),border= NA)
grid(col=rgb(0.3,0.3,0.3,0.3))

# oims <- ifelse(is.na(oims),0,oims)
plot(sapply(seq(0,1.5,by=0.01),function(ii){mean(oims2*ii)}),
     (2594+1381)*sapply(seq(0,1.5,by=0.01),function(ii){mean(rr_norm(oims2*ii)$mu)})/mean(rr_norm(oims2)$mu),type="l",
     ylab='#strokes',xlab='mean fish intake [g/day]',main='Same using oims')
abline(v = mean(oims2))
abline(h=(2594+1381))
rect(300/7,0,450/7,10000,col=rgb(0.1,0.1,0.1,0.1),border= NA)
grid(col=rgb(0.3,0.3,0.3,0.3))
# It is important to note that the current intake is what hinges the curve, so
# different means of calculating the intake is what fastenes the curve to the
# current absolute # of strokes. 



hist(dlintak,101)


# oims <- sapply(unique(fishdata$ind),function(ii){mean(fishdata[fishdata$ind==ii,]$fishperday_all,na.rm=T)})
oims <- fishdata %>% group_by(ind) %>% summarize(oim = sum(fishperday_all,na.rm=T)/2) %>% pull(oim)


hist(oims,51)
dnstmp <- density(rowMeans(ms$eats*ms$amount),from=0)
lines(dnstmp$x,500*dnstmp$y/(max(dnstmp$y)),type="l",lwd=2)

quantile(rowMeans(ms$eats*ms$amount)[rowMeans(ms$eats*ms$amount)>0])
quantile(oims,na.rm=T)

js<- fishdata %>% group_by(ind) %>% summarise(nf = sum(eatsfish)) %>% pull(nf) %>% table

table(rowSums(ms$eats))/dim(ms$eats)[2]
js/sum(js)






hist(ms$eats*ms$amount)
rowSums(eats*exp(lx2))/n_days
plot(density(rowMeans(ms$eats*ms$amount),from=0),xlim=c(0,1000))
rug(oim)
cv * cbind(c(rowSums(ft$Sol[ix,c(1,2)])),ft$Sol[ix,c(3,4,5)])


exp(rowSums(cvtmp * ft$Sol[ix,]))/(1+exp(rowSums(cvtmp * ft$Sol[ix,])))
