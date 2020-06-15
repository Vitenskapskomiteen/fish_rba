
# Script to initialize folderstructure etc for 
# VKM: Benefit- risk assessment of Fish.

# 
# Setting up the folder structure:

# @Josef, Sofie etc you can add your own paths here, e.g.
# home = c('josteins home path','josefs homepath'), and we could, when
# running the code locally, just ## out the ones we don't use

rm(list=ls())
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

# I have synced the folders under 'Risk and benefit assessment of fish'
# on Sharepoint/Risken to my local folders. For those who have Onedrive that should
# work nicely, try the 'Synkroniser' button on the main 'Dokumenter' page.
# As you might see from above my local folder is C:/Users/JSTA... this will
# of course not work for you, so you can set your own. 


# Files in folder 'cleandata' should be RData files
# files_toread <- paste0(org::project$cleandata,'/',dir(org::project$cleandata))
# for (ii in 1:length(files_toread)){load(files_toread[[ii]])}
# These now include the intakes for 4 surveys 
# Norkost 3  - adults, 2 days, n= 1787. Covariates in indsNK3 and intakes in nk3data
# Ungkost 4  -  4 yo, 4 consecutive days, n= 399 Covariates in indsUK4  and intakes in uk4data
# Ungkost 9  -  9 yo, 4 consecutive days, n= 636 Covariates in indsUK9  and intakes in uk9data
# Ungkost 13 - 13 yo, 4 consecutive days, n= 687 Covariates in indsUK13 and intakes in uk13data

# Additionally I have generated a file for the hierarchical system of code
# for the different types of food (Publicly in Norway known as AE-18, all here are given name kbs_code and kbs_name).
# ae18 contains all the codes in the system with names and hierarchical
# groupings.
# 
# All these are saved in the 'dietarydata.RData' file
load(paste0(org::project$cleandata,'/dietarydata.RData'))

# These datasets are stored as tibbles (like a slightly advanced data.frame, used in dplyr).
# This allows for easy "pipelining" using dplyr and less need for loops
# and if statements etc. 

# Just to show a few examples of how to gather/calculate the intakes;
# Extracting the kbs_codes for fish;
fishcodes <- ae18 %>% filter(hlev1_name == 'FISKFisk,skalldyr9') %>% select(c(kbs_code,kbs_name))
head(fishcodes)




# Generating intakes
# 1. Extract arrayof concentrations for each code
colnames(concs)
concentrations <- concs %>% select(c(kbs_code = 'kbs_code',conc = 'Energi'))
# Above will extract the 'concentrations' of energy in the foods,
# This now holds ALL concentrations regardless of whether they are consumed.

# 2. Select which kbs_codes to include in the intake calculations.
# E.g. Excluding supplements
ae18 %>% filter(hlev1_name == 'TILSKUDD Kosttilskudd 8')
keep_codes   <- ae18 %>% filter(hlev1 != 18) %>% select(kbs_code)

# 3. Do a calculation for the observed individual means for Norkost 3 (nk3data)

intake_oims <-   nk3data %>% # Take the dietary intake tibble
  inner_join(.,concentrations,by='kbs_code') %>%  # join it with the concentrations to make a new column
  filter(kbs_code %in% keep_codes$kbs_code) %>% # filter out only the codes we want (i.e. only fish or remove supplements)
  mutate(perfood_in = amounts*conc) %>% #make a new column (perfood_in) with the products of amounts and concentrations
  group_by(day,ind) %>% # group intakes by day and individuals and
  summarise(perday_in = sum(perfood_in,na.rm=T)) %>% #take the sum, this sums all amounts*conc for each day within each individual
  group_by(ind) %>% # group the daily intake by individual
  summarise(oims = mean(perday_in)) # and take the mean within each individual
# Some warning on the use of factors for the kbs_codes here, but
# I think we're fine. 

plot(density(intake_oims$oims,from=0),main='Energy intake - Observed individual means',ylab='',xlab='kJoule?',yaxt='n')


# Skip last two lines to get per day intakes;
intake_perday <-   nk3data %>% # Take the dietary intake tibble
  inner_join(.,concentrations,by='kbs_code') %>%  # join it with the concentrations to make a new column
  filter(kbs_code %in% keep_codes$kbs_code) %>% # filter out only the codes we want (i.e. only fish or remove supplements)
  mutate(perfood_in = amounts*conc) %>% #make a new column with the products of amounts and concentrations
  group_by(day,ind) %>% # group intakes by day and individuals and
  summarise(perday_in = sum(perfood_in,na.rm=T)) #take the sum, this sums all amounts*conc for each day within each individual

lines(density(intake_perday$perday_in,from=0),lty=2) # Slightly more variation in daily intakes

# We could also merge (using inner_join) this with individual level covariates;
energidata <-   nk3data %>% # Take the dietary intake tibble
  inner_join(.,concentrations,by='kbs_code') %>%  # join it with the concentrations to make a new column
  filter(kbs_code %in% keep_codes$kbs_code) %>% # filter out only the codes we want (i.e. only fish or remove supplements)
  mutate(perfood_in = amounts*conc) %>% #make a new column with the products of amounts and concentrations
  group_by(day,ind) %>% # group intakes by day and individuals and
  summarise(perday_in = sum(perfood_in,na.rm=T)/1000) %>% #take the sum, this sums all amounts*conc for each day within each individual
  inner_join(.,indsNK3,by='ind')

par(mfrow=c(1,2))
plot(perday_in~age+sex,data=energidata)


# A quick model of the energyintake. These are generalized mixed models
# where we partition the variance between individuals and between days 
# within individuals. 
# After fitting such a model we can 'simulate' intakes to correct for
# two things:
# - low number of days, we can get a better estimate of long-term mean intakes for individuals
# - biases in the subpopulation, i.e. we can correct for the high percentage of highly educated individuals or we can appropriately get intake estimates for a subgroup.
# Simulating such requires a bit more functions coded, and I haven't done that quite yet.


# Example;
library(MCMCglmm)
# Here we model the daily intake with a fixed effect of sex, age and level of education.
# All males are assumed to vary at the level of individuals (and females): ~idh(sex):ind,
# All males/females are also assuemd to vary between days: rcov = ~idh(sex):units; 
# (Units is the term for residuals in the MCMCglmm universe)
# Fitting the model;
f1 <- MCMCglmm(perday_in~sex+age+educ,
               random= ~ idh(sex):ind,
               rcov  = ~ idh(sex):units,
               data = energidata)
summary(f1)
# Here we see that males on average have a daily energyintake 
# 2.7-3.2 abov females at 9.1-10 mega joule (MJ). It's slightyl
# confusing here, since this is theoretically for an adult of age 0,
# with a mean age of 46 (as in Norkost), the means are;
# Females: 9.6 - 0.035*46 = 7.99
# Males: 9.6 - 0.035*46 + 3 = 11.

# Original Norkost report states
# on average 8,0 for females and 10.9 for males. 

# the simplest model is just the daily means for males and females
f2 <- MCMCglmm(perday_in~sex,
               random= ~ idh(sex):ind,
               rcov  = ~ idh(sex):units,
               data = energidata)
colMeans(f2$Sol)
# 8 for kvinner og 8+2.94 for menn.

# THere is almost twice the amount of variability among men (G-structure outptu),
# and also twice as much variability between days within men (R-structure).
# Energyintake decreases slightly with age, but it's small (0.04)

plot(density(energidata %>% filter(sex=='female') %>% pull(perday_in),from=0),col='red',main='Daily energyintakes',xlab = 'megajoule',ylab='',lwd=3)
lines(density(energidata %>% filter(sex=='male') %>% pull(perday_in),from=0),col='blue',lwd=3)





## Just summing 'fish'
fishcodes
# hlev2 %in% c('FISK_F Fisk,fet 119','FIS_MHFisk,mager+halvfet118')
# hlev1 %in% 'FISKFisk,skalldyr9'
keep_codes <- ae18 %>% filter(hlev1_name %in% 'FISKFisk,skalldyr9') #%>% pull(kbs_code)
fiskedata <-   nk3data %>% # Take the dietary intake tibble
  filter(kbs_code %in% keep_codes$kbs_code) %>% # filter out only the codes we want (i.e. only fish or remove supplements)
  group_by(day,ind) %>% # group intakes by day and individuals and
  summarise(fishperday_in = sum(amounts,na.rm=T)) %>% #take the sum, this sums all amounts*conc for each day within each individual
  inner_join(.,indsNK3,by='ind')
# How to also have it output the ones that are 0?
# Perhaps we should have a dummy category with 0 intake?

fiskedata <-   nk3data %>% # Take the dietary intake tibble
  filter(kbs_code %in% keep_codes$kbs_code) %>% # filter out only the codes we want (i.e. only fish or remove supplements)
  group_by(day,ind) %>% # group intakes by day and individuals and
  summarise(fishperday_in = sum(amounts,na.rm=T))# %>% #take the sum, this sums all amounts*conc for each day within each individual
  
# Need to do this in a different way to have all individuals
# in the dataframe, including the ones with no reported fish-intake.
tmp <- expand.grid(ind =levels(fiskedata$ind),
                   day = levels(fiskedata$day))
# Laking covariates for those not eating...
fiskedat <- tmp %>% full_join(.,fiskedata,by=c('ind','day'))
fiskedat <- fiskedat %>% mutate(eatsfish=if_else(is.na(fishperday_in),0,1)) %>%
  inner_join(.,indsNK3,by='ind')
# This only returns the ones with non-zero fishintake;

prio2 = list(R = list(V = diag(2),nu=3,fix=1),
             G = list(G1 = list(V= diag(2)/1   , nu=3)))
fiskemod1 <- MCMCglmm(cbind(eatsfish,log(fishperday_in))~trait*sex+trait*age,
               random= ~ idh(trait):ind,
               rcov  = ~ idh(trait):units,
               family = c('categorical','gaussian'),
               data = fiskedat,
               prior = prio2,
               nitt = 25000)
# Note to self idh(trait) yields covariance between probability of eating fish and amount. Ignore.

