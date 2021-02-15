# Dose-response from figure in Bechthold 2017/2019, fig 3 fish v stroke. 
# Relative risk.

tmp <- read.csv('C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/rawdata/wpd_datasets.csv',
                skip=1)



mnf <- splinefun(c(0,tmp[1:60,1]),c(1,tmp[1:60,2]))
mnu <- splinefun(c(0,tmp[1:60,3]),c(1,tmp[1:60,4]))
mnl <- splinefun(c(0,tmp[1:59,5]),c(1,tmp[1:59,6]))

rr_norm <- function(fish){
  fish <- pmin(fish,125)
  return(list(mu = mnf(fish),sd = (mnf(fish)-mnl(fish))/qnorm(0.975)))
}
# These are symmetrical. the Ci is reported to be 95%, so we assume
# a normal distribution of the risk, given an intake of fish 
fishs <- seq(0,125,by=1)
plot(fishs,mnf(fishs),type="l",lty=1,ylim=c(0.7,1.2))
lines(fishs,mnu(fishs),type="l",lty=2)
lines(fishs,mnl(fishs),type="l",lty=2)

plot(fishs,mnf(fishs),lty=1)
lines(tmp[1:59,3],tmp[1:59,4],type="l",lty=2)
lines(tmp[1:59,5],tmp[1:59,6],type="l",lty=2)

points(rep(100,100),rnorm(100,rr_norm(100)[1],rr_norm(100)[2]))


# So to recap- this is the dose-response curve linking fish intake in g/day
# from Bechthold 2017 (Figure 3), which I pushed through the webplotdigitizer
# https://apps.automeris.io/wpd/
# and extracted points (imported as csv above). These are Relative risks.
# THe paper claims no non-linearity in response, citing a p-value, but we use 
# this nevertheless.