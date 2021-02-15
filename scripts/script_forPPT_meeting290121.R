

#
tmp <- dir('C:/Users/JSTA/Documents/R_workingDir/fish_rba/notater/Fish_n_stroke_2021_cache/html',
    pattern="*.RData")
load(file=paste0('C:/Users/JSTA/Documents/R_workingDir/fish_rba/notater/Fish_n_stroke_2021_cache/html/',
tmp[[4]]))



p = sapply(seq(21,66,by=5),function(jj){
  sum(mysamp(fiskemod3,n_ind=1e4,n_days=1,ages=c(jj:(jj+4)))$eats)})
# One portion of fish is 185 g, 2-3 recommended
foo <- function(ii){c(quantile(rowMeans(ii$eats*ii$amount),c(0.025,0.05,0.5,0.95,0.975)),
                      sum(rowMeans(ii$eats*ii$amount)>((185*2)/7))/dim(ii$eats)[1],
                      sum(rowMeans(ii$eats*ii$amount)>((185*3)/7))/dim(ii$eats)[1],
                      frac(ii$eats))}
# foo2 <- function(ii){c(quantile(ii,c(0.025,0.05,0.5,0.95,0.975)),
                      # sum(ii>((185*2)/7))/length(ii),sum(ii>((185*3)/7))/length(ii))}

a = sapply(seq(21,66,by=5),function(jj){
  foo(mysamp(fiskemod3,n_ind=1e4,n_days=150,ages=c(jj:(jj+4))))})

ags <- c('21-25','26-30','31-35','36-40'
         ,'41-45','46-50','51-55','56-60','61-65','66-70')
matplot(t(a[6:7,]),type="l",xaxt='n',ylim=c(0,1),
        ylab='Probability of achieving recommendations')

png('probfish.png',res=200,width=1200,height=800,pointsize = 10)


plot(a[9,],type="o",ylim=c(0,1),ylab='Probability of eating fish on any given day',xaxt='n',
     xlab='Age group')
axis(1,at=1:10,labels=ags)

p_nk <- sapply(seq(21,66,by=5),function(jj){
  fishdata %>% filter(age %in% c(jj:(jj+4))) %>% pull(eatsfish) %>% frac})

points(1:10,p_nk[2,],col='red')
legend('topright',legend=c('Modelled probability','Raw data (Norkost)'),
       pch=c(1,1),col=c('black','red'))        
dev.off()
        # So perhaps a quick slide by slide:

am_nk <- sapply(seq(21,66,by=5),function(jj){
  fishdata %>% filter(age %in% c(jj:(jj+4))) %>% pull(fishperday_all) %>% quantile(na.rm=T,c(0.025,0.05,0.5,0.95,0.975))})



png('intkfish.png',res=200,width=1200,height=800,pointsize = 10)
par(mfrow=c(1,1))
matplot(t(a[c(1,3,5),]),type="o",lwd=2,log='y',pch=1,
        lty=c(2,1,2),col='black',ylab='Long-term intake [g/day]',
        xlab='Age group',xaxt='n',ylim=c(2,500))
axis(1,at=1:10,labels=ags)
abline(h = (185*2)/7,lty=2,lwd=2,col='grey')
abline(h = (185*3)/7,lty=2,lwd=2,col='grey')
dev.off()


par(mfrow=c(3,4))
for (jj in seq(21,66,by=5)){
  tmp <- fishdata %>% filter(age %in% c(jj:(jj+4))) %>% group_by(ind) %>% summarise(oim = mean(fishperday_all,na.rm=T)) %>% pull(oim) 
  modtmp <- mysamp(fiskemod3,n_ind=1e4,n_days=150,ages=c(jj:(jj+4)));
  # hist(tmp,101,main='',xlab='Fish intake [g]',yaxt='n',ylab='')
  plot(density(rowMeans(modtmp$eats*modtmp$amount),from=0),xlim=c(0,300),
       main='',xlab='Fish intake [g]',yaxt='n',ylab='',bty='n')
  rug(tmp)
}



## STROKE

# 1. How to get from dietary changes to # strokes?
#   - Get the 'current' dietary intake'
#   - Find a suitable dose-response linking fish intake with risk
#   - Find incidence data 
#   - Assume that current intake through the DR curve yields current risk
#     and that current risk leads to the current level of incidence.
#   - Assume changes in the diet --> new risk --> new # strokes
#


# Fish
# litt rådata
frac <- function(ii){table(ii)/length(ii)}
fishdata %>% group_by(ind) %>% summarise(n = sum(eatsfish),sex=sex[1],age=age[1]) %>%
  filter(sex=='female') %>% pull(n) %>% frac
fishdata %>% group_by(ind) %>% summarise(n = sum(eatsfish),sex=sex[1],age=age[1]) %>%
  filter(sex=='male') %>% pull(n) %>% frac


js <- mysamp(fiskemod3,n_ind=1e4,n_days=1,drawnew=F)

# DR and intake for a subgroup

ms <- mysamp(fiskemod3,n_ind=1e5,n_days=150,ages=c(18,49))
dlintak <- rowMeans(ms$eats*ms$amount)

fishs <- seq(0,125,by=1)
layout(matrix(c(1,2),nrow=2),heights=c(1,0.3))
par(mar=c(0,4,3,3))
plot(fishs,mnf(fishs),type="l",
     ylab='Relative risk',xlab='mean fish intake [g/day]',
     # main = 'Estimated number of strokes in adults (18-49) \n in relaton to fish intake',cex.main=0.7,
     xaxt='n',xlim=c(0,150),
     ylim=c(0.7,1.2))
lines(fishs,mnu(fishs),type="l",lty=2)
lines(fishs,mnl(fishs),type="l",lty=2)


abline(v = mean(dlintak))
abline(h=mnf(mean(dlintak)))
# rect((2*185)/7,0,(3*185)/7,10000,col=rgb(0.1,0.1,0.1,0.1),border= NA)
grid(col=rgb(0.3,0.3,0.3,0.3))
par(mar=c(3,4,0,3))
plot(density(dlintak,from=0),main='',xlim=c(0,150),yaxt='n',ylab='')
grid(col=rgb(0.3,0.3,0.3,0.3),ny=0)
abline(v=mean(dlintak))
# oims <- ifelse(is.na(oims),0,oims)


png('absrisk1.png',res=200,width=1200,height=800,pointsize = 10)
# PLOT OF REAL SCALED INTAKES
layout(matrix(c(1,2),nrow=2),heights=c(1,0.3))
par(mar=c(0,4,3,3))
plot(sapply(seq(0,2.5,by=0.01),function(ii){mean(dlintak*ii)}),
     (651+497)*sapply(seq(0,2.5,by=0.01),function(ii){mean(rr_norm(dlintak*ii)$mu)})/mean(rr_norm(dlintak)$mu),type="l",
     ylab='Number of strokes',xlab='mean fish intake [g/day]',
     main = 'Ages 18-49 - Group mean risk from individual level intakes',cex.main=0.7,
     xaxt='n',xlim=c(0,125),
     ylim=c(1100,1250),lwd=2)
abline(v = mean(dlintak),lwd=2,col=rgb(0.9,0,0.1,0.8))
abline(h=(651+497),lwd=2,col=rgb(0.9,0,0.1,0.8))
rect((2*185)/7,0,(3*185)/7,10000,col=rgb(0.1,0.1,0.1,0.1),border= NA)
grid(col=rgb(0.3,0.3,0.3,0.3))
par(mar=c(3,4,0,3))
plot(density(dlintak,from=0),lwd=2,main='',xlim=c(0,125),yaxt='n')
grid(col=rgb(0.3,0.3,0.3,0.3),ny=0)
abline(v=mean(dlintak),lwd=2,col=rgb(0.9,0,0.1,0.8))
# oims <- ifelse(is.na(oims),0,oims)
dev.off()

png('absrisk2.png',res=200,width=1200,height=800,pointsize = 10)
# PLOT OF REAL SCALED INTAKES
layout(matrix(c(1,2),nrow=2),heights=c(1,0.3))
par(mar=c(0,4,3,3))
plot(sapply(seq(0,2.5,by=0.01),function(ii){mean(dlintak*ii)}),
     (651+497)*sapply(seq(0,2.5,by=0.01),function(ii){rr_norm(mean(dlintak*ii))$mu})/rr_norm(mean(dlintak))$mu,type="l",
     ylab='Number of strokes',xlab='mean fish intake [g/day]',
     main = 'Ages 18-49 - Group risk from mean individual level intakes',cex.main=0.7,
     xaxt='n',xlim=c(0,125),
     ylim=c(1100,1250),lwd=2)
abline(v = mean(dlintak),lwd=2,col=rgb(0.9,0,0.1,0.8))
abline(h=(651+497),lwd=2,col=rgb(0.9,0,0.1,0.8))
rect((2*185)/7,0,(3*185)/7,10000,col=rgb(0.1,0.1,0.1,0.1),border= NA)
grid(col=rgb(0.3,0.3,0.3,0.3))
par(mar=c(3,4,0,3))
plot(density(dlintak,from=0),lwd=2,main='',xlim=c(0,125),yaxt='n')
grid(col=rgb(0.3,0.3,0.3,0.3),ny=0)
abline(v=mean(dlintak),lwd=2,col=rgb(0.9,0,0.1,0.8))
# oims <- ifelse(is.na(oims),0,oims)
dev.off()



png('absrisk3.png',res=200,width=1200,height=800,pointsize = 10)
# PLOT OF REAL SCALED INTAKES
layout(matrix(c(1,2),nrow=2),heights=c(1,0.3))
par(mar=c(0,4,3,3))
plot(seq(0,125,by=1),
     (651+497)*sapply(seq(0,125,by=1),function(ii){
       rr_norm(ii)$mu})/rr_norm(mean(dlintak))$mu,type="l",
     ylab='Number of strokes',xlab='mean fish intake [g/day]',
     main = 'Ages 18-49 - Group risk from mean individual level intakes',cex.main=0.7,
     xaxt='n',xlim=c(0,125),
     ylim=c(1100,1250),lwd=2)
abline(v = mean(dlintak),lwd=2,col=rgb(0.9,0,0.1,0.8))
abline(h=(651+497),lwd=2,col=rgb(0.9,0,0.1,0.8))
rect((2*185)/7,0,(3*185)/7,10000,col=rgb(0.1,0.1,0.1,0.1),border= NA)
grid(col=rgb(0.3,0.3,0.3,0.3))
par(mar=c(3,4,0,3))
plot(density(dlintak,from=0),lwd=2,main='',xlim=c(0,125),yaxt='n')
grid(col=rgb(0.3,0.3,0.3,0.3),ny=0)
abline(v=mean(dlintak),lwd=2,col=rgb(0.9,0,0.1,0.8))
# oims <- ifelse(is.na(oims),0,oims)
dev.off()






# DR and intake for a subgroup

# ms2 <- mysamp(fiskemod3,n_ind=1e5,n_days=150,ages=c(,49))
mf <- function(ii){rowMeans(ii$eats*ii$amount)}
dlintak_B <- sapply(seq(30,65,by=5),function(ii){
  mf(mysamp(fiskemod3,n_ind=1e5,n_days=150,ages =c(ii,ii+4)))})

inc_deaths <- data.frame(
  males = c(0,	0,	1,	2,	8.6,	8.6,	23.2,	44,	74.8,	110,	146.8,	167.4,	115.2,	29.6),
  females = c(0,	0,	0,	0.8,	5,	4.6,	13.8,	28.4,	57.2,	87.6,	175.4,	265.4,	290.2,	147.4),
  ages = c('30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-70','71-75','76-80','81-85','86-90','91-95','95-'))
  
matplot(inc_deaths[,1:2],type="o",ylab='Deaths',
        xaxt ='n',pch=1,lwd=2,main='Average yearly deaths due to stroke 2015-2019')
axis(1,at=1:14,labels = inc_deaths$ages,las=2)



# Do from ages 40 and up to
# png('absrisk4.png',res=200,width=1200,height=800,pointsize = 10)
# PLOT OF REAL SCALED INTAKES
for (jj in 5:8){
  png(paste0('absrisk',as.character(jj),'.png'),res=200,width=1200,height=800,pointsize = 10)
      
      layout(matrix(c(1,2),nrow=2),heights=c(1,0.3))
      par(mar=c(0,4,3,3))
      plot(seq(0,125,by=1),
           sum(inc_deaths[jj,1:2])*sapply(seq(0,125,by=1),function(ii){
             rr_norm(ii)$mu})/rr_norm(mean(dlintak_B[,jj]))$mu,type="l",
           ylab='Death due to stroke',xlab='mean fish intake [g/day]',
           main = paste0('Ages ',inc_deaths$ages[jj],' - Group risk from mean individual level intakes'),cex.main=0.7,
           xaxt='n',xlim=c(0,125),
           lwd=2)
      abline(v = mean(dlintak_B[,jj]),lwd=2,col=rgb(0.9,0,0.1,0.8))
      abline(h=sum(inc_deaths[jj,1:2]),lwd=2,col=rgb(0.9,0,0.1,0.8))
      rect((2*185)/7,0,(3*185)/7,10000,col=rgb(0.1,0.1,0.1,0.1),border= NA)
      grid(col=rgb(0.3,0.3,0.3,0.3))
      par(mar=c(3,4,0,3))
      plot(density(dlintak_B[,jj],from=0),lwd=2,main='',xlim=c(0,125),yaxt='n')
      grid(col=rgb(0.3,0.3,0.3,0.3),ny=0)
      abline(v=mean(dlintak_B[,jj]),lwd=2,col=rgb(0.9,0,0.1,0.8))
      # oims <- ifelse(is.na(oims),0,oims)
      
      dev.off()
}


# Uncertainty in the risk itself -
# can we draw many risks for each intake, and scale them?
# Normal d over normal d is tricky stuff, but perhaps they scale not so badly?
hist(rnorm(1e5,rr_norm(median(dlintak_B[,4]))$mu,rr_norm(median(dlintak_B[,4]))$sd),101)

# So the distribution of relative risks we gett look not too bad.
# 
hist(rnorm(1e5,rr_norm(3)$mu,rr_norm(3)$sd)/
       rnorm(1e5,rr_norm(median(dlintak_B[,4]))$mu,rr_norm(median(dlintak_B[,4]))$sd),
       101)
# There are several levels of variability we can take into account:
# - variation in individual level intakes [g/d] (mean,median or full distribution)
# - variation in relative risk (either use the mean risk, given intake, or use full distribution)
# - variation in death-rates (using binomial draw, rather than actual raw numbers)
# - 

# # Making quick two figs with no axis highlighting either distribution
# # OR single number
# png(paste0('distr1.png'),res=200,width=600,height=600,pointsize = 10)
# 
# plot(seq(-3,3,by=0.01),dnorm(seq(-3,3,by=.01),0,1),type="l",axes='n',
#      lwd=5,col=rgb(0.9,0.01,0.2,1))
# axis(1,labels=FALSE,lwd=2)
# lines(c(0,0),c(-1,1),lwd=3,lty=2,col='grey')
# dev.off()
# 
# png(paste0('distr2.png'),res=200,width=600,height=600,pointsize = 10)
# plot(seq(-3,3,by=0.01),dnorm(seq(-3,3,by=.01),0,1),type="l",axes='n',
#      lwd=3,col='grey',lty=2)
# axis(1,labels=FALSE,lwd=2)
# lines(c(0,0),c(-1,1),lwd=5,lty=1,col=rgb(0.9,0.01,0.2,1))
# dev.off()