
calcOIMS <- function(data,units,colinx,ps=c(0.25,0.5,0.75,0.95,0.975),drawplot=FALSE){
  # data - input must have $ind as index into individuals and
  # will generate output from the intake in columns colinx.
  # if drawplot = T a simple densityplot will be printed with
  # a density for each $sex
  
  # Simple function to extract some statistics
  # from data, in the form of daily intakes
  
  
  myf <- function(ii){c(mean(ii),sd(ii),quantile(ii,ps))}
  
  # oims <- data.frame(t(sapply(unique(data$ind),function(ii){
  #   colMeans(data[which(data$ind==ii),colinx])})))

  oims <- data.frame(sapply(colinx,function(jj){
    t(sapply(unique(data$ind),function(ii){
    mean(data[which(data$ind==ii),colinx],na.rm=T)}))}))
  colnames(oims) <- colnames(data)[colinx]
    
  oims$sex = data$sex[1:length(unique(data$ind))];
  
  if (drawplot){
    for (ii in 1:length(colinx)){
      
      plot(density(oims[oims$sex=='female',ii],from=0),col='red',main=colnames(oims)[ii],
           xlab=paste0('Daily intakes [',units[[ii]],'/day]'),ylab='',lwd=3)
      lines(density(oims[oims$sex=='male',ii],from=0),col='blue',lwd=3)
      rug(oims[oims$sex=='female',ii],col='red')
      rug(oims[oims$sex=='male',ii],col='blue')
    }
  }
  
  stats <- data.frame(rep(NA,2+length(ps)),row.names=c('mean','sd',sapply(1:length(ps),function(ii){paste0('p',as.character(ps[ii]*100))})))
  for (ii in 1:length(colinx)){
    stats[[colnames(oims)[ii]]] <- myf(oims[,ii])
  }
  stats <- stats[,-1]
  
  stats_male <- data.frame(rep(NA,2+length(ps)),row.names=c('mean','sd',sapply(1:length(ps),function(ii){paste0('p',as.character(ps[ii]*100))})))
  for (ii in 1:length(colinx)){
    stats_male[[colnames(oims)[ii]]] <- myf(oims[which(data$sex[1:length(unique(data$ind))]=='male'),ii])
  }
  stats_male <- stats_male[,-1]
  stats_female <- data.frame(rep(NA,2+length(ps)),row.names=c('mean','sd',sapply(1:length(ps),function(ii){paste0('p',as.character(ps[ii]*100))})))
  for (ii in 1:length(colinx)){
    stats_female[[colnames(oims)[ii]]] <- myf(oims[which(data$sex[1:length(unique(data$ind))]=='female'),ii])
  }
  stats_female <- stats_female[,-1]
  # These checks out for the Iodine Oims in the report.
  return(list(oims=oims,stats=stats,stats_male=stats_male,stats_female=stats_female))
}