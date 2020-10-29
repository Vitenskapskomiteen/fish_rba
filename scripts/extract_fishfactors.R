# Script to document the reduction factors for fish and fish content
# in compound foods

# Load in factors (some have two)
fishfc <- readxl::read_excel(paste0(org::project$raw,'/FaktorerFisk.xlsx'),col_names=F)


fishfc$...2

sapply(1:100,function(ii){which(fishfc$...2 ==nk3data$kbs_code[ii])})


nk3data$amounts_cor = nk3data$amounts

for ( jj in which(nk3data$kbs_code %in% fishfc$...2)){
  nk3data[jj,]$amounts_cor = nk3data[jj,]$amounts * prod(fishfc[which(nk3data[jj,]$kbs_code == fishfc$...2),3:4],na.rm=T)  
}


uk4data$amounts_cor = uk4data$amounts
for ( jj in which(uk4data$kbs_code %in% fishfc$...2)){
  uk4data[jj,]$amounts_cor = uk4data[jj,]$amounts * prod(fishfc[which(uk4data[jj,]$kbs_code == fishfc$...2),3:4],na.rm=T)  
}
uk9data$amounts_cor = uk9data$amounts
for ( jj in which(uk9data$kbs_code %in% fishfc$...2)){
  uk9data[jj,]$amounts_cor = uk9data[jj,]$amounts * prod(fishfc[which(uk9data[jj,]$kbs_code == fishfc$...2),3:4],na.rm=T)  
}
uk13data$amounts_cor = uk13data$amounts
for ( jj in which(uk13data$kbs_code %in% fishfc$...2)){
  uk13data[jj,]$amounts_cor = uk13data[jj,]$amounts * prod(fishfc[which(uk13data[jj,]$kbs_code == fishfc$...2),3:4],na.rm=T)  
}

save(list=c("nk3data","uk4data","uk9data","uk13data","indsNK3","indsUK13","indsUK4","indsUK9","concs","ae18","Fish_groups"),
     file=paste0(org::project$cleandata,'/StartClean.RData'))
