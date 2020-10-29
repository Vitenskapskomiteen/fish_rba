# Calculating sources for nutrients;


intake_sources <-   nk3data %>% # Take the dietary intake tibble
  inner_join(.,concs,by='kbs_code') %>%  # join it with the concentrations to make a new column
  mutate(tmpiodine = amounts_cor*I,tmpvitd=amounts_cor*VitD,tmpse=amounts_cor*Se,
         tmpb12=amounts_cor*B12,tmpomega3=amounts_cor*`Omega 3`,tmpEPA=amounts_cor*`C20:5n-3`,
         tmpDHA=amounts_cor*`C22:6n-3`,tmpEnergy = amounts_cor*Energi,tmpsum = amounts) %>%
  inner_join(.,ae18,by='kbs_code') %>%
  group_by(hlev1_name) %>%
  summarise(sumIod = sum(tmpiodine,na.rm=T),
            sumSe  = sum(tmpse,na.rm=T),
            sumB12 = sum(tmpb12,na.rm=T),
            sumOmega3 = sum(tmpomega3,na.rm=T),
            sumVitD = sum(tmpvitd,na.rm=T),
            sumEPA = sum(tmpEPA,na.rm=T),
            sumDHA = sum(tmpDHA,na.rm=T),
            sumEne = sum(tmpEnergy,na.rm=T),
            sumGrm = sum(tmpsum,na.rm=T)) %>%
  mutate(.,Iod = sumIod/sum(sumIod),
         Se = sumSe/sum(sumSe),
         B12 = sumB12/sum(sumB12),
         Omega3 = sumOmega3/sum(sumOmega3),
         VitD = sumVitD/sum(sumVitD),
         EPA = sumEPA/sum(sumEPA),
         DHA = sumDHA/sum(sumDHA),
         Energy = sumEne/sum(sumEne),
         Gram = sumGrm/sum(sumGrm)) 

tmp <- c('Iod','Se','B12','Omega3','VitD','EPA','DHA','Energy','Gram')

Fdgrps <- sapply(1:18,function(ii){
  trimws(substr(as.character(intake_sources$hlev1_name[ii]),
  max(unlist(gregexpr("[A-Z]",as.character(intake_sources$hlev1_name[ii])))),
(unlist(gregexpr("[0-9]",as.character(intake_sources$hlev1_name[ii])))-1)))})
Fdgrps[10] = 'Melk'
cls <- colorspace::qualitative_hcl(18)#,h = c(300, 75), c = c(35, 95), l = c(15, 90), power = c(0.8, 1.2))

cls <- brewer.pal(18,"Paired")
barplot(sapply(1:length(tmp),function(ii){intake_sources %>% slice(1:16) %>% pull(tmp[ii])}),beside=F,xlim=c(0,length(tmp)+6),width=1,
        col = rep(cls,2),legend=Fdgrps[1:16],names.arg=tmp)

  
layout(matrix(c(1,1,2,2),2,2,byrow=FALSE),widths=c(4,2),heights = c(0.1,1,0.1,1))
par(mar=c(5.1,4.1,4.1,2.1))
barplot(sapply(1:length(tmp),function(ii){intake_sources %>% slice(1:16) %>% pull(tmp[ii])}),beside=F,names.arg=tmp,
        col = rep(cls,2))
par(mar=c(5.1,1,4.1,2.1))
plot.new()
legend('bottomleft',legend=rev(Fdgrps[1:16]),fill=rev(rep(cls,2)[1:16]))
       


sapply(1:7,function(ii){paste0(tmp[ii],': ', as.character(round(100*(intake_sources %>% slice(8) %>% select(tmp[ii])))),'%')})
     


# %>% #make a new column (perfood_in) with the products of amounts and concentrations
#   group_by(day,ind) %>% # group intakes by day and individuals and
#   summarise(vitd= sum(tmpvitd,na.rm=T),iodine = sum(tmpiodine,na.rm=T),
#             b12= sum(tmpb12,na.rm=T),se = sum(tmpse,na.rm=T),omega3=sum(tmpomega3,na.rm=T)) %>% #take the sum, this sums all amounts*conc for each day within each individual
#   group_by(ind) %>% # group the daily intake by individual
#   inner_join(.,indsNK3,by='ind')


head(intake_sources)
levels(ae18$hlev1_name)

fishcodes <- ae18 %>% filter(hlev1_name == 'FISKFisk,skalldyr9') %>% select(c(kbs_code,kbs_name))

# Can we add a field to the intake_soruces that contain the hlev1s?


tmp <- left_join(intake_sources,ae18, by='kbs_code')


hist(tmp %>% group_by(hlev1_name) %>% summarise(vd = sum(tmpvitd,na.rm=T)) %>%
  mutate(vitfrac = vd/sum(vd)*100) %>% select(vitfrac))





p1 <- ggplot(intake_sources,aes(x="",y=sumIod/sum(sumIod),fill=hlev1_name)) +
  geom_bar(stat = "identity",width=1,col="white") 
p2 <- ggplot(intake_sources,aes(x="",y=sumSe/sum(sumSe),fill=hlev1_name)) +
  geom_bar(stat = "identity",width=1,col="white") 
p3 <- ggplot(intake_sources,aes(x="",y=sumB12/sum(sumB12),fill=hlev1_name)) +
  geom_bar(stat = "identity",width=1,col="white") 

grid.arrange(p1,p2,p3,nrow=1)

+
  coord_polar("y",start=0) +
  theme_void() +
  scale_fill_brewer()+
  ggtitle(paste0('Kilder Iodine '))
