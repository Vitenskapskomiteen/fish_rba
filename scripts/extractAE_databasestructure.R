# JOS: 050520.
# This is a messy, but functional script that extracts the
# foodstuff hierarchy from the pdf from Monica. The main
# output is a data.frame with ALL foodcodes, their name and
# their placement in the higher order hierarchy.

# E.g. to get all KBS-codes that belong to group 77 (Sitrus) at
# level 3,
tmp_ae[which(tmp_ae$hlev3 == 77),]
# or Jams, lev2 73
tmp_ae[which(tmp_ae$hlev2 == 118),]$kbs_name

# alternatively using filter
js <- ae18 %>% filter(hlev2_name == unique(hlev2_name)[3])
# Or only fish
ae18 %>% filter(hlev1_name == unique(hlev1_name)[11])
# There was some error with some flours?

unique(ae18[which(ae18$hlev1==4),]$hlev2_name)
# Oui, byggmel % is coded as a level 2 category (misplaced due to numbers inside name).



# BELOW IS CODE TO EXTRACT HIERARCHY FROM THE PDF. Save for posterity.
# Trying to read in the hierarchy of the AE-18 codes:

# Perhaps use the fact that all higher level categories are in capital letters?
tmp[which(sapply(1:dim(tmp)[1],function(ii){toupper(substring(tmp[ii,2],1,4)) == substring(tmp[ii,2],1,4)}) & tmp$kbs_no<6),]
# These are categories and not food.


sum(sapply(1:dim(ae18)[1],function(ii){toupper(substring(ae18[ii,2],1,3)) != substring(ae18[ii,2],1,3)}))



library(dplyr)
ae18 <- pdftools::pdf_text("C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/Data fra KBS/matv_grupper_med_kdoer_AE18.pdf")
strlins <- c();
for (jj  in 1:length(ae18)){
  # If first page, skip first 5, for all others skip top line.
  # Sometimes the last on a page and the first on p+1 are the
  # same, if they are categories it seems.
  f1 <- ae18[[jj]] %>% strsplit(split="\r\n")
  strlins = c(strlins,sapply((2+4*(jj==1)):length(f1[[1]]),function(ii){gsub("  ","",f1[[1]][[ii]])}))
}
nnds <- regmatches(strlins, gregexpr("[[:digit:]]+", strlins))

stringr::str_extract(f1[[1]],"[0-9]+")
# So if the first is 04 (its the date/header, so ignore)
# if it is 1,2,3,4,5 it is a nomer of the hierarchy.
# If they are of 3 digits or more, they are proper codes.
# THere are up to 5 hierarchical levels before the main code
# Atleast for meat it seems.
codes <- ae18 %>% strsplit(split="\r\n") %>% unlist %>% stringr::str_extract("[0-9]+")
test  <- ae18 %>% strsplit(split="\r\n") %>% unlist %>% stringr::str_extract("[:digit:]")
names <- ae18 %>% strsplit(split="\r\n") %>% unlist %>% stringr::str_extract_all("[aA-zZ]+")

AE_18 <- data.frame(kbs_no = as.numeric(codes),
                    names  = sapply(1:length(names),
                                    function(ii){toString(names[[ii]])}))
# So here we have them all, but we need to clean it up.
# First off, codes==04 is just the headers and can be removed.
# Then we need to track the entries starting with 1,2,3,4,5, because
# they are the higher level categories of the following codes


tmp = AE_18[-c(which(AE_18$names == AE_18$names[1])),]
tmp = tmp[-c(1,2,3,4),] #removing headers from 1st page
codes = codes[-c(2,3,4,5,which(AE_18$names == AE_18$names[1]))]
# Some of these nnds which are of length>1 are due to 
# foodnames ending with numbers. GAH!
nnds[which(sapply(nnds,length)>1)[1:10]] #
# So all 'higher level' entries start with 1,2,3,4,5
which(as.numeric((sapply(1:length(nnds),function(ii){nnds[[ii]][1]}))) %in% c(1,2,3,4,5))
tmpL1 = NA
tmpL1n = NA
tmpL2 = NA
tmpL2n = NA
tmpL3 = NA
tmpL3n = NA
tmpL4 = NA
tmpL4n=NA
tmpL5 = NA
tmpL5n=NA
# But how to store this? A df with food_kode, food_name,level1_no,leve1_name,level2_no,level2_name etc?
tmp_ae = data.frame()
for (ii in 1:dim(tmp)[1]){
  # toupper(substring(tmp[ii,2],1,3)) != substring(tmp[ii,2],1,3)
  if (tmp[ii,]$kbs_no ==1 & toupper(substring(tmp[ii,2],1,3)) == substring(tmp[ii,2],1,3)){#length(nnds[[ii]])==2){
    # highest level
    tmpL1 = as.numeric(nnds[[ii]][[2]])
    tmpL1n = trimws(gsub(codes[ii],"",strlins[ii]),"b")
    tmpL2 = NA
    tmpL2n = NA
    tmpL3 = NA
    tmpL3n = NA
    tmpL4 = NA
    tmpL4n=NA
    tmpL5 = NA
    tmpL5n=NA
    
  } else if (tmp[ii,]$kbs_no==2 & toupper(substring(tmp[ii,2],1,3)) == substring(tmp[ii,2],1,3)){#length(nnds[[ii]])==2){
    tmpL2 = as.numeric(nnds[[ii]][[2]])
    tmpL2n = trimws(gsub(codes[ii],"",strlins[ii]),"b")
    tmpL3  = NA
    tmpL3n = NA
    tmpL4  = NA
    tmpL4n = NA
    tmpL5  = NA
    tmpL5n = NA
    
  } else if (tmp[ii,]$kbs_no==3 & toupper(substring(tmp[ii,2],1,3)) == substring(tmp[ii,2],1,3)){#length(nnds[[ii]])==2){
    tmpL3  = as.numeric(nnds[[ii]][[2]])
    tmpL3n = trimws(gsub(codes[ii],"",strlins[ii]),"b")
    tmpL4  = NA
    tmpL4n = NA
    tmpL5  = NA
    tmpL5n = NA
  } else if (tmp[ii,]$kbs_no==4 & toupper(substring(tmp[ii,2],1,3)) == substring(tmp[ii,2],1,3)){#length(nnds[[ii]])==2){
    tmpL4  = as.numeric(nnds[[ii]][[2]])
    # 
    tmpL4n = trimws(gsub(codes[ii],"",strlins[ii]),"b")
    tmpL5  = NA
    tmpL5n = NA
  } else if (tmp[ii,]$kbs_no==5 & toupper(substring(tmp[ii,2],1,3)) == substring(tmp[ii,2],1,3)){#length(nnds[[ii]])==2){
    tmpL5  = as.numeric(nnds[[ii]][[2]])
    tmpL5n = trimws(gsub(codes[ii],"",strlins[ii]),"b")
  } else {
    # Here we have an actual food
    entr <- data.frame(kbs_code = as.numeric(codes[ii]),
                       kbs_name = trimws(gsub(codes[ii],"",strlins[ii])), #trimws(gsub("[0-9]+","",strlins[ii]),"b"),
                       hlev1 = tmpL1,hlev1_name = tmpL1n,
                       hlev2 = tmpL2,hlev2_name = tmpL2n,
                       hlev3 = tmpL3,hlev3_name = tmpL3n,
                       hlev4 = tmpL4,hlev4_name = tmpL4n,
                       hlev5 = tmpL5,hlev5_name = tmpL5n)
    tmp_ae = rbind(tmp_ae,entr)
  }
}


js <- tmp_ae %>% filter(hlev2==31)

sapply(1:12,function(ii){c(length(unique(tmp_ae[,ii])))})
# Some of the categories have spaces while copies sometimes dont.

which(table(tmp_ae[,2])>1)

tmp_ae[agrep(unique(tmp_ae[,12])[2],tmp_ae[,12]),]
unique(tmp_ae[,12])
for (aa in c(3,5,7,9,11)){
  for (jj in which(!is.na(unique(tmp_ae[,aa])))){
    tmp_ae[which(tmp_ae[,aa] == unique(tmp_ae[,aa])[jj]),aa+1] =   tmp_ae[which(tmp_ae[,aa]==unique(tmp_ae[,aa])[jj])[1],aa+1]
    }
}
# Er noen problemer her, some of these strings have numbers inside
# then, also for the higher level classification, making this thing
# fking messy. The names and Kbs_codes are correct, but their grouping
# might be a bit off.
ae18 <- tmp_ae;
# Changing the kbs_kode to be factor
tmp  <- ae18 %>% mutate(kbs_code = as.factor(kbs_code))
ae18 <- tmp
save(ae18,file='C:/Users/JSTA/Folkehelseinstituttet/Risk and benefit assessment of fish - Dokumenter/Eksponeringsgruppen/cleandata/ae18.Rdata')
