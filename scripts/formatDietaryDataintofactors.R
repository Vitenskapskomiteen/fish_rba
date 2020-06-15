# Changing the cleandata to have kbs_code as factors,
# also ind as factor in the dietary data structures.
load(paste0(org::project$cleandata,'/dietarydata.RData'))
ae18  <- ae18 %>% mutate(kbs_code = as.factor(kbs_code))

nk3data <- nk3data %>% mutate(ind = as.factor(ind),
                              kbs_code = as.factor(kbs_code))

uk4data <- uk4data %>% mutate(ind = as.factor(ind),
                              kbs_code = as.factor(kbs_code))

uk9data <- uk9data %>% mutate(ind = as.factor(ind),
                              kbs_code = as.factor(kbs_code))
uk13data <- uk13data %>% mutate(ind = as.factor(ind),
                              kbs_code = as.factor(kbs_code))

save.image(paste0(org::project$cleandata,'/dietarydata.RData'))
