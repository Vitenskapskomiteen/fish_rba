# FUnction to read in concentration data.
#
read_in_concentrations<- function(usepath=NULL){
  if (is.null(usepath)){
    tmpcolno <- dim(read_excel(paste0(org::project$raw,'/alle matvarer AE18 med naeringsstoffer og energi 190320MHC.xlsx'),skip=5))[2]
    concs <- read_excel(paste0(org::project$raw,'/alle matvarer AE18 med naeringsstoffer og energi 190320MHC.xlsx'),skip=5,
                        col_type=c('numeric','text',rep('numeric',tmpcolno-2)))
  } else {
    tmpcolno <- dim(read_excel(paste0(usepath,'/alle matvarer AE18 med naeringsstoffer og energi 190320MHC.xlsx'),skip=5))[2]
    concs <- read_excel(paste0(usepath,'/alle matvarer AE18 med naeringsstoffer og energi 190320MHC.xlsx'),skip=5,
                        col_type=c('numeric','text',rep('numeric',tmpcolno-2)))
    
  }
  # Changing to be per gram;
  for (ii in 3:dim(concs)[2]){
    concs[,ii] = concs[,ii]/100
  }
  
  concs$kbs_code = as.factor(concs$Kode)
  return(concs)
}
