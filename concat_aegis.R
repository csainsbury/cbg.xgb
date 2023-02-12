library(data.table)
library(tidyverse)

# load aegis files
# load data from folder
filepath <- c('~/Documents/data/CBGdata/aegis_raw/')
list <- list.files(filepath)

for (i in c(1:length(list))) {
    aegisFile <- fread(paste0(filepath, list[i])) # load last file in list
    
    print(paste0(filepath, list[i]))
    print(dim(aegisFile))

    if (i==1) {
        aegis_export = aegisFile
    } else {
        aegis_export = rbind(aegis_export, aegisFile)
    }
}

write.table(aegis_export, file = '~/Documents/data/CBGdata/process_folder/aegis_concat.csv', sep = ',', row.names = F)

# aegisFile <- fread(paste0(filepath, list[length(list)])) # load last file in list
