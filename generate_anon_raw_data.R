## generate sample unipoc data
library(data.table)
library(tidyverse)
library(padr)

source('~/Documents/code/cbg_code/cbg_functions.R')
source('~/Documents/code/cbg_code/cbg.xgb.functions.R')

# load data from folder
filepath <- c('~/Documents/data/CBGdata/unipoc_raw/unipoc_basic_cleaned.csv')
unipoc_file <- fread(filepath) # load last file in list

x <- unipoc_file # original nrow 3851368

# remove location - replace with a factor
x$loc <- as.numeric(factor(x$loc))
x$uID <- as.numeric(factor(x$uID))

## sample IDs. original 215k
n_sample <- 1000

ids <- unique(x$uID)
ids = sample(ids, n_sample)

new_x_unipoc <- x[uID %in% ids]

## save_out
write.table(new_x_unipoc, file = '~/Documents/code/cbg_code/working_cbg_repo/unipoc_sample_anon.csv', sep = ',', row.names = F)

###########################################################################
## genrate sample aegispoc data

# load data from folder
filepath <- c('~/Documents/data/CBGdata/process_folder/')
list <- list.files(filepath)
aegisFile <- fread(paste0(filepath, list[length(list)])) # load last file in list

x <- aegisFile

#subset <- x %>% select(V148, V150, V101, V104, V140)
#colnames(subset) <- c('CHI', 'location', 'dateTime', 'Glu', 'op')

x$V148 <- as.numeric(factor(x$V148))
x$V150 <- as.numeric(factor(x$V150))
x$V140 <- as.numeric(factor(x$V140))

ids <- unique(x$V148)
ids = sample(ids, n_sample)

new_x_aegis <- x[V148 %in% ids]

## save_out
write.table(new_x_aegis, file = '~/Documents/code/cbg_code/working_cbg_repo/aegis_sample_anon.csv', sep = ',', row.names = F)

