library(data.table)
library(tidyverse)
library(padr)

source('~/Documents/code/cbg_code/titan_version_060223/cbg_functions.R')
source('~/Documents/code/cbg_code/titan_version_060223/cbg.xgb.functions.R')

# load data from folder
filepath <- c('~/Documents/data/CBGdata/process_folder/')
list <- list.files(filepath)
if (exists('aegisFile') == FALSE) {
  aegisFile <- fread(paste0(filepath, list[length(list)])) # load last file in list
}

x <- aegisFile

# # if single_day is defined, truncate all data at the end of that day
# if (single_day != "") {
#   dates = as.Date(substr(x$V101,1,10), format = '%d/%m/%Y')
#   x$dates = dates
#   x = x[dates <= as.Date(single_day)]
#   x$dates <- NULL
# }

# process aegis data into correct format for analysis
x <- suppressWarnings(process_aegis(x))
