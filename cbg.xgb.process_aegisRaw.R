library(data.table)
library(tidyverse)
library(padr)

source('~/Documents/code/cbg_code/cbg_functions.R')
source('~/Documents/code/cbg_code/cbg.xgb.functions.R')

# load data from folder
filepath <- c('Documents/data/CBGdata/process_folder/')
list <- list.files(filepath)
# aegisFile <- fread(paste0(filepath, list[length(list)])) # load last file in list
aegisFile <- fread(paste0(filepath, 'aegis_concat.csv'))
x <- aegisFile

# process aegis data into correct format for analysis
x <- suppressWarnings(process_aegis(x))
x <- prepare_aegis(x, interest_days)
data.orig <- x

x[, 'admission_vec' := admission_N_vector(unix_dateTime, lockout), by=.(uID)]

# this function truncates to day of interest
x <- initial_process_label(x, interest_days, minimum_n_cbgs, hypo_threshold)

print(paste0('pre ratio: ', nrow(x)))

if (ratio >0 ) {
  x <- case_control_ratio(x, ratio)
}

print(paste0('post ratio: ', nrow(x)))

s <- admission_process(x)

s <- manage_prior_admission_data(s)

    # only keep data where the last day of admission is today / specified date
    # the single day is the last day of the train period
    # ie where interest days == 7, single day will identify IDs where the 6th day of admission is the day specified - so the 7 day model can be applied
    if (single_day != "") {
      s[, 'last_date' := max(dateTime), by=.(uID)]
      s$last_date <- as.Date(substr(s$last_date, 1, 10), format = '%Y-%m-%d')
      
      s <- s[last_date == single_day]
      
      # remove last_date column
      s[, last_date := NULL]
    }

wide_data <- suppressWarnings(wideData(s))

export_data <- manage_colnames(wide_data)
    
