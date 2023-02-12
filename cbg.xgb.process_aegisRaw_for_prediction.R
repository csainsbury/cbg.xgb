library(data.table)
library(tidyverse)
library(padr)

source('/home/chris/Documents/code/cbg_code/inpatient_CBG_xgb-main/cbg_functions.R')
source('/home/chris/Documents/code/cbg_code/inpatient_CBG_xgb-main/cbg.xgb.functions.R')

# load data from folder
filepath <- c('/home/chris/Documents/data/CBGdata/process_folder/')
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

x <- prepare_aegis(x, interest_days, prediction_run) # prediction_run == 1 if predicting for a specific day, otherwise 0
data.orig <- x

x[, 'admission_vec' := admission_N_vector(unix_dateTime, lockout), by=.(uID)]

# this function truncates to day of interest
x <- production_initial_process_no_label(x, interest_days, minimum_n_cbgs, hypo_threshold, single_day)

#print(paste0('pre ratio: ', nrow(x)))

# if (ratio >0 ) {
#   x <- case_control_ratio(x, ratio)
# }

# print(paste0('post ratio: ', nrow(x)))

s <- admission_process(x)

s <- manage_prior_admission_data(s)

wide_data <- suppressWarnings(wideData(s))

export_data <- manage_colnames(wide_data)

