## prepare unipoc training data
interest_days_vec <- c(11,12,13,14)
threshold_vec <- c(3, 4, 5)
ratio_vec     <- c(-99)

lockout = 7

library(data.table)
library(tidyverse)
library(padr)

source('/home/chris/Documents/code/cbg_code/inpatient_CBG_xgb-main/cbg_functions.R')
source('~/Documents/code/cbg_code/inpatient_CBG_xgb-main/cbg.xgb.functions.R')

# load data from folder
filepath <- c('~/Documents/data/CBGdata/unipoc_raw/unipoc_basic_cleaned.csv')
unipoc_file <- fread(filepath) # load last file in list

x <- unipoc_file

# make unipoc data look like aegis data
x <- x %>% rename(location = loc)
x$unix_dateTime <- as.numeric(as.POSIXct(x$dateTime, format = '%Y-%m-%d %H:%M:%S'))
x <- x %>% select(-datetime)

# x = x[1:10000,]

x_for_interestday_loop <- x # freeze x for the interestday loop

# interest_days loop from here:
for (interest_days in interest_days_vec) {
    print(paste0('interest_days: ', interest_days))

            minimum_n_cbgs <- interest_days + 2  # minimum number of cbgs to include
            x <- prepare_aegis(x_for_interestday_loop, interest_days, 0)

            data.orig <- x

            x[, 'admission_vec' := admission_N_vector(unix_dateTime, lockout), by=.(uID)]

            x_for_threshold_loop <- x # freeze x for the hypo_threshold loop

            ## threshold from loop here:
            for (hypo_threshold in threshold_vec) {
                print(paste0('hypo_threshold: ', hypo_threshold))

                    x <- initial_process_label(x_for_threshold_loop, interest_days, minimum_n_cbgs, hypo_threshold)

                    x_for_ratio_loop <- x # freeze x for the ratio loop

                    ## ratio loop from here:
                    for (ratio in ratio_vec) {
                        print(paste0('ratio: ', ratio))

                            x <- x_for_ratio_loop

                            if ((nrow(x) - sum(x$label)) >= (sum(x$label) * ratio)) {

                                print(paste0('pre ratio: ', nrow(x)))

                                if (ratio > 0 ) {
                                
                                        x <- case_control_ratio(x, ratio)
                                
                                }

                                print(paste0('post ratio: ', nrow(x)))


                            s <- admission_process(x)
                            s <- manage_prior_admission_data(s)
                            wide_data <- suppressWarnings(wideData(s))
                            export_data <- manage_colnames(wide_data)

                            if (ratio == -99) {
                                ratio <- 'all'
                            }

                            save_path <- c('~/Documents/data/CBGdata/unipoc_xgboost_ready/')
                            write.table(export_data,
                                        file = paste0(save_path, 'duration_', interest_days, '_threshold_', hypo_threshold, '_ratio_', ratio, '.csv'),
                                        sep = ',',
                                        row.names = F)
                            } else {
                                print('not enough data for this ratio')
                            }
                    }
            }
}
## now run the xgboost model
