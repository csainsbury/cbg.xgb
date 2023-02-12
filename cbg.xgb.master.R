##############################################################
## prepare raw aegisPOC data

# set variables
interest_days  <- 21          # admission duration of interest
minimum_n_cbgs <- interest_days + 2  # minimum number of cbgs to include
hypo_threshold <- 3           # hypo threshold of interest
lockout = 7                   # lockout period to assume new admission 
ratio = -99

source('Documents/code/cbg_code/cbg.xgb.process_aegisRaw.R')

save_path <- c('Documents/data/CBGdata/xgboost_ready/')
write.table(export_data,
            file = paste0(save_path, 'duration_', interest_days, '_threshold_', hypo_threshold, 'xx.csv'),
            sep = ',',
            row.names = F)



##############################################################
## as a loop

days_vec <- c(7) # c(4,5,6,7,21,30)
threshold_vec <- (3) # c(3,4,5)

for (d in days_vec) {
  print(d)
  for (t in threshold_vec) {
    
    # set variables
    interest_days  <- d          # admission duration of interest
    minimum_n_cbgs <- interest_days + 2  # minimum number of cbgs to include
    hypo_threshold <- t           # hypo threshold of interest
    lockout = 7                   # lockout period to assume new admission 
    ratio = -99
    
    source('Documents/code/cbg_code/cbg.xgb.process_aegisRaw.R')
    
    save_path <- c('Documents/data/CBGdata/single_day_xgboost_ready/')
    write.table(export_data,
                file = paste0(save_path, 'duration_', interest_days, '_threshold_', hypo_threshold, 'xx.csv'),
                sep = ',',
                row.names = F)
    
  }
}

##############################################################
## as a loop to find single days vals

single_day = c('2023-02-02')

days_vec <- c(3,4,5,6,7) # c(4,5,6,7,21,30)
t = 99

for (d in days_vec) {
  print(d)
  #for (t in threshold_vec) {
    
    # set variables
    interest_days  <- d          # admission duration of interest
    minimum_n_cbgs <- interest_days + 2  # minimum number of cbgs to include
    hypo_threshold <- t           # hypo threshold of interest
    lockout = 7                   # lockout period to assume new admission 
    ratio = -99
    prediction_run <- 1          # flag to set the correct length of admission to extract
    
    source('Documents/code/cbg_code/inpatient_CBG_xgb-main/cbg.xgb.process_aegisRaw_for_prediction.R')
    
    save_path <- c('Documents/data/CBGdata/single_day_xgboost_ready/')
    write.table(export_data,
                file = paste0(save_path, single_day, '_','duration_', interest_days, '_threshold_', hypo_threshold, 'xx.csv'),
                sep = ',',
                row.names = F)
    
  #}
}

##############################################################
## generate multiple days output for validation

single_day_vec = as.character(seq(as.Date('2022-08-01'), as.Date('2023-02-02'), 1))

for (single_day in single_day_vec) {
  
  days_vec <- c(3,4,5,6,7) # c(4,5,6,7,21,30)
  t = 99
  
  for (d in days_vec) {
    print(d)
    #for (t in threshold_vec) {
    
    # set variables
    interest_days  <- d          # admission duration of interest
    minimum_n_cbgs <- interest_days + 2  # minimum number of cbgs to include
    hypo_threshold <- t           # hypo threshold of interest
    lockout = 7                   # lockout period to assume new admission 
    ratio = -99
    prediction_run <- 1          # flag to set the correct length of admission to extract
    
    source('Documents/code/cbg_code/inpatient_CBG_xgb-main/cbg.xgb.process_aegisRaw_for_prediction.R')
    
    save_path <- c('Documents/data/CBGdata/single_day_xgboost_ready/')
    write.table(export_data,
                file = paste0(save_path, single_day, '_','duration_', interest_days, '_threshold_', hypo_threshold, 'xx.csv'),
                sep = ',',
                row.names = F)
    
    #}
  }
  
}
