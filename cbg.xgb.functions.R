exclude_locations <- function(data, exclude_vector) {
  
  x_ex <- data
  
  for (e in c(1:length(exclude_vector))) {
    x_ex <- x_ex[-grep(exclude_vector[e], x_ex$loc, ignore.case = TRUE), ]
  }
  
  return(x_ex)
}

initial_process_label <- function(x, days_n, minimum_n_cbgs, hypo_threshold) {
  
  # x[uID==301523746]
  
  # add unique ID by 2 cols - uID and admission vec
  x <- x %>% mutate(ID = group_indices(x, .dots=c("uID", "admission_vec")))
  x <- x[order(x$uID, x$unix_dateTime), ]
  
  ## truncate all admissions to the right length
  x$day <- as.Date(substr(x$dateTime, 1, 10))
  x[, 'correct_duration_days' := ifelse(day <= (min(day) + (days_n - 1)), 1, 0), by=.(ID)]
  x <- x[correct_duration_days == 1]
  
  # limit to those with minimum n CBGs
  x[, 'N_truncated' := .N, by=.(ID)]
  x <- x[N_truncated >= minimum_n_cbgs]
  
  # split out last day and add label
  # actively select nth day (as specified by interest_days rather than going for the maximum day. more precise)
  x[, 'flag_last_day' := ifelse(day == (min(day) + (days_n - 1)), 1, 0), by=.(ID)]
  
  # are target values present
  x[, 'target_values' := ifelse(sum(flag_last_day) > 0, 1, 0), by=.(ID)]
  x <- x[target_values == 1]
  
  ## calculate data
  x[, 'label' := ifelse(min(Glu[flag_last_day == 1]) <= hypo_threshold, 1, 0), by=.(ID)]
  
  # remove the last day from the training set
  x <- x[flag_last_day == 0]
  
  x[, 'N' := .N, by=.(ID)]
  x <- x[N>days_n]
  
  x[, 'n' := c(1 : .N), by=.(ID)]
  print(paste0('admissions with endpoint during day of interest: ', sum(x[n==1]$label)))
  print(paste0('total n admissions: ', nrow(x[n==1])))
  
  return(x)
  
}

production_initial_process_no_label <- function(x, days_n, minimum_n_cbgs, hypo_threshold, single_day) {
  
  # x[uID==301523746]
  
  single_day <- as.Date(single_day)
  
  # add unique ID by 2 cols - uID and admission vec
  x <- x %>% mutate(ID = group_indices(x, .dots=c("uID", "admission_vec")))
  x <- x[order(x$uID, x$unix_dateTime), ]
  
  ##### debug
  # flag if any values on 290123
  x$day <- as.Date(substr(x$dateTime, 1, 10))
  x[, 'flag_on_day' := ifelse(day == single_day, 1, 0), by=.(ID)]
  x[, 'first_day_of_admission' := min(day), by=.(ID)]
  
  cohort_of_interest <- x[first_day_of_admission == (single_day - (days_n - 2))]
  
  # remove data after the prediction day of interest
  cohort_of_interest <- cohort_of_interest[day <= single_day]
  
  x <- cohort_of_interest
  
  # limit to those with minimum n CBGs
  x[, 'N_truncated' := .N, by=.(ID)]
  x <- x[N_truncated >= minimum_n_cbgs]
  
  x[, 'N' := .N, by=.(ID)]
  x <- x[N>days_n]
  
    plot(x$dateTime, x$Glu, pch = 16, cex = 0.6, ylim = c(1, 28))
    for (i in unique(x$ID)) {
      lines(x[ID==i]$dateTime, x[ID==i]$Glu, col=i)
    }
    abline(4, 0, col = 'orange', lty = 3)
    abline(3, 0, col = 'red', lty = 3)
  
  # split out last day and add label
  # actively select nth day (as specified by interest_days rather than going for the maximum day. more precise)
  #x[, 'flag_last_day' := ifelse(day == (min(day) + (days_n - 1)), 1, 0), by=.(ID)]
  
  # are target values present
  #x[, 'target_values' := ifelse(sum(flag_last_day) > 0, 1, 0), by=.(ID)]
  #x <- x[target_values == 1]
  
  ## calculate data
  #x[, 'label' := ifelse(min(Glu[flag_last_day == 1]) <= hypo_threshold, 1, 0), by=.(ID)]
  
  # remove the last day from the training set
  #x <- x[flag_last_day == 0]
  
  x[, 'n' := c(1 : .N), by=.(ID)]
  #print(paste0('admissions with endpoint during day of interest: ', sum(x[n==1]$label)))
  print(paste0('total n admissions for testing: ', nrow(x[n==1])))
  
  return(x)
  
}

prior_process <- function(x, days_n, minimum_n_cbgs) {
  
  # add unique ID by 2 cols - uID and admission vec
  x <- x %>% mutate(ID = group_indices(x, .dots=c("uID", "prior_admission_vec"))) 
  
  ## truncate all admissions to the right length
  x$day <- as.Date(x$dateTime)
  if (nrow(x) > 0) {
    x[, 'correct_duration_days' := ifelse(day <= (min(day) + (days_n - 1)), 1, 0), by=.(ID)]
  }

  
  # limit to those with minimum n CBGs
  x[, 'N_truncated' := .N, by=.(ID)]
  x <- x[N_truncated >= minimum_n_cbgs]
  
  return(x)
  
}

case_control_ratio <- function(x, ratio) {
  
  # produce fixed ratio of case to no case
  event_ids <- unique(x[label==1]$ID)
  no_event_ids <- unique(x[label==0]$ID)
  id_sample <- no_event_ids[sample(length(no_event_ids), round(length(event_ids) * ratio), 0)]
  
  neg <- x[ID %in% id_sample]
  pos <- x[ID %in% event_ids]
  
  x <- rbind(neg, pos)
  
  print(paste0('number of admissions with endpoints', sum(x[n==1]$label)))
  print(paste0('total number of admissions', nrow(x[n==1])))
  
  return(x)
  
}

# process by admission function
admission_process <- function(dt) {
  
  s <- dt
  
  s[, 'max_by_day' := max(Glu), by=.(ID, day)]
  s[, 'min_by_day' := min(Glu), by=.(ID, day)]
  s[, 'median_by_day' := median(Glu), by=.(ID, day)]
  s[, 'iqr_by_day' := quantile(Glu)[4]-quantile(Glu)[2], by=.(ID, day)]
  s[, 'sd_by_day' := sd(Glu) ,by=.(ID, day)]
  s[, 'mean_by_day' := mean(Glu) ,by=.(ID, day)]
  s[, 'cV_by_day' := sd_by_day/mean_by_day ,by=.(ID, day)]
  
  s[, 'training_min' := min(Glu) ,by=.(ID)]
  s[, 'training_max' := max(Glu) ,by=.(ID)]
  
  s[, 'day_n' := c(1:.N), by=.(ID, day)]
  s[, 'day_N' := .N, by=.(ID, day)]
  s[, 'gradient' := as.numeric(lm(Glu ~ dateTime)$coefficients[2]), by=.(ID)]
  s[, 'sd_cbg' := sd(Glu) ,by=.(ID)]
  s[, 'mean_cbg' := mean(Glu) ,by=.(ID)]
  s[, 'cV' := sd_cbg/mean_cbg ,by=.(ID)]
  
  s[, 'ID_n' := c(1:.N), by=.(ID)]
  s[, 'ID_N' := .N, by=.(ID)]
  
  return(s)
}

prior_admission_process <- function(dt) {
  
  s <- dt
  
  if (nrow(s) > 0) {
    s[, 'prior_overall_min' := min(Glu) ,by=.(ID)]
    s[, 'prior_overall_max' := max(Glu) ,by=.(ID)]
    s[, 'prior_duration' := max(unix_dateTime) - min(unix_dateTime) ,by=.(ID)]
    s[, 'prior_gradient' := as.numeric(lm(Glu ~ dateTime)$coefficients[2]), by=.(ID)]
    s[, 'prior_sd_cbg' := sd(Glu) ,by=.(ID)]
    s[, 'prior_mean_cbg' := mean(Glu) ,by=.(ID)]
    s[, 'prior_cV' := prior_sd_cbg/prior_mean_cbg ,by=.(ID)]
    s[, 'prior_N_tests' := .N ,by=.(ID)]
  }
  
  return(s)
  
}

process_aegis <- function(x) {
  
  subset <- x %>% select(V148, V150, V101, V104, V140)
  colnames(subset) <- c('CHI', 'location', 'dateTime', 'Glu', 'op')
  subset$CHI <- as.numeric(subset$CHI)
  subset <- subset[CHI > 0]

  subset <- unique(subset)
  
  chi_remove = c(1, 11, 1133233, 1111111111, 2222222222, 3333333333, 4444444444, 5555555555, 6666666666,
                 7777777777, 8888888888, 9999999999, 111111111, 222222222, 333333333,
                 444444444, 555555555, 666666666, 777777777, 888888888, 999999999, 3232323232,
                 2121212121)
  subset <- subset[!subset$CHI %in% chi_remove]
  
  # subset <- subset[CHI > 1111111111] # what is this?
  
  subset$Glu <- ifelse(subset$Glu == '<1.1', 1, subset$Glu)
  subset$Glu <- ifelse(subset$Glu == '>27.8', 27.9, subset$Glu)
  subset$dateTime <- as.POSIXct(subset$dateTime, format = "%d/%m/%Y %H:%M")
  
  subset$numericGlu = as.numeric(subset$Glu)
  subset$numericGlu[is.na(subset$numericGlu)] <- -99
  subset <- subset[numericGlu > 0]
  
  subset$unix_dateTime <- returnUnixDateTime(subset$dateTime)
  
  # remove unwanted locations
  remove_vector <- c('XDUMP', 'RENAL DIALYSI', 'Renal Dialysis', 'RENALDIALYSIS', 'Renal Unit', 'Missing meters',
                     'Missing Metersx', 'RMissing Meters', '44 LABOUR WARD', 'Biochem (K)', 'Biochemistry',
                     'Biochemistry RA', 'Mother\\Baby Uni', 'PICU')
  subset <- subset[!subset$location %in% remove_vector]
  
  # rename CHI to uID
  subset <- subset %>% select(-Glu)
  subset <- subset %>% rename(uID = CHI,
                              Glu = numericGlu)
    
  return(subset)
  
}

wideData <- function(dataIn, is_training_data = 1) {
  
  s <- dataIn
  
  ## per ID generate wide data
  ids <-unique(s$ID)
  increment = 1

  for (i in c(1:length(ids))) {
    
    if (i %% 100 == 0) {print(i/length(ids))}
    
    sub <- s[ID == ids[i]]
    sub <- sub[day_n == 1]
    
    # if there are no labels ie this is a prospective test set:
    # add the CHI to the label column
    if (is_training_data == 0){
          if (exists('sub$label[1]') == FALSE) {
          sub$label <- sub$uID
      }
    }

    
    sub <- sub %>% select(ID, dateTime, day, Glu,
                          max_by_day, min_by_day, median_by_day, day_N, iqr_by_day, cV_by_day,
                          training_min, training_max, cV, gradient,
                          prior_overall_min, prior_overall_max, prior_duration, prior_gradient,
                          prior_sd_cbg, prior_mean_cbg, prior_cV, prior_N_tests, time_from_index,
                          label)
    
    if ((nrow(sub) > 0) & (as.numeric(min(diff(as.Date(sub$dateTime)))) <= 1)) { # second arguement needed to ensure that minimum time step is not greater than 1 day (thicken will no work if it is)
      
      #sub$day_numeric <- c(1:nrow(sub))
      
      #if ((as.Date(sub$dateTime[nrow(sub)]) - as.Date(sub$dateTime[1]) < (interest_days - 1))) {
      if ((sub$day[nrow(sub)] - sub$day[1]) < (interest_days - 1)) {
          
        sub <- as.data.frame(sub)
        sub[nrow(sub)+1,] <- NA
        sub <- as.data.table(sub)
        
        sub$dateTime[nrow(sub)] <- (sub$dateTime[1] + ((interest_days - 1) * 60*60*24))
        
      }
      
      cV <- sub$cV[1]
      gradient <- sub$gradient[1]
      training_min <- sub$training_min[1]
      training_max <- sub$training_max[1]
      id <- sub$ID[1]
      prior_overall_min <- sub$prior_overall_min[1]
      prior_overall_max <- sub$prior_overall_max[1]
      prior_duration <- sub$prior_duration[1]
      prior_gradient <- sub$prior_gradient[1]
      prior_sd_cbg <- sub$prior_sd_cbg[1]
      prior_mean_cbg <- sub$prior_mean_cbg[1]
      prior_cV <- sub$prior_cV[1]
      prior_N_tests <- sub$prior_N_tests[1]
      time_from_index <- sub$time_from_index[1]
      label <- sub$label[1]
      
      sub <- sub %>% select(dateTime, Glu, max_by_day, min_by_day, median_by_day, day_N, iqr_by_day, cV_by_day)
      sub <- sub %>% thicken("1 day") %>% select(-dateTime) %>% pad()
      
      #print(nrow(sub))
      ## exception handling
      ## if there are 2 rows of NAs as the last rows of sub (due to padding): remove one of these rows
      if (nrow(sub) > interest_days) {

        sub <- sub[1:interest_days, ]

      }
      
      # sub.locf <- na.locf(sub)
      sub.locf <- sub
      sub.locf$Glu <- NULL
      
      require(reshape2)
      melt.sub <- melt(sub.locf, id.vars='dateTime_day')
      m <- as.data.frame(t(melt.sub))
      m$cV <- cV
      m$gradient <- gradient
      m$training_min <- training_min
      m$training_max <- training_max
      m$id <- id
      m$prior_overall_min <- prior_overall_min
      m$prior_overall_max <- prior_overall_max
      m$prior_duration <- prior_duration
      m$prior_gradient <- prior_gradient
      m$prior_sd_cbg <- prior_sd_cbg
      m$prior_mean_cbg <- prior_mean_cbg
      m$prior_cV <- prior_cV
      m$prior_N_tests <- prior_N_tests
      m$time_from_index <- time_from_index
      m$label <- label
      m <- m[-1, ]
      colnames(m) <- c(m[1,1:(ncol(m) - 15)],'cV' ,'gradient' , 'training_min', 'training_max','id',
                       'prior_overall_min', 'prior_overall_max', 'prior_duration', 'prior_gradient',
                       'prior_sd_cbg', 'prior_mean_cbg', 'prior_cV', 'prior_N_tests', 'time_from_index',
                       'label')
      m <- m[-1, ]
      
      if (increment == 1) {
        export <- m
        increment = increment + 1
      } else {
        export <- rbind(export, m)
        increment = increment + 1
      }
      
    }
    
    
  }
  
  return(export)
}

prepare_aegis <- function(data, k, predicton_run) {
  
  m <- data
    
    m <- m[order(m$uID, m$dateTime), ]
    
    lockout = 7
    m$unix_dateTime <- returnUnixDateTime(m$dateTime)
    m[, 'n_admissions' := admission_N(unix_dateTime, lockout), by=.(uID)]
    # label each admission
    m[, 'admission_vec' := admission_N_vector(unix_dateTime, lockout), by=.(uID)]
    
    # add interval last-first measurement
    m[, 'interval_seconds' := as.numeric(max(unix_dateTime) - min(unix_dateTime)), by=.(uID, admission_vec)]
    m[, 'interval_days' := as.numeric(max(as.Date(dateTime)) - min(as.Date(dateTime))), by=.(uID, admission_vec)]
    m <- m[order(m$uID, m$dateTime), ]
    m[, 'n' := c(1:.N), by=.(uID, admission_vec)]
    m[, 'N' := .N, by=.(uID, admission_vec)]
    
    single_m <- m[n==1]
    
    single_m <- single_m[order(-single_m$interval_days), ]
    
    # minimum number of admission duration days for study cohort
    # will equal train + test period unless prediction_run == 1, in which it will only
    # equal the train period

    ## don't think this is correct. remove the if statement and just use k
    
    if (predicton_run == 1) {
      cohort_selection_threshold <- k - 2
    } else {
      cohort_selection_threshold <- k
    }
    
    
    # plot(single_m$interval_days, cex = 0,main = paste0('admission duration: ', cohort_selection_threshold, ' days'))
    # lines(single_m$interval_days, lwd = 3)
    # abline(h = cohort_selection_threshold, col = 'red')
    
    last_id <- which(single_m$interval_days == cohort_selection_threshold)[length(which(single_m$interval_days == cohort_selection_threshold))]
    
    cohort <- single_m[1:last_id, ]
    print(dim(cohort))
    
    cohort = data.table('uID' = cohort$uID,
                        'admission_vector' = cohort$admission_vec)
    
    cf <- merge(cohort, m,
                by.x = c('uID', 'admission_vector'),
                by.y = c('uID', 'admission_vec'))
    
    # recalculate admission duration intervals
    cf[, 'new_interval_seconds' := max(unix_dateTime) - min(unix_dateTime), by =.(uID, admission_vector)]
    
    # label admissions within cf
    cf[, 'admission_vec' := admission_N_vector(unix_dateTime, lockout), by=.(uID)]
    
    u_IDs <- unique(cf$uID)
    
    options(warn = 1)  
    
    export_concat <- cf[1,]
    export_concat <- export_concat[-1,]
    for (j in c(1:length(u_IDs))) {
      
      if (j %% 1000 == 0) {
        print(paste0('total: ', length(u_IDs), ' | done: ',print(j)))
      }
      
      sub = cf[uID == u_IDs[j]]
      
      # find each admission
      admissionsList <- as.data.frame(table(sub$admission_vec))$Var1
      
      for (m in c(1:length(admissionsList))) {
        subsub <- sub[admission_vec == admissionsList[m]]
        
        duration <- as.numeric(max(as.Date(subsub$dateTime)) - min(as.Date(subsub$dateTime)))
        
        dateList <- as.Date(subsub$dateTime)
        
        # take first n days only - to avoid epoch effect
        start_date <- min(dateList)
        end_date <- start_date + cohort_selection_threshold
        
        export <- subsub[as.Date(dateTime) >= start_date &as.Date(dateTime) <= end_date]
        
        export_concat <- rbind(export_concat, subsub)
        
      }
      
    }
    
    
    use_cohort <- export_concat
    return(use_cohort)
    
    # write.table(use_cohort, file = paste0('~/Documents/data/CBGdata/huge_unipoc_time_series_cohort_first_', cohort_selection_threshold, '_days.csv'), sep = ',', row.names = F)
    
  
  
}

manage_colnames <- function(data) {
  
  exp <- data
  
  coln = colnames(exp)
  max_names <- coln[coln == 'max_by_day']
  min_names <- coln[coln == 'min_by_day']
  med_names <- coln[coln == 'median_by_day']
  N_names <- coln[coln == 'day_N']
  iqr_names <- coln[coln == 'iqr_by_day']
  cVday_names <- coln[coln == 'cV_by_day']
  
  other_names <- coln[coln != 'max_by_day' & coln != 'min_by_day' & coln != 'iqr_by_day' &
                        coln != 'median_by_day' & coln != 'day_N'  & coln != 'cV_by_day']
  
  for (c in c(1:length(max_names))) {
    max_names[c] <- paste0(max_names[c], '_', c)
  }
  for (c in c(1:length(min_names))) {
    min_names[c] <- paste0(min_names[c], '_', c)
  }
  for (c in c(1:length(iqr_names))) {
    iqr_names[c] <- paste0(iqr_names[c], '_', c)
  }
  for (c in c(1:length(med_names))) {
    med_names[c] <- paste0(med_names[c], '_', c)
  }
  for (c in c(1:length(N_names))) {
    N_names[c] <- paste0(N_names[c], '_', c)
  }
  for (c in c(1:length(cVday_names))) {
    cVday_names[c] <- paste0(cVday_names[c], '_', c)
  }
  
  coln_n <- c(max_names, min_names, med_names, N_names, iqr_names, cVday_names, other_names)
  colnames(exp) <- coln_n
  
  export <- exp
  
  for (j in c(1:ncol(export))) {
    export[, j] <- as.numeric(export[, j])
  }
  
  return(export)
  
}

manage_prior_admission_data <- function(data) {
  
  s <- data
  
  # get data from most recent admission, and all prior admission N
  s[, 'qualifying_admission_vec' := admission_N_vector(unix_dateTime, lockout), by=.(uID)]
  
  # identify the most recent admission - to ensure each ID appears only once
  s[, 'last_admission_flag' := ifelse(qualifying_admission_vec == max(qualifying_admission_vec), 1, 0), by=.(uID)]
  
  # prior data from ids with multiple admissions
  ma <- s[last_admission_flag==0]
  # ids from prior admissions:
  ids_old_admissions <- unique(ma$uID)
  # first CBGs from all last (index) admissions
  index_dates <- s[last_admission_flag == 1 &ID_n == 1]
  index_dates <- index_dates %>% select(uID, unix_dateTime)
  index_dates <- index_dates %>% rename(index_first_unix = unix_dateTime)
  
  # merge with all CBG data
  # to get all prior CBGs for the uIDs of interest
  all_prior_cbgs <- merge(index_dates, data.orig, by.x = 'uID', by.y = 'uID')
  all_prior_cbgs <- all_prior_cbgs[unix_dateTime < index_first_unix]
    
    # process all prior admissions from the prior CBG dataset
    all_prior_cbgs[, 'prior_admission_vec' := admission_N_vector(unix_dateTime, lockout), by=.(uID)]
    all_prior_cbgs <- prior_process(all_prior_cbgs, interest_days, minimum_n_cbgs) 
    
    all_prior_cbgs <- prior_admission_process(all_prior_cbgs)
    
    if (nrow(all_prior_cbgs) > 0) { # exception handling for when there is no prior data for any ids
      
    # add time from indexs
    all_prior_cbgs[, 'time_from_index' := index_first_unix - max(unix_dateTime), by=.(uID)]
    all_prior_cbgs[, 'last_prior_admission_flag' := ifelse(prior_admission_vec == max(prior_admission_vec), 1, 0), by=.(uID)]
    
    # only take the last admission
    last_prior_admission <- all_prior_cbgs[last_prior_admission_flag == 1]
    last_prior_admission[, 'single_row_n' := c(1:.N), by=.(uID)]
    # single row
    last_prior_admission <- last_prior_admission[single_row_n==1]
    
    # file for merge back to index admission dataset
    last_prior_admission <- last_prior_admission %>% select(uID, prior_overall_min, prior_overall_max, prior_duration, prior_gradient, prior_sd_cbg, prior_mean_cbg,  prior_cV, prior_N_tests, time_from_index)
    
  } else { # create cols with values as NAs if no prior data
    
    s_ids <- unique(s$uID)
    cols  <- c('prior_overall_min', 'prior_overall_max', 'prior_duration', 'prior_gradient', 'prior_sd_cbg', 'prior_mean_cbg',  'prior_cV', 'prior_N_tests', 'time_from_index')
    null_cols <- as.data.frame(matrix(NA, nrow = length(s_ids), ncol = length(cols)))
    colnames(null_cols) <- cols
    last_prior_admission <- data.frame('uID' = s_ids)
    last_prior_admission <- cbind(last_prior_admission, null_cols)
  }
  
  # merge prior back to index dataset
  s_merge <- merge(s, last_prior_admission, by.x = 'uID', by.y = 'uID', all.x = T)
  
  ## lastly
  ## ensure that only data from the last admission goes forward into training
  s <- s_merge[last_admission_flag == 1]
  
  return(s)
  
}
