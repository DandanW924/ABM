#' ABM functions for R package
#' 
#' Copyright (c) 2016, GE Digital. All rights reserved.
#' Proprietary and confidential
#' Unauthorized copying of this file, via any medium is strictly prohibited
#' This cannot be copied and/or distributed without the express permission of GE Digital
#' 
#' Author: Hyung-il Ahn (hyungil.ahn@ge.com), GE Digital



if ("package:plyr" %in% search()) { detach("package:plyr", unload=TRUE) }
if ("package:dplyr" %in% search()) { detach("package:dplyr", unload=TRUE) }


library(plyr)
library(dplyr)

library(randomForest)
library(survival)
library(muhaz)

library(ggplot2)
library(ROCR)

#' @title Create an object of class "ABMdata" for modeling functions
#' @description  Create an object for storing analysis and modeling results
#' @param data data.frame or data.table
#' @param unit_id_colname unit identifier column name
#' @param ts_index_colname time series column name
#' @return object of class ABMdata
#' @export
createABMdata <- function (data, unit_id_colname, comp_id_colname = '', ts_index_colname, dtime_format_string) {
  dt <- list()
  class(dt) <- "ABMdata"
  
  dt$unit_id_colname <- unit_id_colname
  dt$ts_index_colname <- ts_index_colname
  dt$comp_id_colname <- comp_id_colname
  
  dt$unit_set <- unique(data[,unit_id_colname])
  dt$ix_numeric_or_integer <- as.numeric(which(sapply(names(data), function(nm){class(data[,nm])[1] == "numeric" | class(data[,nm])[1] == "integer"}) == T))
  dt$ix_integer <- as.numeric(which(sapply(names(data), function(nm){ class(data[,nm])[1] == "integer"}) == T))
  dt$ix_numeric <- as.numeric(which(sapply(names(data), function(nm){class(data[,nm])[1] == "numeric"}) == T))
  
  
  data$dtime_idx <- as.POSIXct(strptime(data[,ts_index_colname], dtime_format_string, tz="UTC"))
  data <- data[order(data[,unit_id_colname], data$dtime_idx),]
  #data <- arrange(data, data[,unit_id_colname], dtime_idx)
  
  if (comp_id_colname != '') {
    data$comp_id = data[, comp_id_colname]
    dt$comp_set = unique(data$comp_id)
  } else {
    data$comp_id = 'not set'
    dt$comp_set = 'not set'
  }
  
  
  N_data = nrow(data)
  
  data$target_event = rep(0, N_data)
  data$eventual_state = rep(0, N_data)
  
  
  data$class_label = rep(0, N_data)
  
  data$fprob = rep(0, N_data)
  data$fprob_r = rep(0, N_data)
  
  data$coxrisk = rep(0, N_data)
  data$bhaz = rep(0, N_data)
  data$coxhaz = rep(0, N_data)
  
  data$coxrisk_sefit = rep(0,N_data)
  data$coxrisk_se = rep(0,N_data)
  
  data$coxhaz_seH = rep(0,N_data)
  data$coxhaz_seL = rep(0,N_data)
  
  data$coxhaz_sefitH = rep(0,N_data)
  data$coxhaz_sefitL = rep(0,N_data)
  
  data$cum_failprob = rep(0, N_data)
  
  data$sum_fprob = rep(0, N_data)
  data$sum_fprob_r = rep(0, N_data)
  data$valid = rep(0, N_data)
  
  
  
  dt$data <- data
  dt$dataset <- data
  
  return(dt)
}

#' @title Set the component id
#' @description Use the given column names to construct and set the component id
#' @param dt an object of class ABMdata
#' @param def_colnames list of column names
#' @return an object of class ABMdata
#' @export
setCompIds <- function (dt, def_colnames) {
  
  data <- dt$data
  data$comp_id_str <- as.character( apply(data[,def_colnames], 1, paste, collapse=" ") )
  data$comp_id <- as.numeric(as.factor(data$comp_id_str))
  
  dt$data <- data
  return(dt)
  
}

getCompSet <- function (dt) {
  data <- dt$data
  return(unique(data$comp_id))
}

#' @title Set the date index
#' @description Use the date/time column and format string to set the date index
#' @param dt an object of class ABMdata
#' @param dtime_colname name of date/time column
#' @param dtime_format_string string used to format the date/time
#' @return an object of class ABMdata
#' @export
# # setDateIndex <- function (dt, dtime_colname, dtime_format_string) {
# #   unit_colname <- dt["unit_colname"]
# #   dt$dtime_idx <- as.POSIXct(strptime(dt[,dtime_colname], dtime_format_string, tz="UTC"))
# #   dt = dt[order(dt[,unit_colname], dt[,"dtime_idx"])]
# #   return(dt)
# # }
# setDateIndex <- function (dt, dtime_colname, dtime_format_string) {
#   #library(lubridate)
#   #parse_date_time(x, c("ymd", "ymd HM"))
#   data <- dt$data
#
#   unit_colname <- dt$unit_colname
#   data$dtime_idx <- as.POSIXct(strptime(data[,dtime_colname], dtime_format_string, tz="UTC"))
#   data <- data[order(data[,unit_colname], data$dtime_idx),]
#
#   dt$data <- data
#   return(dt)
# }

convertDateFormat <- function (dt, dtime_colname, dtime_format_string) {
  data <- dt$data
  
  data[, dtime_colname] <- as.POSIXct(strptime(data[, dtime_colname], dtime_format_string, tz="UTC"))
  
  dt$data <- data
  return(dt)
  
}




# set_event_ <- function (x, event_colname, eventual_state_colname) {
#
#   status_tag = x[nrow(x), eventual_state_colname]
#
#   if (status_tag == 1) { # unhealthy
#     x[nrow(x), event_colname] = 1
#   }
#
#   return(x)
# }

set_event <- function (eventual_state) {
  x = rep(0, length(eventual_state))
  state_tag = eventual_state[length(eventual_state)]
  #print(state_tag)
  if (state_tag == 1) { # unhealthy
    x[length(x)] = 1
  }
  
  return(x)
}

# when target_event (e.g., "failure" whose value is 1 for only the last record corresponding to the actual failure time )
# should be drived from eventual_state (e.g., "eventual_failure" whose value is 1 for all records in component with failure )
#
# should run after setCompId
setCompEventFromEventualState <- function (dt, eventual_state_colname) {
  
  
  data <- dt$data
  
  dt$eventual_state_colname <- eventual_state_colname
  dt$target_event_colname <- "target_event"
  
  data$eventual_state = data[, eventual_state_colname]
  
  
  # data = ddply(data, .(comp_id), set_event_, "target_event", "eventual_state") ## take too long to compute
  
  data <- as.data.frame( data %>% group_by(comp_id) %>% mutate( target_event = set_event(eventual_state) ) )
  
  dt$data <- data
  return(dt)
  
}




set_eventual_state <- function (target_event) {
  
  x = rep(0, length(target_event))
  state_tag = target_event[length(target_event)]
  
  if (state_tag == 1) { # unhealthy
    x = rep(1, length(target_event))
  }
  
  return(x)
  
}

setCompEventualStateFromEvent <- function (dt, event_colname) {
  
  
  #   data <- dt$data
  #
  #   dt$event_colname <- event_colname
  #
  #   data$eventual_state = 0
  #   dt$eventual_state_colname <- "eventual_state"
  #
  #
  #   data = ddply(data, .(comp_id), set_eventual_state, event_colname, "eventual_state")
  #
  #   dt$data <- data
  #   return(dt)
  
  
  data <- dt$data
  
  dt$eventual_state_colname <- "eventual_state"
  
  data$target_event = data[, event_colname]
  dt$target_event_colname <- event_colname
  
  data <- as.data.frame( data %>% group_by(comp_id) %>% mutate( eventual_state = set_eventual_state(target_event) ) )
  
  dt$data <- data
  return(dt)
  
  
}



setCompCycleAndTime <- function (dt, unit_cumulative_cycle_colname = '', unit_cumulative_time_colname = '') {
  
  data <- dt$data
  
  dt$unit_cumulative_cycle_colname <- unit_cumulative_cycle_colname
  dt$unit_cumulative_time_colname <- unit_cumulative_time_colname
  
  if (unit_cumulative_cycle_colname != '') {
    #names(data)[which(names(data) == unit_cumulative_cycle_colname)] = 'unit_cumulative_cycle'
    data$unit_cumulative_cycle = data[,unit_cumulative_cycle_colname]
    data <- as.data.frame( data
                           %>% group_by(comp_id)
                           %>% mutate( comp_cycle = unit_cumulative_cycle - unit_cumulative_cycle[1],
                                       this_cycle = c(diff(comp_cycle),1),
                                       comp_max_cycle = max(comp_cycle, na.rm=T) ) )
  }
  
  if (unit_cumulative_time_colname != '') {
    #names(data)[which(names(data) == unit_cumulative_time_colname)] = 'unit_cumulative_time'
    data$unit_cumulative_time = data[,unit_cumulative_time_colname]
    data <- as.data.frame( data
                           %>% group_by(comp_id)
                           %>% mutate( comp_time = unit_cumulative_time - unit_cumulative_time[1],
                                       this_time = c(diff(comp_time),1),
                                       comp_max_time = max(comp_time, na.rm=T) ) )
  }
  #head(data[data$comp_id==getCompSet(dt)[1],]$comp_cycle)
  
  dt$data <- data
  return(dt)
  
}


set_comp_label <- function (x, window_size_for_label) {
  
  x$class_label = as.integer( rep(0,nrow(x)) )
  
  state_tag = x$eventual_state[1] # the same as eventual_state[nrow(x)]
  #state_tag = x$target_event[nrow(x)]  # tricky when eventual_state[nrow(x)] == 1 but target_event[nrow(x)] = 0 due to subset_condition in setDatasetForModel
  
  if (state_tag == 1) {# unhealthy
    x$class_label = as.integer( (x$measure[nrow(x)] - x$measure) < window_size_for_label )
  }
  
  return(x$class_label)
}




set_comp_fe <- function (x, fe_list) {
  
  
  # fe_list = c("zalt", "wfmp", "zwf36", "zt49", "ztoil_smoothed", "ztoil", "pcn1k", "zpcn25", "zpcn12", "ztla", "zpoil", "zpoil_smoothed", "degt", "degt_smoothed", "ibp1", "ibp2", "ibp3", "gpcn25_smoothed", "ibe1", "ibe2", "ibe3", "zxm", "zvb2r", "brat", "wbi", "zvb2r_smoothed", "zvb1f_smoothed", "zvb1f", "ivs12", "ivs13", "vsvnom", "zvb1f_d", "ztla_d", "zpcn25_d", "gpcn25_d", "gwfm", "gwfm_d", "degt_d", "zt49_d", "dpoil", "dpoil_smoothed", "zt1a", "sat", "zwf36_d_smoothed", "cas", "delfn", "deln1", "dtamb", "egthdm", "egthdm_d", "iai", "iaie", "iaie1", "iaie2", "iaie3", "iaiwg", "satsl", "satslf", "sedi_ecsi", "sedi_ecsn", "sedi_ecsv", "sedi_etsi", "sedi_etsn", "sedi_etsv", "sloatl", "sloatl_d", "sloatl_smoothed", "tatsl", "tatslf")
  
  x0 = x
  
  for (fe_nm in fe_list){
    
    x0[is.na(x[,fe_nm]),fe_nm] = 0
    cs <- cumsum(x0[,fe_nm])
    cnt <- cumsum(!is.na(x[,fe_nm]))
    cnt[cnt==0]=1  ## to avoid NaN (divided by 0 below)
    avg = cs/cnt
    x[,paste(fe_nm, "_cumavg", sep='')] = avg
    x[,paste(fe_nm, "_cumsum", sep='')] = cs
    
  }
  
  
  
  #     fe_list = c("zalt", "wfmp", "zwf36", "zt49", "ztoil_smoothed", "ztoil", "pcn1k", "zpcn25", "zpcn12", "ztla", "zpoil", "zpoil_smoothed", "degt", "degt_smoothed", "ibp1", "ibp2", "ibp3", "gpcn25_smoothed", "ibe1", "ibe2", "ibe3", "zxm", "zvb2r", "brat", "wbi", "zvb2r_smoothed", "zvb1f_smoothed", "zvb1f", "ivs12", "ivs13", "vsvnom", "zvb1f_d", "ztla_d", "zpcn25_d", "gpcn25_d", "gwfm", "gwfm_d", "degt_d", "zt49_d", "dpoil", "dpoil_smoothed", "zt1a", "sat", "zwf36_d_smoothed", "cas", "delfn", "deln1", "dtamb", "egthdm", "egthdm_d", "iai", "iaie", "iaie1", "iaie2", "iaie3", "iaiwg", "satsl", "satslf", "sedi_ecsi", "sedi_ecsn", "sedi_ecsv", "sedi_etsi", "sedi_etsn", "sedi_etsv", "sloatl", "sloatl_d", "sloatl_smoothed", "tatsl", "tatslf")
  #
  #     roll_avg <- function(k, fe_list, roll_win){
  #       rr = sapply(fe_list, function(fe_nm){ mean( x[which(x$comp.cycles > (x[k,]$comp.cycles - roll_win) & x$comp.cycles <= x[k,]$comp.cycles), fe_nm ]  , na.rm=T) } )
  #       return(rr)
  #     }
  #
  #     roll_win = 250
  #     ssa = sapply(1:nrow(x), roll_avg, fe_list, roll_win)
  #
  #     assign( eval(varname), set_variable_fn(varname), envir=.GlobalEnv )
  
  
  #   fe_list = c("zalt", "wfmp", "zwf36", "zt49", "ztoil_smoothed", "ztoil", "pcn1k", "zpcn25", "zpcn12", "ztla", "zpoil", "zpoil_smoothed", "degt", "degt_smoothed", "ibp1", "ibp2", "ibp3", "gpcn25_smoothed", "ibe1", "ibe2", "ibe3", "zxm", "zvb2r", "brat", "wbi", "zvb2r_smoothed", "zvb1f_smoothed", "zvb1f", "ivs12", "ivs13", "vsvnom", "zvb1f_d", "ztla_d", "zpcn25_d", "gpcn25_d", "gwfm", "gwfm_d", "degt_d", "zt49_d", "dpoil", "dpoil_smoothed", "zt1a", "sat", "zwf36_d_smoothed", "cas", "delfn", "deln1", "dtamb", "egthdm", "egthdm_d", "iai", "iaie", "iaie1", "iaie2", "iaie3", "iaiwg", "satsl", "satslf", "sedi_ecsi", "sedi_ecsn", "sedi_ecsv", "sedi_etsi", "sedi_etsn", "sedi_etsv", "sloatl", "sloatl_d", "sloatl_smoothed", "tatsl", "tatslf")
  #
  #   roll_avg <- function(fe_nm, roll_win){
  #     rr = sapply(1:nrow(x), function(k){ mean( x[which(x$comp.cycles > (x[k,]$comp.cycles - roll_win) & x$comp.cycles <= x[k,]$comp.cycles), fe_nm ]  , na.rm=T) } )
  #     return(rr)
  #   }
  #
  #   roll_win = 250
  #   ssa = sapply(fe_list, roll_avg, roll_win)
  
  
  
  return(x)
}



getHealthyCompIds <- function (dt) {
  
  data <- dt$data
  
  tmp <- as.data.frame( data %>% filter(eventual_state==0) %>% group_by(comp_id) %>% summarise( siz = n()) )
  comp_ids = tmp$comp_id
  
  #comp_ids = unique(data[which(data$eventual_state==0),]$comp_id)
  return(comp_ids)
}

getFailedCompIds <- function (dt) {
  data <- dt$data
  
  tmp <- as.data.frame( data %>% filter(eventual_state==1) %>% group_by(comp_id) %>% summarise( siz = n()) )
  comp_ids = tmp$comp_id
  
  #comp_ids = unique(data[which(data$eventual_state==1),]$comp_id)
  return(comp_ids)
}

getAllCompIds <- function (dt) {
  data <- dt$data
  
  comp_ids = unique(data$comp_id)
  return(comp_ids)
}



getHealthyCompNum <- function (dt) {
  
  data <- dt$data
  
  comp_num = nrow( data %>% filter(eventual_state==0) %>% group_by(comp_id) %>% summarise( siz = n()) )
  # comp_num = length(unique(data[which(data$eventual_state==0),]$comp_id))
  return(comp_num)
}

getFailedCompNum <- function (dt) {
  data <- dt$data
  
  comp_num = nrow( data %>% filter(eventual_state==1) %>% group_by(comp_id) %>% summarise( siz = n()) )
  # comp_num = length(unique(data[which(data$eventual_state==1),]$comp_id))
  return(comp_num)
}

getAllCompNum <- function (dt) {
  data <- dt$data
  
  comp_num = nrow( data %>% group_by(comp_id) %>% summarise( siz = n()) )
  #comp_num = length(unique(data$comp_id))
  return(comp_num)
}


removeColumns <- function (dt, removed_feature_colnames) {
  
  data <- dt$data
  
  rfc = removed_feature_colnames
  
  dt$data <- data[,c(setdiff(names(data), rfc))]
  return(dt)
}




# perc   0.0 ~ 1.0
# select all columns where more than "perc" % of records have valid (nonNA) value
selectValidColumns <- function (dt, perc, tested_feature_colnames) {
  
  data <- dt$data
  
  tfc = tested_feature_colnames
  col_idx = sapply(tfc, function(i){sum(is.na(data[,i])) < (1-perc)*nrow(data)} )
  
  dt$valid_colnames = tfc[col_idx]
  dt$data <- data[,c(setdiff(names(data), tfc),tfc[col_idx])]
  return(dt)
}




impute_fe <- function(x) {
  cl = class(x)
  x <- na.roughfix(x)
  if (cl == 'integer') {
    x <- as.integer(x)
  }
  return(x)
}

# impute_comp <- function(x, ix_numeric) {
#   x[,ix_numeric] <- na.roughfix(x[,ix_numeric])
#   return(x)
# }


#' Should be used after setting comp_id
#' for numeric type only
imputeFeatures <- function (dt, imputed_feature_colnames, option_method) { #fill in missing value
  
  data <- dt$data
  
  
  if (option_method == 'across_components') {
    data[,imputed_feature_colnames] <- na.roughfix(data[,imputed_feature_colnames]) #Rough Imputation of Missing Values in random forest
    
  } else if (option_method == 'component_wise') {
    #data = ddply(data, .(comp_id), impute_comp, imputed_feature_colnames)
    
    #     for (k in unique(data$comp_id)) {
    #       tmp <- filter(data, comp_id == k)
    #       data[which(data$comp_id==k),] = impute_comp(tmp, imputed_feature_colnames)
    #     }
    
    data <- as.data.frame( data %>% group_by(comp_id) %>% mutate_each(funs(impute_fe), one_of(imputed_feature_colnames))    )
  }
  
  dt$data <- data
  return(dt)
}



#' Should be used after setting comp_id and setDatasetForModel when you should not impute in data level and should impute in dataset level
#' for numeric type only
imputeFeaturesOnDataset <- function (dt, imputed_feature_colnames, option_method) {
  
  dataset <- dt$dataset
  
  
  if (option_method == 'across_components') {
    dataset[,imputed_feature_colnames] <- na.roughfix(dataset[,imputed_feature_colnames])
    
  } else if (option_method == 'component_wise') {
    #dataset = ddply(dataset, .(comp_id), impute_comp, imputed_feature_colnames)
    
    #     for (k in unique(dataset$comp_id)) {
    #       tmp <- filter(dataset, comp_id == k)
    #       dataset[which(dataset$comp_id==k),] = impute_comp(tmp, imputed_feature_colnames)
    #     }
    
    dataset <- as.data.frame( dataset %>% group_by(comp_id) %>% mutate_each(funs(impute_fe), one_of(imputed_feature_colnames))    )
  }
  
  dt$dataset <- dataset
  return(dt)
}


cal_cumavg <- function (x) {
  x0 = x
  x0[is.na(x)] = 0
  cs <- cumsum(x0)
  cnt <- cumsum(!is.na(x))
  cnt[cnt==0]=1  ## to avoid NaN (divided by 0 below)
  avg = cs/cnt
  
  return(avg)
}

cal_cumsum <- function (x) {
  x0 = x
  x0[is.na(x)] = 0
  cs <- cumsum(x0)
  
  return(cs)
}



cal_rollsum <- function(fe_nm, x, roll_win){
  rr = sapply(1:nrow(x), function(k){ sum( x[which(x$comp_cycle > (x[k,]$comp_cycle - roll_win) & x$comp_cycle <= x[k,]$comp_cycle), fe_nm ]  , na.rm=T) } )
  
  return(rr)
}


generateCumAvgFeatures <- function (dt, feature_colnames) {
  
  data <- dt$data
  
  for (fe_nm in feature_colnames) {
    data[,paste(fe_nm, "_cumavg", sep='')] = data[,fe_nm]
  }
  
  data <- as.data.frame( data %>% group_by(comp_id) %>% mutate_each(funs(cal_cumavg), one_of(paste(feature_colnames, '_cumavg',sep='')))    )
  
  dt$data <- data
  return(dt)
  
}

generateCumSumFeatures <- function (dt, feature_colnames) {
  
  data <- dt$data
  
  for (fe_nm in feature_colnames) {
    data[,paste(fe_nm, "_cumsum", sep='')] = data[,fe_nm]
  }
  
  data <- as.data.frame( data %>% group_by(comp_id) %>% mutate_each(funs(cal_cumsum), one_of(paste(feature_colnames, '_cumsum',sep='')))    )
  
  dt$data <- data
  return(dt)
  
}

cal_rollavg <- function(k, x, feature_colnames, roll_win){
  
  x_k_comp_cycle = x[k,]$comp_cycle
  
  rr <- as.data.frame( x %>% filter(comp_cycle > (x_k_comp_cycle - roll_win) & comp_cycle <= x_k_comp_cycle)
                       %>% summarise_each(funs(m=mean(.,na.rm=T)), one_of(paste(feature_colnames, '_rollavg',sep=''))) )
  
  return(rr)
}

generateRollAvgFeatures <- function (dt, feature_colnames, roll_win) {
  
  data <- dt$data
  
  for (fe_nm in feature_colnames) {
    data[,paste(fe_nm, "_rollavg", sep='')] = data[,fe_nm]
  }
  
  for (j in unique(data$comp_id)) {
    print(j)
    x <- filter(data, comp_id == j)
    ssa = sapply(1:nrow(x), cal_rollavg, x, feature_colnames, roll_win)
    data[which(data$comp_id==j),paste(feature_colnames, '_rollavg',sep='')] = data.frame(t(ssa))
  }
  
  dt$data <- data
  return(dt)
}


cal_rollsum <- function(k, x, feature_colnames, roll_win){
  
  x_k_comp_cycle = x[k,]$comp_cycle
  
  rr <- as.data.frame( x %>% filter(comp_cycle > (x_k_comp_cycle - roll_win) & comp_cycle <= x_k_comp_cycle)
                       %>% summarise_each(funs(m=sum(.,na.rm=T)), one_of(paste(feature_colnames, '_rollsum',sep=''))) )
  
  return(rr)
}


generateRollSumFeatures <- function (dt, feature_colnames, roll_win) {
  
  
  data <- dt$data
  
  for (fe_nm in feature_colnames) {
    data[,paste(fe_nm, "_rollsum", sep='')] = data[,fe_nm]
  }
  
  for (j in unique(data$comp_id)) {
    x <- filter(data, comp_id == j)
    ssa = sapply(1:nrow(x), cal_rollsum, x, feature_colnames, roll_win)
    data[which(data$comp_id==j),paste(feature_colnames, '_rollsum',sep='')] = data.frame(t(ssa))
  }
  
  dt$data <- data
  return(dt)
  
}




setDatasetForModel <- function (dt, subset_condition='', info_colnames='', feature_colnames ='') {
  
  dataset <- dt$data  # dataset for model
  
  dt$dataset_feature_colnames = feature_colnames
  dt$dataset_info_colnames = info_colnames
  
  select_columns = union(info_colnames, feature_colnames)
  
  if ( select_columns[1] == '') {
    sel_col = names(dataset)
  } else {
    sel_col = select_columns
  }
  
  if (subset_condition[1] == '') {
    dataset = dataset[,sel_col]
    
  } else {
    dataset <- as.data.frame( dataset %>% filter(subset_condition==T) %>% group_by(comp_id) %>% mutate( target_event = set_event(eventual_state) ) )
    dataset = dataset[,sel_col]
    
  }
  
  dataset <- dataset %>% arrange(comp_id)
  
  
  dataset_endpts <- get_comp_endpts ( dataset )  # dataset %>% group_by(comp_id) %>%  slice(n())
  #dataset_endpts = plyr::ddply(dataset, .(comp_id), function(x){x[nrow(x),]})
  dataset_endpts <- dataset_endpts[,sel_col]
  
  dt$dataset_endpts <- dataset_endpts
  
  dt$dataset <- dataset
  
  return(dt)
}


# assuming that we had $failure set during previous data prep
# should run after setCompEventFromEventualState and setCompCycleAndTime
# option = 'CycleBasedLabel' or 'TimeBasedLabel' or 'EventualStateBasedLabel'
setCompLabel <- function (dt, window_size_for_label, label_option='EventualStateBasedLabel') {
  
  dt$window_size_for_label = window_size_for_label
  dt$label_option = label_option
  
  dataset <- dt$dataset
  
  if (label_option == 'CycleBasedLabel') {
    dataset <- as.data.frame( dataset
                              %>% group_by(comp_id)
                              %>% mutate( class_label = set_comp_label(data.frame(comp_id=comp_id,eventual_state=eventual_state, measure=comp_cycle), window_size_for_label)) )
  } else if (label_option == 'TimeBasedLabel') {
    dataset <- as.data.frame( dataset
                              %>% group_by(comp_id)
                              %>% mutate( class_label = set_comp_label(data.frame(eventual_state=eventual_state, measure=comp_time), window_size_for_label)) )
  } else { 'EventualStateBasedLabel'
           dataset <- as.data.frame( dataset
                                     %>% group_by(comp_id)
                                     %>% mutate( class_label = eventual_state) )
  }
  
  dt$dataset <- dataset
  return(dt)
  
}

### Feature selection-related functions
## Run after setDatasetForModel
#' @param feature_colnames the selected column names for Univariate CoxPH
#' @param time_or_cycle_option "time" or "cycle" to select the variable for baseline hazard
#' @param filepath_to_save_pdf pdf filename to save the result graphs
univariateCoxPH <- function (dt, feature_colnames, time_or_cycle_option = 'time', output_filename) {
  
  dataset <- dt$dataset # dataset for model
  sel_col = which(names(dataset) %in% feature_colnames)
  
  if (time_or_cycle_option == 'cycle') {
    start_t = dataset$comp_cycle
    stop_t = dataset$comp_cycle + dataset$this_cycle
    target_event = dataset$target_event
    
  } else {
    start_t = dataset$comp_time
    stop_t = dataset$comp_time + dataset$this_time
    target_event = dataset$target_event
    
  }
  
  
  pdf( paste(output_filename, '_univariate_coxph', ".pdf", sep='') )
  
  
  coxout = c()
  
  ix_u = c()
  for (ix in sel_col) {
    
    fe_name = names(dataset)[ix]
    #print(fe_name)
    fe = dataset[,fe_name]
    fe0 = fe
    
    if (class(fe)[1] == 'character' ||  class(fe)[1] == 'POSIXlt'   ||  class(fe)[1] == 'POSIXct'   ||  class(fe)[1] == 'POSIXt'  ||  class(fe)[1] == 'logical' || sum(!is.na(fe)) <= 1 || sum(!is.na(fe)) <= 1)
      next
    
    #print(fe_name)
    ix_u = c(ix_u, ix)
    
    median_val = median(fe0, na.rm=T)
    fe = rep(0, length(fe0))
    fe [fe0 > median_val] = 1
    
    
    sdd = data.frame(start_t, stop_t, target_event, fe)
    
    sdd=sdd[which(sdd$stop_t > sdd$start_t & sdd$start_t >= 0),]
    
    if (sum(!is.na(sdd$fe) & sdd$target_event==1) == 0)
      next
    
    #  print(fe_name)
    #  print(table(fe))
    
    titlestr0 = paste(fe_name, " : ", "feature = 0")
    titlestr1 = paste(fe_name, " : ", "feature = 1")
    
    
    coxph1 = coxph(Surv(start_t, stop_t, target_event) ~  fe , data=sdd, method='breslow') #, na.action=na.exclude)
    #  print(summary(coxph1))
    #  plot(survfit(coxph1))
    
    #   sf_cfe0 = survfit(coxph1, newdata=data.frame( fe=0))
    #   plot(sf_cfe0, main=titlestr0)
    #   sf_cfe1 = survfit(coxph1, newdata=data.frame( fe=1))
    #   plot(sf_cfe1, main=titlestr1)
    
    logtest_pvalue = summary(coxph1)$logtest['pvalue']
    coeff_pvalue = summary(coxph1)$coefficients[,"Pr(>|z|)"]
    
    #print(paste('AIC:', AIC(coxph1)))
    #print(paste('BIC:', BIC(coxph1)))
    
    # plot(survfit(coxph1,newdata=data.frame(fe=0)))
    # plot(survfit(coxph1,newdata=data.frame(fe=1)))
    
    
    cox_zph = cox.zph(coxph1) # evaluation specifying how the survival times should be transformed before the test is performed. 
    #Possible values are "km", "rank", "identity" or a function of one argument.
    zph_pvalue = cox_zph$table[1,"p"]
    
    
    
    fit_fe = survfit(Surv(start_t, stop_t, target_event) ~  fe, data=sdd)
    plot(fit_fe,  conf.int=T, main=paste(fe_name, " (survfit)"))
    
    #   n_ev_fe0 = sum(fit_fe[1]$n.event)
    #   n_ev_fe1 = sum(fit_fe[2]$n.event)
    #   print(paste("n_ev_fe0:", n_ev_fe0))
    #   print(paste("n_ev_fe1:", n_ev_fe1))
    
    #   sf_fe0 = survfit(Surv(start_t, stop_t, target_event) ~  fe, data=sdd, subset=(fe==0) )
    #   plot(sf_fe0, main=titlestr0)
    #   sf_fe1 =  survfit(Surv(start_t, stop_t, target_event) ~  fe, data=sdd, subset=(fe==1) )
    #   plot(sf_fe1, main=titlestr1)
    
    if (length(coxout) == 0) {
      coxout = c(fe_name, as.numeric(summary(coxph1)$logtest['test']), as.numeric(summary(coxph1)$logtest['pvalue']), coeff_pvalue, zph_pvalue, AIC(coxph1), BIC(coxph1))
    } else {
      coxout = rbind(coxout, c(fe_name, as.numeric(summary(coxph1)$logtest['test']), as.numeric(summary(coxph1)$logtest['pvalue']), coeff_pvalue, zph_pvalue, AIC(coxph1), BIC(coxph1) ) )
    }
    
    print(c(fe_name, as.numeric(summary(coxph1)$logtest['test']), as.numeric(summary(coxph1)$logtest['pvalue']), coeff_pvalue, zph_pvalue, AIC(coxph1), BIC(coxph1) ))
    
  }
  
  cnm = c('feature', 'likelihood ratio', 'test pvalue', 'coeff pvalue', 'zph pvalue', 'AIC', 'BIC')
  
  colnames(coxout) = cnm
  write.csv(coxout, paste( output_filename, '_univariate_coxph.csv', sep='') , row.names=F)
  
  
  dev.off()
  
  dt$dataset <- dataset
  
  return(dt)
}



#' component_wise = T or F
#' label_option = 'eventual_state' or 'class_label'
separateDistributions <- function (dt, feature_colnames, component_wise = T, class_option = 'eventual_state', output_filename) {
  
  dataset <- dt$dataset # dataset for model
  sel_col = which(names(dataset) %in% feature_colnames)
  
  out = dataset
  
  if (class_option == 'eventual_state') {
    out$t_class = out$eventual_state
  } else if (class_option == 'class_label') {
    out$t_class = out$class_label
  }
  
  out_F = out[out$t_class==F,]
  out_T = out[out$t_class==T,]
  
  # den_F <- density(out_F$egthdm,na.rm=T)
  # den_T <- density(out_T$egthdm,na.rm=T)
  # den_F <- density(out_F$ecsn,na.rm=T)
  # den_T <- density(out_T$ecsn,na.rm=T)
  # plot(den_F, col='blue')
  # lines(den_T, col='red')
  #
  # hist(out_F$egthdm, col='blue')
  # hist(out_T$egthdm, col='red', add=T)
  
  
  if (component_wise == F) {
    
    pdf( paste(output_filename, '_group_mean.pdf', sep='') )
    
    classout = sapply(names(out), function(i) {class(out[,i])[1]})
    
    sel_col = intersect(sel_col,which(classout == 'numeric' | classout == 'integer'))
    
    
    ####  GROUP-WISE Mean-value Analysis
    
    
    statout = c()
    ix_u = c()
    for (ix in sel_col) {
      
      fe_name = names(out)[ix] #paste(names(out)[ix],'_cumavg', sep='')
      if (length(which(names(out)==fe_name))==0)
        next
      
      print(fe_name)
      
      
      val_F = out_F[,fe_name]
      val_T = out_T[,fe_name]
      
      if (class(val_F)[1] == 'character' ||  class(val_F)[1] == 'POSIXlt'   ||  class(val_F)[1] == 'POSIXct'   ||  class(val_F)[1] == 'POSIXt'  ||  class(val_F)[1] == 'logical' || sum(!is.na(val_F)) <= 1 || sum(!is.na(val_T)) <= 1)
        next
      
      ix_u = c(ix_u, ix)
      mean_F = mean(val_F, na.rm=T)
      mean_T = mean(val_T, na.rm=T)
      
      
      tp =  try(t.test(val_F,val_T) )
      #paste("p", format.pval(tp$p.value))
      
      bdata = data.frame(val=c(val_F,val_T), t_class=c(out_F[,"t_class"],out_T[,"t_class"]))
      boxplot(val~t_class, data=bdata, outline=F, range=1.5, main= fe_name  )
      stats_F <- quantile(val_F, c(.05,.25,.5,.75,.95), na.rm = TRUE)
      stats_T <- quantile(val_T, c(.05,.25,.5,.75,.95), na.rm = TRUE)
      points(1:2, c(mean_F, mean_T), pch = 23, cex = 0.75, bg = "red")
      
      dprime = abs(mean(val_F,na.rm=T)-mean(val_T,na.rm=T))/sqrt(sd(val_F,na.rm=T)^2 + sd(val_T,na.rm=T)^2)
      
      #   glm_b <- glm(t_class ~ val, data=bdata, family=binomial)
      #   bdata$fprob = predict(glm_b, newdata=bdata, type='response')
      #
      #
      #   pred = prediction(bdata$fprob, bdata$t_class)
      #   #
      #   #
      #   roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
      #   plot(roc.perf)
      #   # abline(a=0, b= 1)
      #   #
      #   # acc.perf = performance(pred, measure = "acc")
      #   # plot(acc.perf)
      #   #
      #   auc.tmp <- performance(pred,"auc")
      #   auc <- as.numeric(auc.tmp@y.values)
      #
      
      auc = 0
      
      print(paste(ix, ':', fe_name, sum(!is.na(val_F)), sum(!is.na(val_T)), "p-value=", format(tp$p.value,  digit=2, nsmall=2), "mean", mean_F, mean_T ) )
      
      den_F <- density(val_F, na.rm=T)
      den_T <- density(val_T ,na.rm=T)
      
      #print(paste(ix, ':', fe_name, sum(!is.na(val_F)), sum(!is.na(val_T)), "p-value=", format(tp$p.value,  digit=2, nsmall=2) ) )
      #print(paste("mean", mean_F, mean_T))
      #print(paste("quartile (F)", stats_F))
      #print(paste("quartile (T)", stats_T))
      
      plot(den_F, col='blue', main=fe_name)
      lines(den_T, col='red')
      
      if (length(statout) == 0) {
        statout = c(fe_name, auc, dprime, tp$p.value, mean_F, mean_T, stats_F, stats_T)
      } else {
        statout = rbind(statout, c(fe_name, auc, dprime, tp$p.value, mean_F, mean_T, stats_F, stats_T) )
      }
      
      
      #h0 = hist(val_F, freq=F, col=rgb(0,0,1,0.5), main=fe_name)
      #h1 = hist(val_T, freq=F, col=rgb(1,0,0,0.5), add=T)
      
      
      h0 = hist(val_F, plot=F)
      h1 = hist(val_T, plot=F)
      h0$density=h0$counts/sum(h0$counts)*100
      h1$density=h1$counts/sum(h1$counts)*100
      
      plot(h0, freq=F, col=rgb(0,0,1,0.5), main=fe_name, ylab='Percentage', xlab=fe_name)
      lines(h1, freq=F, col=rgb(1,0,0,0.5))
      
      
    }
    
    dev.off()
    
    cnm = c('feature', 'AUC', 'd-prime', 'p-value', 'mean healthy (F)', 'mean unhealthy (T)', '5% healthy', '25% healthy', '50% healthy', '75% healthy', '95% healthy', '5% unhealthy', '25% unhealthy', '50% unhealthy', '75% unhealthy', '95% unhealthy') #, 'OM$ Opportunity', 'OM$/SV opportunity')
    
    colnames(statout) = cnm
    write.csv(statout, paste(output_filename, '_stat_group_mean.csv', sep='') , row.names=F)
    
    
  } else if (component_wise == T) {
    
    
    ####  COMPONENT-WISE Mean-value Analysis
    
    ix_u = sel_col
    
    mean_val <- function(x, ix_u){
      
      out_F = x[x$t_class==F,]
      out_T = x[x$t_class==T,]
      #ix_u = names(out)[ix_u]
      #print(out_F[,ix_u])
      mean_F = colMeans(out_F[,ix_u], na.rm=T)
      mean_T = colMeans(out_T[,ix_u], na.rm=T)
      
      return (list( mF=mean_F, mT=mean_T, c_id = x$comp_id[1] ))
      
    }
    
    mean_vals = plyr::dlply(out, .(comp_id), mean_val, ix_u)
    
    library(foreach)
    mean_vals_T0 <- foreach(i=1:length(unique(out$comp_id)), .multicombine=FALSE) %do% {
      return( mean_vals[[i]]$mT )
    }
    mean_vals_c_id <- foreach(i=1:length(unique(out$comp_id)), .multicombine=FALSE) %do% {
      return( mean_vals[[i]]$c_id )
    }
    
    mean_vals_T = plyr::ldply(mean_vals_T0)
    mean_vals_T$comp_id = as.numeric(unlist(plyr::ldply(mean_vals_c_id))) #plyr::ldply(mean_vals_c_id)
    
    mean_vals_F0 <- foreach(i=1:length(unique(out$comp_id)), .multicombine=FALSE) %do% {
      return(mean_vals[[i]]$mF)
    }
    
    mean_vals_F = plyr::ldply(mean_vals_F0)
    mean_vals_F$comp_id = as.numeric(unlist(plyr::ldply(mean_vals_c_id))) #plyr::ldply(mean_vals_c_id)
    
    pdf( paste('./', output_filename, '_individual_mean.pdf', sep='') )
    
    statout = c()
    
    
    for (ix in ix_u) {
      
      fe_name = names(out)[ix]
      val_F = mean_vals_F[,fe_name]
      val_T = mean_vals_T[,fe_name]
      
      if (class(val_F)[1] == 'character' ||  class(val_F)[1] == 'POSIXlt'  ||  class(val_F)[1] == 'POSIXct'   ||  class(val_F)[1] == 'POSIXt'   ||  class(val_F)[1] == 'logical' || sum(!is.na(val_F)) <= 1 || sum(!is.na(val_T)) <= 1 )
        next
      
      mean_F = mean(val_F, na.rm=T)
      mean_T = mean(val_T, na.rm=T)
      
      tp = t.test(val_F,val_T)
      #paste("p", format.pval(tp$p.value))
      
      
      bdata = data.frame(val=c(val_F,val_T), t_class=c(rep(F, length(val_F)),rep(T, length(val_T))))
      boxplot(val~t_class, data=bdata, outline=F, range=1.5, main= fe_name  )
      stats_F <- quantile(val_F, c(.05,.25,.5,.75,.95), na.rm = TRUE)
      stats_T <- quantile(val_T, c(.05,.25,.5,.75,.95), na.rm = TRUE)
      points(1:2, c(mean_F, mean_T), pch = 23, cex = 0.75, bg = "red")
      
      dprime = abs(mean(val_F,na.rm=T)-mean(val_T,na.rm=T))/sqrt(sd(val_F,na.rm=T)^2 + sd(val_T,na.rm=T)^2)
      
      #   glm_b <- glm(t_class ~ val, data=bdata, family=binomial)
      #   bdata$fprob = predict(glm_b, newdata=bdata, type='response')
      
      
      #   pred = prediction(bdata$fprob, bdata$t_class)
      #   #
      #   #
      #   roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
      #   plot(roc.perf)
      #   # abline(a=0, b= 1)
      #   #
      #   # acc.perf = performance(pred, measure = "acc")
      #   # plot(acc.perf)
      #   #
      #   auc.tmp <- performance(pred,"auc")
      #   auc <- as.numeric(auc.tmp@y.values)
      
      
      den_F <- density(val_F, na.rm=T)
      den_T <- density(val_T ,na.rm=T)
      
      print(paste(ix, ':', fe_name, sum(!is.na(val_F)), sum(!is.na(val_T)), "p-value=", format(tp$p.value,  digit=2, nsmall=2), "mean", mean_F, mean_T ) )
      # print(paste("mean", mean_F, mean_T))
      # print(paste("quartile (F)", stats_F))
      # print(paste("quartile (T)", stats_T))
      
      plot(den_F, col='blue', main=fe_name)
      lines(den_T, col='red')
      
      if (length(statout) == 0) {
        statout = c(fe_name, dprime, tp$p.value, mean_F, mean_T, stats_F, stats_T)
      } else {
        statout = rbind(statout, c(fe_name, dprime, tp$p.value, mean_F, mean_T, stats_F, stats_T) )
      }
      
      #h0 = hist(val_F, freq=F, col=rgb(0,0,1,0.5), main=fe_name)
      #h1 = hist(val_T, freq=F, col=rgb(1,0,0,0.5), add=T)
      
      
      h0 = hist(val_F, plot=F)
      h1 = hist(val_T, plot=F)
      h0$density=h0$counts/sum(h0$counts)*100
      h1$density=h1$counts/sum(h1$counts)*100
      
      plot(h0, freq=F, col=rgb(0,0,1,0.5), main=fe_name, ylab='Percentage', xlab=fe_name)
      lines(h1, freq=F, col=rgb(1,0,0,0.5))
      
      
    }
    
    cnm = c('feature', 'dprime', 'p-value', 'mean healthy (F)', 'mean unhealthy (T)', '5% healthy', '25% healthy', '50% healthy', '75% healthy', '95% healthy', '5% unhealthy', '25% unhealthy', '50% unhealthy', '75% unhealthy', '95% unhealthy') #, 'OM$ Opportunity', 'OM$/SV opportunity')
    
    colnames(statout) = cnm
    write.csv(statout, paste(output_filename, '_stat_individual_mean.csv', sep='') , row.names=F)
    
    dev.off()
    
  }
  
  dt$dataset <- dataset
  
  return(dt)
  
  
}





## the part for CV_FLAG == F  in the template code
# work on dataset_endpts
featureSelectionTestUsingRF <- function (dt, option_endpts = T, output_filename='') {
  
  
  feature_colnames = dt$dataset_feature_colnames
  
  
  pdf( paste('./', output_filename, '_feature_selection_using_RF.pdf', sep='') )
  
  if (option_endpts == T) {
    out <- dt$dataset_endpts # endpts dataset for model
    sel_col = which(names(out) %in% feature_colnames)
    out$t_class = out$eventual_state
    
  } else {
    
    out <- dt$dataset # dataset for model
    sel_col = which(names(out) %in% feature_colnames)
    out$t_class = out$class_label
    
    out_train1 = out[which(out$class_label == 1 ), ]
    out_train0 = out[which(out$class_label == 0 ), ]
    
    out_train1 = out_train1[seq(1,nrow(out_train1),10),]
    out_train0 = out_train0[seq(1,nrow(out_train0),100),]
    
    out = rbind(out_train0, out_train1)
    
  }
  trainset = out[, c("t_class", feature_colnames)]
  
  # ensure the results are repeatable
  set.seed(7)
  # load the library
  library(mlbench)
  library(caret)
  library(DMwR)
  
  # calculate correlation matrix
  correlationMatrix <- cor(trainset[,-1])
  
  correlationMatrix[is.na(correlationMatrix)] = 0
  
  
  # summarize the correlation matrix
  #print(correlationMatrix)
  # find attributes that are highly corrected (ideally >0.75)
  highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
  # print indexes of highly correlated attributes
  #print(highlyCorrelated)
  
  if (length(highlyCorrelated) != 0) {
    trainset = trainset[,-which(names(trainset) %in% rownames(correlationMatrix)[highlyCorrelated])]
  }
  
  trainset$t_class = as.factor(trainset$t_class)
  table(trainset$t_class)
  
  trainset0 <- trainset
  
  #train_smote <- SMOTE(t_class ~ ., data=trainset, perc.over = 200, perc.under=200, k=3)
  
  #   > table(train_smote$t_class)
  #
  #   0   1
  #   124  93
  
  
  #trainset <- train_smote
  #tr_imputed = rfImpute(t_class ~ ., data = trainset[seq(1,nrow(trainset),5),])
  trainset$fprob <- NULL
  
  rfm1 <- randomForest(t_class ~ ., data = trainset, importance=TRUE, mtry = 4, proximity=TRUE, keep.forest=TRUE, na.action=na.roughfix) #, classwt=c(10,1) )
  varImpPlot(rfm1,type=1)
  MDSplot(rfm1, trainset$t_class)
  
  
  library(doParallel)
  cl <- makeCluster( detectCores()-1 )
  registerDoParallel(cl) # register foreach backend
  
  
  rfGrid <-  expand.grid(mtry = c(2, 4, 7))
  
  # ensure results are repeatable
  set.seed(7)
  # prepare training scheme
  control <- trainControl(method="repeatedcv", number=10, repeats=3, allowParallel = T)
  # train the model
  rfmodel <- train(t_class ~ ., data = trainset, method="rf", trControl=control, tuneGrid=rfGrid )
  # estimate variable importance
  importance <- varImp(rfmodel, scale=F)
  # summarize importance
  print(importance)
  # plot importance
  plot(importance)
  #
  vi <- varImp(rfmodel, scale=F)$importance
  vi$max <- apply(vi, 1, max)
  #row.names(vi[order(-vi$max),][1:8,])
  # [1] "on_time"                      "n2_0_mean"                    "apu_n1_0_mean"                "gross_weight_lbs_0_mean"
  # [5] "egt_average_0_mean"           "cr_sloatl_smoothed"           "selected_altitude_mcp_0_mean" "wind_direction_true_0_mean"
  
  stopCluster(cl)
  
  # without SMOTE
  # [1] "wind_direction_true_0_mean" "gross_weight_lbs_0_mean"    "to_iaie"                    "wind_speed_0_mean"
  # [5] "wind_speed_1_mean"          "apu_n1_0_mean"              "n2_0_mean"                  "to_zvb1f_d"
  # [9] "on_time"                    "cr_agw"                     "to_iaie1"                   "egt_average_0_mean"
  # [13] "to_ibp2"                    "to_ibe2"
  
  
  # > rfmodel$bestTune
  # mtry
  # 2    4
  
  # > rfmodel$finalModel
  #
  # Call:
  #   randomForest(x = x, y = y, mtry = param$mtry)
  # Type of random forest: classification
  # Number of trees: 500
  # No. of variables tried at each split: 4
  #
  # OOB estimate of  error rate: 1.08%
  # Confusion matrix:
  #   0   1 class.error
  # 0 372   0  0.00000000
  # 1   7 272  0.02508961
  
  
  #   control <- rfeControl(functions=rfFuncs, method="cv", number=10)
  #   # run the RFE algorithm
  #   results <- rfe(trainset[,-1], trainset[,1], sizes=2^(2:4), rfeControl=control)
  #   # summarize the results
  #   print(results)
  #   # list the chosen features
  #   predictors(results)
  #   # plot the results
  #   plot(results, type=c("g", "o"))
  #
  
  
  
  trainset$fprob = 0
  
  trainset$fprob = predict(rfm1, newdata=trainset, type='prob')[,2]
  # #partialPlot(rfm1, trainset, x.var = "egthdm")
  # importance(rfm1, type=1)
  varImpPlot(rfm1,type=1)
  #
  
  library(ROCR)
  trainset <- trainset[order(trainset$fprob),]
  colX = c("blue","red")[as.factor(trainset$t_class)]
  plot(seq(1:nrow(trainset)),trainset$fprob,type="p",col=colX,pch=19,cex.axis = 1.2,cex.lab=1.5,main="A/I Failure Probability",xlab="Index",ylab="Probability")
  
  
  ROCRpredTest = prediction(trainset$fprob, trainset$t_class)
  ROCRperf <- performance(ROCRpredTest,"tpr","fpr")
  plot(ROCRperf,colorize = TRUE,print.cutoffs.at = seq(0,1,0.1),text.adj=c(-0.2,1.7),main="ROC Curve",cex.axis = 1.2)
  
  ####
  pred.obj <- prediction (trainset$fprob, trainset$t_class)
  RP.perf <- performance(pred.obj, "rec","prec");
  plot (RP.perf);
  
  ROC.perf <- performance(pred.obj, "fpr","tpr");
  plot (ROC.perf);
  
  plot  (RP.perf@alpha.values[[1]],RP.perf@x.values[[1]]);
  lines (RP.perf@alpha.values[[1]],RP.perf@y.values[[1]]);
  lines (ROC.perf@alpha.values[[1]],ROC.perf@x.values[[1]]);
  
  dev.off()
  
  
  dt$ranked_features_rf = row.names(vi[order(-vi$max),])
  dt$var_imp = vi[order(-vi$max),]
  
  return(dt)
  
}





get_bhaz <- function( meterval, mhaz ) {
  #plot(mhaz$est.grid, mhaz$haz.est)
  
  idx1 = max(which(mhaz$est.grid <= meterval))
  idx2 = min(which(mhaz$est.grid >= meterval))
  
  if (idx1 == -Inf) {
    val = 0
  }
  else if (idx2 == Inf)  {
    val = mhaz$haz.est[length(mhaz$haz.est)]
  }
  else if (idx2 <= idx1) {
    val = mhaz$haz.est[idx1]
  } else {
    fl1 = mhaz$haz.est[idx1]
    fl2 = mhaz$haz.est[idx2]
    
    val = fl1 + (fl2-fl1)*(meterval-mhaz$est.grid[idx1])/(mhaz$est.grid[idx2]-mhaz$est.grid[idx1])
  }
  
  return(val)
  
}

get_comp_endpts <- function (dataset) {
  dataset_endpts <- dataset  %>% group_by(comp_id) %>%  slice(n())
  #res_end_pts = ddply(dataset, .(comp_id), function(x){x[nrow(x),]})
  
  return(dataset_endpts)
}


get_cum_failprob <- function (x, alpha_coef=1) {
  
  #alpha_coef = 1 #0.0001
  h = x$coxhaz
  H = c(h[1], h[1:(length(h)-1)]+h[-1])
  x$cum_failprob = 1-exp(-alpha_coef*cumsum(0.5*H*x$t_this_time))
  
  h = x$coxhaz_sefitH
  H = c(h[1],h[1:(length(h)-1)]+h[-1])
  x$cum_failprob_sefitH = 1-exp(-alpha_coef*cumsum(0.5*H*x$t_this_time))
  h = x$coxhaz_sefitL
  H = c(h[1],h[1:(length(h)-1)]+h[-1])
  x$cum_failprob_sefitL = 1-exp(-alpha_coef*cumsum(0.5*H*x$t_this_time))
  
  h = x$coxhaz_seH
  H = c(h[1],h[1:(length(h)-1)]+h[-1])
  x$cum_failprob_seH = 1-exp(-alpha_coef*cumsum(0.5*H*x$t_this_time))
  
  h = x$coxhaz_seL
  H = c(h[1],h[1:(length(h)-1)]+h[-1])
  x$cum_failprob_seL = 1-exp(-alpha_coef*cumsum(0.5*H*x$t_this_time))
  
  return(x)
}

get_sum_fprob <- function (x) {
  
  x$sum_fprob = cumsum(x$fprob)
  
  return(x)
}


get_max_fprob <- function (x) {
  
  x$max_fprob = cummax(x$fprob)
  
  return(x)
}



get_sum_fprob_r <- function (x) {
  
  x$sum_fprob_r = cumsum(x$fprob_r)
  
  return(x)
}





#' option_label = 'class_label' or 'eventual_state'
## run after setDatasetForModel
crossValidatedModels <- function (dt, num_folds, option_label='class_label', option_cox=T, time_or_cycle_option = 'cycle', option_classification='') {
  
  set.seed(12345)
  
  dataset = dt$dataset  
  
  feature_colnames = dt$dataset_feature_colnames
  
  comp_set = unique(dataset$comp_id)
  num_comps = length(comp_set)
  fold = rep(1:num_folds,length.out=num_comps)
  ids = sample(comp_set,num_comps)
  
  if (option_label == 'class_label') {
    dataset$t_class = dataset$class_label
  } else if (option_label == 'eventual_state') {
    dataset$t_class = dataset$eventual_state
  }
  
  
  if (time_or_cycle_option == 'cycle') {
    dataset$t_time = dataset$comp_cycle
    dataset$t_this_time = dataset$this_cycle
  } else {
    dataset$t_time = dataset$comp_time
    dataset$t_this_time = dataset$this_time
  }
  
  
  sel_col = c(feature_colnames, "t_class")
  
  cross_validate <- function (cv_id) {
    #for (cv_id in 1:num_folds) {
    
    set.seed(1234)
    
    tr = ids[which(fold==cv_id)]
    test_ids = which(dataset$comp_id %in% tr)
    
    trainset = dataset[-test_ids, sel_col]  # used for classification below, only including feature_colnames with t_class
    testset = dataset[test_ids, sel_col]    # used for classification below, only including feature_colnames with t_class
    
    trainset0 = dataset[-test_ids,]   # used for CoxPH 
    testset0 = dataset[test_ids,]     # used for CoxPH 
    
    
    #trainset[is.na(trainset)]=0
    #testset[is.na(testset)]=0
    
    
    #     ### imputation ###   only used when imputation hasn't been done before this function call
    #         ix_numeric=sapply(names(trainset), function(nm){class(trainset[,nm])[1] == "numeric" | class(trainset[,nm])[1] == "integer"  })
    # 
    #         trainset1 = trainset
    #         trainset1[,ix_numeric] <- na.roughfix(trainset[,ix_numeric])
    # 
    #         testset1 = testset
    #         testset1[,ix_numeric] <- na.roughfix(testset[,ix_numeric])
    # 
    #         trainset = trainset1
    #         testset = testset1
    #     ###################
    
    
    #out_train = trainset[which(!is.na(trainset[,"status"])),ix_u,which(names(trainset)=='status'))][seq(1,nrow(out),100),]
    #out_train = trainset[which(!is.na(trainset[,"status"])), ][seq(1,nrow(trainset),10),]
    #out_train = trainset[which(!is.na(trainset[,"label"])), ][seq(1,nrow(trainset),10),]
    
    #   out_train1 = trainset[which(trainset[,"label"] == 1 ), ]
    #   out_train0 = trainset[which(trainset[,"label"] == 0 ), ]
    #   out_train0 = out_train0[seq(1,nrow(out_train0),20),]
    
    out_train1 = trainset[which(trainset$t_class == 1 ), ]
    out_train0 = trainset[which(trainset$t_class == 0 ), ]
    
    if (option_label == 'class_label') {
      out_train1 = out_train1[seq(1,nrow(out_train1),10),]
      out_train0 = out_train0[seq(1,nrow(out_train0),100),]
      
    } else {
      out_train1 = out_train1[seq(1,nrow(out_train1),100),]
      out_train0 = out_train0[seq(1,nrow(out_train0),100),]
      
    }
    
    out_train = rbind(out_train0, out_train1)
    #out_train_smote <- SMOTE(label ~ ., data=out_train, perc.over = 1000, perc.under=70)
    
    
    out_train$t_class = as.factor(out_train$t_class)
    table(out_train$t_class)
    
    rfm <- randomForest(t_class ~ ., data = out_train, importance=TRUE, mtry = 4) #, na.action=na.omit) #, classwt=c(1,5) )
    #out_train$label = as.factor(out_train$label)
    #rfm <- randomForest(label ~ ., data = out_train, importance=TRUE, mtry = 4) #, classwt=c(1,5) )
    
    
    ##partialPlot(rfm, out_train, x.var = "egthdm")
    #importance(rfm, type=1)
    #varImpPlot(rfm,type=1)
    
    # library(caret)
    # forest.model1 <- train(status ~ ., data = out_train, importance=TRUE)
    # plot(varImp(forest.model1))
    # varImp(forest.model1)
    
    trainset0$fprob = predict(rfm, newdata=trainset, type='prob')[,2]
    testset0$fprob = predict(rfm, newdata=testset, type='prob')[,2]
    
    
    
    #     trainset0$sum_fprob = rep(0, nrow(trainset0))
    #     trainset0 = ddply(trainset0, .(comp_id), get_sum_fprob)
    # 
    # 
    #     testset0$sum_fprob = rep(0, nrow(testset0))
    #     testset0 = ddply(testset0, .(comp_id), get_sum_fprob)
    
    
    
    #    res_end_pts = ddply(trainset0, .(comp_id), function(x){x[nrow(x),]})
    #
    #    res_colorcodes = rep('',nrow(res_end_pts))
    #    res_colorcodes[which(paste(res_end_pts$status)==0)]='blue'  # healthy
    #    res_colorcodes[which(paste(res_end_pts$status)==1)]='red'   # removed
    #
    #    print( qplot( comp_cycle, sum_fprob, data=trainset0, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp_cycle, sum_fprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #           +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #    )
    #
    #
    #     testset0$sum_fprob = rep(0, nrow(testset0))
    #     testset0 = ddply(testset0, .(comp_id), get_sum_fprob)
    #
    #    res_end_pts = ddply(testset0, .(comp_id), function(x){x[nrow(x),]})
    #
    #    res_colorcodes = rep('',nrow(res_end_pts))
    #    res_colorcodes[which(paste(res_end_pts$status)==0)]='blue'  # healthy
    #    res_colorcodes[which(paste(res_end_pts$status)==1)]='red'   # removed
    #
    #    print( qplot( comp_cycle, sum_fprob, data=testset0, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp_cycle, sum_fprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #           +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #    )
    #
    #    print( qplot( comp_cycle, fprob, data=testset0, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp_cycle, fprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #           +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #    )
    #
    
    #   wt = table(out_train$label)
    #   wt['0'] = 1
    #   wt['1'] = 40
    #
    #   ksvm0 = ksvm(label ~ ., data=out_train, type='C-svc', kernel='rbfdot', prob.model=TRUE, C=10,  kpar="automatic", class.weights=wt, shrinking=TRUE)
    #   #
    #   trainset0$fprob = predict(ksvm0, newdata = trainset, type = "probabilities")[,2]
    #   testset0$fprob = predict(ksvm0, newdata = testset, type = "probabilities")[,2]
    #
    #   out_train$fprob = predict(ksvm0, newdata = out_train, type = "probabilities")[,2]
    
    
    #
    #     glm1 <- glm(status ~ sum_fprob + comp_cycle, data=trainset0, family=binomial)
    #     trainset0$fprob_r = predict(glm1, newdata=trainset0, type='response')
    #     testset0$fprob_r = predict(glm1, newdata=testset0, type='response')
    
    #
    # trainset0$sum_fprob = rep(0, nrow(trainset0))
    # trainset0 = ddply(trainset0, .(comp_id), get_sum_fprob)
    #
    #
    # testset0$sum_fprob = rep(0, nrow(testset0))
    # testset0 = ddply(testset0, .(comp_id), get_sum_fprob)
    #
    
    
    if (option_cox == T) {
      
      if (time_or_cycle_option == 'cycle') {
        trainset0$t_time = trainset0$comp_cycle
        testset0$t_time = testset0$comp_cycle
        
        start_t = trainset0$comp_cycle
        stop_t = trainset0$comp_cycle + trainset0$this_cycle
        target_event = trainset0$target_event
        
      } else {
        trainset0$t_time = trainset0$comp_time
        testset0$t_time = testset0$comp_time
        
        start_t = trainset0$comp_time
        stop_t = trainset0$comp_time + trainset0$this_time
        target_event = trainset0$target_event
        
      }
      
      
      fprob = trainset0$fprob  # covariate
      sdd = data.frame(start_t, stop_t, target_event, fprob)
      
      sdd = sdd[which(sdd$stop_t > sdd$start_t & sdd$start_t >= 0),]
      
      coxph1 = coxph(Surv(start_t, stop_t, target_event) ~  fprob , data=sdd, method='breslow') #, na.action=na.exclude)
      
      #  print(summary(coxph1))
      #  plot(survfit(coxph1))
      
      
      
      #  coef(summary(coxph1))
      
      newdata = data.frame(fprob=testset0$fprob) 
      pred = predict(coxph1, newdata=newdata, type="risk" , reference='sample', se.fit=TRUE)
      ## reference='sample' is needed to compute  exp(coef(coxph1)*(newdata$fe5- overall_sample_mean)) where overall_sample_mean = mean(newdata$fe5)
      ## otherwise, default is reference='strata' that uses strata mean for each strata
      
      testset0$coxrisk = pred$fit
      testset0$coxrisk_sefit = pred$se.fit
      
      fe_names = rownames(coef(summary(coxph1)))
      
      
      if (length(fe_names)==1) {
        testset0$coxrisk_se = exp( 1.96*coef(summary(coxph1))[,'se(coef)'] * ( newdata[,fe_names]- mean(newdata[,fe_names]) ) )
      } else {
        testset0$coxrisk_se = t(exp( 1.96*coef(summary(coxph1))[,'se(coef)'] %*% t( newdata[,fe_names]- t(matrix(rep(colMeans(newdata[,fe_names]),nrow(newdata)),ncol=nrow(newdata))) )) )
      }
      
      
      
      trainset_endpts =  ddply(trainset0, .(comp_id), function(x){x[nrow(x),]})
      
      library(muhaz)
      
      mhaz = with(trainset_endpts, muhaz(t_time, target_event))
      
      
      #plot(mhaz$est.grid, mhaz$haz.est)
      
      testset0$bhaz = sapply(testset0$t_time, get_bhaz, mhaz)
      #trainset0$bhaz = sapply(trainset0$t_time, get_bhaz, mhaz)
      
      
      
      testset0$coxhaz= testset0$bhaz * testset0$coxrisk
      #trainset0$coxhaz= trainset0$bhaz * trainset0$coxrisk
      
      
      testset0$coxhaz_seH = testset0$bhaz * testset0$coxrisk * testset0$coxrisk_se
      testset0$coxhaz_seL = testset0$bhaz * testset0$coxrisk / testset0$coxrisk_se
      
      testset0$coxhaz_sefitH = testset0$bhaz * (testset0$coxrisk + 1.96*testset0$coxrisk_sefit)
      testset0$coxhaz_sefitL = testset0$bhaz * (testset0$coxrisk - 1.96*testset0$coxrisk_sefit)
      
      
      dataset[test_ids,]$coxrisk <<- testset0$coxrisk
      dataset[test_ids,]$bhaz <<- testset0$bhaz
      dataset[test_ids,]$coxhaz <<- testset0$coxhaz
      
      dataset[test_ids,]$coxrisk_se <<- testset0$coxrisk_se
      dataset[test_ids,]$coxrisk_sefit <<- testset0$coxrisk_sefit
      
    }
    
    dataset[test_ids,]$fprob <<- testset0$fprob
    #dataset[test_ids,]$fprob_r <<- testset0$fprob_r
    
    
    # store the results in a list for return
    list(testset0$fprob, testset0$coxrisk, testset0$bhaz, testset0$coxhaz, testset0$coxrisk_se, testset0$coxrisk_sefit) #,  testset0$fprob_r)
    
  }
  
  for (cv_id in 1:num_folds) {
    #print(cv_id)
    cross_validate(cv_id)
  }
  
  
  #print(system.time ( ( resR = lapply (1:nFolds,  cross_validate) ) ) )
  
  
  dt$dataset <- dataset
  
  return(dt)
  
  
}



set_vals_init_samples <- function (x, coxvars, nsamples=100) {
  
  if (nsamples != 0) {
    for (v in coxvars) {
      if (v=='t_time' || v=='t_class')
        next
      
      nn = ifelse(nrow(x) < nsamples, nrow(x), nsamples) # nrow(x) #
      x[1:nn,v] = x[nn,v]
      
    }
  }
  
  return(x)
}


# COXPH_DIRECT = F     # with or without RF (F = with RF)
# END_POINT_MODEL = F  # for randomforest   : F = cycle-based model
# COX_TIMEDEP = F  # cox independent
# USE_CYCLE_SAMPLES = T


# we don't do classification ( don't need END_POINT_MODEL tag)
# COXPH_DIRECT == T & USE_CYCLE_SAMPLES = T
# option_timedep == COX_TIMEDEP
crossValidatedCoxPHModels <- function (dt, num_folds, time_or_cycle_option = 'cycle', option_timedep = T, num_init_samples = 100) {
  
  set.seed(12345)
  
  dataset = dt$dataset  
  
  
  fe_col = dt$dataset_feature_colnames
  
  comp_set = unique(dataset$comp_id)
  num_comps = length(comp_set)
  fold = rep(1:num_folds,length.out=num_comps)
  ids = sample(comp_set,num_comps)
  
  
  if (time_or_cycle_option == 'cycle') {
    dataset$t_time = dataset$comp_cycle
    dataset$t_this_time = dataset$this_cycle
    
    #fe_col = setdiff(fe_col, 'comp_cycle')
  } else {
    dataset$t_time = dataset$comp_time
    dataset$t_this_time = dataset$this_time
    
    #fe_col = setdiff(fe_col, 'comp_time')
  }
  
  
  
  cross_validate <- function (cv_id) {
    #for (cv_id in 1:num_folds) {
    
    set.seed(1234)
    
    tr = ids[which(fold==cv_id)]
    test_ids = which(dataset$comp_id %in% tr)
    
    trainset = dataset[-test_ids,]
    testset = dataset[test_ids,]
    
    trainset_endpts =  ddply(trainset, .(comp_id), function(x){x[nrow(x),]})
    testset_endpts = ddply(testset, .(comp_id), function(x){x[nrow(x),]})
    
    
    
    if (option_timedep == F) {
      
      sel_col = c(fe_col, "target_event", "t_time")
      
      sdd = trainset_endpts[, sel_col] 
      
      coxph1 = coxph(Surv(t_time,  target_event==1 ) ~ . , data=sdd)
      print(summary(coxph1))
      
      ## without imputed data -- will not work
      #                                             coef   exp(coef)    se(coef)      z Pr(>|z|)
      #         duty_cycle_cumavg                3.265e+00   2.618e+01   4.394e+00  0.743 0.457490
      #         gross_weight_lbs_0_mean_cumavg  -3.790e-05   1.000e+00   1.027e-05 -3.690 0.000225 ***
      #         to_iaie_cumavg                   6.153e+00   4.703e+02   3.050e+00  2.018 0.043616 *
      #         to_delfn_cumavg                 -5.269e-01   5.904e-01   2.023e-01 -2.605 0.009182 **
      #         to_zxm_cumavg                    2.401e+02  1.916e+104   6.318e+01  3.801 0.000144 ***
      #
      
      ## imputed data:
      # n= 147, number of events= 30
      #
      #                                     coef   exp(coef)    se(coef)      z Pr(>|z|)
      # duty_cycle_cumavg                3.542e+01   2.421e+15   2.657e+01  1.333 0.182446
      # gross_weight_lbs_0_mean_cumavg  -2.120e-04   9.998e-01   8.282e-05 -2.560 0.010456 *
      # to_iaie_cumavg                   6.846e+00   9.400e+02   5.157e+00  1.327 0.184369
      # to_delfn_cumavg                 -5.895e-01   5.546e-01   1.840e-01 -3.204 0.001355 **
      # to_zxm_cumavg                    2.882e+02  1.467e+125   7.755e+01  3.716 0.000202 ***
      
      
    } else {
      
      #         set_comp_fe_sum <- function (x, fe_list) {
      #           x0 = x
      #
      #           for (fe_nm in fe_list){
      #
      #             x0[is.na(x[,fe_nm]),fe_nm] = 0
      #             cs = cumsum(x0[,fe_nm])
      #            #x[,paste(fe_nm, "_cumsum", sep='')] = cs
      #            x[,fe_nm] = cs
      #
      #           }
      #            return(x)
      #         }
      #
      #         train0 <- ddply(train0, .(comp_id), set_comp_fe_sum, c("duty_cycle_cumavg", "gross_weight_lbs_0_mean_cumavg", "to_iaie_cumavg", "to_delfn_cumavg", "to_zxm_cumavg"))
      #         test0 <- ddply(test0, .(comp_id), set_comp_fe_sum, c("duty_cycle_cumavg", "gross_weight_lbs_0_mean_cumavg", "to_iaie_cumavg", "to_delfn_cumavg", "to_zxm_cumavg"))
      
      start_t = trainset$t_time
      stop_t = trainset$t_time + trainset$t_this_time
      failure_t = trainset$target_event
      
      #         train0$duty_cycle_cumavg = train0$duty_cycle_cumavg_cumsum
      #         train0$gross_weight_lbs_0_mean_cumavg = train0$gross_weight_lbs_0_mean_cumavg_cumsum
      #         train0$to_iaie_cumavg = train0$to_iaie_cumavg_cumsum
      #         train0$to_delfn_cumavg = train0$to_delfn_cumavg_cumsum
      #         train0$to_zxm_cumavg = train0$to_zxm_cumavg_cumsum
      
      #         duty_cycle_cumavg = train0$duty_cycle_cumavg_cumsum
      #         gross_weight_lbs_0_mean_cumavg = train0$gross_weight_lbs_0_mean_cumavg_cumsum
      #         to_iaie_cumavg = train0$to_iaie_cumavg_cumsum
      #         to_delfn_cumavg = train0$to_delfn_cumavg_cumsum
      #         to_zxm_cumavg = train0$to_zxm_cumavg_cumsum
      
      
      #         test0$duty_cycle_cumavg = test0$duty_cycle_cumavg_cumsum
      #         test0$gross_weight_lbs_0_mean_cumavg = test0$gross_weight_lbs_0_mean_cumavg_cumsum
      #         test0$to_iaie_cumavg = test0$to_iaie_cumavg_cumsum
      #         test0$to_delfn_cumavg = test0$to_delfn_cumavg_cumsum
      #         test0$to_zxm_cumavg = test0$to_zxm_cumavg_cumsum
      
      #     duty_cycle_cumavg = train0$duty_cycle_cumavg
      #     gross_weight_lbs_0_mean_cumavg = train0$gross_weight_lbs_0_mean_cumavg
      #     to_iaie_cumavg = train0$to_iaie_cumavg
      #     to_delfn_cumavg = train0$to_delfn_cumavg
      #     to_zxm_cumavg = train0$to_zxm_cumavg
      
      #fe_col = c( "gross_weight_lbs_0_mean_cumavg",  "to_delfn_cumavg")
      covars = trainset[,fe_col]
      
      # sdd = data.frame(start, stop, failure,   duty_cycle_cumavg, gross_weight_lbs_0_mean_cumavg, to_iaie_cumavg, to_delfn_cumavg, to_zxm_cumavg)
      #sdd = data.frame(start, stop, failure,   duty_cycle_cumavg, gross_weight_lbs_0_mean_cumavg,  to_delfn_cumavg, to_zxm_cumavg)
      #sdd = data.frame(start, stop, failure,  gross_weight_lbs_0_mean_cumavg,  to_delfn_cumavg)
      sdd = data.frame(start_t, stop_t, failure_t, covars)
      
      #sdd = data.frame(start,stop, failure, fe1, fe2)
      
      sdd=sdd[which(sdd$stop_t > sdd$start_t & sdd$start_t >=0),]
      
      coxph1 = coxph(Surv(start_t, stop_t, failure_t) ~  . , data=sdd, method='breslow') #, na.action=na.exclude)
      summary(coxph1)
      
    }
    
    
    sddtest = testset[,c("comp_id", fe_col)]
    sddtest = ddply(sddtest, .(comp_id), set_vals_init_samples, fe_col, num_init_samples)
    sddtest = sddtest[,-which(names(sddtest) == "comp_id")]
    
    #sddtest = testset[,fe_col]
    pred = predict(coxph1, newdata=sddtest, type="risk" , reference='sample', se.fit=TRUE)
    
    
    testset$coxrisk = pred$fit
    testset$coxrisk_sefit = pred$se.fit
    
    
    
    sddtrain = trainset[,c("comp_id", fe_col)]
    sddtrain = ddply(sddtrain, .(comp_id), set_vals_init_samples, fe_col, num_init_samples)
    sddtrain = sddtrain[,-which(names(sddtrain) == "comp_id")]
    
    pred_train = predict(coxph1, newdata=sddtrain, type="risk" , reference='sample', se.fit=TRUE)
    
    
    trainset$coxrisk = pred_train$fit
    trainset$coxrisk_sefit = pred_train$se.fit
    
    
    
    
    
    fe_names = rownames(coef(summary(coxph1)))
    
    
    #     if (length(fe_names)==1) {
    #       testset0$coxrisk_se = exp( 1.96*coef(summary(coxph1))[,'se(coef)'] * ( newdata[,fe_names]- mean(newdata[,fe_names]) ) )
    #     } else {
    #       testset0$coxrisk_se = t(exp( 1.96*coef(summary(coxph1))[,'se(coef)'] %*% t( newdata[,fe_names]- t(matrix(rep(colMeans(newdata[,fe_names]),nrow(newdata)),ncol=nrow(newdata))) )) )
    #     }
    #
    
    
    
    mhaz = with(trainset_endpts[, c("t_time","eventual_state")], muhaz(t_time, eventual_state==1))
    
    
    #plot(mhaz$est.grid, mhaz$haz.est)
    
    testset$bhaz = sapply(testset$t_time, get_bhaz, mhaz)
    trainset$bhaz = sapply(trainset$t_time, get_bhaz, mhaz)
    
    
    testset$coxhaz= testset$bhaz * testset$coxrisk
    trainset$coxhaz= trainset$bhaz * trainset$coxrisk
    
    
    #     trainset$cum_failprob = rep(0, nrow(trainset))
    #     trainset = ddply(trainset, .(comp_id), get_cum_failprob, alpha_coef)
    
    
    #     res_end_pts = ddply(trainset, .(comp_id), function(x){x[nrow(x),]})
    # 
    #     res_colorcodes = rep('',nrow(res_end_pts))
    #     res_colorcodes[which(paste(res_end_pts$eventual_state)==0)]='blue'  # healthy
    #     res_colorcodes[which(paste(res_end_pts$eventual_state)==1)]='red'   # removed
    # 
    # 
    #     print( qplot( t_time, coxrisk, data=trainset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, coxrisk), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #     )
    # 
    # 
    #     print( qplot( t_time, coxhaz, data=trainset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, coxhaz), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #     )
    # 
    # 
    #     print( qplot( t_time, cum_failprob, data=trainset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, cum_failprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #     )
    # 
    
    
    
    
    #     testset$cum_failprob = rep(0, nrow(testset))
    #     testset = ddply(testset, .(comp_id), get_cum_failprob, alpha_coef)
    # 
    # 
    #     res_end_pts = ddply(testset, .(comp_id), function(x){x[nrow(x),]})
    # 
    #     res_colorcodes = rep('',nrow(res_end_pts))
    #     res_colorcodes[which(paste(res_end_pts$eventual_state)==0)]='blue'  # healthy
    #     res_colorcodes[which(paste(res_end_pts$eventual_state)==1)]='red'   # removed
    # 
    #   
    #     print( qplot( t_time, coxrisk, data=testset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, coxrisk), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #     )
    # 
    #     print( qplot( t_time, coxhaz, data=testset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, coxhaz), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #     )
    # 
    #     print( qplot( t_time, cum_failprob, data=testset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, cum_failprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #     )
    
    
    dataset[test_ids,]$bhaz <<- testset$bhaz
    dataset[test_ids,]$coxrisk <<- testset$coxrisk
    dataset[test_ids,]$coxhaz <<- testset$coxhaz
    dataset[test_ids,]$fprob <<- testset$coxhaz  # here, set fprob = coxhaz
    
    dataset[test_ids,]$coxrisk_se <<- testset$coxrisk_se
    dataset[test_ids,]$coxrisk_sefit <<- testset$coxrisk_sefit
    
    
    
    # store the results in a list for return
    list( testset$coxrisk, testset$bhaz, testset$coxhaz, testset$coxrisk_se, testset$coxrisk_sefit, testset$fprob )
    
    
  }
  
  
  for (cv_id in 1:num_folds) {
    #print(cv_id)
    cross_validate(cv_id)
  }
  
  #print(system.time ( ( resR = lapply (1:nFolds,  cross_validate) ) ) )
  
  
  dt$dataset <- dataset
  
  return(dt)
  
  
}






# COXPH_DIRECT == F & USE_CYCLE_SAMPLES = T
# option_use_endpoints_for_classification == END_POINT_MODEL

crossValidatedClassificationModels <- function (dt, num_folds, option_label='class_label', time_or_cycle_option = 'cycle', option_use_endpoints_for_classification = F) {
  
  set.seed(12345)
  
  dataset = dt$dataset  
  
  fe_col = dt$dataset_feature_colnames
  
  comp_set = unique(dataset$comp_id)
  num_comps = length(comp_set)
  fold = rep(1:num_folds,length.out=num_comps)
  ids = sample(comp_set,num_comps)
  
  if (option_label == 'class_label') {
    dataset$t_class = dataset$class_label
  } else if (option_label == 'eventual_state') {
    dataset$t_class = dataset$eventual_state
  }
  
  
  if (time_or_cycle_option == 'cycle') {
    dataset$t_time = dataset$comp_cycle
    dataset$t_this_time = dataset$this_cycle
  } else {
    dataset$t_time = dataset$comp_time
    dataset$t_this_time = dataset$this_time
  }
  
  
  #sel_col = c(which(names(dataset) %in% feature_colnames), which(names(dataset)=="t_class"),  which(names(dataset)=="t_time"))
  #fe_col = which(names(dataset) %in% feature_colnames)
  
  
  cross_validate <- function (cv_id) {
    #for (cv_id in 1:num_folds) {
    
    set.seed(1234)
    
    tr = ids[which(fold==cv_id)]
    test_ids = which(dataset$comp_id %in% tr)
    
    trainset = dataset[-test_ids,]
    testset = dataset[test_ids,]
    
    trainset_endpts =  ddply(trainset, .(comp_id), function(x){x[nrow(x),]})
    testset_endpts = ddply(testset, .(comp_id), function(x){x[nrow(x),]})
    
    
    if (option_use_endpoints_for_classification == T) {
      # end-point model
      trainset_endpts$t_class = as.factor(trainset_endpts$t_class)
      rfm1 <- randomForest(t_class ~ ., data = trainset_endpts[, c("t_class", fe_col)], importance=TRUE, mtry = 4)
      
    } else {
      # cycle-samples model
      trainz <- trainset[ , c("t_class", fe_col)]
      train_uh = trainz[which(trainz$t_class==1),]
      train_h = trainz[which(trainz$t_class==0),]
      train_uhh = rbind(train_uh, train_h[seq(1,nrow(train_h),10),])
      
      train_uhh$t_class = as.factor(train_uhh$t_class)
      rfm1 <- randomForest(t_class ~ ., data = train_uhh, importance=TRUE, mtry = 4)
    }
    
    testset$fprob = predict(rfm1, newdata=testset, type='prob')[,2]
    trainset$fprob = predict(rfm1, newdata=trainset, type='prob')[,2]
    
    trainset_endpts$fprob = predict(rfm1, newdata=trainset_endpts, type='prob')[,2]
    testset_endpts$fprob = predict(rfm1, newdata=testset_endpts, type='prob')[,2]
    
    
    dataset[-test_ids,]$fprob = trainset$fprob
    dataset[test_ids,]$fprob = testset$fprob
    
    
    testset$coxhaz= testset$fprob
    trainset$coxhaz= trainset$fprob
    
    
    
    #     trainset$cum_failprob = rep(0, nrow(trainset))
    #     trainset = ddply(trainset, .(comp_id), get_cum_failprob, alpha_coef)
    # 
    # 
    #     res_end_pts = ddply(trainset, .(comp_id), function(x){x[nrow(x),]})
    # 
    #     res_colorcodes = rep('',nrow(res_end_pts))
    #     res_colorcodes[which(paste(res_end_pts$eventual_state)==0)]='blue'  # healthy
    #     res_colorcodes[which(paste(res_end_pts$eventual_state)==1)]='red'   # removed
    # 
    #     #     print( qplot( t_time, fprob, data=trainset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, fprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #     #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #     #     )
    # 
    #     print( qplot( t_time, coxrisk, data=trainset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, coxrisk), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #     )
    # 
    # 
    #     print( qplot( t_time, coxhaz, data=trainset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, coxhaz), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #     )
    # 
    # 
    #     print( qplot( t_time, cum_failprob, data=trainset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, cum_failprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #     )
    
    
    
    
    
    #     testset$cum_failprob = rep(0, nrow(testset))
    #     testset = ddply(testset, .(comp_id), get_cum_failprob, alpha_coef)
    # 
    # 
    #     res_end_pts = ddply(testset, .(comp_id), function(x){x[nrow(x),]})
    # 
    #     res_colorcodes = rep('',nrow(res_end_pts))
    #     res_colorcodes[which(paste(res_end_pts$eventual_state)==0)]='blue'  # healthy
    #     res_colorcodes[which(paste(res_end_pts$eventual_state)==1)]='red'   # removed
    # 
    #     print( qplot( t_time, fprob, data=testset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, fprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #     )
    # 
    # #     print( qplot( t_time, coxrisk, data=testset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, coxrisk), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    # #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    # #     )
    # # 
    # #     print( qplot( t_time, coxhaz, data=testset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, coxhaz), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    # #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    # #     )
    # 
    #     print( qplot( t_time, cum_failprob, data=testset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, cum_failprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #     )
    
    
    dataset[test_ids,]$fprob <<- testset$fprob
    dataset[test_ids,]$coxhaz <<- testset$coxhaz
    
    
    # store the results in a list for return
    list(testset$fprob, testset$coxhaz)
    
    
  }
  
  for (cv_id in 1:num_folds) {
    #print(cv_id)
    cross_validate(cv_id)
  }
  
  #print(system.time ( ( resR = lapply (1:nFolds,  cross_validate) ) ) )
  
  dt$dataset <- dataset
  
  return(dt)
  
  
}







# COXPH_DIRECT == F & USE_CYCLE_SAMPLES = T
# option_use_endpoints_for_classification == END_POINT_MODEL
# option_timedep == COX_TIMEDEP
crossValidatedClassificationCoxPHModels <- function (dt, num_folds, option_label='class_label', time_or_cycle_option = 'cycle', option_use_endpoints_for_classification = F, option_timedep = T) {
  
  set.seed(12345)
  
  dataset = dt$dataset
  
  fe_col = dt$dataset_feature_colnames
  
  comp_set = unique(dataset$comp_id)
  num_comps = length(comp_set)
  fold = rep(1:num_folds,length.out=num_comps)
  ids = sample(comp_set,num_comps)
  
  if (option_label == 'class_label') {
    dataset$t_class = dataset$class_label
  } else if (option_label == 'eventual_state') {
    dataset$t_class = dataset$eventual_state
  }
  
  
  if (time_or_cycle_option == 'cycle') {
    dataset$t_time = dataset$comp_cycle
    dataset$t_this_time = dataset$this_cycle
  } else {
    dataset$t_time = dataset$comp_time
    dataset$t_this_time = dataset$this_time
  }
  
  
  #sel_col = c(which(names(dataset) %in% feature_colnames), which(names(dataset)=="t_class"),  which(names(dataset)=="t_time"))
  #fe_col = which(names(dataset) %in% feature_colnames)
  
  
  cross_validate <- function (cv_id) {
    #for (cv_id in 1:num_folds) {
    
    set.seed(1234)
    
    tr = ids[which(fold==cv_id)]
    test_ids = which(dataset$comp_id %in% tr)
    
    trainset = dataset[-test_ids,]
    testset = dataset[test_ids,]
    
    trainset_endpts =  ddply(trainset, .(comp_id), function(x){x[nrow(x),]})
    testset_endpts = ddply(testset, .(comp_id), function(x){x[nrow(x),]})
    
    
    if (option_use_endpoints_for_classification == T) {
      # end-point model
      trainset_endpts$t_class = as.factor(trainset_endpts$t_class)
      rfm1 <- randomForest(t_class ~ ., data = trainset_endpts[, c("t_class", fe_col)], importance=TRUE, mtry = 4)
      
    } else {
      # cycle-samples model
      trainz <- trainset[ , c("t_class", fe_col)]
      train_uh = trainz[which(trainz$t_class==1),]
      train_h = trainz[which(trainz$t_class==0),]
      train_uhh = rbind(train_uh, train_h[seq(1,nrow(train_h),10),])
      
      train_uhh$t_class = as.factor(train_uhh$t_class)
      rfm1 <- randomForest(t_class ~ ., data = train_uhh, importance=TRUE, mtry = 4)
    }
    
    testset$fprob = predict(rfm1, newdata=testset, type='prob')[,2]
    trainset$fprob = predict(rfm1, newdata=trainset, type='prob')[,2]
    
    trainset_endpts$fprob = predict(rfm1, newdata=trainset_endpts, type='prob')[,2]
    testset_endpts$fprob = predict(rfm1, newdata=testset_endpts, type='prob')[,2]
    
    
    if (option_timedep == F) {
      
      #qplot(cycles_since_install_sedi, fprob, data=trainset0, colour=paste(status))
      #qplot(cycles_since_install_sedi, fprob, data=testset0, colour=paste(status))
      
      
      sel_col = c("fprob", "target_event", "t_time")
      
      sdd = trainset_endpts[, sel_col] #trainset0[, coxvars]
      
      coxph1 = coxph(Surv(t_time,  target_event==1 ) ~ fprob , data=sdd)
      print(summary(coxph1))
      
      
    } else {  # option_timedep == T
      
      start_t = trainset$t_time
      stop_t = trainset$t_time + trainset$t_this_time
      failure_t = trainset$target_event
      fprob = trainset$fprob
      
      sdd = data.frame(start_t, stop_t, failure_t, fprob)
      
      
      sdd=sdd[which(sdd$stop_t > sdd$start_t & sdd$start_t >=0),]
      
      coxph1 = coxph(Surv(start_t, stop_t, failure_t) ~  . , data=sdd, method='breslow') #, na.action=na.exclude)
      summary(coxph1)
      
    }
    
    dataset[-test_ids,]$fprob = trainset$fprob
    dataset[test_ids,]$fprob = testset$fprob
    
    
    
    sddtest = data.frame(fprob = testset[, "fprob"])
    pred = predict(coxph1, newdata=sddtest, type="risk" , reference='sample', se.fit=TRUE)
    
    
    testset$coxrisk = pred$fit
    testset$coxrisk_sefit = pred$se.fit
    
    
    sddtrain = data.frame(fprob = trainset[, "fprob"])
    pred_train = predict(coxph1, newdata=sddtrain, type="risk" , reference='sample', se.fit=TRUE)
    
    
    trainset$coxrisk = pred_train$fit
    trainset$coxrisk_sefit = pred_train$se.fit
    
    
    
    
    fe_names = rownames(coef(summary(coxph1)))
    
    
    #     if (length(fe_names)==1) {
    #       testset0$coxrisk_se = exp( 1.96*coef(summary(coxph1))[,'se(coef)'] * ( newdata[,fe_names]- mean(newdata[,fe_names]) ) )
    #     } else {
    #       testset0$coxrisk_se = t(exp( 1.96*coef(summary(coxph1))[,'se(coef)'] %*% t( newdata[,fe_names]- t(matrix(rep(colMeans(newdata[,fe_names]),nrow(newdata)),ncol=nrow(newdata))) )) )
    #     }
    #
    
    
    
    #mhaz = with(trainset0[, coxvars], muhaz(csi, status==1))
    mhaz = with(trainset_endpts[, c("t_time","t_class")], muhaz(t_time, t_class==1))
    
    
    #plot(mhaz$est.grid, mhaz$haz.est)
    
    testset$bhaz = sapply(testset$t_time, get_bhaz, mhaz)
    trainset$bhaz = sapply(trainset$t_time, get_bhaz, mhaz)
    
    
    testset$coxhaz= testset$bhaz * testset$coxrisk
    trainset$coxhaz= trainset$bhaz * trainset$coxrisk
    
    
    #     trainset$cum_failprob = rep(0, nrow(trainset))
    #     trainset = ddply(trainset, .(comp_id), get_cum_failprob, alpha_coef)
    # 
    # 
    #     res_end_pts = ddply(trainset, .(comp_id), function(x){x[nrow(x),]})
    # 
    #     res_colorcodes = rep('',nrow(res_end_pts))
    #     res_colorcodes[which(paste(res_end_pts$eventual_state)==0)]='blue'  # healthy
    #     res_colorcodes[which(paste(res_end_pts$eventual_state)==1)]='red'   # removed
    # 
    #     #     print( qplot( t_time, fprob, data=trainset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, fprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #     #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #     #     )
    # 
    #     print( qplot( t_time, coxrisk, data=trainset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, coxrisk), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #     )
    # 
    # 
    #     print( qplot( t_time, coxhaz, data=trainset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, coxhaz), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #     )
    # 
    # 
    #     print( qplot( t_time, cum_failprob, data=trainset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, cum_failprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #     )
    # 
    # 
    # 
    # 
    # 
    #     testset$cum_failprob = rep(0, nrow(testset))
    #     testset = ddply(testset, .(comp_id), get_cum_failprob, alpha_coef)
    # 
    # 
    #     res_end_pts = ddply(testset, .(comp_id), function(x){x[nrow(x),]})
    # 
    #     res_colorcodes = rep('',nrow(res_end_pts))
    #     res_colorcodes[which(paste(res_end_pts$eventual_state)==0)]='blue'  # healthy
    #     res_colorcodes[which(paste(res_end_pts$eventual_state)==1)]='red'   # removed
    # 
    #     print( qplot( t_time, fprob, data=testset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, fprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #     )
    # 
    #     print( qplot( t_time, coxrisk, data=testset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, coxrisk), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #     )
    # 
    #     print( qplot( t_time, coxhaz, data=testset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, coxhaz), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #     )
    # 
    #     print( qplot( t_time, cum_failprob, data=testset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, cum_failprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
    #            +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    #     )
    
    
    
    dataset[test_ids,]$bhaz <<- testset$bhaz
    dataset[test_ids,]$coxrisk <<- testset$coxrisk
    dataset[test_ids,]$coxhaz <<- testset$coxhaz
    dataset[test_ids,]$fprob <<- testset$fprob # here, set fprob = coxhaz
    
    dataset[test_ids,]$coxrisk_se <<- testset$coxrisk_se
    dataset[test_ids,]$coxrisk_sefit <<- testset$coxrisk_sefit
    
    
    
    
    
    # store the results in a list for return
    list(testset$fprob, testset$coxrisk, testset$bhaz, testset$coxhaz, testset$coxrisk_se, testset$coxrisk_sefit) 
    
  }
  
  
  for (cv_id in 1:num_folds) {
    #print(cv_id)
    cross_validate(cv_id)
  }
  
  #print(system.time ( ( resR = lapply (1:nFolds,  cross_validate) ) ) )
  
  dt$dataset <- dataset
  
  return(dt)
  
}


