library(dplyr) # should not library(plyr) with this
library(randomForest)

#' @title Create an object of class "ABMdata" for modeling functions
#' @description  Create an object for storing analysis and modeling results
#' @param data data.frame or data.table
#' @param unit_id_colname unit identifier column name
#' @param ts_index_colname time series column name
#' @return object of class ABMdata
#' @export
createABMdata <- function (data, unit_id_colname, ts_index_colname, dtime_format_string) {
  dt <- list()
  class(dt) <- "ABMdata"
  
  dt$unit_id_colname <- unit_id_colname
  dt$ts_index_colname <- ts_index_colname
  
  
  dt$unit_set <- unique(data[,unit_id_colname])
  dt$ix_numeric_or_integer <- as.numeric(which(sapply(names(data), function(nm){class(data[,nm])[1] == "numeric" | class(data[,nm])[1] == "integer"}) == T))
  dt$ix_integer <- as.numeric(which(sapply(names(data), function(nm){ class(data[,nm])[1] == "integer"}) == T))
  dt$ix_numeric <- as.numeric(which(sapply(names(data), function(nm){class(data[,nm])[1] == "numeric"}) == T))
  
  
  data$dtime_idx <- as.POSIXct(strptime(data[,ts_index_colname], dtime_format_string, tz="UTC"))
  data <- data[order(data[,unit_id_colname], data$dtime_idx),]
  #data <- arrange(data, data[,unit_id_colname], dtime_idx)
 
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
  data$cum_failprob = rep(0, N_data)

  dt$data <- data
  
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
  dt$comp_set <- unique(data$comp_id)
  
  dt$data <- data
  return(dt)

}

# #' @title Set the date index
# #' @description Use the date/time column and format string to set the date index
# #' @param dt an object of class ABMdata
# #' @param dtime_colname name of date/time column
# #' @param dtime_format_string string used to format the date/time
# #' @return an object of class ABMdata
# #' @export
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
  
  if (state_tag == 1) { # unhealthy
    x[length(x)] = 1
  }
  
  return(x)
}

# when "failure" (=1 for only the last record corresponding to the actual failure time ) 
# should be drived from "eventual_failure" (=1 for all records in component with failure )
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
                                           this_cycle = c(0, diff(comp_cycle)),
                                           comp_max_cycle = max(comp_cycle, na.rm=T) ) )
  }
  
  if (unit_cumulative_time_colname != '') {
    #names(data)[which(names(data) == unit_cumulative_time_colname)] = 'unit_cumulative_time'
    data$unit_cumulative_time = data[,unit_cumulative_time_colname]
    data <- as.data.frame( data 
                           %>% group_by(comp_id) 
                           %>% mutate( comp_time = unit_cumulative_time - unit_cumulative_time[1],
                                          this_time = c(0, diff(comp_time)),
                                          comp_max_time = max(comp_time, na.rm=T) ) )
  }
  #head(data[data$comp_id==dt$comp_set[1],]$comp_cycle)
  
  dt$data <- data
  return(dt)
   
}

set_comp_label <- function (x, window_size_for_label) {
  
  x$class_label = as.integer( rep(0,nrow(x)) )
  
  state_tag = x$eventual_state[1]
  if (state_tag == 1) {# unhealthy
    x$class_label = as.integer( (x$measure[nrow(x)] - x$measure) < window_size_for_label )
  } 
  
  return(x$class_label)
}

# assuming that we had $failure set during previous data prep
# should run after setCompEventFromEventualState and setCompCycleAndTime
# option = 'CycleBasedLabel' or 'TimeBasedLabel' or 'EventualStateBasedLabel'
setCompLabel <- function (dt, window_size_for_label, label_option='EventualStateBasedLabel') {
  
  dt$window_size_for_label = window_size_for_label
  dt$label_option = label_option
  
  data <- dt$data
  
  if (label_option == 'CycleBasedLabel') {
    data <- as.data.frame( data 
                           %>% group_by(comp_id) 
                           %>% mutate( class_label = set_comp_label(data.frame(comp_id=comp_id,eventual_state=eventual_state, measure=comp_cycle), window_size_for_label)) )
  } else if (label_option == 'TimeBasedLabel') {
    data <- as.data.frame( data 
                           %>% group_by(comp_id) 
                           %>% mutate( class_label = set_comp_label(data.frame(eventual_state=eventual_state, measure=comp_time), window_size_for_label)) )
  } else { 'EventualStateBasedLabel'
    data <- as.data.frame( data 
                           %>% group_by(comp_id) 
                           %>% mutate( class_label = eventual_state) )
  }
  
  dt$data <- data
  return(dt)
  
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



# perc   0.0 ~ 1.0
# select all columns where more than "perc" % of records have valid (nonNA) value
selectColumns <- function (dt, perc, tested_feature_colnames) {
  
  data <- dt$data
  
  tfc = tested_feature_colnames
  col_idx = sapply(tfc, function(i){sum(is.na(data[,i])) < (1-perc)*nrow(data)} )
  
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
imputeFeatures <- function (dt, imputed_feature_colnames, method) {
  
  data <- dt$data
  
 
  if (method == 'across_components') {
    data[,imputed_feature_colnames] <- na.roughfix(data[,imputed_feature_colnames])
    
  } else if (method == 'component_wise') {
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




setDatasetForModel <- function (dt, subset_condition, select_columns) {
  dataset <- dt$data  # dataset for model
  
  sel_col = which(names(dataset) %in% select_columns) # which(names(dataset)=="target")
  dataset = dataset[subset_condition,sel_col]
  
  dt$dataset <- dataset
  return(dt)
}

