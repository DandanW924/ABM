#' Example code to show how to use ABM functions
#' 
#' Copyright (c) 2016, GE Digital. All rights reserved.
#' Proprietary and confidential
#' Unauthorized copying of this file, via any medium is strictly prohibited
#' This cannot be copied and/or distributed without the express permission of GE Digital
#' 
#' Author: Hyung-il Ahn (hyungil.ahn@ge.com), GE Digital



inputpath="C:\\Users\\212448513\\Documents\\work\\ABM\\productization" 
setwd(inputpath)

source('./ABMdata.R')

library(data.table)
#cf6<- fread("~/Box Sync/ABMpack/data_cf6_0717.csv", sep='~')
cf6 <- fread("./data_cf6_0717.csv", sep="~")
#td = cf6[1:30,]
#td = as.data.frame(td)
#saveRDS(td, './data_cf6_0717_test.rds')

cpn89_data_gen = T

if (cpn89_data_gen == T) {
  
  data0 = readRDS('./data_cf6_0717_test.rds')
  #data0 = readRDS('~/Box Sync/ABMpack/data_cf6_0717.rds')
  
  # > names(data)[1:30]
  # [1] "src_tl_num"                "aircraft_type"             "src_tl_num_substr"         "src_icao_cd"
  # [5] "serlzd_eng_ser_num"        "engine_type"               "enginefitstring"           "install_datetime"
  # [9] "engine_position"           "serlzd_eng_stat_cd"        "aircraft"                  "fleet"
  # [13] "model"                     "cpn"                       "mpn"                       "msn"
  # [17] "position"                  "install_date"              "removal_date"              "removal_year"
  # [21] "removal_month"             "removal_code"              "time_since_installation"   "cycles_since_installation"
  # [25] "days_since_installation"   "repair_type"               "removal_station"           "removal_department"
  # [29] "install_station"           "cash_removal"
  
  td = data0
  
  
  td$to_src_flght_dttm = strptime(td$to_src_flght_dttm, "%Y-%m-%d %H:%M:%S" , tz="UTC")
  td$install_date = strptime(td$install_date, "%Y-%m-%d", tz="UTC")
  td$removal_date = strptime(td$removal_date, "%Y-%m-%d", tz="UTC")
  
  td$to_src_flght_dttm <- as.POSIXct( td$to_src_flght_dttm )
  td$install_date <- as.POSIXct( td$install_date  )
  td$removal_date <- as.POSIXct( td$removal_date  )
  
  td = td[order(td$serlzd_eng_ser_num, td$to_src_flght_dttm),]
  
  td$comp_id = paste(td$cpn, td$msn, td$install_date, td$serlzd_eng_ser_num)
  idx_na = which(is.na(td$cpn))
  td[idx_na,]$comp_id = as.character(td[idx_na,]$serlzd_eng_ser_num)
  td$comp_id_str = td$comp_id
  td$comp_id <- as.numeric(as.factor(td$comp_id))
  
  
  
  td$sedi_ecsn = td$to_sedi_ecsn
  td$sedi_etsn = td$to_sedi_etsn
  
  
  ### set eventual_failure = 1 on all records for components with eventual removal
  td$eventual_failure = 1  # unhealthy
  td[idx_na,]$eventual_failure = 0  # healthy
  
  
  ## Removed components
  td_cpn89 = td[which(td$cpn=='3011089'),]
  
  td_uh = td_cpn89[which( (td_cpn89$repair_type == "O - OVERHAUL" | td_cpn89$repair_type == "R - REPAIR") & !is.na(td_cpn89$duty_cycle) ),]
  #td_uh = td_cpn89[which( td_cpn89$cycles_since_installation > 100 & (td_cpn89$repair_type == "O - OVERHAUL" | td_cpn89$repair_type == "R - REPAIR") & !is.na(td_cpn89$duty_cycle) ),]
  
  
  td_h = td[which(is.na(td$cpn) & !is.na(td$duty_cycle) ),]
  
  
  healthy_comp_ids = unique(td_h$comp_id)
  removed_comp_ids = unique(td_uh$comp_id)
  
  length(unique(td_h$comp_id))
  length(unique(td_uh$comp_id))
  
  #tu = rbind(td_uh, td_h)
  
  
  td_uh = td_cpn89[which( td_cpn89$comp_id %in% removed_comp_ids ),]
  td_h = td[which(td$comp_id %in% healthy_comp_ids),]
  
  tu = rbind(td_uh, td_h)
  
  #sapply(names(tu)[252:523], function(i) { length(which(tu[ , i]==-9999 | is.na(tu[,i]) ))  })
  sapply(names(tu)[252:523], function(i) { tu[ which(tu[,i]==-9999),i] <<- NA   })
  
  
  data_cpn89 = tu
  saveRDS(data_cpn89, '~/Box Sync/ABMpack/data_cf6_0717_cpn89.rds')
  
} else {
  
  data_cpn89 = readRDS('./data_cf6_0717_cpn89.rds')
  #data_cpn89 = readRDS('~/Box Sync/ABMpack/data_cf6_0717_cpn89.rds')
}


abm = createABMdata(data_cpn89, "serlzd_eng_ser_num", "comp_id", "to_src_flght_dttm", "%Y-%m-%d %H:%M:%S")

# abm = convertDateFormat(abm, "install_date",  "%Y-%m-%d")
# abm = convertDateFormat(abm, "removal_date",  "%Y-%m-%d")
# abm = convertDateFormat(abm, "to_src_flght_dttm",   "%Y-%m-%d %H:%M:%S")



# comp_id has been given already. so not need to set again
### set serlzd_eng_ser_num to comp_id for healthy components (cpn = NA in this data)
# idx_na = which(is.na(abm$data$cpn))
# abm$data[idx_na,]$comp_id = as.character(abm$data[idx_na,]$serlzd_eng_ser_num)
# abm = setCompIds(abm, c("cpn", "msn", "install_date", "serlzd_eng_ser_num"))






abm = setCompEventFromEventualState(abm, "eventual_failure")

#abm = setCompEventualStateFromEvent(abm, "target_event")


###
set_comp_install_date_for_healthy <- function (x) {
  
  state_tag = x$eventual_state[1]
  
  if (state_tag == 0) {# healthy
    x$install_date = x$dtime_idx[1]
  }
  return(x$install_date)
  
}

abm$data <- as.data.frame( abm$data
                           %>%  group_by(comp_id)
                           %>%  mutate(install_date = set_comp_install_date_for_healthy(data.frame(eventual_state=eventual_state,dtime_idx=dtime_idx, install_date=install_date)))
)


set_valid <- function (x) {
  
  if (sum(!is.na(x$cpn)) > 0) {
    last_removal_date = max( x[which(!is.na(x$cpn)),]$removal_date )
    valid = ( ( x$dtime_idx > last_removal_date+24*3600 | !is.na(x$cpn) ) & !is.na(x$sedi_ecsn) ) #& x$sedi_ecsn > 0)
  } else {
    valid = rep(TRUE, nrow(x))  # "purely healthy"
  }
  
  return(valid)
}

abm$data <- as.data.frame( abm$data
                           %>%  group_by(serlzd_eng_ser_num)
                           %>%  mutate(valid = set_valid(data.frame(cpn=cpn,removal_date=removal_date,dtime_idx=dtime_idx, sedi_ecsn=sedi_ecsn)))
                           %>%  filter(valid == T) )





abm = setCompCycleAndTime(abm, unit_cumulative_cycle_colname = 'sedi_ecsn', unit_cumulative_time_colname = 'sedi_etsn')


num_healthy_comp = getHealthyCompNum(abm)
# [1] 118
num_failed_comp = getFailedCompNum(abm)
# [1] 31
num_all_comp = getAllCompNum(abm)
# [1] 149

print(num_healthy_comp)
print(num_failed_comp)
print(num_all_comp)

abm00 = abm # for save

col_index = c( grep( "_third_qtr",names(abm$data)), grep( "_first_qtr",names(abm$data)), grep( "_median",names(abm$data)))

abm = removeColumns(abm, names(abm$data)[col_index])


col_index =  which(names(abm$data)=='to_agw'):which(names(abm$data)=='to_zxm')
col_index = c(col_index, which(names(abm$data)=='cr_agw'):which(names(abm$data)=='cr_zxm'))
col_index = c(col_index, which(names(abm$data)=='airspeed_calibrated_1_or_only_0_mean'):which(names(abm$data)=='wing_anti_ice_1_mean'))
col_index = c(col_index, which(names(abm$data)=='duty_cycle'):which(names(abm$data)=='cycle'))



#sapply(names(abm$data), function(i){as.integer(100*sum(is.na(abm$data[,i]))/nrow(abm$data))})
abm = selectValidColumns(abm, 0.01, names(abm$data)[col_index])

feature_colnames = abm$valid_colnames

###
#feature_colnames = names(abm$data)[ (which(names(abm$data)=='valid')+1):ncol(abm$data) ]
#feature_colnames = feature_colnames[sapply(feature_colnames, function(nm){class(abm$data[,nm][1])}) == 'numeric']

abm = imputeFeatures(abm, option_method='across_components', feature_colnames) #  'across_components' 'component_wise'
# 'component_wise may produce NAs, so may make errors in randomForest


#length(which(is.na(abm$data[,feature_colnames])))

#head( abm$data[abm$data$comp_id==getCompSet(abm)[1],]$class_label )

#head( filter(abm$data, comp_id==getCompSet(abm)[1] ))
#x = filter(abm$data, comp_id==getCompSet(abm)[1] )

# healthy comp_id
#unique(abm$data[which(abm$data$eventual_state==0),]$comp_id)

abm = generateCumAvgFeatures(abm,feature_colnames)
abm = generateCumSumFeatures(abm,feature_colnames)



subset_condition = (abm$data$comp_cycle >= 0 & abm$data$this_cycle > 0) #& abm$data$comp_time >= 0 & abm$data$this_time > 0)

info_colnames = names(abm$data)[1:which(names(abm$data)=='comp_max_time')]
feature_colnames = names(abm$data)[(which(names(abm$data)=='comp_max_time')+1):length(names(abm$data))]

abm = setDatasetForModel(abm, subset_condition, info_colnames, feature_colnames)
abm = setCompLabel(abm, window_size_for_label=1000, label_option='CycleBasedLabel')  # should always run setCompLabel after setDatasetForModel
#abm = imputeFeaturesOnDataset(abm, option_method='across_components', feature_colnames) #  'across_components' 'component_wise'
# 'component_wise may produce NAs, so may make errors in randomForest

abm0 = abm  # for save


feature_select = F
if (feature_select == T) {
  ## some helper functions for feature selection
  abm = featureSelectionTestUsingRF(abm, option_endpts = T, output_filename='outtest')  # for dataset_endpts
  abm = featureSelectionTestUsingRF(abm, option_endpts = F, output_filename='outtest')  # for dataset
  
  abm = univariateCoxPH(abm, feature_colnames, 'time',  "outtest")
  
  abm = separateDistributions(abm, feature_colnames, component_wise = T, class_option = 'eventual_state', "outtest")
  abm = separateDistributions(abm, feature_colnames, component_wise = F, class_option = 'eventual_state', "outtest")
  ###
}

#feature_colnames = abm$ranked_features_rf


option_test = 1#4

if (option_test == 1) {
  
  abm = abm0
  
  #   sig_ix_u = c('comp_cycle','to_cas_cumavg', 'cr_zwf36_cumavg', 'to_delfn_cumavg', 'cr_zpcn25_cumavg', 'cr_zt1a_cumavg')
  #   sig_ix_u = c(sig_ix_u, 'duty_cycle_cumavg','on_time_cumavg', 'cycle_cumavg') 
  #   feature_colnames  = sig_ix_u
  
  feature_colnames  = c("duty_cycle_cumavg", "gross_weight_lbs_0_mean_cumavg", "to_iaie_cumavg", "to_delfn_cumavg", "to_zxm_cumavg")
  
  info_colnames = names(abm$data)[1:which(names(abm$data)=='comp_max_time')]
  
  # create abm$dataset
  abm = setDatasetForModel(abm, subset_condition, info_colnames, feature_colnames)
  abm = setCompLabel(abm, window_size_for_label=1000, label_option='CycleBasedLabel')
  #abm = imputeFeaturesOnDataset(abm, option_method='across_components', feature_colnames) #  'across_components' 'component_wise'
  # 'component_wise may produce NAs, so may make errors in randomForest
  
  abm1 = abm # for save
  
  # to guarantee the same model results 
  # check if dataset  row order (comp_id ordered) the same,  column order  (feature ordered) the same
  
  
  # note that crossValidatedModels use the RF classification and Time-dependent CoxPH.   
  # The use of this function can be replaced by crossValidatedClassificationModels with option_timedep = T
  #abm = crossValidatedModels(abm, num_folds = 5, option_label='class_label', option_cox=T, time_or_cycle_option = 'cycle', option_classification='')
  abm = crossValidatedModels(abm, num_folds = 5, option_label='eventual_state', option_cox=T, time_or_cycle_option = 'cycle', option_classification='')
  
} else if (option_test == 2) {
  
  abm = abm0
  
  feature_colnames  = c("duty_cycle_cumavg", "gross_weight_lbs_0_mean_cumavg", "to_iaie_cumavg", "to_delfn_cumavg", "to_zxm_cumavg")
  info_colnames = names(abm$data)[1:which(names(abm$data)=='comp_max_time')]
  
  # create abm$dataset
  abm = setDatasetForModel(abm, subset_condition, info_colnames, feature_colnames)
  abm = setCompLabel(abm, window_size_for_label=1000, label_option='CycleBasedLabel')
  #abm = imputeFeaturesOnDataset(abm, option_method='across_components', feature_colnames) #  'across_components' 'component_wise'
  # 'component_wise may produce NAs, so may make errors in randomForest
  
  abm1 = abm 
  
  abm = crossValidatedCoxPHModels (abm, num_folds = 5, time_or_cycle_option = 'cycle', option_timedep = F, num_init_samples = 100)
  
} else if (option_test == 3) {
  
  abm = abm0
  
  feature_colnames  = c("duty_cycle_cumavg", "gross_weight_lbs_0_mean_cumavg", "to_iaie_cumavg", "to_delfn_cumavg", "to_zxm_cumavg")
  info_colnames = names(abm$data)[1:which(names(abm$data)=='comp_max_time')]
  
  # create abm$dataset
  abm = setDatasetForModel(abm, subset_condition, info_colnames, feature_colnames)
  abm = setCompLabel(abm, window_size_for_label=1000, label_option='CycleBasedLabel')
  #abm = imputeFeaturesOnDataset(abm, option_method='across_components', feature_colnames) #  'across_components' 'component_wise'
  # 'component_wise may produce NAs, so may make errors in randomForest
  
  abm1 = abm 
  
  abm = crossValidatedClassificationModels (abm, num_folds = 5, option_label='eventual_state', time_or_cycle_option = 'cycle', option_use_endpoints_for_classification = F)
  #abm = crossValidatedClassificationModels (abm, num_folds = 5, option_label='class_label', time_or_cycle_option = 'cycle', option_use_endpoints_for_classification = F)
  
} else if (option_test == 4) {
  
  abm = abm0
  sig_ix_u = c('comp_cycle','to_cas_cumavg', 'cr_zwf36_cumavg', 'to_delfn_cumavg', 'cr_zpcn25_cumavg', 'cr_zt1a_cumavg')
  sig_ix_u = c(sig_ix_u, 'duty_cycle_cumavg','on_time_cumavg', 'cycle_cumavg') 
  feature_colnames  = sig_ix_u
  
  #  feature_colnames = names(abm$data)[(which(names(abm$data)=='comp_max_time')+1):length(names(abm$data))]
  #feature_colnames  = c("duty_cycle_cumavg", "gross_weight_lbs_0_mean_cumavg", "to_iaie_cumavg", "to_delfn_cumavg", "to_zxm_cumavg")
  
  info_colnames = names(abm$data)[1:which(names(abm$data)=='comp_max_time')]
  
  # create abm$dataset
  abm = setDatasetForModel(abm, subset_condition, info_colnames, feature_colnames)
  abm = setCompLabel(abm, window_size_for_label=1000, label_option='CycleBasedLabel')
  #abm = imputeFeaturesOnDataset(abm, option_method='across_components', feature_colnames) #  'across_components' 'component_wise'
  # 'component_wise may produce NAs, so may make errors in randomForest
  
  abm1 = abm 
  
  #abm = crossValidatedClassificationCoxPHModels (abm, num_folds = 5, option_label='eventual_state', time_or_cycle_option = 'cycle', option_use_endpoints_for_classification = F, option_timedep = F)
  abm = crossValidatedClassificationCoxPHModels (abm, num_folds = 5, option_label='eventual_state', time_or_cycle_option = 'cycle', option_use_endpoints_for_classification = F, option_timedep = T)
  
}

pdf("./test_rry.pdf")

dataset = abm$dataset


dataset$sum_fprob = rep(0, nrow(dataset))
dataset = ddply(dataset, .(comp_id), get_sum_fprob)


alpha_coef = 0.01
if (option_test == 2) {
  alpha_coef = 1
} else if (option_test == 3) {
  alpha_coef = 0.001
} else if (option_test == 4) {
  alpha_coef = 0.1
} 

dataset$cum_failprob = rep(0, nrow(dataset))
dataset = ddply(dataset, .(comp_id), get_cum_failprob, alpha_coef)

res_end_pts = ddply(dataset, .(comp_id), function(x){x[nrow(x),]})

res_colorcodes = rep('',nrow(res_end_pts))
res_colorcodes[which(paste(res_end_pts$eventual_state)==0)]='blue'  # healthy
res_colorcodes[which(paste(res_end_pts$eventual_state)==1)]='red'   # removed

if (option_test != 2) {
  print( qplot( t_time, sum_fprob, data=dataset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, sum_fprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
         +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
  )
  
  print( qplot( t_time, fprob, data=dataset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, fprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
         +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
  )
  
}

if (option_test != 3) {
  
  print( qplot( t_time, bhaz, data=dataset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, bhaz), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
         +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
  )
  
  print( qplot( t_time, coxrisk, data=dataset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, coxrisk), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
         +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
  )
  
  
  print( qplot( t_time, coxhaz, data=dataset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, coxhaz), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
         +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
  )
  
}

print( qplot( t_time, cum_failprob, data=dataset, group=paste(comp_id), colour=paste(eventual_state), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(t_time, cum_failprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
       +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
)

#dataset$pred_class = rep(0, nrow(dataset))

#ddply(dataset, .(comp_id), function(x){ sum(x$fprob_r > 0.5) > 0 } )

#predi = sapply(res_end_pts$comp_id, function(i){ x=dataset[which(dataset$comp_id==i),]; return(sum(x$fprob_r > 0.5) > 0 )  })

pred = prediction(res_end_pts$sum_fprob, res_end_pts$eventual_state)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)


pred = prediction(dataset$cum_failprob, dataset$eventual_state)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)


dev.off()
