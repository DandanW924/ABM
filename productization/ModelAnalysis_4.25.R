
source('./ABMdata.R')

#library(data.table)
#cf6<- fread("./data_cf6_0717.csv", sep='~')

#td = cf6
#td = as.data.frame(td)
#saveRDS(td, './data_cf6_0717.rds')

data0 = readRDS('./data_cf6_0717.rds')

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

## Removed components
td_cpn89 = td[which(td$cpn=='3011089'),]
tu = td_cpn89

CEOD = T
OVERHAUL_ONLY = T

if (CEOD == T) {
  if (OVERHAUL_ONLY == T) {
    
    td_uh = tu[which( (tu$repair_type == "O - OVERHAUL" | tu$repair_type == "R - REPAIR") & !is.na(tu$duty_cycle) ),]
  
  } else {

    #td_uh =  tu[which( !is.na(tu$duty_cycle) ),]
    td_uh = tu[which( !is.na(tu$duty_cycle) & tu$cycles_since_installation > 100 ),]
  }
  
  td_h = td[which(is.na(td$cpn) & !is.na(td$duty_cycle) ),]
  
} else { # CEOD == F
  if (OVERHAUL_ONLY == T) {
  
    td_uh = tu[which( (tu$repair_type == "O - OVERHAUL" | tu$repair_type == "R - REPAIR") ),]

  } else {
    
    #td_uh =  tu 
    td_uh = tu[which( tu$cycles_since_installation > 100 ),]
  }
  
  ## Healthy components
  td_h = td[which(is.na(td$cpn)),]
  
}


tu = rbind(td_uh, td_h)

#sapply(names(tu)[252:523], function(i) { length(which(tu[ , i]==-9999 | is.na(tu[,i]) ))  })
sapply(names(tu)[252:523], function(i) { tu[ which(tu[,i]==-9999),i] <<- NA   })


data = tu


abm = createABMdata(data, "serlzd_eng_ser_num", "to_src_flght_dttm", "%Y-%m-%d %H:%M:%S")

abm = convertDateFormat(abm, "install_date",  "%Y-%m-%d")
abm = convertDateFormat(abm, "removal_date",  "%Y-%m-%d")
abm = convertDateFormat(abm, "to_src_flght_dttm",   "%Y-%m-%d %H:%M:%S")

abm$data$src_flght_dttm = abm$data$to_src_flght_dttm




### set serlzd_eng_ser_num to comp_id for healthy components (cpn = NA in this data)
idx_na = which(is.na(abm$data$cpn))
abm$data[idx_na,]$comp_id = as.character(abm$data[idx_na,]$serlzd_eng_ser_num)


abm = setCompIds(abm, c("cpn", "msn", "install_date", "serlzd_eng_ser_num"))



### set eventual_failure = 1 on all records for components with eventual removal
abm$data$eventual_failure = 1  # unhealthy
abm$data[idx_na,]$eventual_failure = 0  # healthy



abm = setCompEventFromEventualState(abm, "eventual_failure") 

#abm = setCompEventualStateFromEvent(abm, "target_event") 


###
set_comp_install_date_for_healthy <- function (x) {
  
  state_tag = x$eventual_state[1]
  
  if (state_tag == 0) {# healthy
    x$install_date = x$src_flght_dttm[1]
  }
  return(x$install_date)
  
}

abm$data <- as.data.frame( abm$data 
                           %>%  group_by(comp_id)  
                           %>%  mutate(install_date = set_comp_install_date_for_healthy(data.frame(eventual_state=eventual_state,src_flght_dttm=src_flght_dttm, install_date=install_date)))
                         )


### special steps related to this data
abm$data$sedi_ecsn = abm$data$to_sedi_ecsn
abm$data$sedi_etsn = abm$data$to_sedi_etsn



abm = setCompCycleAndTime(abm, unit_cumulative_cycle_colname = 'sedi_ecsn', unit_cumulative_time_colname = 'sedi_etsn') 
abm = setCompLabel(abm, window_size_for_label=1000, label_option='CycleBasedLabel') 


set_valid <- function (x) {
  
  if (sum(!is.na(x$cpn)) > 0) {
    last_removal_date = max( x[which(!is.na(x$cpn)),]$removal_date )
    valid = ( ( x$src_flght_dttm > last_removal_date+24*3600 | !is.na(x$cpn) ) & !is.na(x$sedi_ecsn) )
  } else {
    valid = rep(TRUE, nrow(x))  # "purely healthy"
  }
  
  return(valid)
}

abm$data <- as.data.frame( abm$data 
                           %>%  group_by(serlzd_eng_ser_num)  
                           %>%  mutate(valid = set_valid(data.frame(cpn=cpn,removal_date=removal_date,src_flght_dttm=src_flght_dttm, sedi_ecsn=sedi_ecsn)))
                           %>%  filter(valid == T) )


getHealthyCompNum(abm)
# [1] 118
getFailedCompNum(abm)
# [1] 31
getAllCompNum(abm)
# [1] 149

data00 = abm$data


col_index = c(which(names(abm$data)=='time_since_installation'):which(names(abm$data)=='days_since_installation') )
col_index = c(col_index, which(names(abm$data)=='to_agw'):which(names(abm$data)=='to_zxm'))
col_index = c(col_index, which(names(abm$data)=='cr_agw'):which(names(abm$data)=='cr_zxm'))
col_index = c(col_index, which(names(abm$data)=='airspeed_calibrated_1_or_only_0_mean'):which(names(abm$data)=='wing_anti_ice_1_third_qtr'))
col_index = c(col_index, which(names(abm$data)=='duty_cycle'):which(names(abm$data)=='cycle'))


#sapply(names(abm$data), function(i){as.integer(100*sum(is.na(abm$data[,i]))/nrow(abm$data))})
abm = selectColumns(abm, 0.8, names(abm$data)[col_index])


###
feature_colnames = names(abm$data)[ (which(names(abm$data)=='valid')+1):ncol(abm$data) ]
#feature_colnames = feature_colnames[sapply(feature_colnames, function(nm){class(abm$data[,nm][1])}) == 'numeric']

abm = imputeFeatures(abm, method='component_wise', feature_colnames) #  'across_components'
  
#head( abm$data[abm$data$comp_id==abm$comp_set[1],]$class_label )

head( filter(abm$data, comp_id==abm$comp_set[1] ))
x = filter(abm$data, comp_id==abm$comp_set[1] )

# healthy comp_id 
#unique(abm$data[which(abm$data$eventual_state==0),]$comp_id)

abm = generateCumAvgFeatures(abm,feature_colnames)

subset_condition = (abm$data$comp_cycle > 0 & abm$data$this_cycle > 0)

select_columns = c("duty_cycle_cumavg", "gross_weight_lbs_0_mean_cumavg", "to_iaie_cumavg", "to_delfn_cumavg", "to_zxm_cumavg") 
select_columns = c(names(abm$data)[1:which(names(abm$data)=='valid')], select_columns)

# create abm$dataset
abm = setDatasetForModel(abm, subset_condition, select_columns) 
  


