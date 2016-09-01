library(plyr)
library(survival)
library(data.table)
library(ggplot2)
library(ROCR)
library(randomForest)
library(caret)
library(muhaz)

library(foreach)
library(doSNOW)

library(ROCR)


cf6<- fread("./data_cf6_0717.csv", sep='~')  # snapshot + ceod


td = cf6
td = as.data.frame(td)


# sapply(unique(cf6$serlzd_eng_ser_num), function(id){ unique(cf6[which(cf6$serlzd_eng_ser_num==id),]$msn) } )

# > nrow(td)
# [1] 406783
# > names(td)
# [1] "src_tl_num"                            "aircraft_type"                         "src_tl_num_substr"
# [4] "src_icao_cd"                           "serlzd_eng_ser_num"                    "engine_type"
# [7] "enginefitstring"                       "install_datetime"                      "engine_position"
# [10] "serlzd_eng_stat_cd"                    "aircraft"                              "fleet"
# [13] "model"                                 "cpn"                                   "mpn"
# [16] "msn"                                   "position"                              "install_date"
# [19] "removal_date"                          "removal_year"                          "removal_month"
# [22] "removal_code"                          "time_since_installation"               "cycles_since_installation"
# [25] "days_since_installation"               "repair_type"                           "removal_station"
# [28] "removal_department"                    "install_station"                       "cash_removal"
# [31] "removal_work_order_number"             "repair_order_number"                   "removal_ata"
# [34] "discrepancy_number"                    "removal_text"                          "repair_text"
# [37] "repair_station"                        "repair_department"                     "repair_date"
# [40] "confirmed_unscheduled_removal"         "vendor_name"                           "repair_parts"
# [43] "tso_removal"                           "install_department"                    "parent_cpn"
# [46] "parent_msn"                            "total_aircraft_time_at_installation"   "total_aircraft_cycles_at_installation"
# [49] "csn"                                   "installation_tag_number"               "installation_work_order_number"
# [52] "installation_mechanic_id"              "total_aircraft_time_at_removal"        "total_aircraft_cycles_at_removal"
# [55] "basis_hours"                           "basis_cycles"                          "removal_mechanic_id"
# [58] "vendor_address"                        "previous_repair_order_number"          "previous_vendor_name"
# [61] "previous_vendor_address"               "tsn_removal"                           "csn_removal"
# [64] "cso_removal"                           "tssv_removal"                          "cssv_removal"
# [67] "tsn_install"                           "csn_install"                           "tso_install"
# [70] "cso_install"                           "tssv_install"                          "cssv_install"
# [73] "removal_reason"                        "to_flght_phs_cd"                       "to_flght_rprt_id"
# [76] "to_msgseqid"                           "to_when_created"                       "to_src_flght_dttm"
# [79] "to_agw"                                "to_brat"                               "to_cas"
# [82] "to_degt"                               "to_degt_d"                             "to_degt_smoothed"
# [85] "to_delfn"                              "to_deln1"                              "to_dpoil"
# [88] "to_dpoil_smoothed"                     "to_dtamb"                              "to_ecsi"
# [91] "to_ecsn"                               "to_ecsv"                               "to_egthdm"
# [94] "to_egthdm_d"                           "to_etsi"                               "to_etsn"
# [97] "to_etsv"                               "to_gpcn25_d"                           "to_gpcn25_smoothed"
# [100] "to_gwfm"                               "to_gwfm_d"                             "to_iai"
# [103] "to_iaie"                               "to_iaie1"                              "to_iaie2"
# [106] "to_iaie3"                              "to_iaiwg"                              "to_ibe1"
# [109] "to_ibe2"                               "to_ibe3"                               "to_ibp1"
# [112] "to_ibp2"                               "to_ibp3"                               "to_ivs12"
# [115] "to_ivs13"                              "to_pcn1k"                              "to_sat"
# [118] "to_satsl"                              "to_satslf"                             "to_sedi_ecsi"
# [121] "to_sedi_ecsn"                          "to_sedi_ecsv"                          "to_sedi_etsi"
# [124] "to_sedi_etsn"                          "to_sedi_etsv"                          "to_sloatl"
# [127] "to_sloatl_d"                           "to_sloatl_smoothed"                    "to_tatsl"
# [130] "to_tatslf"                             "to_vsvnom"                             "to_wbi"
# [133] "to_wfmp"                               "to_zalt"                               "to_zpcn12"
# [136] "to_zpcn25"                             "to_zpcn25_d"                           "to_zpoil"
# [139] "to_zpoil_d"                            "to_zpoil_smoothed"                     "to_zt1a"
# [142] "to_zt49"                               "to_zt49_d"                             "to_ztla"
# [145] "to_ztla_d"                             "to_ztoil"                              "to_ztoil_smoothed"
# [148] "to_zvb1f"                              "to_zvb1f_d"                            "to_zvb1f_smoothed"
# [151] "to_zvb2r"                              "to_zvb2r_smoothed"                     "to_zwf36"
# [154] "to_zwf36_d_smoothed"                   "to_zxm"                                "cr_flght_phs_cd"
# [157] "cr_flght_rprt_id"                      "cr_msgseqid"                           "cr_when_created"
# [160] "cr_src_flght_dttm"                     "cr_agw"                                "cr_brat"
# [163] "cr_cas"                                "cr_degt"                               "cr_degt_d"
# [166] "cr_degt_smoothed"                      "cr_delfn"                              "cr_deln1"
# [169] "cr_dpoil"                              "cr_dpoil_smoothed"                     "cr_dtamb"
# [172] "cr_ecsi"                               "cr_ecsn"                               "cr_ecsv"
# [175] "cr_egthdm"                             "cr_egthdm_d"                           "cr_etsi"
# [178] "cr_etsn"                               "cr_etsv"                               "cr_gpcn25_d"
# [181] "cr_gpcn25_smoothed"                    "cr_gwfm"                               "cr_gwfm_d"
# [184] "cr_iai"                                "cr_iaie"                               "cr_iaie1"
# [187] "cr_iaie2"                              "cr_iaie3"                              "cr_iaiwg"
# [190] "cr_ibe1"                               "cr_ibe2"                               "cr_ibe3"
# [193] "cr_ibp1"                               "cr_ibp2"                               "cr_ibp3"
# [196] "cr_ivs12"                              "cr_ivs13"                              "cr_pcn1k"
# [199] "cr_sat"                                "cr_satsl"                              "cr_satslf"
# [202] "cr_sedi_ecsi"                          "cr_sedi_ecsn"                          "cr_sedi_ecsv"
# [205] "cr_sedi_etsi"                          "cr_sedi_etsn"                          "cr_sedi_etsv"
# [208] "cr_sloatl"                             "cr_sloatl_d"                           "cr_sloatl_smoothed"
# [211] "cr_tatsl"                              "cr_tatslf"                             "cr_vsvnom"
# [214] "cr_wbi"                                "cr_wfmp"                               "cr_zalt"
# [217] "cr_zpcn12"                             "cr_zpcn25"                             "cr_zpcn25_d"
# [220] "cr_zpoil"                              "cr_zpoil_d"                            "cr_zpoil_smoothed"
# [223] "cr_zt1a"                               "cr_zt49"                               "cr_zt49_d"
# [226] "cr_ztla"                               "cr_ztla_d"                             "cr_ztoil"
# [229] "cr_ztoil_smoothed"                     "cr_zvb1f"                              "cr_zvb1f_d"
# [232] "cr_zvb1f_smoothed"                     "cr_zvb2r"                              "cr_zvb2r_smoothed"
# [235] "cr_zwf36"                              "cr_zwf36_d_smoothed"                   "cr_zxm"
# [238] "comp_id"                               "install_id"                            "airport_depart"
# [241] "airport_destination"                   "environment_id_depart"                 "environment_id_destination"
# [244] "adi_qar_flight_record_number"          "ffd_id"                                "duty_cycle"
# [247] "on_time"                               "cycle"

# > sum(td$to_src_flght_dttm!='' & !is.na(td$to_src_flght_dttm))
# [1] 406783
# > sum(td$cr_src_flght_dttm!='' & !is.na(td$cr_src_flght_dttm))
# [1] 379478


td$src_flght_dttm = strptime(td$to_src_flght_dttm, "%Y-%m-%d %H:%M:%S" , tz="UTC")
td$install_date = strptime(td$install_date, "%Y-%m-%d", tz="UTC")
td$removal_date = strptime(td$removal_date, "%Y-%m-%d", tz="UTC")

td$src_flght_dttm <- as.POSIXct( td$src_flght_dttm )
td$install_date <- as.POSIXct( td$install_date  )
td$removal_date <- as.POSIXct( td$removal_date  )

td = td[order(td$serlzd_eng_ser_num, td$src_flght_dttm),]

### sedi_ecsn exists on takeoff snapshot only
### 0. should be ordered by  "src_flght_dttm"
### 1. for each cpn, the snapshot records should be added, so  instead of having "NA  ", we should have "3011089  ", "3000073  " each
### 2. should include only snapshot records since the last removal date for each engine

td$comp_id = paste(td$cpn, td$msn, td$install_date, td$serlzd_eng_ser_num)
idx_na = which(is.na(td$cpn))
td[idx_na,]$comp_id = as.character(td[idx_na,]$serlzd_eng_ser_num)
td$comp_id_str = td$comp_id
td$comp_id <- as.numeric(as.factor(td$comp_id))


# ### trackdata comparison
# trdata = read.csv("./3011089_track.csv", header=T, sep=',', stringsAsFactors=FALSE)
# trdata$install_date = strptime(trdata$Install.Date, "%d%b%Y", tz="UTC")
# trdata$install_date <- as.POSIXct( trdata$install_date  )
#
# #trdata$comp_id_strr = paste(trdata$Location, trdata$CPN, trdata$MSN, trdata$install_date)
# #trdata$comp_id_strr = paste(trdata$Location, trdata$MSN, trdata$install_date)
# trdata$comp_id_strr = paste(trdata$Location, trdata$install_date)
#
# #td$comp_id_strr = paste(td$src_tl_num_substr, td$cpn, td$msn, format.Date(td$install_date, "%Y-%m-%d"))
# #td$comp_id_strr = paste(td$src_tl_num_substr, td$msn, format.Date(td$install_date, "%Y-%m-%d"))
# td$comp_id_strr = paste(td$src_tl_num_substr, format.Date(td$install_date, "%Y-%m-%d"))
#
# intersect(unique(trdata$comp_id_strr) , unique(td$comp_id_strr))
#
# tails = intersect(unique(trdata$Location), unique(td$src_tl_num_substr))
# sta=sapply(tails,function(i){ ind=which(td$src_tl_num_substr==i); indx=which(trdata$Location==i); paste(paste( unique(td[ind,]$comp_id_strr), collapse=", "), " : ", paste(unique(trdata[indx,]$comp_id_strr),collapse=", ")) })
# # format.Date(td$install_date, "%Y-%m-%d")
# unique(td[which(td$src_tl_num_substr==586),]$comp_id_strr)

### set unhealthy for all records in eventual removal
td$status = 1  # unhealthy
td[idx_na,]$status = 0  # healthy
## we set healthy only for current working engines.  Thus, all records from removed components are set to unhealthy, too many records
# > table(td$status)
# 0      1
# 221879 184904
#
# > sum(is.na(td$cpn))
# [1] 221879




td$label = 1  # unhealthy for cycle-based classification purpose
td[idx_na,]$label = 0  # healthy for cycle-based classification purpose


# esn_set = unique(td$serlzd_eng_ser_num)
# td1 = data.frame()
# for (esn_t in esn_set) {
#
#   x = td[which(td$serlzd_eng_ser_num==esn_t),]
#   if (sum(!is.na(x$cpn)) > 0) {
#     #print(nrow(x[which(!is.na(x$cpn)),]))
#     #print(unique(x[which(!is.na(x$cpn)),]$removal_date))
#     last_removal_date = max( x[which(!is.na(x$cpn)),]$removal_date )
#     #print(esn_t)
#     #print(last_removal_date)
#     x1 = x[which(x$src_flght_dttm > last_removal_date+24*3600 | !is.na(x$cpn)),]
#     td1 = rbind(td1,x1)
#   } else {
#     td1 = rbind(td1,x)
#   }
# }

# > length(unique(td$serlzd_eng_ser_num))
# [1] 114

td$sedi_ecsn = td$to_sedi_ecsn
td$sedi_etsn = td$to_sedi_etsn

# x = td[which(td$serlzd_eng_ser_num==unique(td$serlzd_eng_ser_num)[3]),]
# plot(difftime(x$src_flght_dttm, x$src_flght_dttm[1]), x$sedi_ecsn)
# adjust_sedi <- function (x) {  # for each esn data
#
#   ti = as.numeric(difftime(x$src_flght_dttm, x$src_flght_dttm[1]))
#   sedi_ecsn = x$sedi_ecsn
#   lm0 = lm(sedi_ecsn ~ ti)
#
#   uu = predict(lm0, data.frame(ti=ti ))
#   #plot(ti, uu)
#
#
#   sedi_etsn = x$sedi_etsn
#   lm1 = lm(sedi_etsn ~ ti)
#   tt = predict(lm1, data.frame(ti=ti ))
#
#   x$sedi_ecsn = uu
#   x$sedi_etsn = tt
#
#   return(x)
# }
#
# td = ddply(td, .(serlzd_eng_ser_num), adjust_sedi)
#


set_valid <- function (x) {

  if (sum(!is.na(x$cpn)) > 0) {
    #print(nrow(x[which(!is.na(x$cpn)),]))
    #print(unique(x[which(!is.na(x$cpn)),]$removal_date))
    last_removal_date = max( x[which(!is.na(x$cpn)),]$removal_date )
    #print(last_removal_date)

    x$valid = ( ( x$src_flght_dttm > last_removal_date+24*3600 | !is.na(x$cpn) ) & !is.na(x$sedi_ecsn) )

  } else {
    x$valid = 1  # "purely healthy"
  }

  return(x)
}

td$valid = 0
td = ddply(td, .(serlzd_eng_ser_num), set_valid)

td1 = td[which(td$valid==1),]

td0 = td
td = td1
# > nrow(td)
# [1] 405876
# > length(unique(td$comp_id))
# [1] 354
# > length(unique(td[td$status==0,]$comp_id))
# [1] 113
# > length(unique(td[td$status==1,]$comp_id))
# [1] 241



lim_cycles = 100
outtitle = paste('AntiIceValve_features_ceod_0707', sep='')

set_comp_cycles <- function (x, lim_cycles) {

  x$comp.cycles = x$sedi_ecsn - x[1,]$sedi_ecsn
  x$comp.times = x$sedi_etsn -  x[1,]$sedi_etsn
  x$this.cycle = c(diff(x$comp.cycles), 1)

  status_tag = x$status[1]


  x$cycles_since_install_sedi = max(x$comp.cycles, na.rm=T)
  x$times_since_install_sedi = max(x$comp.times, na.rm=T)

  if (status_tag == 1) {# unhealthy
    x$label = 0
    x$label = as.integer(x$comp.cycles[nrow(x)] - x$comp.cycles < lim_cycles)

  } else {
    x$label = 0
  }

  return(x)
}



td$comp.cycles = 0
td$comp.times = 0
td$this.cycle = 0
td$cycles_since_install_sedi = 0
td$times_since_install_sedi = 0
td = ddply(td, .(comp_id), set_comp_cycles, lim_cycles)

#td = td[which(td$this.cycle > 0 & td$comp.cycles >= 0),]
# > nrow(td)
# [1] 358383


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



#td = td[order(td$serlzd_eng_ser_num, td$src_flght_dttm),]

# > sum(is.na(td$duty_cycle))
# [1] 343275
# > sum(is.na(td$cycle))
# [1] 343275
# > sum(is.na(td$on_time))
# [1] 343275


# to_fe_list = c("to_zalt", "to_wfmp", "to_zwf36", "to_zt49", "to_ztoil_smoothed", "to_ztoil", "to_pcn1k", "to_zpcn25", "to_zpcn12", "to_ztla", "to_zpoil", "to_zpoil_smoothed", "to_degt", "to_degt_smoothed", "to_ibp1", "to_ibp2", "to_ibp3", "to_gpcn25_smoothed", "to_ibe1", "to_ibe2", "to_ibe3", "to_zxm", "to_zvb2r", "to_brat", "to_wbi", "to_zvb2r_smoothed", "to_zvb1f_smoothed", "to_zvb1f", "to_ivs12", "to_ivs13", "to_vsvnom", "to_zvb1f_d", "to_ztla_d", "to_zpcn25_d", "to_gpcn25_d", "to_gwfm", "to_gwfm_d", "to_degt_d", "to_zt49_d", "to_dpoil", "to_dpoil_smoothed", "to_zt1a", "to_sat", "to_zwf36_d_smoothed", "to_cas", "to_delfn", "to_deln1", "to_dtamb", "to_egthdm", "to_egthdm_d", "to_iai", "to_iaie", "to_iaie1", "to_iaie2", "to_iaie3", "to_iaiwg", "to_satsl", "to_satslf", "to_sedi_ecsi", "to_sedi_ecsn", "to_sedi_ecsv", "to_sedi_etsi", "to_sedi_etsn", "to_sedi_etsv", "to_sloatl", "to_sloatl_d", "to_sloatl_smoothed", "to_tatsl", "to_tatslf")
# cr_fe_list = c("cr_zalt", "cr_wfmp", "cr_zwf36", "cr_zt49", "cr_ztoil_smoothed", "cr_ztoil", "cr_pcn1k", "cr_zpcn25", "cr_zpcn12", "cr_ztla", "cr_zpoil", "cr_zpoil_smoothed", "cr_degt", "cr_degt_smoothed", "cr_ibp1", "cr_ibp2", "cr_ibp3", "cr_gpcn25_smoothed", "cr_ibe1", "cr_ibe2", "cr_ibe3", "cr_zxm", "cr_zvb2r", "cr_brat", "cr_wbi", "cr_zvb2r_smoothed", "cr_zvb1f_smoothed", "cr_zvb1f", "cr_ivs12", "cr_ivs13", "cr_vsvnom", "cr_zvb1f_d", "cr_ztla_d", "cr_zpcn25_d", "cr_gpcn25_d", "cr_gwfm", "cr_gwfm_d", "cr_degt_d", "cr_zt49_d", "cr_dpoil", "cr_dpoil_smoothed", "cr_zt1a", "cr_sat", "cr_zwf36_d_smoothed", "cr_cas", "cr_delfn", "cr_deln1", "cr_dtamb", "cr_egthdm", "cr_egthdm_d", "cr_iai", "cr_iaie", "cr_iaie1", "cr_iaie2", "cr_iaie3", "cr_iaiwg", "cr_satsl", "cr_satslf", "cr_sedi_ecsi", "cr_sedi_ecsn", "cr_sedi_ecsv", "cr_sedi_etsi", "cr_sedi_etsn", "cr_sedi_etsv", "cr_sloatl", "cr_sloatl_d", "cr_sloatl_smoothed", "cr_tatsl", "cr_tatslf")
# coed_fe_list = c("duty_cycle", "on_time", "cycle", "gross_weight_lbs_0_mean")
#
# fe_list = c(to_fe_list, cr_fe_list, coed_fe_list) # c("zalt", "wfmp", "zwf36", "zt49", "ztoil_smoothed", "ztoil", "pcn1k", "zpcn25", "zpcn12", "ztla", "zpoil", "zpoil_smoothed", "degt", "degt_smoothed", "ibp1", "ibp2", "ibp3", "gpcn25_smoothed", "ibe1", "ibe2", "ibe3", "zxm", "zvb2r", "brat", "wbi", "zvb2r_smoothed", "zvb1f_smoothed", "zvb1f", "ivs12", "ivs13", "vsvnom", "zvb1f_d", "ztla_d", "zpcn25_d", "gpcn25_d", "gwfm", "gwfm_d", "degt_d", "zt49_d", "dpoil", "dpoil_smoothed", "zt1a", "sat", "zwf36_d_smoothed", "cas", "delfn", "deln1", "dtamb", "egthdm", "egthdm_d", "iai", "iaie", "iaie1", "iaie2", "iaie3", "iaiwg", "satsl", "satslf", "sedi_ecsi", "sedi_ecsn", "sedi_ecsv", "sedi_etsi", "sedi_etsn", "sedi_etsv", "sloatl", "sloatl_d", "sloatl_smoothed", "tatsl", "tatslf")
#
# for (fe in fe_list) {
#   td[,paste(fe, "_cumavg", sep='')]=NA
#   td[,paste(fe, "_cumsum", sep='')]=NA
# }
#
# td = ddply(td, .(comp_id), set_comp_fe, fe_list)



set_failure_code <- function (x) {

  # print(x$comp_id[1])
  status_tag = x$status[nrow(x)]
  #   print(nrow(x))
  #   print('##')
  #   print(x$status)
  if (status_tag == 1) {# unhealthy
    x$failure[nrow(x)] = 1
  }

  return(x)
}



#td = td[which(td$src_flght_dttm > as.POSIXct('2013-12-31', tz='UTC')  & !is.na(td$duty_cycle)),]
#td = td[which(td$src_flght_dttm > as.POSIXct('2013-12-31', tz='UTC')  ),]


td$failure = 0
td = ddply(td, .(comp_id), set_failure_code)


set_comp_install_date <- function (x) {

  status_tag = x$status[1]

  if (status_tag == 0) {# healthy
    x$install_date = x$src_flght_dttm[1]
  }
  return(x)

}


td = ddply(td, .(comp_id), set_comp_install_date)


## Removed components
td_cpn89 = td[which(td$cpn=='3011089'),]
tu = td_cpn89

CEOD = T
OVERHAUL_ONLY = T

if (CEOD == T) {
  if (OVERHAUL_ONLY == T) {
    #tu = td_cpn89[which( !is.na(td_cpn89$duty_cycle) ),]
    #    tu = tu[which( tu$cycles_since_installation > 100  & (tu$repair_type == "O - OVERHAUL            " | tu$repair_type == "R - REPAIR              ") & !is.na(tu$duty_cycle) ),]
    #    tu = tu[which( tu$cycles_since_installation > 100  & (tu$repair_type == "O - OVERHAUL            " | tu$repair_type == "R - REPAIR              ") & !is.na(tu$duty_cycle) ),]
    td_uh = tu[which( (tu$repair_type == "O - OVERHAUL            " | tu$repair_type == "R - REPAIR              ") & !is.na(tu$duty_cycle) ),]

    #sapply(unique(tu$comp_id), function(i) { nrow(tu[which(tu$comp_id==i),])})
    #sapply( unique(tu$mpn), function(i){  length(unique(tu[which(tu$mpn==i),]$comp_id)) } )
  } else {
    td_uh =  td_cpn89[which( !is.na(td_cpn89$duty_cycle) ),]
    #tu = tu[which( tu$cycles_since_installation > 100  & !is.na(tu$duty_cycle) ),]
    #tu = tu[which( tu$cycles_since_installation > 100 ),] # & !is.na(tu$duty_cycle) ),]

    #tu = tu[which( !is.na(tu$duty_cycle) ),]

    #sapply( unique(tu$mpn), function(i){  length(unique(tu[which(tu$mpn==i),]$comp_id)) } )

  }

  #tu = tu[which(tu$mpn=='326215-4'),]


  ## Healthy components
  #td_h = td[which(is.na(td$cpn)),]  #& !is.na(td$duty_cycle) ),]
  td_h = td[which(is.na(td$cpn) & !is.na(td$duty_cycle) ),]



} else { # CEOD == F
  if (OVERHAUL_ONLY == T) {
    #tu = td_cpn89[which( !is.na(td_cpn89$duty_cycle) ),]
    #tu = tu[which( tu$cycles_since_installation > 100  & (tu$repair_type == "O - OVERHAUL            " | tu$repair_type == "R - REPAIR              ") ),]
    td_uh = tu[which( (tu$repair_type == "O - OVERHAUL            " | tu$repair_type == "R - REPAIR              ") ),]
    #sapply( unique(tu$mpn), function(i){  length(unique(tu[which(tu$mpn==i),]$comp_id)) } )
  } else {

    td_uh =  tu #[which( tu$cycles_since_installation > 100 ),]

  }

  ## Healthy components
  td_h = td[which(is.na(td$cpn)),]

}

length(unique(td_uh$comp_id))
length(unique(td_h$comp_id))



#tu = rbind(td_uh, td_h)

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


ix_numeric=sapply(names(tu), function(nm){class(tu[,nm])[1] == "numeric"  })

tu1 = tu
tu1[,ix_numeric] <- na.roughfix(tu[,ix_numeric])

#data.frame(sapply(names(tu), function(nm){sum(is.na(tu[,nm]))}), sapply(names(tu1), function(nm){sum(is.na(tu1[,nm]))}))

#td = tu1   ##  using imputation
td2 = td
td = tu


comp_ids = unique(td$comp_id)
removed_comp_ids = unique(td[td$status==1,]$comp_id)
healthy_comp_ids = setdiff(comp_ids, removed_comp_ids)

#  comp_ids = unique(td[which(is.na(td$cycles_since_installation) | (td$cycles_since_installation > 100 & td$status==1)),]$comp_id)
#  length(comp_ids)
# # # [1] 120
#  removed_comp_ids = unique(td[which(td$cycles_since_installation > 100 & td$status==1),]$comp_id)
#  length(removed_comp_ids)
# # # [1] 30
# #
# # # excluded the removed components with early-cycle removal
# # # > length(unique(td[which(td$cycles_since_installation < 100 & td$status==1),]$comp_id))
# # # [1] 10
# #
# # #early_removed_comp_ids = unique(td[which(td$cycles_since_installation < 100 & td$status==1),]$comp_id)
# #
#  healthy_comp_ids = setdiff(comp_ids, removed_comp_ids)
#  length(healthy_comp_ids)
# # # [1] 90


#####
# > comp_ids = unique(td[which(is.na(td$cycles_since_installation) | (td$cycles_since_installation > 100 & td$status==1)),]$comp_id)
# > length(comp_ids)
# [1] 112
# > removed_comp_ids = unique(td[which(td$cycles_since_installation > 100 & td$status==1),]$comp_id)
# > length(removed_comp_ids)
# [1] 20
# > healthy_comp_ids = setdiff(comp_ids, removed_comp_ids)
# >  length(healthy_comp_ids)
# [1] 92
####


# comp_ids = unique(td[which(is.na(td$cycles_since_installation) | (td$cycles_since_installation > 100 & td$comp.cycles > 100 & td$status==1)),]$comp_id)
# # > length(comp_ids)
# # [1] 119
# removed_comp_ids = unique(td[which(td$cycles_since_installation > 100 & td$comp.cycles > 100 & td$status==1),]$comp_id)
# # > length(removed_comp_ids)
# # [1] 190
#
# # excluded the removed components with early-cycle removal
# # > length(unique(td[which(td$cycles_since_installation < 100 & td$status==1),]$comp_id))
# # [1] 49
#
# #early_removed_comp_ids = unique(td[which(td$cycles_since_installation < 100 & td$status==1),]$comp_id)
#
# healthy_comp_ids = setdiff(comp_ids, removed_comp_ids)
# # > length(healthy_comp_ids)
# [1] 150




#sig_ix_u = c('delfn_cumavg', 'iaie_cumavg','pcn1k_cumavg','zpcn12_cumavg','zwf36_cumavg','cas_cumavg','zalt_cumavg','zvb2r_cumavg','zvb2r_smoothed_cumavg','iai_cumavg','ibp3_cumavg','sloatl_smoothed_cumavg','zpcn25_d_cumavg','zt1a_cumavg','zt49_d_cumavg')
#sig_ix_u = c(sig_ix_u, c('iaie_cumsum','pcn1k_cumsum','zpcn12_cumsum','zwf36_cumsum','cas_cumsum','zalt_cumsum','zvb2r_cumsum','zvb2r_smoothed_cumsum','iai_cumsum','ibp3_cumsum','sloatl_smoothed_cumsum','zpcn25_d_cumsum','zt1a_cumsum','zt49_d_cumsum'))
#sig_ix_u = paste('to_', sig_ix_u, sep='')

#sig_ix_u = c(sig_ix_u, c('iaie','pcn1k','zpcn12','zwf36','cas','zalt','zvb2r','zvb2r_smoothed','iai','ibp3','sloatl_smoothed','zpcn25_d','zt1a','zt49_d'))
#sig_ix_u = c(sig_ix_u, c('duty_cycle','on_time', 'cycle','duty_cycle_cumavg','on_time_cumavg', 'cycle_cumavg') #,'duty_cycle_cumsum','on_time_cumsum', 'cycle_cumsum'))

#sig_ix_u = names(td)[grep("cumavg",names(td))]

#sig_ix_u = c('iaie_cumavg','cas_cumavg','delfn_cumavg','zpcn12_cumavg')
#sig_ix_u = c('cas_cumavg', 'zwf36_cumavg','zpcn25_d_cumavg','iai_cumavg','delfn_cumavg')

# to_zwf36,to_iaie,to_delfn,cr_zpcn25,cr_zt1a
# on_time, duty_cycle, cycle

#sig_ix_u = c('to_zwf36_cumavg','to_iaie_cumsum','to_egthdm_cumavg','to_delfn_cumavg','cr_zpcn25_cumavg','cr_zt1a_cumavg','on_time_cumsum','duty_cycle_cumsum', 'cycle_cumsum')
dataset = td

#dataset = dataset[which(dataset$comp_id %in% healthy_comp_ids | dataset$comp_id %in% removed_comp_ids ),]

dataset = dataset[which(dataset$comp.cycles > 0 & dataset$this.cycle > 0),]


set.seed(12345)
nFolds = 2
nObs = length(unique(dataset$comp_id))
fold = rep(1:nFolds,length.out=nObs)
ids = sample(1:nObs,nObs)



dataset$fprob = rep(0, nrow(dataset))
dataset$fprob_r = rep(0, nrow(dataset))

dataset$coxrisk= rep(0, nrow(dataset))
dataset$bhaz= rep(0, nrow(dataset))
dataset$coxhaz= rep(0, nrow(dataset))

dataset$coxrisk_sefit = rep(0,nrow(dataset))
dataset$coxrisk_se = rep(0,nrow(dataset))

dataset$coxhaz_seH = rep(0,nrow(dataset))
dataset$coxhaz_seL = rep(0,nrow(dataset))

dataset$coxhaz_sefitH = rep(0,nrow(dataset))
dataset$coxhaz_sefitL = rep(0,nrow(dataset))

dataset$cum_failprob= rep(0, nrow(dataset))
#dataset$pred_class = rep(FALSE, nrow(dataset))





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

#sig_ix_u = c('comp.cycles', 'to_tatslf_cumavg',  'to_satslf_cumavg', 'cr_wfmp_cumavg', 'to_cas_cumavg', 'to_tatsl_cumavg', 'cr_zwf36_cumavg', 'to_delfn_cumavg', 'to_satsl_cumavg', 'cr_egthdm_cumavg', 'cr_zpcn25_cumavg', 'cr_zt1a_cumavg')
#sig_ix_u = as.numeric(sapply(sig_fes, function(i){ which(names(dataset)==i)}))
sig_ix_u = c('comp.cycles','to_cas_cumavg', 'cr_zwf36_cumavg', 'to_delfn_cumavg', 'cr_zpcn25_cumavg', 'cr_zt1a_cumavg')
#sig_ix_u = c('comp.cycles','to_cas_cumavg', 'cr_zwf36_cumavg', 'to_delfn_cumavg', 'cr_zpcn25_cumavg', 'cr_zt1a_cumavg', 'to_tatsl_cumsum')

#sig_ix_u=c()

if (CEOD == T) {
  sig_ix_u = c(sig_ix_u, 'duty_cycle_cumavg','on_time_cumavg', 'cycle_cumavg', 'duty_cycle_cumsum','on_time_cumsum', 'cycle_cumsum')
}

sig_ix_u = c("duty_cycle_cumavg", "gross_weight_lbs_0_mean_cumavg", "to_iaie_cumavg", "to_delfn_cumavg", "to_zxm_cumavg") #, "to_zvb1f_smoothed_cumavg")

# to_zwf36,to_iaie,to_delfn,cr_zpcn25,cr_zt1a
# on_time, duty_cycle, cycle


library(randomForest)



get_cum_failprob <- function (x, alpha=1) {

  #alpha = 1 #0.0001
  h = x$coxhaz
  H = c(h[1], h[1:(length(h)-1)]+h[-1])
  x$cum_failprob = 1-exp(-alpha*cumsum(0.5*H*x$this.cycle))

  h = x$coxhaz_sefitH
  H = c(h[1],h[1:(length(h)-1)]+h[-1])
  x$cum_failprob_sefitH = 1-exp(-alpha*cumsum(0.5*H*x$this.cycle))
  h = x$coxhaz_sefitL
  H = c(h[1],h[1:(length(h)-1)]+h[-1])
  x$cum_failprob_sefitL = 1-exp(-alpha*cumsum(0.5*H*x$this.cycle))

  h = x$coxhaz_seH
  H = c(h[1],h[1:(length(h)-1)]+h[-1])
  x$cum_failprob_seH = 1-exp(-alpha*cumsum(0.5*H*x$this.cycle))

  h = x$coxhaz_seL
  H = c(h[1],h[1:(length(h)-1)]+h[-1])
  x$cum_failprob_seL = 1-exp(-alpha*cumsum(0.5*H*x$this.cycle))

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


cross_validate <- function (cv_id) {
  #for (cv_id in 1:nFolds) {

  LIM_CYCLE = F
  if (LIM_CYCLE == T) {
    dataset$target = dataset$label
  } else {
    dataset$target = dataset$status
  }

  sel_col = c(which(names(dataset) %in% sig_ix_u), which(names(dataset)=="target"))

  tr = unique(dataset$comp_id)[ids[fold==cv_id]]
  test_ids = which(dataset$comp_id %in% tr)

  #    tr_train = unique(dataset$comp_id)[ids[fold==cv_id]]
  #    tr_test = unique(dataset$comp_id)[ids[fold!=cv_id]]
  #    table(sapply(tr_train, function(i){td[which(td$comp_id==i),]$status[1]}))
  #    table(sapply(tr_test, function(i){td[which(td$comp_id==i),]$status[1]}))


  trainset0 = dataset[-test_ids,]
  testset0 = dataset[test_ids,]

  trainset = dataset[-test_ids,sel_col]
  testset = dataset[test_ids,sel_col]


  #trainset[is.na(trainset)]=0
  #testset[is.na(testset)]=0


  ### imputation ###
  ix_numeric=sapply(names(trainset), function(nm){class(trainset[,nm])[1] == "numeric" | class(trainset[,nm])[1] == "integer"  })

  trainset1 = trainset
  trainset1[,ix_numeric] <- na.roughfix(trainset[,ix_numeric])

  testset1 = testset
  testset1[,ix_numeric] <- na.roughfix(testset[,ix_numeric])

  trainset = trainset1
  testset = testset1
  ###################


  #out_train = trainset[which(!is.na(trainset[,"status"])),ix_u,which(names(trainset)=='status'))][seq(1,nrow(out),100),]
  #out_train = trainset[which(!is.na(trainset[,"status"])), ][seq(1,nrow(trainset),10),]
  #out_train = trainset[which(!is.na(trainset[,"label"])), ][seq(1,nrow(trainset),10),]

  #   out_train1 = trainset[which(trainset[,"label"] == 1 ), ]
  #   out_train0 = trainset[which(trainset[,"label"] == 0 ), ]
  #   out_train0 = out_train0[seq(1,nrow(out_train0),20),]

  out_train1 = trainset[which(trainset$target == 1 ), ]
  out_train0 = trainset[which(trainset$target == 0 ), ]

  if (LIM_CYCLE == T) {
    out_train1 = out_train1[seq(1,nrow(out_train1),10),]
    out_train0 = out_train0[seq(1,nrow(out_train0),100),]

  } else {
    out_train1 = out_train1[seq(1,nrow(out_train1),100),]
    out_train0 = out_train0[seq(1,nrow(out_train0),100),]

  }

  out_train = rbind(out_train0, out_train1)
  #out_train_smote <- SMOTE(label ~ ., data=out_train, perc.over = 1000, perc.under=70)


  out_train$target = as.factor(out_train$target)
  table(out_train$target)

  rfm <- randomForest(target ~ ., data = out_train, importance=TRUE, mtry = 2) #, na.action=na.omit) #, classwt=c(1,5) )
  #out_train$label = as.factor(out_train$label)
  #rfm <- randomForest(label ~ ., data = out_train, importance=TRUE, mtry = 2) #, classwt=c(1,5) )


  #partialPlot(rfm, out_train, x.var = "egthdm")
  importance(rfm, type=1)
  varImpPlot(rfm,type=1)

  # library(caret)
  # forest.model1 <- train(status ~ ., data = out_train, importance=TRUE)
  # plot(varImp(forest.model1))
  # varImp(forest.model1)

  trainset0$fprob = predict(rfm, newdata=trainset, type='prob')[,2]
  testset0$fprob = predict(rfm, newdata=testset, type='prob')[,2]
  # dataset[test_ids,]$fprob = testset0$fprob




  trainset0$sum_fprob = rep(0, nrow(trainset0))
  trainset0 = ddply(trainset0, .(comp_id), get_sum_fprob)


  testset0$sum_fprob = rep(0, nrow(testset0))
  testset0 = ddply(testset0, .(comp_id), get_sum_fprob)



  #    res_end_pts = ddply(trainset0, .(comp_id), function(x){x[nrow(x),]})
  #
  #    res_colorcodes = rep('',nrow(res_end_pts))
  #    res_colorcodes[which(paste(res_end_pts$status)==0)]='blue'  # healthy
  #    res_colorcodes[which(paste(res_end_pts$status)==1)]='red'   # removed
  #
  #    print( qplot( comp.cycles, sum_fprob, data=trainset0, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, sum_fprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
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
  #    print( qplot( comp.cycles, sum_fprob, data=testset0, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, sum_fprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
  #           +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
  #    )
  #
  #    print( qplot( comp.cycles, fprob, data=testset0, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, fprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
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



  glm1 <- glm(status ~ sum_fprob + comp.cycles, data=trainset0, family=binomial)
  trainset0$fprob_r = predict(glm1, newdata=trainset0, type='response')
  testset0$fprob_r = predict(glm1, newdata=testset0, type='response')
  #
  #
  # trainset0$sum_fprob = rep(0, nrow(trainset0))
  # trainset0 = ddply(trainset0, .(comp_id), get_sum_fprob)
  #
  #
  # testset0$sum_fprob = rep(0, nrow(testset0))
  # testset0 = ddply(testset0, .(comp_id), get_sum_fprob)
  #


  COX_RUN = T

  if (COX_RUN == T) {

    start = trainset0$comp.cycles
    stop = trainset0$comp.cycles + trainset0$this.cycle
    failure = trainset0$failure

    fe1 = trainset0$fprob #trainset0$fprob  # covariate
    #fe2 = trainset0$comp.cycles
    sdd = data.frame(start,stop, failure, fe1)
    #sdd = data.frame(start,stop, failure, fe1, fe2)

    sdd=sdd[which(sdd$stop>sdd$start & sdd$start>=0),]



    coxph1 = coxph(Surv(start, stop, failure) ~  fe1 , data=sdd, method='breslow') #, na.action=na.exclude)
    #coxph1 = coxph(Surv(start, stop, failure) ~  fe1 + fe2 , data=sdd, method='breslow') #, na.action=na.exclude)

    #  print(summary(coxph1))
    #  plot(survfit(coxph1))



    # > coef(summary(coxph1))
    # coef exp(coef)  se(coef)  robust se        z     Pr(>|z|)
    # fe5 0.6618724  1.938418 0.1165586 0.09848901 6.720266 1.813927e-11
    newdata = data.frame(fe1=testset0$fprob) #, fe2=testset$comp.cycles)
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

    mhaz = with(trainset_endpts, muhaz(comp.cycles, failure))


    #plot(mhaz$est.grid, mhaz$haz.est)

    testset0$bhaz = sapply(testset0$comp.cycles, get_bhaz, mhaz)
    #trainset0$bhaz = sapply(trainset0$comp.cycles, get_bhaz, mhaz)



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
  dataset[test_ids,]$fprob_r <<- testset0$fprob_r


  # store the results in a list for return
  list(testset0$fprob, testset0$coxrisk, testset0$bhaz, testset0$coxhaz, testset0$coxrisk_se, testset0$coxrisk_sefit,  testset0$fprob_r)

}




# cl <- makeCluster(8, type = "SOCK", outfile="./dosnow.log")
# registerDoSNOW(cl)
# getDoParWorkers()
# getDoParName()
# getDoParVersion()
#
#
#
# #clusterEvalQ(cl, library(e1071))
# clusterEvalQ(cl, library(kernlab))
# #clusterEvalQ(cl, library(MASS))
# clusterEvalQ(cl, library(plyr))
# clusterEvalQ(cl, library(DMwR))  # for SMOTE
# #clusterEvalQ(cl, library(mboost)) # for glmboost
# clusterEvalQ(cl, library(survival))
# clusterEvalQ(cl, library(muhaz))
# #clusterEvalQ(cl, library(ada))
# clusterEvalQ(cl, library(randomForest))
# #clusterEvalQ(cl, library(gbm))
# #clusterEvalQ(cl, library(rpart))
#
#
#
# clusterExport(cl,c('dataset',  'get_bhaz', 'get_sum_fprob', 'ids', 'fold', 'sig_ix_u'))

#print(system.time ( ( resR = parLapply (cl, 1:nFolds,  cross_validate) ) ) )
print(system.time ( ( resR = lapply (1:nFolds,  cross_validate) ) ) )

# for (cv_id in 1:nFolds) {
#   print(cv_id)
#   cross_validate(cv_id)
# }

# print('pass')
#
#
# for (k in 1:nFolds ) {
#
#   print(k)
#
#   tr = unique(dataset$comp_id)[ids[fold==k]]
#   test_ids = which(dataset$comp_id %in% tr)
#   #trainset = dataset[-test_ids,]
#   testset = dataset[test_ids,]
#
#  #if(nrow(testset) == 1)
#   #  next
#
#   testset$fprob = resR[[k]][[1]]
# #   testset$coxrisk= resR[[k]][[2]]
# #   testset$bhaz=  resR[[k]][[3]]
# #   testset$coxhaz=  resR[[k]][[4]]
# #
# #   testset$coxrisk_se = resR[[k]][[5]]
# #   testset$coxrisk_sefit = resR[[k]][[6]]
# #  testset$fprob_r = resR[[k]][[7]]
# #
#
#   dataset[test_ids,]$fprob = testset$fprob
# #   dataset[test_ids,]$coxrisk = testset$coxrisk
# #   dataset[test_ids,]$bhaz = testset$bhaz
# #   dataset[test_ids,]$coxhaz = testset$coxhaz
# #   dataset[test_ids,]$coxrisk_se = testset$coxrisk_se
# #   dataset[test_ids,]$coxrisk_sefit = testset$coxrisk_sefit
# #  dataset[test_ids,]$fprob_r = testset$fprob_r
# #
# #   dataset[test_ids,]$coxhaz_seH = dataset[test_ids,]$bhaz * dataset[test_ids,]$coxrisk * dataset[test_ids,]$coxrisk_se
# #   dataset[test_ids,]$coxhaz_seL = dataset[test_ids,]$bhaz * dataset[test_ids,]$coxrisk / dataset[test_ids,]$coxrisk_se
# #
# #   dataset[test_ids,]$coxhaz_sefitH = dataset[test_ids,]$bhaz * (dataset[test_ids,]$coxrisk + 1.96*dataset[test_ids,]$coxrisk_sefit)
# #   dataset[test_ids,]$coxhaz_sefitL = dataset[test_ids,]$bhaz * (dataset[test_ids,]$coxrisk - 1.96*dataset[test_ids,]$coxrisk_sefit)
#
#
# }
#
# stopCluster(cl)


dataset$sum_fprob = rep(0, nrow(dataset))
dataset = ddply(dataset, .(comp_id), get_sum_fprob)

dataset$sum_fprob_r = rep(0, nrow(dataset))
dataset = ddply(dataset, .(comp_id), get_sum_fprob_r)

dataset$cum_failprob = rep(0, nrow(dataset))
dataset = ddply(dataset, .(comp_id), get_cum_failprob, 1)



#ll = lapply(unique(dataset$comp_id), function(id) { y = dataset[dataset$comp_id==id,]; y[nrow(y),]})
#res_end_pts = ldply(ll,data.frame)

res_end_pts = ddply(dataset, .(comp_id), function(x){x[nrow(x),]})

res_colorcodes = rep('',nrow(res_end_pts))
res_colorcodes[which(paste(res_end_pts$status)==0)]='blue'  # healthy
res_colorcodes[which(paste(res_end_pts$status)==1)]='red'   # removed

print( qplot( comp.cycles, sum_fprob, data=dataset, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, sum_fprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
       +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
)

print( qplot( comp.cycles, sum_fprob_r, data=dataset, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, sum_fprob_r), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
       +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
)

print( qplot( comp.cycles, fprob, data=dataset, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, fprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
       +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
)

print( qplot( comp.cycles, fprob_r, data=dataset, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, fprob_r), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
       +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
)


print( qplot( comp.cycles, cum_failprob, data=dataset, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, cum_failprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
       +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
)

dataset$pred_class = rep(0, nrow(dataset))

ddply(dataset, .(comp_id), function(x){ sum(x$fprob_r > 0.5) > 0 } )

predi =sapply(res_end_pts$comp_id, function(i){ x=dataset[which(dataset$comp_id==i),]; return(sum(x$fprob_r > 0.5) > 0 )  })


glm_f <- glm(status ~ cum_failprob + comp.cycles, data=dataset, family=binomial)
dataset$predi_f = predict(glm_f, newdata=dataset, type='response')


glm_f <- glm(status ~ cum_failprob + comp.cycles, data=res_end_pts, family=binomial)
dataset$predi_f = predict(glm_f, newdata=dataset, type='response')
res_end_pts$predi_f = predict(glm_f, newdata=res_end_pts, type='response')
#



print( qplot( comp.cycles, predi_f, data=dataset, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, predi_f), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
       +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
)

predi_ff =sapply(res_end_pts$comp_id, function(i){ x=dataset[which(dataset$comp_id==i),]; return(sum(x$predi_f > 0.55) > 0 )  })

table(res_end_pts$status, predi_ff)
# > predi_ff =sapply(res_end_pts$comp_id, function(i){ x=dataset[which(dataset$comp_id==i),]; return(sum(x$predi_f > 0.55) > 0 )  })
#
# > table(res_end_pts$status, predi_ff)
# predi_ff
# FALSE TRUE
# 0    63   50
# 1    66  110


pred = prediction(dataset$cum_failprob, dataset$status)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)

pred = prediction(res_end_pts$sum_fprob, res_end_pts$status)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)


# > table(res_end_pts$status,res_end_pts$sum_fprob > 100 & res_end_pts$comp.cycles < 2000)
#
# FALSE TRUE
# 0    99   49
# 1    52  130

# > table(dataset$status, dataset$cum_failprob > 0.25 & dataset$comp.cycles < 2000)
#
#     FALSE   TRUE
# 0 195465 151548
# 1  64149 101413

# > table(res_end_pts$status,res_end_pts$cum_failprob > 0.25 & res_end_pts$comp.cycles < 2000)
#
#   FALSE TRUE
# 0    99   49
# 1    70  112



# > length(unique(td$comp_id))
# [1] 384
# > sum(td$failure==1)
# [1] 236
# > length(unique(td[td$status==1,]$comp_id))
# [1] 236

# > length(unique(td[td$status==1,]$serlzd_eng_ser_num))
# [1] 105
# > length(unique(td[td$status==0,]$serlzd_eng_ser_num))
# [1] 149
# > length(unique(td$serlzd_eng_ser_num))
# [1] 149
# > length(unique(td[td$status==0,]$comp_id))
# [1] 330


check_ecsn <- function (x) {

  print(sum(diff(x$sedi_ecsn) < 0)/nrow(x))
  return(x)
}


td = ddply(td, .(serlzd_eng_ser_num), check_ecsn)

COXPH_UNI = F

if (COXPH_UNI == T) {

  dataset = td

  start = dataset$comp.cycles
  stop = dataset$comp.cycles + dataset$this.cycle
  failure = dataset$failure


  pdf( paste('./', outtitle, '_univariate_coxph.pdf', sep='') )

  ix_start = which(names(dataset)=='agw')
  ix_end = which(names(dataset)=='tatslf')
  loop_index = c(ix_start:ix_end)


  if (takeoff_or_cruise == 'TAKEOFF') {
    ## ix_c1, ix_c2 are only for takeoff
    ix_c1 = which(names(dataset)=='comp.cycles')
    ix_c2 = which(names(dataset)=='comp.times')
    loop_index = c(ix_start:ix_end, ix_c1, ix_c2 )
  }


  loop_index = setdiff(loop_index, which(names(dataset)=='engine_position'))

  coxout = c()

  ix_u = c()
  for (ix in loop_index) {

    fe_name = names(dataset)[ix]
    fe = dataset[,fe_name]
    fe0 = fe

    if (class(fe)[1] == 'character' ||  class(fe)[1] == 'POSIXlt'   ||  class(fe)[1] == 'POSIXct'   ||  class(fe)[1] == 'POSIXt'  ||  class(fe)[1] == 'logical' || sum(!is.na(fe)) <= 1 || sum(!is.na(fe)) <= 1)
      next

    print(fe_name)
    ix_u = c(ix_u, ix)

    median_val = median(fe0, na.rm=T)
    fe = rep(0, length(fe0))
    fe [fe0 > median_val] = 1


    sdd = data.frame(start,stop, failure, fe)

    sdd=sdd[which(sdd$stop>sdd$start & sdd$start>=0),]

    if (sum(!is.na(sdd$fe) & sdd$failure==1) == 0)
      next

    #  print(fe_name)
    #  print(table(fe))

    titlestr0 = paste(fe_name, " : ", "feature = 0")
    titlestr1 = paste(fe_name, " : ", "feature = 1")


    coxph1 = coxph(Surv(start, stop, failure) ~  fe , data=sdd, method='breslow') #, na.action=na.exclude)
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


    cox_zph = cox.zph(coxph1)
    zph_pvalue = cox_zph$table[1,"p"]



    fit_fe = survfit(Surv(start, stop, failure) ~  fe, data=sdd)
    plot(fit_fe,  conf.int=T, main=paste(fe_name, " (survfit)"))

    #   n_ev_fe0 = sum(fit_fe[1]$n.event)
    #   n_ev_fe1 = sum(fit_fe[2]$n.event)
    #   print(paste("n_ev_fe0:", n_ev_fe0))
    #   print(paste("n_ev_fe1:", n_ev_fe1))

    #   sf_fe0 = survfit(Surv(start, stop, failure) ~  fe, data=sdd, subset=(fe==0) )
    #   plot(sf_fe0, main=titlestr0)
    #   sf_fe1 =  survfit(Surv(start, stop, failure) ~  fe, data=sdd, subset=(fe==1) )
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
  write.csv(coxout, paste('./', outtitle, '_univariate_coxph.csv', sep='') , row.names=F)

  dev.off()

}




comp_ids = unique(td$comp_id)
length(comp_ids)

removed_comp_ids = unique(td[which(td$status==1),]$comp_id)
length(removed_comp_ids)
# # [1] 31
#
healthy_comp_ids = setdiff(comp_ids, removed_comp_ids)
length(healthy_comp_ids)
# # [1] 118


out = td


MEAN_VALUE_ANALYSIS == T

if (MEAN_VALUE_ANALYSIS == T) {

  out_F = out[out$status==F,]
  out_T = out[out$status==T,]

  # den_F <- density(out_F$egthdm,na.rm=T)
  # den_T <- density(out_T$egthdm,na.rm=T)
  # den_F <- density(out_F$ecsn,na.rm=T)
  # den_T <- density(out_T$ecsn,na.rm=T)
  # plot(den_F, col='blue')
  # lines(den_T, col='red')
  #
  # hist(out_F$egthdm, col='blue')
  # hist(out_T$egthdm, col='red', add=T)


  pdf( paste('./', outtitle, '_to_status_0721_ceod.pdf', sep='') )

  #ix_start = which(names(out)=='to_agw')
  #ix_end = which(names(out)=='tatslf')

  loop_index = c(which(names(out)=='time_since_installation'):which(names(out)=='days_since_installation') )
  loop_index = c(loop_index, which(names(out)=='to_agw'):which(names(out)=='to_zxm'))
  loop_index = c(loop_index, which(names(out)=='cr_agw'):which(names(out)=='cr_zxm'))
  loop_index = c(loop_index, which(names(out)=='airspeed_calibrated_1_or_only_0_mean'):which(names(out)=='wing_anti_ice_1_third_qtr'))
  loop_index = c(loop_index, which(names(out)=='duty_cycle'):which(names(out)=='cycle'))
  loop_index_wo_sum = loop_index

  loop_index = c(loop_index, which(names(out)=='comp.cycles'):which(names(out)=='cycle_cumsum'))

  #loop_index_wo_sum = c(loop_index_wo_sum, which(names(out)=='comp.cycles'):which(names(out)=='times_since_install_sedi'))


  classout = sapply(names(out), function(i) {class(out[,i])[1]})

  loop_index = intersect(loop_index,which(classout == 'numeric' | classout == 'integer'))

  loop_index_wo_sum = intersect(loop_index_wo_sum,which(classout == 'numeric' | classout == 'integer'))

  #loop_index = c(grep("_cumavg", names(out)))  #c(ix_start:ix_end)  grep("_cumsum", names(out)),


  # if (takeoff_or_cruise == 'TAKEOFF') {
  #   ## ix_c1, ix_c2 are only for takeoff
  #   ix_c1 = which(names(out)=='comp.cycles')
  #   ix_c2 = which(names(out)=='comp.times')
  #   loop_index = c(ix_start:ix_end, ix_c1, ix_c2 )
  # }


  #loop_index = setdiff(loop_index, which(names(out)=='engine_position'))



  ####  GROUP-WISE Mean-value Analysis


  statout = c()
  ix_u = c()
  for (ix in loop_index) {

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

    bdata = data.frame(val=c(val_F,val_T), status=c(out_F[,"status"],out_T[,"status"]))
    boxplot(val~status, data=bdata, outline=F, range=1.5, main= fe_name  )
    stats_F <- quantile(val_F, c(.05,.25,.5,.75,.95), na.rm = TRUE)
    stats_T <- quantile(val_T, c(.05,.25,.5,.75,.95), na.rm = TRUE)
    points(1:2, c(mean_F, mean_T), pch = 23, cex = 0.75, bg = "red")

    dprime = abs(mean(val_F,na.rm=T)-mean(val_T,na.rm=T))/sqrt(sd(val_F,na.rm=T)^2 + sd(val_T,na.rm=T)^2)

    #   glm_b <- glm(status ~ val, data=bdata, family=binomial)
    #   bdata$fprob = predict(glm_b, newdata=bdata, type='response')
    #
    #
    #   pred = prediction(bdata$fprob, bdata$status)
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
  write.csv(statout, paste('./', outtitle, '_stat_status_0721_ceod.csv', sep='') , row.names=F)





  ####  COMPONENT-WISE Mean-value Analysis

  ix_u = loop_index_wo_sum

  mean_val <- function(x, ix_u){

    out_F = x[x$status==F,]
    out_T = x[x$status==T,]
    #ix_u = names(out)[ix_u]
    #print(out_F[,ix_u])
    mean_F = colMeans(out_F[,ix_u], na.rm=T)
    mean_T = colMeans(out_T[,ix_u], na.rm=T)

    return (list( mF=mean_F, mT=mean_T, c_id = x$comp_id[1] ))

  }

  mean_vals = dlply(out, .(comp_id), mean_val, ix_u)

  library(foreach)
  mean_vals_T0 <- foreach(i=1:length(unique(out$comp_id)), .multicombine=FALSE) %do% {
    return( mean_vals[[i]]$mT )
  }
  mean_vals_c_id <- foreach(i=1:length(unique(out$comp_id)), .multicombine=FALSE) %do% {
    return( mean_vals[[i]]$c_id )
  }

  mean_vals_T = ldply(mean_vals_T0)
  mean_vals_T$comp_id = as.numeric(unlist(ldply(mean_vals_c_id))) #ldply(mean_vals_c_id)

  mean_vals_F0 <- foreach(i=1:length(unique(out$comp_id)), .multicombine=FALSE) %do% {
    return(mean_vals[[i]]$mF)
  }

  mean_vals_F = ldply(mean_vals_F0)
  mean_vals_F$comp_id = as.numeric(unlist(ldply(mean_vals_c_id))) #ldply(mean_vals_c_id)

  pdf( paste('./', outtitle, '_mean_0721_ceod.pdf', sep='') )

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


    bdata = data.frame(val=c(val_F,val_T), status=c(rep(F, length(val_F)),rep(T, length(val_T))))
    boxplot(val~status, data=bdata, outline=F, range=1.5, main= fe_name  )
    stats_F <- quantile(val_F, c(.05,.25,.5,.75,.95), na.rm = TRUE)
    stats_T <- quantile(val_T, c(.05,.25,.5,.75,.95), na.rm = TRUE)
    points(1:2, c(mean_F, mean_T), pch = 23, cex = 0.75, bg = "red")

    dprime = abs(mean(val_F,na.rm=T)-mean(val_T,na.rm=T))/sqrt(sd(val_F,na.rm=T)^2 + sd(val_T,na.rm=T)^2)

    #   glm_b <- glm(status ~ val, data=bdata, family=binomial)
    #   bdata$fprob = predict(glm_b, newdata=bdata, type='response')


    #   pred = prediction(bdata$fprob, bdata$status)
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
  write.csv(statout, paste('./', outtitle, '_mean_0721_ceod.csv', sep='') , row.names=F)

  dev.off()

}




outtr = rbind(mean_vals_F[which(unique(out$comp_id) %in% healthy_comp_ids),], mean_vals_T[which(unique(out$comp_id) %in% removed_comp_ids),])
outtr$status =c(rep(0, length(healthy_comp_ids)),rep(1, length(removed_comp_ids)))

# > hist(outtr[which(outtr$status==1),]$cycles_since_installation)
# > hist(outtr[which(outtr$status==1),]$comp.cycles)
# > hist(outtr[which(outtr$status==0),]$comp.cycles)

#plot(dataset[which(dataset$status==0),]$comp.cycles,dataset[which(dataset$status==0),]$duty_cycle)


# dataset = outtr
# #dataset[is.na(dataset)]=0
# dataset$fprob = 0
#
# dataset$status = as.factor(dataset$status)

## imputation ###
# v_indx = sapply(names(dataset), function(i) { length(which(is.na(dataset[,i]))) != nrow(dataset)})
# dataset1 = dataset[,v_indx]
#
# ix_numeric=sapply(names(dataset1), function(nm){class(dataset1[,nm])[1] == "numeric"  })
#
# dataset1[,ix_numeric] <- na.roughfix(dataset1[,ix_numeric])
#
# dataset = dataset1
##################

aidx = which(out$comp_id==214)
out[aidx,]$sedi_ecsn[1] = 12044
out[aidx,]$comp.cycles = out[aidx,]$sedi_ecsn - out[aidx[1],]$sedi_ecsn
out[aidx,]$comp.times = out[aidx,]$sedi_etsn -  out[aidx[1],]$sedi_etsn
out[aidx,]$this.cycle = c(diff(out[aidx,]$comp.cycles), 1)

out[aidx,]$cycles_since_install_sedi = max(out[aidx,]$comp.cycles, na.rm=T)
out[aidx,]$times_since_install_sedi = max(out[aidx,]$comp.times, na.rm=T)












ds = out  # for all cycles (not just endpoint)


### imputation ###
ix_numeric=sapply(names(ds), function(nm){class(ds[,nm])[1] == "numeric" | class(ds[,nm])[1] == "integer"  })

ds1 = ds
ds1[,ix_numeric] <- na.roughfix(ds[,ix_numeric])

ds = ds1
###


# component-wise imputation
#
# ix_numeric=sapply(names(ds), function(nm){class(ds[,nm])[1] == "numeric" | class(ds[,nm])[1] == "integer"  })
#
# impute_comp <- function(x, ix_numeric) {
#   x1 = x
#   x1[,ix_numeric] <- na.roughfix(x[,ix_numeric])
#
#   x = x1
#   return(x)
# }
#
# ds = ddply(ds, .(comp_id), impute_comp, ix_numeric )
# ###


### feature recalculation based on imputated data
for (fe in fe_list) {
  ds[,paste(fe, "_cumavg", sep='')]=NA
  ds[,paste(fe, "_cumsum", sep='')]=NA
}

fe_list = c("duty_cycle", "gross_weight_lbs_0_mean", "to_iaie", "to_delfn", "to_zxm") #, "to_zvb1f_smoothed_cumavg")
ds = ddply(ds, .(comp_id), set_comp_fe, fe_list)






ds$status <- as.factor(ds$status)

ds = ds[which(ds$comp.cycles > 0 & ds$this.cycle > 0),]
ds = ds[which(ds$comp_id != 295),]  # sedi_ecsn, comp.cycles wrong

#dataset = dataset[which(dataset$comp_id %in% unique(ds$comp_id)),]

#ds_end_pts[order(ds_end_pts$comp_id),]$duty_cycle_cumavg
#outtr[order(outtr$comp_id),]$duty_cycle

set_unhealthy_failure <-function (x){
  if(x$status[1]==1) {
    x$failure[nrow(x)]=1
  }
  return(x)
}
ds = ddply(ds, .(comp_id), set_unhealthy_failure )

ds_end_pts = ddply(ds, .(comp_id), function(x){x[nrow(x),]})

#ds_end_pts[which(ds_end_pts$failure != ds_end_pts$status),]$comp_id

dataset = ds_end_pts
dataset$fprob = rep(0, nrow(dataset))

set.seed(12)
nFolds = 2 # length(unique(ds$comp_id)) #10
nObs = length(unique(ds$comp_id))
fold = rep(1:nFolds,length.out=nObs)
ids = sample(1:nObs,nObs)

# > sapply(1:10, function(i){ sum(td[which(td$comp_id %in% ids[fold==i]),]$failure) })
# [1] 21 28 20 25 25 24 25 24 23 24
# > sapply(1:10, function(i){ sum(td[which(td$comp_id %in% ids[fold==i]),]$status) })
# [1] 3242 2458 2844 4989 3249 5762 2158 4686 2338 2534


ds$fprob = rep(0, nrow(ds))
ds$fprob_r = rep(0, nrow(ds))

ds$coxrisk= rep(0, nrow(ds))
ds$bhaz= rep(0, nrow(ds))
ds$coxhaz= rep(0, nrow(ds))

ds$coxrisk_sefit = rep(0,nrow(ds))
ds$coxrisk_se = rep(0,nrow(ds))

ds$coxhaz_seH = rep(0,nrow(ds))
ds$coxhaz_seL = rep(0,nrow(ds))

ds$coxhaz_sefitH = rep(0,nrow(ds))
ds$coxhaz_sefitL = rep(0,nrow(ds))

ds$cum_failprob= rep(0, nrow(ds))
#ds$pred_class = rep(FALSE, nrow(ds))



CV_FLAG = T

if (CV_FLAG == T) {

  set.seed(1234)

  # selected with SMOTE
  #sig_fe_list = c("on_time", "n2_0_mean", "apu_n1_0_mean", "gross_weight_lbs_0_mean", "egt_average_0_mean", "cr_sloatl_smoothed", "selected_altitude_mcp_0_mean", "wind_direction_true_0_mean")
  #sig_fe_list = c("on_time", "n2_0_mean", "gross_weight_lbs_0_mean", "egt_average_0_mean", "selected_altitude_mcp_0_mean")
  sig_fe_list = c("duty_cycle_cumavg", "gross_weight_lbs_0_mean_cumavg", "to_iaie_cumavg", "to_delfn_cumavg", "to_zxm_cumavg") #, "to_zvb1f_smoothed_cumavg")

  #     library(corrplot)
  #     M <- cor(dataset[,sig_fe_list])
  #     corrplot(M, method = "ellipse")

  # selected without SMOTE
  #sig_fe_list = c("wind_direction_true_0_mean", "gross_weight_lbs_0_mean",    "to_iaie",  "apu_n1_0_mean", "n2_0_mean", "to_zvb1f_d" ,"on_time", "cr_agw")
  #sig_fe_list = c("gross_weight_lbs_0_mean",    "to_iaie",  "n2_0_mean", "to_zvb1f_d" ,"on_time" ,  "to_zwf36", "to_delfn","cr_zpcn25","cr_zt1a")

  #     "egt_average_0_mean", "cr_sloatl_smoothed", "selected_altitude_mcp_0_mean"
  #     "to_iaie",  "to_zvb1f_d", "cr_agw"

  #   sig_fe_list = c("to_zwf36","to_iaie","to_delfn","cr_zpcn25","cr_zt1a", "on_time", "duty_cycle", "cycle")


  COXPH_DIRECT = F     # with or without RF (F = with RF)
  END_POINT_MODEL = F  # for randomforest   : F = cycle-based model
  COX_TIMEDEP = F  # cox independent
  USE_CYCLE_SAMPLES = T


  for (k in 1:nFolds) {


    tr = unique(dataset$comp_id)[ids[fold==k]]
    #test_ids = which(comp_ids %in% tr)
    #print(tr)
    test_ids = which(dataset$comp_id %in% tr)
    trainset = dataset[-test_ids, c("status", sig_fe_list)]
    testset = dataset[test_ids,  c("status", sig_fe_list)]

    trainset0 <- dataset[-test_ids, ]
    testset0 <- dataset[test_ids, ]

    ds_test_ids = which(ds$comp_id %in% tr)
    train0 <- ds[-ds_test_ids,]
    test0 <- ds[ds_test_ids,]

    trainset$status = as.factor(trainset$status)
    trainset0$csi = trainset0$cycles_since_install_sedi


    #train_smote <- SMOTE(status ~ ., data=trainset, perc.over = 200, perc.under=200, k=3)
    #trainset <- train_smote


    if (COXPH_DIRECT == F) {

      if (END_POINT_MODEL == T) {
        # end-point model
        rfm1 <- randomForest(status ~ ., data = trainset, importance=TRUE, mtry = 4)

      } else {
        # cycle-samples model
        train <- ds[-ds_test_ids, c("status", sig_fe_list)]
        train_uh = train[which(train$status==1),]
        train_h = train[which(train$status==0),]
        train_uhh = rbind(train_uh, train_h[seq(1,nrow(train_h),10),])
        rfm1 <- randomForest(status ~ ., data = train_uhh, importance=TRUE, mtry = 4)
      }

      test0$fprob = predict(rfm1, newdata=test0, type='prob')[,2]
      train0$fprob = predict(rfm1, newdata=train0, type='prob')[,2]

      trainset$fprob = predict(rfm1, newdata=trainset, type='prob')[,2]
      testset$fprob = predict(rfm1, newdata=testset, type='prob')[,2]

      trainset0$fprob = trainset$fprob
      testset0$fprob = testset$fprob

      if (COX_TIMEDEP == F) {

        #qplot(cycles_since_install_sedi, fprob, data=trainset0, colour=paste(status))
        #qplot(cycles_since_install_sedi, fprob, data=testset0, colour=paste(status))


        ## COX PH
        csi = trainset0$csi
        status = as.factor(trainset$status)
        fe = trainset$fprob

        sdd = data.frame(csi, status, fe)

        #sdd=sdd[which(sdd$csi>0),]


        coxph1 = coxph(Surv(csi,  status==1 ) ~ fe, data=sdd)
        print(summary(coxph1))




      } else {  # COX_TIMEDEP == T


        start = train0$comp.cycles
        stop = train0$comp.cycles + train0$this.cycle
        failure = train0$failure
        fe = train0$fprob

        sdd = data.frame(start, stop, failure,  fe)

        sdd=sdd[which(sdd$stop>sdd$start & sdd$start>=0),]

        coxph1 = coxph(Surv(start, stop, failure) ~  . , data=sdd, method='breslow') #, na.action=na.exclude)


      }

      dataset[-test_ids,]$fprob = trainset$fprob
      dataset[test_ids,]$fprob = testset$fprob

      ds[-ds_test_ids,]$fprob = train0$fprob
      ds[ds_test_ids,]$fprob = test0$fprob



    } else {  # COXPH_DIRECT == T

      trainset0 = ddply(train0, .(comp_id), function(x){x[nrow(x),]})


      trainset0$csi = trainset0$cycles_since_install_sedi

      coxvars = c("status","csi","duty_cycle_cumavg", "gross_weight_lbs_0_mean_cumavg", "to_iaie_cumavg", "to_delfn_cumavg", "to_zxm_cumavg")

      #       > trainset0[which(trainset0$csi==-Inf),]$comp_id
      #       [1] 295
      #       > trainset0[which(trainset0$comp_id==295),]
      # > plot(train0[which(train0$comp_id==295),]$sedi_ecsn)
      # > x=train0[which(train0$comp_id==295),]
      # > plot(difftime(x$src_flght_dttm, x$src_flght_dttm[1]), x$sedi_ecsn)

      #sdd=sdd[which(sdd$csi>0),]


      if (COX_TIMEDEP == F) {

        sdd = trainset0[, coxvars]

        coxph1 = coxph(Surv(csi,  status==1 ) ~ . , data=sdd)
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

        start = train0$comp.cycles
        stop = train0$comp.cycles + train0$this.cycle
        failure = train0$failure

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

          duty_cycle_cumavg = train0$duty_cycle_cumavg
          gross_weight_lbs_0_mean_cumavg = train0$gross_weight_lbs_0_mean_cumavg
          to_iaie_cumavg = train0$to_iaie_cumavg
          to_delfn_cumavg = train0$to_delfn_cumavg
          to_zxm_cumavg = train0$to_zxm_cumavg


       # sdd = data.frame(start, stop, failure,   duty_cycle_cumavg, gross_weight_lbs_0_mean_cumavg, to_iaie_cumavg, to_delfn_cumavg, to_zxm_cumavg)
        #sdd = data.frame(start, stop, failure,   duty_cycle_cumavg, gross_weight_lbs_0_mean_cumavg,  to_delfn_cumavg, to_zxm_cumavg)
         sdd = data.frame(start, stop, failure,  gross_weight_lbs_0_mean_cumavg,  to_delfn_cumavg)

        #sdd = data.frame(start,stop, failure, fe1, fe2)

        sdd=sdd[which(sdd$stop>sdd$start & sdd$start>=0),]

        coxph1 = coxph(Surv(start, stop, failure) ~  . , data=sdd, method='breslow') #, na.action=na.exclude)
        summary(coxph1)

      }

    }


    set_endrisk_v <- function (x, pred_fit, pred_sefit, set_comp_id) {

      idx = which(set_comp_id == x$comp_id[1])
      x$coxrisk = pred_fit[idx]
      x$coxrisk_sefit = pred_sefit[idx]

      return(x)
    }


    set_endrisk_v0 <- function (x, pred_fit, pred_sefit, set_comp_id) {

      idx = max(which(set_comp_id == x$comp_id[1]))
      x$coxrisk = pred_fit[idx]
      x$coxrisk_sefit = pred_sefit[idx]

      return(x)
    }

    set_vals_init_samples <- function (x, coxvars, nsamples=500) {

      for (v in coxvars) {
        if (v=='csi' || v=='status')
          next

        nn = ifelse(nrow(x) < nsamples, nrow(x), nsamples) # nrow(x) #
        x[1:nn,v] = x[nn,v]

      }

      return(x)
    }


    if (COXPH_DIRECT == T) {


      if (USE_CYCLE_SAMPLES == T) {
        test0$csi = test0$comp.cycles #cycles_since_install_sedi
      #  sddtest = test0[,coxvars]

                sddtest = test0[,c("comp_id", coxvars)]
                sddtest = ddply(sddtest, .(comp_id), set_vals_init_samples, coxvars, 1000)
                sddtest = sddtest[,-which(names(sddtest) == "comp_id")]

        pred = predict(coxph1, newdata=sddtest, type="risk" , reference='sample', se.fit=TRUE)


        test0$coxrisk = pred$fit
        test0$coxrisk_sefit = pred$se.fit
        testset0 = ddply(testset0, .(comp_id), set_endrisk_v0,  pred$fit, pred$se.fit, test0$comp_id )


        train0$csi = train0$comp.cycles #cycles_since_install_sedi
       # sddtrain = train0[,coxvars]

                sddtrain = train0[,c("comp_id", coxvars)]
                sddtrain = ddply(sddtrain, .(comp_id), set_vals_init_samples, coxvars, 500)
                sddtrain = sddtrain[,-which(names(sddtrain) == "comp_id")]

        pred_train = predict(coxph1, newdata=sddtrain, type="risk" , reference='sample', se.fit=TRUE)


        train0$coxrisk = pred_train$fit
        train0$coxrisk_sefit = pred_train$se.fit
        trainset0 = ddply(trainset0, .(comp_id), set_endrisk_v0,  pred_train$fit, pred_train$se.fit, train0$comp_id )


      } else {

        testset0$csi = testset0$cycles_since_install_sedi
        sddtest = testset0[,coxvars]

        pred = predict(coxph1, newdata=sddtest, type="risk" , reference='sample', se.fit=TRUE)

        testset0$coxrisk = pred$fit
        testset0$coxrisk_sefit = pred$se.fit

        test0 = ddply(test0, .(comp_id), set_endrisk_v,  pred$fit, pred$se.fit, testset0$comp_id )


        trainset0$csi = trainset0$cycles_since_install_sedi
        sddtrain = trainset0[,coxvars]

        pred_train = predict(coxph1, newdata=sddtrain, type="risk" , reference='sample', se.fit=TRUE)

        trainset0$coxrisk = pred_train$fit
        trainset0$coxrisk_sefit = pred_train$se.fit

        train0 = ddply(train0, .(comp_id), set_endrisk_v,  pred_train$fit, pred_train$se.fit, trainset0$comp_id )

      }




    } else { ## COXPH_DIRECT == F

      if (USE_CYCLE_SAMPLES == T) {


#         get_max_fprob <- function (x) {
#
#           x$max_fprob = cummax(x$fprob)
#
#           return(x)
#         }
#
#         test0$max_fprob = 0
#         test0 = ddply(test0, .(comp_id), get_max_fprob )



        pred = predict(coxph1, newdata=data.frame(fe=test0$fprob), type="risk" , reference='sample', se.fit=TRUE)



        test0$coxrisk = pred$fit
        test0$coxrisk_sefit = pred$se.fit
        testset0 = ddply(testset0, .(comp_id), set_endrisk_v0,  pred$fit, pred$se.fit, test0$comp_id )


#        train0$max_fprob = 0
#         train0 = ddply(train0, .(comp_id), get_max_fprob )

        pred_train = predict(coxph1, newdata=data.frame(fe=train0$fprob), type="risk" , reference='sample', se.fit=TRUE)

        train0$coxrisk = pred_train$fit
        train0$coxrisk_sefit = pred_train$se.fit
        trainset0 = ddply(trainset0, .(comp_id), set_endrisk_v0,  pred_train$fit, pred_train$se.fit, train0$comp_id )



      } else {

        # > coef(summary(coxph1))
        # coef exp(coef)  se(coef)  robust se        z     Pr(>|z|)
        # fe5 0.6618724  1.938418 0.1165586 0.09848901 6.720266 1.813927e-11
        newdata = data.frame(fe=testset0$fprob) #, fe2=testset$comp.cycles)
        pred = predict(coxph1, newdata=newdata, type="risk" , reference='sample', se.fit=TRUE)
        ## reference='sample' is needed to compute  exp(coef(coxph1)*(newdata$fe5- overall_sample_mean)) where overall_sample_mean = mean(newdata$fe5)
        ## otherwise, default is reference='strata' that uses strata mean for each strata

        testset0$coxrisk = pred$fit
        testset0$coxrisk_sefit = pred$se.fit

        test0 = ddply(test0, .(comp_id), set_endrisk_v,  pred$fit, pred$se.fit, testset0$comp_id )

        pred_train = predict(coxph1, newdata=data.frame(fe=trainset0$fprob), type="risk" , reference='sample', se.fit=TRUE)


        trainset0$coxrisk = pred_train$fit
        trainset0$coxrisk_sefit = pred_train$se.fit

        train0 = ddply(train0, .(comp_id), set_endrisk_v,  pred_train$fit, pred_train$se.fit, trainset0$comp_id )

      }

    }






    fe_names = rownames(coef(summary(coxph1)))


    #     if (length(fe_names)==1) {
    #       testset0$coxrisk_se = exp( 1.96*coef(summary(coxph1))[,'se(coef)'] * ( newdata[,fe_names]- mean(newdata[,fe_names]) ) )
    #     } else {
    #       testset0$coxrisk_se = t(exp( 1.96*coef(summary(coxph1))[,'se(coef)'] %*% t( newdata[,fe_names]- t(matrix(rep(colMeans(newdata[,fe_names]),nrow(newdata)),ncol=nrow(newdata))) )) )
    #     }
    #



    #mhaz = with(trainset0[, coxvars], muhaz(csi, status==1))
    mhaz = with(trainset0[, c("csi","status")], muhaz(csi, status==1))


    #plot(mhaz$est.grid, mhaz$haz.est)

    test0$bhaz = sapply(test0$comp.cycles, get_bhaz, mhaz)
    train0$bhaz = sapply(train0$comp.cycles, get_bhaz, mhaz)


    test0$coxhaz= test0$bhaz * test0$coxrisk
    train0$coxhaz= train0$bhaz * train0$coxrisk


    train0$cum_failprob = rep(0, nrow(train0))
    train0 = ddply(train0, .(comp_id), get_cum_failprob, 0.1)


    res_end_pts = ddply(train0, .(comp_id), function(x){x[nrow(x),]})

    res_colorcodes = rep('',nrow(res_end_pts))
    res_colorcodes[which(paste(res_end_pts$status)==0)]='blue'  # healthy
    res_colorcodes[which(paste(res_end_pts$status)==1)]='red'   # removed

    print( qplot( comp.cycles, fprob, data=train0, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, fprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
           +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    )

    print( qplot( comp.cycles, coxrisk, data=train0, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, coxrisk), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
           +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    )


    print( qplot( comp.cycles, coxhaz, data=train0, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, coxhaz), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
           +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    )


    print( qplot( comp.cycles, cum_failprob, data=train0, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, cum_failprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
           +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    )





test0$cum_failprob = rep(0, nrow(test0))
test0 = ddply(test0, .(comp_id), get_cum_failprob, 0.1)


res_end_pts = ddply(test0, .(comp_id), function(x){x[nrow(x),]})

res_colorcodes = rep('',nrow(res_end_pts))
res_colorcodes[which(paste(res_end_pts$status)==0)]='blue'  # healthy
res_colorcodes[which(paste(res_end_pts$status)==1)]='red'   # removed

print( qplot( comp.cycles, fprob, data=test0, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, fprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
       +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
)

print( qplot( comp.cycles, coxrisk, data=test0, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, coxrisk), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
       +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
)

print( qplot( comp.cycles, coxhaz, data=test0, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, coxhaz), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
       +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
)

print( qplot( comp.cycles, cum_failprob, data=test0, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, cum_failprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
       +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
)



    ds[-ds_test_ids,]$bhaz = train0$bhaz
    ds[ds_test_ids,]$bhaz = test0$bhaz

    ds[-ds_test_ids,]$coxrisk = train0$coxrisk
    ds[ds_test_ids,]$coxrisk = test0$coxrisk

    ds[-ds_test_ids,]$coxhaz = train0$coxhaz
    ds[ds_test_ids,]$coxhaz = test0$coxhaz



    #     test0$coxhaz_seH = test0$bhaz * test0$coxrisk * test0$coxrisk_se
    #     test0$coxhaz_seL = test0$bhaz * test0$coxrisk / test0$coxrisk_se
    #
    #     test0$coxhaz_sefitH = test0$bhaz * (test0$coxrisk + 1.96*test0$coxrisk_sefit)
    #     test0$coxhaz_sefitL = test0$bhaz * (test0$coxrisk - 1.96*test0$coxrisk_sefit)
    #
    #
    #     dataset[test_ids,]$coxrisk <<- test0$coxrisk
    #     dataset[test_ids,]$bhaz <<- test0$bhaz
    #     dataset[test_ids,]$coxhaz <<- test0$coxhaz
    #
    #     dataset[test_ids,]$coxrisk_se <<- test0$coxrisk_se
    #     dataset[test_ids,]$coxrisk_sefit <<- test0$coxrisk_sefit



  }





  ds$cum_failprob = rep(0, nrow(ds))
  ds = ddply(ds, .(comp_id), get_cum_failprob, 0.1)
  #ds = ddply(ds, .(comp_id), get_cum_failprob, 0.01)


  ds$sum_fprob = rep(0, nrow(ds))
  ds= ddply(ds, .(comp_id), get_sum_fprob)

ds$max_fprob = rep(0, nrow(ds))
ds= ddply(ds, .(comp_id), get_max_fprob)

  res_end_pts = ddply(ds, .(comp_id), function(x){x[nrow(x),]})

  res_colorcodes = rep('',nrow(res_end_pts))
  res_colorcodes[which(paste(res_end_pts$status)==0)]='blue'  # healthy
  res_colorcodes[which(paste(res_end_pts$status)==1)]='red'   # removed


  print( qplot( comp.cycles, fprob, data=ds, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, fprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
         +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
  )

  print( qplot( comp.cycles, sum_fprob, data=ds, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, sum_fprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
         +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
  )

print( qplot( comp.cycles, max_fprob, data=ds, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, max_fprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
       +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
)



  print( qplot( comp.cycles, coxrisk, data=ds, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, coxrisk), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
         +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
  )

  print( qplot( comp.cycles, coxhaz, data=train0, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, coxhaz), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
         +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
  )



  print( qplot( comp.cycles, cum_failprob, data=ds, group=paste(comp_id), colour=paste(status), geom='line') + scale_color_manual(values=c('#157DEC','#F535AA')) + geom_point( data=res_end_pts, aes(comp.cycles, cum_failprob), colour=res_colorcodes, size=5) + theme(legend.position = "none") +  scale_x_continuous('COMPONENT CYCLES') + scale_y_continuous('')
         +  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
  )






  if (END_POINT_MODEL ==  T) {

    #sig_fe_list = c("on_time", "n2_0_mean", "gross_weight_lbs_0_mean", "egt_average_0_mean", "selected_altitude_mcp_0_mean",  "to_iaie","to_delfn", "to_zxm", "to_zvb1f_smoothed")
    # > table(dataset$status, dataset$fprob>0.25)}
    #
    # FALSE TRUE
    # 0    98   20
    # 1     5   26

    # sig_fe_list = c("on_time", "n2_0_mean", "gross_weight_lbs_0_mean", "to_iaie","to_delfn", "to_zxm", "to_zvb1f_smoothed")
    # > table(dataset$status, dataset$fprob>0.23)
    #
    # FALSE TRUE
    # 0    97   21
    # 1     4   27


    # sig_fe_list = c("on_time", "n2_0_mean", "apu_n1_0_mean", "gross_weight_lbs_0_mean", "egt_average_0_mean", "cr_sloatl_smoothed", "selected_altitude_mcp_0_mean", "wind_direction_true_0_mean")
    # without smote
    # > table(dataset$status, dataset$fprob>0.225)
    #
    # FALSE TRUE
    # 0    96   22
    # 1     5   26



    #sig_fe_list = c("n2_0_mean", "gross_weight_lbs_0_mean", "to_iaie","to_delfn", "to_zxm", "to_zvb1f_smoothed")
    # > table(dataset$status, dataset$fprob>0.25)
    #
    # FALSE TRUE
    # 0   103   15
    # 1     5   26





    # sig_fe_list = c("duty_cycle", "gross_weight_lbs_0_mean", "to_iaie","to_delfn", "to_zxm", "to_zvb1f_smoothed")
    #
    # table(dataset$status, dataset$fprob>0.28)
    #
    # FALSE TRUE
    # 0   105   13
    # 1     3   28


    # sig_fe_list = c("duty_cycle", "gross_weight_lbs_0_mean", "to_iaie", "to_delfn", "to_zvb1f_smoothed")
    # > table(dataset$status, dataset$fprob>0.35)
    #
    # FALSE TRUE
    # 0   103   15
    # 1     6   25


    # sig_fe_list = c("duty_cycle", "gross_weight_lbs_0_mean", "to_iaie", "to_zxm", "to_zvb1f_smoothed")
    # > table(dataset$status, dataset$fprob>0.25)
    #
    # FALSE TRUE
    # 0   101   17
    # 1     5   26

    # sig_fe_list = c("duty_cycle", "gross_weight_lbs_0_mean", "to_iaie","to_delfn", "to_zxm")
    # > table(dataset$status, dataset$fprob>0.28)
    #
    # FALSE TRUE
    # 0   100   18
    # 1     5   26


    # >  table(dataset$status, dataset$fprob>0.23)
    #
    # FALSE TRUE
    # 0    97   21
    # 1     5   26

    #table(dataset$status, dataset$fprob>0.43)

    #table(dataset$status, dataset$fprob>0.39)
    #
    # FALSE TRUE
    # 0    96   22
    # 1     5   26

    #> table(dataset$status, dataset$fprob>0.5)
    #
    # FALSE TRUE
    # 0   100   18
    # 1     6   25

    # sig_fe_list = c("on_time", "n2_0_mean", "gross_weight_lbs_0_mean", "egt_average_0_mean", "selected_altitude_mcp_0_mean")
    #
    # > table(dataset$status, dataset$fprob>0.45)
    #
    # FALSE TRUE
    # 0    92   26
    # 1    13   18
    #



    pred = prediction(dataset$fprob, dataset$status)

    varImpPlot(rfm1,type=1)
    #

    dataset_s <- dataset[order(dataset$fprob),]
    colX = c("blue","red")[as.factor(dataset_s$status)]
    plot(seq(1:nrow(dataset_s)),dataset_s$fprob,type="p",col=colX,pch=19,cex.axis = 1.2,cex.lab=1.5,main="A/I Failure Probability",xlab="Index",ylab="Probability")

    acc.perf = performance(pred, measure = "acc")
    plot(acc.perf)

    ROCRpredTest = prediction(dataset_s$fprob, dataset_s$status)
    ROCRperf <- performance(ROCRpredTest,"tpr","fpr")
    plot(ROCRperf,colorize = TRUE,print.cutoffs.at = seq(0,1,0.1),text.adj=c(-0.2,1.7),main="ROC Curve",cex.axis = 1.2)

    ####
    #   pred.obj <- prediction (dataset_s$fprob, dataset_s$status)
    #   RP.perf <- performance(pred.obj, "rec","prec");
    #   plot (RP.perf);
    #
    #   ROC.perf <- performance(pred.obj, "fpr","tpr");
    #   plot (ROC.perf);
    #
    #   plot  (RP.perf@alpha.values[[1]],RP.perf@x.values[[1]]);
    #   lines (RP.perf@alpha.values[[1]],RP.perf@y.values[[1]]);
    #   lines (ROC.perf@alpha.values[[1]],ROC.perf@x.values[[1]]);

  }


} else { # CV_FLAG == F


  sig_fe_list0 = names(out)[ix_u]

  sig_fe_list1 = sig_fe_list0[c(4:415)]
  sig_fe_list2 = sig_fe_list1[ sapply(sig_fe_list1, function(i) { length(which(is.na(dataset[,i]))) != nrow(dataset)}) ]
  #sig_ix_u = names(td)[grep("cumavg",names(td))]

  sig_fe_list = sig_fe_list2[ -grep("sedi", sig_fe_list2)]

  sig1 = sig_fe_list[c(1:131, 292:294)]
  sig2 = sig_fe_list[c(132:291)]
  sig2_sel = sig2[grep("_mean",sig2)]
  sig_fe_list = c(sig1, sig2_sel)


  trainset = dataset[, c("status", sig_fe_list)]


  #   ## imputation ###
  #   trainset1 = trainset[,sapply(names(trainset), function(i) { length(which(is.na(trainset[,i]))) != nrow(trainset)})]
  #
  #   ix_numeric=sapply(names(trainset1), function(nm){class(trainset1[,nm])[1] == "numeric"  })
  #
  #
  #   trainset1[,ix_numeric] <- na.roughfix(trainset1[,ix_numeric])
  #
  #   trainset = trainset1
  #   ##################


  #   AGW,  cr_agw, gross_weight_lbs
  #   ZALT, to_zalt, pressure_altitude
  #   ZWF36, to_zwf36, engine_fuel_flow_total_lbs_hr
  #   ZT49, egt
  #   ZPCN25, n2
  #   AIW, wing_anti_ice
  #   ZT1A, air_temperature_total
  #   ZVB … parameters, vibration_level_n2


  # ensure the results are repeatable
  set.seed(7)
  # load the library
  library(mlbench)
  library(caret)
  library(DMwR)

  # calculate correlation matrix
  correlationMatrix <- cor(trainset[,-1])
  na_ind = which(is.na(correlationMatrix[,1]))
  correlationMatrix <- correlationMatrix[-na_ind, -na_ind]
  # summarize the correlation matrix
  #print(correlationMatrix)
  # find attributes that are highly corrected (ideally >0.75)
  highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.9)
  # print indexes of highly correlated attributes
  print(highlyCorrelated)

  trainset = trainset[,-which(names(trainset) %in% rownames(correlationMatrix)[highlyCorrelated])]
  #
  trainset$status = as.factor(trainset$status)
  table(trainset$status)

  trainset0 <- trainset

  #train_smote <- SMOTE(status ~ ., data=trainset, perc.over = 200, perc.under=200, k=3)

  #   > table(train_smote$status)
  #
  #   0   1
  #   124  93


  #trainset <- train_smote

  rfm1 <- randomForest(status ~ ., data = trainset, importance=TRUE, mtry = 4, proximity=TRUE, keep.forest=FALSE)  # classwt=c(10,1) )
  varImpPlot(rfm1,type=1)
  MDSplot(rfm1, trainset$status)

  rfGrid <-  expand.grid(mtry = c(2, 4, 7))

  # ensure results are repeatable
  set.seed(7)
  # prepare training scheme
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  # train the model
  rfmodel <- train(status ~ ., data = trainset, method="rf", trControl=control, tuneGrid=rfGrid )
  # estimate variable importance
  importance <- varImp(rfmodel, scale=F)
  # summarize importance
  print(importance)
  # plot importance
  plot(importance)

  vi <- varImp(rfmodel, scale=F)$importance
  vi$max <- apply(vi, 1, max)
  row.names(vi[order(-vi$max),][1:8,])
  # [1] "on_time"                      "n2_0_mean"                    "apu_n1_0_mean"                "gross_weight_lbs_0_mean"
  # [5] "egt_average_0_mean"           "cr_sloatl_smoothed"           "selected_altitude_mcp_0_mean" "wind_direction_true_0_mean"

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


  control <- rfeControl(functions=rfFuncs, method="cv", number=10)
  # run the RFE algorithm
  results <- rfe(trainset[,-1], trainset[,1], sizes=2^(2:4), rfeControl=control)
  # summarize the results
  print(results)
  # list the chosen features
  predictors(results)
  # plot the results
  plot(results, type=c("g", "o"))




  trainset$fprob = 0

  trainset$fprob = predict(rfm1, newdata=trainset, type='prob')[,2]
  # #partialPlot(rfm1, trainset, x.var = "egthdm")
  # importance(rfm1, type=1)
  varImpPlot(rfm1,type=1)
  #

  trainset <- trainset[order(trainset$fprob),]
  colX = c("blue","red")[as.factor(trainset$status)]
  plot(seq(1:nrow(trainset)),trainset$fprob,type="p",col=colX,pch=19,cex.axis = 1.2,cex.lab=1.5,main="A/I Failure Probability",xlab="Index",ylab="Probability")


  ROCRpredTest = prediction(trainset$fprob, trainset$status)
  ROCRperf <- performance(ROCRpredTest,"tpr","fpr")
  plot(ROCRperf,colorize = TRUE,print.cutoffs.at = seq(0,1,0.1),text.adj=c(-0.2,1.7),main="ROC Curve",cex.axis = 1.2)

  ####
  pred.obj <- prediction (trainset$fprob, trainset$status)
  RP.perf <- performance(pred.obj, "rec","prec");
  plot (RP.perf);

  ROC.perf <- performance(pred.obj, "fpr","tpr");
  plot (ROC.perf);

  plot  (RP.perf@alpha.values[[1]],RP.perf@x.values[[1]]);
  lines (RP.perf@alpha.values[[1]],RP.perf@y.values[[1]]);
  lines (ROC.perf@alpha.values[[1]],ROC.perf@x.values[[1]]);




}





# roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
# plot(roc.perf)
# # abline(a=0, b= 1)
#
# acc.perf = performance(pred, measure = "acc")
# plot(acc.perf)
#
# auc.tmp <- performance(pred,"auc")
# auc <- as.numeric(auc.tmp@y.values)

# out_train = outtr
# # out_train[is.na(out_train)]=0
# # out_train$status = as.factor(out_train$status)
# # rfm <- randomForest(status ~ ., data = out_train, importance=TRUE, mtry = 2)
# # #partialPlot(rfm, out_train, x.var = "egthdm")
# # importance(rfm, type=1)
# # varImpPlot(rfm,type=1)
# #
# #
# # library(caret)
# # forest.model1 <- train(status ~ ., data = out_train, importance=TRUE)
# # plot(varImp(forest.model1))
# # varImp(forest.model1)
# #
# # pdf(paste("./rf_rank_", outtitle, ".pdf", sep=''))
# # plot(varImp(forest.model1))
# #
# # dev.off()
#
#
# #
# # aa=train(status ~., data=dataset[,c("status", sig_fe_list)],
# #       method='rf',TuneLength=3,
# #       trControl=trainControl(
# #         method='cv',number=10,
# #         classProbs = TRUE)
#
#
#
# #RocImp <- filterVarImp(x=out_train[,-ncol(out_train)], y=out_train$status)
#
#
#
# #tapply(td$cpn, list(td$comp_id), function(t){length(t)})
#
#
# # dic_id_str = unique(paste(td$cpn, td$msn, td$install_date))
# # dic_id_str = dic_id_str[which(dic_id_str != "NA  ")]
# # dic_id_str = c(dic_id_str,  "NA  ")
#
# # library(doParallel)
# # cl <- makeCluster( detectCores()-1 )
# # registerDoParallel(cl) # register foreach backend
# # # clusterEvalQ(cl, library(MASS))
# #
# #
# # set_func <- function (td, dic_id_str, i_t, dic_id_str_t) {
# #   idx = which(paste(td$cpn, td$msn, td$install_date) == dic_id_str_t)
# #   td[idx, ]$comp_id = i_t
# #
# #
# #   if (dic_id_str_t == "NA  ") {
# #
# #     dic_na = unique(td[which(paste(td$cpn, td$msn, td$install_date)=="NA  "),]$serlzd_eng_ser_num)
# #     td[idx, ]$comp_id = sapply(td[which(paste(td$cpn, td$msn, td$install_date)=="NA  "),]$serlzd_eng_ser_num, function(a) {length(dic_id_str)-1 + which(a==dic_na)})
# #   }
# #   return(td)
# # }
# #
# # td_new <- foreach(i=1:length(dic_id_str), .multicombine=FALSE) %dopar% {
# #   return(set_func(td, dic_id_str, i, dic_id_str[i]))
# # }
# #
# # tdn = do.call(rbind.data.frame, td_new)
# #
# # stopCluster(cl)
#
# # > nrow(td[which(paste(td$cpn, td$msn, td$install_date)=="NA  "),])
# # [1] 9851
# # > unique(td[which(paste(td$cpn, td$msn, td$install_date)=="NA  "),]$serlzd_eng_ser_num)
# # [1] 702582 702395 702818 702410 702161 702550 702653 704414 702290 702314 704405 704382 702833 702294 702431 702597 702409 702631 702654
# # [20] 702599 702781 702655 702727 702701 702288 702313 702365 702598 702825 704321 702686 702835 702512 702911 702726 702657 702786 702340
# # [39] 702537 704374 704492 702486 702754 702913 704305 704301 702396 702470 702922 702301 702706 702302 702750 704390 702292 702819 702894
# # [58] 702581 702652 702893 702498 702397 704337 704306 702895 702826 702429 702881 702360 702914 702820 702362 704338 704307 702921 702758
# # [77] 702518 702803 702831 702836 702804 702303 702853 702732 704388 702445 702918 702495 702811 702580 702304 702110 702915 702547 702730
# # [96] 702532 702363 704336 702852 704406 702412 704415 706267 702812 702779 702920
#
#
# # > head(sapply(td[which(paste(td$cpn, td$msn, td$install_date)=="NA  "),]$serlzd_eng_ser_num, function(a) {which(a==dic_na)}))
# # [1] 1 2 2 3 3 3
# # > head(sapply(td[which(paste(td$cpn, td$msn, td$install_date)=="NA  "),]$serlzd_eng_ser_num, function(a) {a}))
# # [1] 702582 702395 702395 702818 702818 702818
#
# ### sedi_ecsn exists on takeoff snapshot only
# ### 0. should be ordered by  "src_flght_dttm"
# ### 1. for each cpn, the snapshot records should be added, so  instead of having "NA  ", we should have "3011089  ", "3000073  " each
# ### 2. should include only snapshot records since the last removal date for each engine
# # > table(sapply(td[which(paste(td$cpn, td$msn, td$install_date)=="NA  "),]$serlzd_eng_ser_num, function(a) {a}))
# #
# # 702110 702161 702288 702290 702292 702294 702301 702302 702303 702304 702313 702314 702340 702360 702362 702363 702365 702395 702396 702397
# # 17    107     57    109    108    118     60     19      9     96    182    161    111    141      2      7     95    167     18     43
# # 702409 702410 702412 702429 702431 702445 702470 702486 702495 702498 702512 702518 702532 702537 702547 702550 702580 702581 702582 702597
# # 138    496      9    134    223     47    172    147      1      1    141     95     15     82      2    154      6    127    364     58
# # 702598 702599 702631 702652 702653 702654 702655 702657 702686 702701 702706 702726 702727 702730 702732 702750 702754 702758 702779 702781
# # 126    228    162    102    349     20    206     28     66    298     31     34    137     31     83     18    111      3      7    254
# # 702786 702803 702804 702811 702812 702818 702819 702820 702825 702826 702831 702833 702835 702836 702852 702853 702881 702893 702894 702895
# # 183     48     49     29      1    227     70     60     78     59     31     79     46     36     68     65    119    178     17     73
# # 702911 702913 702914 702915 702918 702920 702921 702922 704301 704305 704306 704307 704321 704336 704337 704338 704374 704382 704388 704390
# # 99     30     20      7      4      7     13     54     16     75     45    224    257     36    185     69    106    178     23      4
# # 704405 704406 704414 704415 704492 706267
# # 188     41     77     33    193     18
#
# # > table(sapply(td1[which(paste(td1$cpn, td1$msn, td1$install_date)=="NA  NA"),]$serlzd_eng_ser_num, function(a) {a}))
# # # 702110 702161 702288 702289 702290 702292 702294 702301 702302 702303 702304 702313 702314 702315 702329 702340 702360 702362 702363 702365
# # # 159   7607   1235   6032   7895   7882   3125    817    438   7233   3074   7266    156   4841    312   6880  14647   2040    506   3941
# # # 702394 702395 702396 702397 702399 702408 702409 702410 702411 702412 702413 702428 702429 702431 702445 702468 702470 702486 702495 702498
# # # 2435   7706   3626    413   4229   1044   4682   3264    502   1136   2378   7561    845   5621   2869    591   1618   1757   2883   2474
# # # 702512 702518 702532 702537 702547 702550 702562 702580 702581 702582 702597 702598 702599 702631 702652 702653 702654 702655 702656 702657
# # # 7669   2695   8849   1010   1411   3373   1650   7799   4124   3540   1339    342   8133   1627   2431   7264   2966   2043   8369   8099
# # # 702686 702687 702688 702701 702706 702726 702727 702730 702731 702732 702749 702750 702754 702757 702758 702760 702779 702781 702786 702803
# # # 3004   1139   2108   3548    840   1122    989   2198   7234   4053   1393   7906   2334    442   2286   2235      1   3213   3681    203
# # # 702804 702811 702812 702817 702818 702819 702820 702825 702826 702827 702828 702829 702831 702833 702834 702835 702836 702849 702850 702852
# # # 7785   7138   2011    163   2523   8224   3235   8757   7716    923   7404   1065   1049   4846   4506   2862   1035   4201   7688   7712
# # # 702853 702854 702867 702869 702881 702882 702893 702894 702895 702904 702911 702912 702913 702914 702915 702916 702917 702918 702919 702920
# # # 257   4532    787    818   4669   1057   8317    283    494     48   4584   2270   6057   5014   8208    300   5874    179   7447   1123
# # # 702921 702922 704109 704301 704302 704305 704306 704307 704310 704319 704321 704336 704337 704338 704374 704381 704382 704388 704389 704390
# # # 4557   2294    455   1530    434   4508   2424   3788   7852    781   2438   3142   3684    462   2516   5013   7796   5129   7464   3949
# # # 704405 704406 704407 704414 704415 704492 704633 705220 705259 706267 706268
# # # 7898   3112   2788   2596    583   5343    239    583   4786   3577    882
#
