


### Feature selection-related functions

univariateCox <- function (dt, feature_colnames) {

}


#' component_wise = T or F
separateDistributions <- function (dt, feature_colnames, component_wise) {

}


## the part for CV_FLAG == F  in the template code
featureSelectionTestUsingRF <- function (dt) {

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

# COXPH_DIRECT = F     # with or without RF (F = with RF)
# END_POINT_MODEL = F  # for randomforest   : F = cycle-based model
# COX_TIMEDEP = F  # cox independent
# USE_CYCLE_SAMPLES = T


# we don't do classification ( don't need END_POINT_MODEL tag)
# COXPH_DIRECT == T & USE_CYCLE_SAMPLES = T
# option_timedep == COX_TIMEDEP
crossValidatedCoxPHModels <- function (dt, num_folds, option_timedep) {

}

# COXPH_DIRECT == F & USE_CYCLE_SAMPLES = T
# option_use_endpoints_for_classification == END_POINT_MODEL
# option_timedep == COX_TIMEDEP
crossValidatedClassificationCoxPHModels <- function (dt, num_folds, option_use_endpoints_for_classification, option_timedep) {

}





## run after setDatasetForModel
crossValidatedModels <- function (dt, num_folds, option_label, option_cox, option_classification) {

  data = dt$data
  dataset = dt$dataset
  comp_set = unique(dataset$comp_id)
  num_comps = length(comp_set)
  fold = rep(1:num_folds,length.out=num_comps)
  ids = sample(1:num_comps,num_comps)


  cross_validate <- function (cv_id) {
    #for (cv_id in 1:num_folds) {


    if (option_label == 'LIM_CYCLE') {
      dataset$target = dataset$label
    } else {
      dataset$target = dataset$eventual_failure
    }


    tr = comp_set[ids[fold==cv_id]]
    test_ids = which(dataset$comp_id %in% tr)

    #    tr_train = unique(dataset$comp_id)[ids[fold==cv_id]]
    #    tr_test = unique(dataset$comp_id)[ids[fold!=cv_id]]
    #    table(sapply(tr_train, function(i){td[which(td$comp_id==i),]$status[1]}))
    #    table(sapply(tr_test, function(i){td[which(td$comp_id==i),]$status[1]}))


    trainset0 = data[-test_ids,]
    testset0 = data[test_ids,]

    trainset = dataset[-test_ids,]
    testset = dataset[test_ids,]


    #trainset[is.na(trainset)]=0
    #testset[is.na(testset)]=0


    ### imputation ###
#     ix_numeric=sapply(names(trainset), function(nm){class(trainset[,nm])[1] == "numeric" | class(trainset[,nm])[1] == "integer"  })
#
#     trainset1 = trainset
#     trainset1[,ix_numeric] <- na.roughfix(trainset[,ix_numeric])
#
#     testset1 = testset
#     testset1[,ix_numeric] <- na.roughfix(testset[,ix_numeric])
#
#     trainset = trainset1
#     testset = testset1
    ###################


    #out_train = trainset[which(!is.na(trainset[,"status"])),ix_u,which(names(trainset)=='status'))][seq(1,nrow(out),100),]
    #out_train = trainset[which(!is.na(trainset[,"status"])), ][seq(1,nrow(trainset),10),]
    #out_train = trainset[which(!is.na(trainset[,"label"])), ][seq(1,nrow(trainset),10),]

    #   out_train1 = trainset[which(trainset[,"label"] == 1 ), ]
    #   out_train0 = trainset[which(trainset[,"label"] == 0 ), ]
    #   out_train0 = out_train0[seq(1,nrow(out_train0),20),]

    out_train1 = trainset[which(trainset$target == 1 ), ]
    out_train0 = trainset[which(trainset$target == 0 ), ]

    if (option_label == 'LIM_CYCLE') {
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


    ##partialPlot(rfm, out_train, x.var = "egthdm")
    #importance(rfm, type=1)
    #varImpPlot(rfm,type=1)

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


    if (option_cox == T) {

      start_t = trainset0$comp.cycles
      stop_t = trainset0$comp.cycles + trainset0$this.cycle
      failure = trainset0$failure

      fe1 = trainset0$fprob #trainset0$fprob  # covariate
      #fe2 = trainset0$comp.cycles
      sdd = data.frame(start_t, stop_t, failure, fe1)
      #sdd = data.frame(start_t, stop_t, failure, fe1, fe2)

      sdd = sdd[which(sdd$stop_t > sdd$start_t & sdd$start_t >= 0),]

      coxph1 = coxph(Surv(start_t, stop_t, failure) ~  fe1 , data=sdd, method='breslow') #, na.action=na.exclude)
      #coxph1 = coxph(Surv(start_t, stop_t, failure) ~  fe1 + fe2 , data=sdd, method='breslow') #, na.action=na.exclude)

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


      dataset[test_ids,]$coxrisk <- testset0$coxrisk
      dataset[test_ids,]$bhaz <- testset0$bhaz
      dataset[test_ids,]$coxhaz <- testset0$coxhaz

      dataset[test_ids,]$coxrisk_se <- testset0$coxrisk_se
      dataset[test_ids,]$coxrisk_sefit <- testset0$coxrisk_sefit

    }

    dataset[test_ids,]$fprob <- testset0$fprob
    dataset[test_ids,]$fprob_r <- testset0$fprob_r


    # store the results in a list for return
    list(testset0$fprob, testset0$coxrisk, testset0$bhaz, testset0$coxhaz, testset0$coxrisk_se, testset0$coxrisk_sefit,  testset0$fprob_r)

  }

  for (cv_id in 1:num_folds) {
    #print(cv_id)
    cross_validate(cv_id)
  }

  #print(system.time ( ( resR = lapply (1:nFolds,  cross_validate) ) ) )



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

getCompEndPoints <- function (dt) {

  #res_end_pts = ddply(dataset, .(comp_id), function(x){x[nrow(x),]})

}


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


# dt = ddply(dt, .(comp_id), get_sum_fprob)
#
# dt = ddply(dt, .(comp_id), get_sum_fprob_r)
#
# dt = ddply(dt, .(comp_id), get_cum_failprob, 1)


# writeData <- function (df_output, output_csv_filepath) {
#   write.csv(df_output, output_csv_filepath, row.names=F)
# }
#
# data = readData('./testbench/data/Rolling average feature.csv')
#
#
#
# run_model_class = RunModeling()
# result =  run_model_class.run_modeling_func(data)
#
# writeData(result, './testbench/data/result_output.csv')
#
#



