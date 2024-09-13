library(dplyr)
library(tidyr)
library(ggplot2)
library(arsenal)


#source(paste(rootdir,"R/degree.R",sep="/"))

#sampleList <- degree(redo=TRUE) #set redo=TRUE to update

ldata.wealth = sampleList[[2]]
g.isolation = sampleList[[3]]
sample.wide = sampleList[[4]]
cdata = sampleList[[5]]
ndata = sampleList[[6]]
sample.long = sampleList[[7]]
sample.prac.wide = sampleList[[8]]


#' @param redo If FALSE, will not redownload and recreate the data if
#' data already exists on disk. If TRUE, it will.
#' 

#model1: round 1, subjects are nodes: coop ~ coop_rp1 + coop_rp2 + gini (if visible); prob_coop ~ coop_rp1 + coop_rp2 (if not visible); run separately for 2 conditions (visible or not)
model1.visible <- function(redo=FALSE) {
    
  #rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
  
  datafile <- paste(rootdir,"data/model1.visible.Rda", sep="/")
  
  if (!redo && file.exists(datafile)) {
    cat("Loading data last updated on ", as.character(file.mtime(datafile)), "\n")
    cat("Call model1.visible(redo=TRUE) to update data.\n")
    load(datafile)
    
    return(list(rfmodel))
  }
  
    
    sample.rf = sample.prac.wide[sample.prac.wide$visibleWealth==1,][c("ID","behavior.1","behavior.p1","behavior.p2","gini")]
    sample.cc = sample.rf[complete.cases(sample.rf),]
    
    sample.cc <- sample.cc %>% 
      mutate(behavior.1 =
               case_when(
                 behavior.1==1 ~ "C",
                 behavior.1==0 ~ "D"
               )
      
    )
    
    train.rf <- sample.cc %>% sample_frac(0.8) 
    test.rf <- anti_join(sample.cc, train.rf)
    train.rf = train.rf[c("behavior.1","behavior.p1","behavior.p2","gini")]
    test.rf = test.rf[c("behavior.1","behavior.p1","behavior.p2","gini")]
    
    set.seed(100)
    control.rf <- trainControl(method='repeatedcv', 
                               number=10, 
                               repeats=3,
                               search = 'random',
                               classProbs=TRUE,
                               summaryFunction = twoClassSummary,
                               allowParallel = TRUE)
    
    rf <- caret::train(behavior.1 ~ behavior.p1 + behavior.p2 + gini,
                       data = train.rf,
                       method = 'rf',
                       metric = 'ROC',
                       tuneLength  = 30, 
                       verbose=TRUE,
                       trControl = control.rf)

    print(rf)
    
    prediction = predict(rf, newdata=test.rf,type="prob")
    test.rf = cbind(test.rf, prediction["C"])
    prediction = predict(rf, newdata=test.rf)
    test.rf = cbind(test.rf, prediction)
    
    #discrimination of model: AUC
    roc = pROC::roc(test.rf$behavior.1,test.rf$C,plot=TRUE)
    roc

    #cutoff for specificity and sensitivity (Youden's J statistic)
    #40. Schisterman, E. F. et al. Optimal cut-point and its corresponding Youden index to
    #discriminate individuals using pooled blood samples. Epidemiology 16, 73–81 (2005).
    library(cutpointr)
    cp <- cutpointr(test.rf, C, behavior.1,
                    method = maximize_metric, metric = sum_sens_spec)
    summary(cp)
    
    #calibration of model (goodness-of-fit)
    #Mcnemar's Test P-Value < 0.05 means significant misclassification (and thus bad goodness-of-fit)
    caret::confusionMatrix(data = factor(test.rf$prediction),
                           reference = factor(test.rf$behavior.1))

    rfmodel = rf
  
  save(rfmodel,file=datafile)
  
  return(list(rfmodel,roc))

}


#model1: round 1, subjects are nodes: coop ~ coop_rp1 + coop_rp2 + gini (if visible); prob_coop ~ coop_rp1 + coop_rp2 (if not visible); run separately for 2 conditions (visible or not)
model1.invisible <- function(redo=FALSE) {
  
  #rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
  
  datafile <- paste(rootdir,"data/model1.invisible.Rda", sep="/")
  
  if (!redo && file.exists(datafile)) {
    cat("Loading data last updated on ", as.character(file.mtime(datafile)), "\n")
    cat("Call model1.invisible(redo=TRUE) to update data.\n")
    load(datafile)
    
    return(list(rfmodel))
  }
    
    sample.rf = sample.prac.wide[sample.prac.wide$visibleWealth==0,][c("ID","behavior.1","behavior.p1","behavior.p2")]
    sample.cc = sample.rf[complete.cases(sample.rf),]
    
    sample.cc <- sample.cc %>% 
      mutate(behavior.1 =
               case_when(
                 behavior.1==1 ~ "C",
                 behavior.1==0 ~ "D"
               )
             
      )
    
    train.rf <- sample.cc %>% sample_frac(0.8) 
    test.rf <- anti_join(sample.cc, train.rf)
    train.rf = train.rf[c("behavior.1","behavior.p1","behavior.p2")]
    test.rf = test.rf[c("behavior.1","behavior.p1","behavior.p2")]
    
    set.seed(100)
    control.rf <- trainControl(method='repeatedcv', 
                               number=10, 
                               repeats=3,
                               search = 'random',
                               classProbs=TRUE,
                               summaryFunction = twoClassSummary,
                               allowParallel = TRUE)
    
    rf <- caret::train(behavior.1 ~ behavior.p1 + behavior.p2,
                       data = train.rf,
                       method = 'rf',
                       metric = 'ROC',
                       tuneLength  = 30, 
                       verbose=TRUE,
                       trControl = control.rf)

    print(rf)

    prediction = predict(rf, newdata=test.rf,type="prob")
    test.rf = cbind(test.rf, prediction["C"])
    prediction = predict(rf, newdata=test.rf)
    test.rf = cbind(test.rf, prediction)

    #discrimination of model: AUC
    roc = pROC::roc(test.rf$behavior.1,test.rf$C,plot=TRUE)
    roc

    #cutoff for specificity and sensitivity (Youden's J statistic)
    #40. Schisterman, E. F. et al. Optimal cut-point and its corresponding Youden index to
    #discriminate individuals using pooled blood samples. Epidemiology 16, 73–81 (2005).
    library(cutpointr)
    cp <- cutpointr(test.rf, C, behavior.1,
                    method = maximize_metric, metric = sum_sens_spec)
    summary(cp)
    
    #calibration of model (goodness-of-fit)
    #Mcnemar's Test P-Value < 0.05 means significant misclassification (and thus bad goodness-of-fit)
    caret::confusionMatrix(data = factor(test.rf$prediction),
                           reference = factor(test.rf$behavior.1))

    rfmodel = rf
  
  
  save(rfmodel,file=datafile)
  
  return(list(rfmodel,roc))
  
}

#model2: rounds 2-10, subjects are nodes: coop ~ prev_coop + gini + (prev_avg_env_wealth - prev_wealth) (if visible); prob_coop ~ prev_coop (if not visible); run separately for 4 conditions (prev_coop 0 or 1, visible or not)
model2.visible <- function(redo=FALSE) {
  
  #rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
  
  datafile <- paste(rootdir,"data/model2.visible.Rda", sep="/")
  
  if (!redo && file.exists(datafile)) {
    cat("Loading data last updated on ", as.character(file.mtime(datafile)), "\n")
    cat("Call model2.visible(redo=TRUE) to update data.\n")
    load(datafile)
    
    return(list(rfmodel))
  }
    
    sample.rf = sample.long[sample.long$visibleWealth==1,][c("ID","behavior","prevCoop","gini","alterPrevWealth","egoPrevWealth")]
    sample.cc = sample.rf[complete.cases(sample.rf),]
    
    train.rf <- sample.cc %>% sample_frac(0.8) 
    test.rf <- anti_join(sample.cc, train.rf)
    train.rf = train.rf[c("behavior","prevCoop","gini","alterPrevWealth","egoPrevWealth")]
    test.rf = test.rf[c("behavior","prevCoop","gini","alterPrevWealth","egoPrevWealth")]
    
    set.seed(100)
    control.rf <- trainControl(method='repeatedcv', 
                               number=10, 
                               repeats=3,
                               search = 'random',
                               classProbs=TRUE,
                               summaryFunction = twoClassSummary,
                               allowParallel = TRUE)
    
    rf <- caret::train(behavior ~ prevCoop + gini + alterPrevWealth + egoPrevWealth,
                       data = train.rf,
                       method = 'rf',
                       metric = 'ROC',
                       tuneLength  = 30, 
                       verbose=TRUE,
                       trControl = control.rf)

    print(rf)
    
    prediction = predict(rf, newdata=test.rf,type="prob")
    test.rf = cbind(test.rf, prediction["C"])
    prediction = predict(rf, newdata=test.rf)
    test.rf = cbind(test.rf, prediction)
    
    #discrimination of model: AUC
    roc = pROC::roc(test.rf$behavior,test.rf$C,plot=TRUE)
    roc

    #cutoff for specificity and sensitivity (Youden's J statistic)
    #40. Schisterman, E. F. et al. Optimal cut-point and its corresponding Youden index to
    #discriminate individuals using pooled blood samples. Epidemiology 16, 73–81 (2005).
    library(cutpointr)
    cp <- cutpointr(test.rf, C, behavior,
                    method = maximize_metric, metric = sum_sens_spec)
    summary(cp)
    
    #calibration of model (goodness-of-fit)
    #Mcnemar's Test P-Value < 0.05 means significant misclassification (and thus bad goodness-of-fit)
    caret::confusionMatrix(data = factor(test.rf$prediction),
                           reference = factor(test.rf$behavior))

    rfmodel = rf
  
  save(rfmodel,file=datafile)
  
  return(list(rfmodel,roc))
  
}


#model2: rounds 2-10, subjects are nodes: coop ~ prev_coop + gini + (prev_avg_env_wealth - prev_wealth) (if visible); prob_coop ~ prev_coop (if not visible); run separately for 4 conditions (prev_coop 0 or 1, visible or not)
model2.invisible <- function(redo=FALSE) {
  
  #rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
  
  datafile <- paste(rootdir,"data/model2.invisible.Rda", sep="/")
  
  if (!redo && file.exists(datafile)) {
    cat("Loading data last updated on ", as.character(file.mtime(datafile)), "\n")
    cat("Call model2.invisible(redo=TRUE) to update data.\n")
    load(datafile)
    
    return(list(rfmodel))
  }
  
    sample.rf = sample.long[sample.long$visibleWealth==1,][c("ID","behavior","prevCoop","egoPrevWealth")]
    sample.cc = sample.rf[complete.cases(sample.rf),]
    
    train.rf <- sample.cc %>% sample_frac(0.8) 
    test.rf <- anti_join(sample.cc, train.rf)
    train.rf = train.rf[c("behavior","prevCoop","egoPrevWealth")]
    test.rf = test.rf[c("behavior","prevCoop","egoPrevWealth")]
    
    set.seed(100)
    control.rf <- trainControl(method='repeatedcv', 
                               number=10, 
                               repeats=3,
                               search = 'random',
                               classProbs=TRUE,
                               summaryFunction = twoClassSummary,
                               allowParallel = TRUE)
    
    rf <- caret::train(behavior ~ prevCoop + egoPrevWealth,
                       data = train.rf,
                       method = 'rf',
                       metric = 'ROC',
                       tuneLength  = 30, 
                       verbose=TRUE,
                       trControl = control.rf)
    
    print(rf)
    
    prediction = predict(rf, newdata=test.rf,type="prob")
    test.rf = cbind(test.rf, prediction["C"])
    prediction = predict(rf, newdata=test.rf)
    test.rf = cbind(test.rf, prediction)
    
    #discrimination of model: AUC
    roc = pROC::roc(test.rf$behavior,test.rf$C,plot=TRUE)
    roc

    #cutoff for specificity and sensitivity (Youden's J statistic)
    #40. Schisterman, E. F. et al. Optimal cut-point and its corresponding Youden index to
    #discriminate individuals using pooled blood samples. Epidemiology 16, 73–81 (2005).
    library(cutpointr)
    cp <- cutpointr(test.rf, C, behavior,
                    method = maximize_metric, metric = sum_sens_spec)
    summary(cp)
    
    #calibration of model (goodness-of-fit)
    #Mcnemar's Test P-Value < 0.05 means significant misclassification (and thus bad goodness-of-fit)
    caret::confusionMatrix(data = factor(test.rf$prediction),
                           reference = factor(test.rf$behavior))

    rfmodel = rf
    
  
  save(rfmodel,file=datafile)
  
  return(list(rfmodel,roc))
  
}

#model3: rounds 1-10, subjects are links: alt_prob ~ prev_connected + ego_coop + alt_coop (alter_prob is the probability of choosing to connect when challenged (asked))
model3 <- function(redo=FALSE) {

  #rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]

  datafile <- paste(rootdir,"data/model3.Rda", sep="/")

  if (!redo && file.exists(datafile)) {
    cat("Loading data last updated on ", as.character(file.mtime(datafile)), "\n")
    cat("Call model3(redo=TRUE) to update data.\n")
    load(datafile)

    return(list(rfmodel))
  }
  
  sample.rf = cdata[c("ID","alterID","game","round","connecting","previouslyconnected","ego_behavior","alter_behavior")]
  sample.cc = sample.rf[complete.cases(sample.rf),]
  
  sample.cc <- sample.cc %>% 
    mutate(connecting = 
             case_when(
               connecting=="makeLink" ~ "C",
               connecting=="notBreakLink" ~ "C",
               connecting=="notMakeLink" ~ "B",
               connecting=="breakLink" ~ "B"
             )
    )
  
  train.rf <- sample.cc %>% sample_frac(0.8) 
  test.rf <- anti_join(sample.cc, train.rf,by=c("ID","alterID","game","round"))
  train.rf = train.rf[c("connecting","previouslyconnected","ego_behavior","alter_behavior")]
  test.rf = test.rf[c("connecting","previouslyconnected","ego_behavior","alter_behavior")]
  
  set.seed(100)
  control.rf <- trainControl(method='repeatedcv', 
                             number=10, 
                             repeats=3,
                             search = 'random',
                             classProbs=TRUE,
                             summaryFunction = twoClassSummary,
                             allowParallel = TRUE)
  
  rf <- caret::train(connecting ~ previouslyconnected + ego_behavior + alter_behavior,
                     data = train.rf,
                     method = 'rf',
                     metric = 'ROC',
                     tuneLength  = 30, 
                     verbose=TRUE,
                     trControl = control.rf)

  print(rf)

  prediction = predict(rf, newdata=test.rf,type="prob")
  test.rf = cbind(test.rf, prediction["C"])
  prediction = predict(rf, newdata=test.rf)
  test.rf = cbind(test.rf, prediction)
  
  #discrimination of model: AUC
  roc = pROC::roc(test.rf$connecting,test.rf$C,plot=TRUE)
  roc

  #cutoff for specificity and sensitivity (Youden's J statistic)
  #40. Schisterman, E. F. et al. Optimal cut-point and its corresponding Youden index to
  #discriminate individuals using pooled blood samples. Epidemiology 16, 73–81 (2005).
  library(cutpointr)
  cp <- cutpointr(test.rf, C, connecting,
                  method = maximize_metric, metric = sum_sens_spec)
  summary(cp)
  
  #calibration of model (goodness-of-fit)
  #Mcnemar's Test P-Value < 0.05 means significant misclassification (and thus bad goodness-of-fit)
  caret::confusionMatrix(data = factor(test.rf$prediction),
                         reference = factor(test.rf$connecting))

  rfmodel = rf

  save(rfmodel,file=datafile)

  return(list(rfmodel,roc))

}



model3_no_previouslyconnected <- function(redo=FALSE) {
  
  #rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
  
  datafile <- paste(rootdir,"data/model3_no_previouslyconnected.Rda", sep="/")
  
  if (!redo && file.exists(datafile)) {
    cat("Loading data last updated on ", as.character(file.mtime(datafile)), "\n")
    cat("Call model3_no_previouslyconnected(redo=TRUE) to update data.\n")
    load(datafile)
    
    return(list(rfmodel))
  }
  
  sample.rf = cdata[c("ID","alterID","game","round","connecting","previouslyconnected","ego_behavior","alter_behavior")]
  sample.cc = sample.rf[complete.cases(sample.rf),]
  
  sample.cc <- sample.cc %>% 
    mutate(connecting = 
             case_when(
               connecting=="makeLink" ~ "C",
               connecting=="notBreakLink" ~ "C",
               connecting=="notMakeLink" ~ "B",
               connecting=="breakLink" ~ "B"
             )
    )
  
  train.rf <- sample.cc %>% sample_frac(0.8) 
  test.rf <- anti_join(sample.cc, train.rf,by=c("ID","alterID","game","round"))
  train.rf = train.rf[c("connecting","previouslyconnected","ego_behavior","alter_behavior")]
  test.rf = test.rf[c("connecting","previouslyconnected","ego_behavior","alter_behavior")]
  
  set.seed(100)
  control.rf <- trainControl(method='repeatedcv', 
                             number=10, 
                             repeats=3,
                             search = 'random',
                             classProbs=TRUE,
                             summaryFunction = twoClassSummary,
                             allowParallel = TRUE)
  
  rf <- caret::train(connecting ~ #previouslyconnected + 
                       ego_behavior + alter_behavior,
                     data = train.rf,
                     method = 'rf',
                     metric = 'ROC',
                     tuneLength  = 30, 
                     verbose=TRUE,
                     trControl = control.rf)
  # 
  # print(rf)
  # 
  # prediction = predict(rf, newdata=test.rf)
  # test.rf = cbind(test.rf, prediction)
  # 
  # #discrimination of logistic model: AUC
  # roc = pROC::roc(test.rf$connecting,test.rf$prediction,plot=TRUE)
  # roc
  # 
  # #cutoff for specificity and sensitivity (Youden's J statistic)
  # #40. Schisterman, E. F. et al. Optimal cut-point and its corresponding Youden index to
  # #discriminate individuals using pooled blood samples. Epidemiology 16, 73–81 (2005).
  # library(cutpointr)
  # cp <- cutpointr(test.rf, prediction, connecting, 
  #                 method = maximize_metric, metric = sum_sens_spec)
  # summary(cp)
  # 
  # #calibration of logistic model (goodness-of-fit)
  # #Mcnemar's Test P-Value < 0.05 means significant misclassification (and thus bad goodness-of-fit)
  # test.rf <- test.rf %>%
  #   mutate(prediction = 
  #            case_when(
  #              prediction>=cp$optimal_cutpoint ~ 1, 
  #              prediction<cp$optimal_cutpoint ~ 0
  #            ))
  # caret::confusionMatrix(data = factor(test.rf$prediction), 
  #                        reference = factor(test.rf$connecting))
  # 
  rfmodel = rf
  
  save(rfmodel,file=datafile)
  
  return(list(rfmodel))
  
}



model3_no_egobehavior <- function(redo=FALSE) {
  
  #rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
  
  datafile <- paste(rootdir,"data/model3_no_egobehavior.Rda", sep="/")
  
  if (!redo && file.exists(datafile)) {
    cat("Loading data last updated on ", as.character(file.mtime(datafile)), "\n")
    cat("Call model3_no_egobehavior(redo=TRUE) to update data.\n")
    load(datafile)
    
    return(list(rfmodel))
  }
  
  sample.rf = cdata[c("ID","alterID","game","round","connecting","previouslyconnected","ego_behavior","alter_behavior")]
  sample.cc = sample.rf[complete.cases(sample.rf),]
  
  sample.cc <- sample.cc %>% 
    mutate(connecting = 
             case_when(
               connecting=="makeLink" ~ "C",
               connecting=="notBreakLink" ~ "C",
               connecting=="notMakeLink" ~ "B",
               connecting=="breakLink" ~ "B"
             )
    )
  
  train.rf <- sample.cc %>% sample_frac(0.8) 
  test.rf <- anti_join(sample.cc, train.rf,by=c("ID","alterID","game","round"))
  train.rf = train.rf[c("connecting","previouslyconnected","ego_behavior","alter_behavior")]
  test.rf = test.rf[c("connecting","previouslyconnected","ego_behavior","alter_behavior")]
  
  set.seed(100)
  control.rf <- trainControl(method='repeatedcv', 
                             number=10, 
                             repeats=3,
                             search = 'random',
                             classProbs=TRUE,
                             summaryFunction = twoClassSummary,
                             allowParallel = TRUE)
  
  rf <- caret::train(connecting ~ previouslyconnected + 
                       #ego_behavior + 
                       alter_behavior,
                     data = train.rf,
                     method = 'rf',
                     metric = 'ROC',
                     tuneLength  = 30, 
                     verbose=TRUE,
                     trControl = control.rf)
  # 
  # print(rf)
  # 
  # prediction = predict(rf, newdata=test.rf)
  # test.rf = cbind(test.rf, prediction)
  # 
  # #discrimination of logistic model: AUC
  # roc = pROC::roc(test.rf$connecting,test.rf$prediction,plot=TRUE)
  # roc
  # 
  # #cutoff for specificity and sensitivity (Youden's J statistic)
  # #40. Schisterman, E. F. et al. Optimal cut-point and its corresponding Youden index to
  # #discriminate individuals using pooled blood samples. Epidemiology 16, 73–81 (2005).
  # library(cutpointr)
  # cp <- cutpointr(test.rf, prediction, connecting, 
  #                 method = maximize_metric, metric = sum_sens_spec)
  # summary(cp)
  # 
  # #calibration of logistic model (goodness-of-fit)
  # #Mcnemar's Test P-Value < 0.05 means significant misclassification (and thus bad goodness-of-fit)
  # test.rf <- test.rf %>%
  #   mutate(prediction = 
  #            case_when(
  #              prediction>=cp$optimal_cutpoint ~ 1, 
  #              prediction<cp$optimal_cutpoint ~ 0
  #            ))
  # caret::confusionMatrix(data = factor(test.rf$prediction), 
  #                        reference = factor(test.rf$connecting))
  # 
  rfmodel = rf
  
  save(rfmodel,file=datafile)
  
  return(list(rfmodel))
  
}





model3_no_alterbehavior <- function(redo=FALSE) {
  
  #rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
  
  datafile <- paste(rootdir,"data/model3_no_alterbehavior.Rda", sep="/")
  
  if (!redo && file.exists(datafile)) {
    cat("Loading data last updated on ", as.character(file.mtime(datafile)), "\n")
    cat("Call model3_no_alterbehavior(redo=TRUE) to update data.\n")
    load(datafile)
    
    return(list(rfmodel))
  }
  
  sample.rf = cdata[c("ID","alterID","game","round","connecting","previouslyconnected","ego_behavior","alter_behavior")]
  sample.cc = sample.rf[complete.cases(sample.rf),]
  
  sample.cc <- sample.cc %>% 
    mutate(connecting = 
             case_when(
               connecting=="makeLink" ~ "C",
               connecting=="notBreakLink" ~ "C",
               connecting=="notMakeLink" ~ "B",
               connecting=="breakLink" ~ "B"
             )
    )
  
  train.rf <- sample.cc %>% sample_frac(0.8) 
  test.rf <- anti_join(sample.cc, train.rf,by=c("ID","alterID","game","round"))
  train.rf = train.rf[c("connecting","previouslyconnected","ego_behavior","alter_behavior")]
  test.rf = test.rf[c("connecting","previouslyconnected","ego_behavior","alter_behavior")]
  
  set.seed(100)
  control.rf <- trainControl(method='repeatedcv', 
                             number=10, 
                             repeats=3,
                             search = 'random',
                             classProbs=TRUE,
                             summaryFunction = twoClassSummary,
                             allowParallel = TRUE)
  
  rf <- caret::train(connecting ~ previouslyconnected + 
                       ego_behavior #+ alter_behavior
                       ,
                     data = train.rf,
                     method = 'rf',
                     metric = 'ROC',
                     tuneLength  = 30, 
                     verbose=TRUE,
                     trControl = control.rf)
  # 
  # print(rf)
  # 
  # prediction = predict(rf, newdata=test.rf)
  # test.rf = cbind(test.rf, prediction)
  # 
  # #discrimination of logistic model: AUC
  # roc = pROC::roc(test.rf$connecting,test.rf$prediction,plot=TRUE)
  # roc
  # 
  # #cutoff for specificity and sensitivity (Youden's J statistic)
  # #40. Schisterman, E. F. et al. Optimal cut-point and its corresponding Youden index to
  # #discriminate individuals using pooled blood samples. Epidemiology 16, 73–81 (2005).
  # library(cutpointr)
  # cp <- cutpointr(test.rf, prediction, connecting, 
  #                 method = maximize_metric, metric = sum_sens_spec)
  # summary(cp)
  # 
  # #calibration of logistic model (goodness-of-fit)
  # #Mcnemar's Test P-Value < 0.05 means significant misclassification (and thus bad goodness-of-fit)
  # test.rf <- test.rf %>%
  #   mutate(prediction = 
  #            case_when(
  #              prediction>=cp$optimal_cutpoint ~ 1, 
  #              prediction<cp$optimal_cutpoint ~ 0
  #            ))
  # caret::confusionMatrix(data = factor(test.rf$prediction), 
  #                        reference = factor(test.rf$connecting))
  # 
  rfmodel = rf
  
  save(rfmodel,file=datafile)
  
  return(list(rfmodel))
  
}
