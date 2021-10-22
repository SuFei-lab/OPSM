#' Compute the value of ROC for markers or compare the values of ROC by using two methods
#'
#' @param data data is a data.frame.The first column is the name of samples, the second column is the class of samples and the rest columns are the expression of features(eg.miRNAs).
#'
#'
#' @return A dataframe containing eight columns for combined ROC, single ROC, accuracy, sensitivity, accuracy, specificity, and Fm values.
#' @export
#'
#' @examples ROC(data)
ROC<-function(data){
  library("rpart")
  library("rpart.plot")
  library("pROC")

  prerat_conb <- c()
  senrat_conb <- c()
  sperat_conb <- c()
  precision_conb<-c()
  F_measure_conb<-c()
  mir_name<-c()

  roc_single_value<-c()
  roc_cob_value<-c()

  select<-data
  select_class <- select[,2]
  cname <-colnames(select)[-c(1:2)]
  data2<-cbind(select[,2],select[,cname])
  colnames(data2)<-c("select_class",as.character(cname))

  fit1 <- rpart(as.formula(paste("select_class ~ ",paste(cname, collapse= "+"))), data=data2,method="class",control=rpart.control(minbucket=1,cp=0.001))
  fit1 <- prune(fit1,cp=fit1$cptable[which.min(fit1$cptable[,"rel error"]),"CP"])

  preresult_select<-table(predict(fit1, newdata=data2,type = "class"),data2[,"select_class"])
  preratio_select<-sum(unname(diag(preresult_select)))/sum(unname(as.matrix(preresult_select)))
  senratio_select<- preresult_select[2,2]/sum(preresult_select[2,2]+preresult_select[1,2])
  speratio_select<- preresult_select[1,1]/sum(preresult_select[2,1]+preresult_select[1,1])
  precision_select<- preresult_select[2,2]/sum(preresult_select[2,2]+preresult_select[2,1])
  F_measure_select<- 2*senratio_select*precision_select/(senratio_select+precision_select)

  mir_in_tree<-unique(fit1[[1]][1])
  mir_in_tree_and_select<-colnames(select)[match(mir_in_tree[,1],colnames(select))]
  mir_in_tree_and_select_not_na<-mir_in_tree_and_select[-which(is.na(mir_in_tree_and_select))]

  if(length(mir_in_tree_and_select_not_na)>=1){
    label_select<-select_class
    for(m in 1:length(mir_in_tree_and_select_not_na)){
      rank_select<-select[,mir_in_tree_and_select_not_na[m]]
      mir_name<-c(mir_name,mir_in_tree_and_select_not_na[m])
      fit_select <- glm(label_select ~ rank_select, family=binomial)
      roc_single_select_plot <- roc(label_select, fit_select$linear.predictors,percent=TRUE, ci=TRUE,col=4,print.auc=TRUE)
      roc_single_select<-roc_single_select_plot[[9]][1]
      roc_single_value<-c(roc_single_value,roc_single_select)

      b_select=data.frame(select[,mir_in_tree_and_select_not_na],label_select)
      fit_cobined_select<-glm(label_select~as.matrix(select[,mir_in_tree_and_select_not_na]),data=b_select,family="binomial")
      roc_cobined_select_plot <- roc(label_select, fit_cobined_select$linear.predictors, col=2,print.auc=TRUE)
      roc_cobined_select<-roc_cobined_select_plot [[9]][1]
      roc_cob_value<-c(roc_cob_value,roc_cobined_select)

      prerat_conb<-c(prerat_conb,preratio_select)
      precision_conb<-c(precision_conb,precision_select)
      senrat_conb<-c(senrat_conb,senratio_select)
      sperat_conb<-c(sperat_conb,speratio_select)
      F_measure_conb<-c(F_measure_conb,F_measure_select)

    }
  }
  result<-data.frame(mir_name,roc_cob_value,roc_single_value,prerat_conb,precision_conb,senrat_conb,sperat_conb,F_measure_conb)
  a<-table(result[,1])
  frequency <- a[order(a,decreasing=TRUE)]
  d <- cbind(frequency,names(frequency))
  colnames(d)<-c("frequency","mir_name")
  newdata<-merge(unique(result),unique(d),by="mir_name")
  return(newdata)
}
