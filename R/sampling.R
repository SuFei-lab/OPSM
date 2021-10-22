#' Using two dataframes to create sample sets.It will return 3*5*count training sets, 3*5*count test sets and 3*5*count evaluation sets in your working space.
#'
#' @param datacase A dataframe of expression profile which the samples are case-sample.The list of gene names is the first column of the dataframe.
#' @param datacontrol A dataframe of expression profile which the samples are control-sample.The list of gene names is the first column of the dataframe.
#' @param count A numeric variable that controls the number of random samples.
#'
#' @return 3*5*count training sets, 3*5*count test sets and 3*5*count evaluation sets in your working space.
#' @export
#'
#' @examples Sampling(data1,data2,100)#data1 is a data.frame of expression profile which the samples are case-sample,data2 is also a data.frame of expression profile which the samples are control-sample
Sampling<-function(datacase,datacontrol,count){
  gene_name<-data.frame(mirname=datacase[,1])
  case_sample<-data.frame(datacase[,-1])
  control_sample<-data.frame(datacontrol[,-1])
  for(k in 1:count){
    a1 <- sample(1:dim(case_sample)[2], floor(dim(case_sample)[2]/3))
    b1<- sample(1:dim(control_sample)[2], floor(dim(control_sample)[2]/3))
    c1_case <- case_sample[,a1]
    c1_control <- control_sample[,b1]

    nn1 <- setdiff(1:dim(case_sample)[2],a1)
    mm1 <- setdiff(1:dim(control_sample)[2],b1)
    a2 <- sample(nn1, floor(dim(case_sample)[2]/3))
    b2 <- sample(mm1, floor(dim(control_sample)[2]/3))
    c2_case <-  case_sample[,a2]
    c2_control <-control_sample[,b2]

    s1<-c(a1,a2)
    s2<-c(b1,b2)
    ss1<-setdiff(1:dim(case_sample)[2],s1)
    ss2<- setdiff(1:dim(control_sample)[2],s2)
    a3 <- sample(ss1,floor(dim(case_sample)[2]/3))
    b3 <- sample(ss2,floor(dim(control_sample)[2]/3))
    c3_case <-  case_sample[,a3]
    c3_control <-control_sample[,b3]

    nn2 <- setdiff(1:dim(case_sample)[2],a2)
    mm2 <- setdiff(1:dim(control_sample)[2],b2)

    nn3<- setdiff(1:dim(case_sample)[2],a3)
    mm3 <- setdiff(1:dim(control_sample)[2],b3)

    a4<-sample(nn1,floor(length(nn1)/5))
    b4<-sample(mm1,floor(length(mm1)/5))
    d4_case <-data.frame(case_sample[,a4])
    d4_control <-data.frame(control_sample[,b4] )
    colnames(d4_control)<-colnames(control_sample)[b4]

    nnn1<-setdiff(nn1,a4)
    mmm1<- setdiff(mm1,b4)
    a5<-sample(nnn1,floor(length(nn1)/5))
    b5<-sample(mmm1,floor(length(mm1)/5))
    d5_case <-data.frame(case_sample[,a5])
    d5_control <-data.frame(control_sample[,b5])
    colnames(d5_control)<-colnames(control_sample)[b5]

    nnn2<-setdiff(nnn1,a5)
    mmm2<- setdiff(mmm1,b5)
    a6<-sample(nnn2,floor(length(nn1)/5))
    b6<-sample(mmm2,floor(length(mm1)/5))
    d6_case <-data.frame(case_sample[,a6])
    d6_control <-data.frame(control_sample[,b6])
    colnames(d6_control)<-colnames(control_sample)[b6]

    nnn3<-setdiff(nnn2,a6)
    mmm3<- setdiff(mmm2,b6)
    a7<-sample(nnn1,floor(length(nn1)/5))
    b7<-sample(mmm1,floor(length(mm1)/5))
    d7_case <-data.frame(case_sample[,a7]	)
    d7_control <-data.frame(control_sample[,b7])
    colnames(d7_control)<-colnames(control_sample)[b7]

    nnn4<-setdiff(nnn3,a7)
    mmm4<- setdiff(mmm3,b7)
    a8<-sample(nnn1,floor(length(nn1)/5))
    b8<-sample(mmm1,floor(length(mm1)/5))
    d8_case <-data.frame(case_sample[,a8])
    d8_control <-data.frame(control_sample[,b8])
    colnames(d8_control)<-colnames(control_sample)[b8]



    test1_case<-data.frame(gene_name,c1_case)
    test1_control<-data.frame(gene_name,c1_control)
    evaluate1_case<-data.frame(gene_name,d4_case	)
    evaluate1_control<-data.frame(gene_name,d4_control	)
    select1_case<-cbind(gene_name,d5_case,d6_case,d7_case,d8_case)
    select1_control<-cbind(gene_name,d5_control,d6_control,d7_control,d8_control)

    write.csv(test1_case,paste("test1_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test1_control,paste("test1_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate1_case,paste("evaluate1_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate1_control,paste("evaluate1_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select1_case,paste("select1_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select1_control,paste("select1_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test2_case<-data.frame(gene_name,c1_case)
    test2_control<-data.frame(gene_name,c1_control)
    evaluate2_case<-data.frame(gene_name,d5_case	)
    evaluate2_control<-data.frame(gene_name,d5_control	)
    select2_case<-cbind(gene_name,d4_case,d6_case,d7_case,d8_case)
    select2_control<-cbind(gene_name,d4_control,d6_control,d7_control,d8_control)

    write.csv(test2_case,paste("test2_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test2_control,paste("test2_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate2_case,paste("evaluate2_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate2_control,paste("evaluate2_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select2_case,paste("select2_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select2_control,paste("select2_control[",k,"].csv",sep=""),quote=F,row.names=T)




    test3_case<-data.frame(gene_name,c1_case)
    test3_control<-data.frame(gene_name,c1_control)
    evaluate3_case<-data.frame(gene_name,d6_case	)
    evaluate3_control<-data.frame(gene_name,d6_control	)
    select3_case<-cbind(gene_name,d4_case,d5_case,d7_case,d8_case)
    select3_control<-cbind(gene_name,d4_control,d5_control,d7_control,d8_control)


    write.csv(test3_case,paste("test3_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test3_control,paste("test3_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate3_case,paste("evaluate3_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate3_control,paste("evaluate3_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select3_case,paste("select3_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select3_control,paste("select3_control[",k,"].csv",sep=""),quote=F,row.names=T)





    test4_case<-data.frame(gene_name,c1_case)
    test4_control<-data.frame(gene_name,c1_control)
    evaluate4_case<-data.frame(gene_name,d7_case	)
    evaluate4_control<-data.frame(gene_name,d7_control	)
    select4_case<-cbind(gene_name,d4_case,d5_case,d6_case,d8_case)
    select4_control<-cbind(gene_name,d4_control,d5_control,d6_control,d8_control)

    write.csv(test4_case,paste("test4_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test4_control,paste("test4_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate4_case,paste("evaluate4_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate4_control,paste("evaluate4_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select4_case,paste("select4_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select4_control,paste("select4_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test5_case<-data.frame(gene_name,c1_case)
    test5_control<-data.frame(gene_name,c1_control)
    evaluate5_case<-data.frame(gene_name,d8_case	)
    evaluate5_control<-data.frame(gene_name,d8_control	)
    select5_case<-cbind(gene_name,d4_case,d5_case,d6_case,d7_case)
    select5_control<-cbind(gene_name,d4_control,d5_control,d6_control,d7_control)

    write.csv(test5_case,paste("test5_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test5_control,paste("test5_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate5_case,paste("evaluate5_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate5_control,paste("evaluate5_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select5_case,paste("select5_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select5_control,paste("select5_control[",k,"].csv",sep=""),quote=F,row.names=T)


    a4<-sample(nn2,floor(length(nn2)/5))
    b4<-sample(mm2,floor(length(mm2)/5))
    d4_case <-data.frame(case_sample[,a4])
    d4_control <-data.frame(control_sample[,b4] )
    colnames(d4_control)<-colnames(control_sample)[b4]

    nnn1<-setdiff(nn2,a4)
    mmm1<- setdiff(mm2,b4)
    a5<-sample(nnn1,floor(length(nn2)/5))
    b5<-sample(mmm1,floor(length(mm2)/5))
    d5_case <-data.frame(case_sample[,a5])
    d5_control <-data.frame(control_sample[,b5])
    colnames(d5_control)<-colnames(control_sample)[b5]

    nnn2<-setdiff(nnn1,a5)
    mmm2<- setdiff(mmm1,b5)
    a6<-sample(nnn2,floor(length(nn2)/5))
    b6<-sample(mmm2,floor(length(mm2)/5))
    d6_case <-data.frame(case_sample[,a6])
    d6_control <-data.frame(control_sample[,b6])
    colnames(d6_control)<-colnames(control_sample)[b6]

    nnn3<-setdiff(nnn2,a6)
    mmm3<- setdiff(mmm2,b6)
    a7<-sample(nnn1,floor(length(nn2)/5))
    b7<-sample(mmm1,floor(length(mm2)/5))
    d7_case <-data.frame(case_sample[,a7])
    d7_control <-data.frame(control_sample[,b7])
    colnames(d7_control)<-colnames(control_sample)[b7]

    nnn4<-setdiff(nnn3,a7)
    mmm4<- setdiff(mmm3,b7)
    a8<-sample(nnn1,floor(length(nn2)/5))
    b8<-sample(mmm1,floor(length(mm2)/5))
    d8_case <-data.frame(case_sample[,a8])
    d8_control <-data.frame(control_sample[,b8])
    colnames(d8_control)<-colnames(control_sample)[b8]

    test6_case<-data.frame(gene_name,c2_case)
    test6_control<-data.frame(gene_name,c2_control)
    evaluate6_case<-data.frame(gene_name,d4_case	)
    evaluate6_control<-data.frame(gene_name,d4_control	)
    select6_case<-cbind(gene_name,d5_case,d6_case,d7_case,d8_case)
    select6_control<-cbind(gene_name,d5_control,d6_control,d7_control,d8_control)

    write.csv(test6_case,paste("test6_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test6_control,paste("test6_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate6_case,paste("evaluate6_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate6_control,paste("evaluate6_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select6_case,paste("select6_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select6_control,paste("select6_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test7_case<-data.frame(gene_name,c2_case)
    test7_control<-data.frame(gene_name,c2_control)
    evaluate7_case<-data.frame(gene_name,d5_case	)
    evaluate7_control<-data.frame(gene_name,d5_control	)
    select7_case<-cbind(gene_name,d4_case,d6_case,d7_case,d8_case)
    select7_control<-cbind(gene_name,d4_control,d6_control,d7_control,d8_control)

    write.csv(test7_case,paste("test7_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test7_control,paste("test7_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate7_case,paste("evaluate7_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate7_control,paste("evaluate7_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select7_case,paste("select7_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select7_control,paste("select7_control[",k,"].csv",sep=""),quote=F,row.names=T)




    test8_case<-data.frame(gene_name,c2_case)
    test8_control<-data.frame(gene_name,c2_control)
    evaluate8_case<-data.frame(gene_name,d6_case	)
    evaluate8_control<-data.frame(gene_name,d6_control	)
    select8_case<-cbind(gene_name,d4_case,d5_case,d7_case,d8_case)
    select8_control<-cbind(gene_name,d4_control,d5_control,d7_control,d8_control)

    write.csv(test8_case,paste("test8_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test8_control,paste("test8_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate8_case,paste("evaluate8_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate8_control,paste("evaluate8_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select8_case,paste("select8_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select8_control,paste("select8_control[",k,"].csv",sep=""),quote=F,row.names=T)





    test9_case<-data.frame(gene_name,c2_case)
    test9_control<-data.frame(gene_name,c2_control)
    evaluate9_case<-data.frame(gene_name,d7_case	)
    evaluate9_control<-data.frame(gene_name,d7_control	)
    select9_case<-cbind(gene_name,d4_case,d5_case,d6_case,d8_case)
    select9_control<-cbind(gene_name,d4_control,d5_control,d6_control,d8_control)

    write.csv(test9_case,paste("test9_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test9_control,paste("test9_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate9_case,paste("evaluate9_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate9_control,paste("evaluate9_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select9_case,paste("select9_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select9_control,paste("select9_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test10_case<-data.frame(gene_name,c2_case)
    test10_control<-data.frame(gene_name,c2_control)
    evaluate10_case<-data.frame(gene_name,d8_case	)
    evaluate10_control<-data.frame(gene_name,d8_control	)
    select10_case<-cbind(gene_name,d4_case,d5_case,d6_case,d7_case)
    select10_control<-cbind(gene_name,d4_control,d5_control,d6_control,d7_control)

    write.csv(test10_case,paste("test10_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test10_control,paste("test10_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate10_case,paste("evaluate10_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate10_control,paste("evaluate10_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select10_case,paste("select10_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select10_control,paste("select10_control[",k,"].csv",sep=""),quote=F,row.names=T)


    a4<-sample(nn3,floor(length(nn3)/5))
    b4<-sample(mm3,floor(length(mm3)/5))
    d4_case <-data.frame(case_sample[,a4])
    d4_control <-data.frame(control_sample[,b4])
    colnames(d4_control)<-colnames(control_sample)[b4]

    nnn1<-setdiff(nn3,a4)
    mmm1<- setdiff(mm3,b4)
    a5<-sample(nnn1,floor(length(nn3)/5))
    b5<-sample(mmm1,floor(length(mm3)/5))
    d5_case <-data.frame(case_sample[,a5])
    d5_control <-data.frame(control_sample[,b5])
    colnames(d5_control)<-colnames(control_sample)[b5]

    nnn2<-setdiff(nnn1,a5)
    mmm2<- setdiff(mmm1,b5)
    a6<-sample(nnn2,floor(length(nn3)/5))
    b6<-sample(mmm2,floor(length(mm3)/5))
    d6_case <-data.frame(case_sample[,a6])
    d6_control <-data.frame(control_sample[,b6])
    colnames(d6_control)<-colnames(control_sample)[b6]

    nnn3<-setdiff(nnn2,a6)
    mmm3<- setdiff(mmm2,b6)
    a7<-sample(nnn1,floor(length(nn3)/5))
    b7<-sample(mmm1,floor(length(mm3)/5))
    d7_case <-data.frame(case_sample[,a7])
    d7_control <-data.frame(control_sample[,b7])
    colnames(d7_control)<-colnames(control_sample)[b7]

    nnn4<-setdiff(nnn3,a7)
    mmm4<- setdiff(mmm3,b7)
    a8<-sample(nnn1,floor(length(nn3)/5))
    b8<-sample(mmm1,floor(length(mm3)/5))
    d8_case <-data.frame(case_sample[,a8])
    d8_control <-data.frame(control_sample[,b8])
    colnames(d8_control)<-colnames(control_sample)[b8]

    test11_case<-data.frame(gene_name,c3_case)
    test11_control<-data.frame(gene_name,c3_control)
    evaluate11_case<-data.frame(gene_name,d4_case	)
    evaluate11_control<-data.frame(gene_name,d4_control	)
    select11_case<-cbind(gene_name,d5_case,d6_case,d7_case,d8_case)
    select11_control<-cbind(gene_name,d5_control,d6_control,d7_control,d8_control)

    write.csv(test11_case,paste("test11_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test11_control,paste("test11_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate11_case,paste("evaluate11_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate11_control,paste("evaluate11_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select11_case,paste("select11_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select11_control,paste("select11_control[",k,"].csv",sep=""),quote=F,row.names=T)

    test12_case<-data.frame(gene_name,c3_case)
    test12_control<-data.frame(gene_name,c3_control)
    evaluate12_case<-data.frame(gene_name,d5_case	)
    evaluate12_control<-data.frame(gene_name,d5_control	)
    select12_case<-cbind(gene_name,d4_case,d6_case,d7_case,d8_case)
    select12_control<-cbind(gene_name,d4_control,d6_control,d7_control,d8_control)

    write.csv(test12_case,paste("test12_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test12_control,paste("test12_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate12_case,paste("evaluate12_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate12_control,paste("evaluate12_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select12_case,paste("select12_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select12_control,paste("select12_control[",k,"].csv",sep=""),quote=F,row.names=T)

    test13_case<-data.frame(gene_name,c3_case)
    test13_control<-data.frame(gene_name,c3_control)
    evaluate13_case<-data.frame(gene_name,d6_case	)
    evaluate13_control<-data.frame(gene_name,d6_control	)
    select13_case<-cbind(gene_name,d4_case,d5_case,d7_case,d8_case)
    select13_control<-cbind(gene_name,d4_control,d5_control,d7_control,d8_control)

    write.csv(test13_case,paste("test13_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test13_control,paste("test13_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate13_case,paste("evaluate13_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate13_control,paste("evaluate13_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select13_case,paste("select13_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select13_control,paste("select13_control[",k,"].csv",sep=""),quote=F,row.names=T)


    test14_case<-data.frame(gene_name,c3_case)
    test14_control<-data.frame(gene_name,c3_control)
    evaluate14_case<-data.frame(gene_name,d7_case	)
    evaluate14_control<-data.frame(gene_name,d7_control	)
    select14_case<-cbind(gene_name,d4_case,d5_case,d6_case,d8_case)
    select14_control<-cbind(gene_name,d4_control,d5_control,d6_control,d8_control)
    write.csv(test14_case,paste("test14_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test14_control,paste("test14_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate14_case,paste("evaluate14_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate14_control,paste("evaluate14_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select14_case,paste("select14_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select14_control,paste("select14_control[",k,"].csv",sep=""),quote=F,row.names=T)

    test15_case<-data.frame(gene_name,c3_case)
    test15_control<-data.frame(gene_name,c3_control)
    evaluate15_case<-data.frame(gene_name,d8_case	)
    evaluate15_control<-data.frame(gene_name,d8_control	)
    select15_case<-cbind(gene_name,d4_case,d5_case,d6_case,d7_case)
    select15_control<-cbind(gene_name,d4_control,d5_control,d6_control,d7_control)

    write.csv(test15_case,paste("test15_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(test15_control,paste("test15_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate15_case,paste("evaluate15_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(evaluate15_control,paste("evaluate15_control[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select15_case,paste("select15_case[",k,"].csv",sep=""),quote=F,row.names=T)
    write.csv(select15_control,paste("select15_control[",k,"].csv",sep=""),quote=F,row.names=T)

  }
}
