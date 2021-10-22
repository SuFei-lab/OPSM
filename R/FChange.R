#' fold change
#'
#' @param datacase A data frame in which the columns are samples, the rows are features.The colunms'names are the names of samples, and the rows'names are the names of features.
#' @param datacontrol A data frame in which the columns are samples, the rows are features.The colunms'names are the names of samples, and the rows'names are the names of features.
#'
#' @return a data frame with two columns which are genenames and fold change.
#' @export
#'
#' @examples  FChange(datacase,datacontrol)


FChange<-function(datacase,datacontrol){


    DE_Gene<-vector();																																					
    DE_Fold<-vector(); 																																							
    Flag<-0;
  
    
    for(m in 1:nrow(datacase)){																																					

      Case_Mean<-mean(as.numeric(t(datacase[m,])[-1,1]));	  																																						
      Control_Mean<-mean(as.numeric(t(datacontrol[m,])[-1,1]));																																						
      Fold_Value<-Case_Mean/Control_Mean;  																																						
      
      Flag=Flag+1;																																							
      DE_Gene[Flag]<-as.character(rownames(datacase)[m])
      DE_Fold[Flag]<-Fold_Value;																																					                   																																							
    }
    
    Out<-c(); 																																						
    Out<-cbind(Gene=DE_Gene, fold=DE_Fold);		
   
    return(Out)
    

}
