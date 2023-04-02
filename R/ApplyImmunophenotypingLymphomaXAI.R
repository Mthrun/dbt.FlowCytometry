ApplyImmunophenotypingLymphomaXAI=function(TreeLists,ALPODS_Populations,TestClsPerPatient){
  
  KrankVsNB_List=TreeLists$KrankVsNB_List
  if(is.null(KrankVsNB_List)){
    stop("ApplyImmunophenotypingLymphomaXAI::TreeLists list element named KrankVsNB_List is missing ")
  }
  CLLvsOthers_List=TreeLists$CLLvsOthers_List
  if(is.null(CLLvsOthers_List)){
    stop("ApplyImmunophenotypingLymphomaXAI::TreeLists list element named CLLvsOthers_List is missing ")
  }
  OtherIllness_List=TreeLists$OtherIllness_List
  if(is.null(OtherIllness_List)){
    stop("ApplyImmunophenotypingLymphomaXAI::TreeLists list element named OtherIllness_List is missing ")
  }
  
  MaybeNB_List=TreeLists$MaybeNB_List
  if(is.null(MaybeNB_List)){
    stop("ApplyImmunophenotypingLymphomaXAI::TreeLists list element named MaybeNB_List is missing ")
  }
  
  Cls_KrankVsNB=TreeBasedClassifiers::PredictFromTreelist(KrankVsNB_List$TreeList,ALPODS_Populations)
  ind_NonNB=which(Cls_KrankVsNB>=2)
  ind_mayNB=which(Cls_KrankVsNB==1)
  
  Cls_CLLvsOther_predicted=TreeBasedClassifiers::PredictFromTreelist(CLLvsOthers_List$TreeList,ALPODS_Populations[ind_NonNB,])
  
  Cls_NB_predicted=TreeBasedClassifiers::PredictFromTreelist(MaybeNB_List$TreeList,ALPODS_Populations[ind_mayNB,])
  
  ind_NonNB_NonCLL=which(Cls_CLLvsOther_predicted>=3)
  Cls_OtherIllness=TreeBasedClassifiers::PredictFromTreelist(OtherIllness_List$TreeList,ALPODS_Populations[ind_NonNB[ind_NonNB_NonCLL],])
  
  #combine cls
  #first non ill
  ClsCombined=rep(NaN,nrow(ALPODS_Populations))
  #second cll
  ClsCombined[ind_mayNB]=Cls_NB_predicted
  ClsCombined[ind_NonNB]=Cls_CLLvsOther_predicted
  #third others illness last
  ClsCombined[ind_NonNB[ind_NonNB_NonCLL]]=Cls_OtherIllness
  
  if(missing(TestClsPerPatient)){
    MCC=NULL
    contingency=NULL
    contingency_norm=NULL
  }else{
    if(requireNamespace("FCPS"))
      MCC=FCPS::ClusterMCC(TestClsPerPatient,ClsCombined)
    else
      MCC=NULL
    
  contingency=table(ClsCombined,TestClsPerPatient)
  contingency
  
  SumTrainClsPerPatients=apply(as.matrix(contingency),2,sum)
  contingency_norm=contingency
  for(i in 1:ncol(contingency_norm))
    contingency_norm[,i]=round( contingency_norm[,i]/SumTrainClsPerPatients[i],3)*100
  }
  
  return(list(MCC=MCC,PredictedCls=ClsCombined,Contingency=contingency,NormalizedContigency=contingency_norm))
  
}