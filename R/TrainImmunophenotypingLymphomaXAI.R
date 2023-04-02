TrainImmunophenotypingLymphomaXAI=function(ALPODS_Populations,TrainClsPerPatient,OrderBySum=FALSE,...)
{
  if(isTRUE(OrderBySum)){
    
    indorder=order(apply(ALPODS_Populations, 2, sum),decreasing = TRUE)
    ALPODS_Populations=ALPODS_Populations[,indorder]
  }

  
  #Step1: Classify ill vs non ill
  KrankVsNB=TrainClsPerPatient
  KrankVsNB[KrankVsNB>2]=3
  
  KrankVsNB_List=TreeBasedClassifiers::GenerateDecisionTree(Data = ALPODS_Populations,Cls = KrankVsNB,PlotIt=F,...)
  Cls_KrankVsNB=TreeBasedClassifiers::PredictFromTreelist(KrankVsNB_List$TreeList,ALPODS_Populations)
  #table(Cls_KrankVsNB,KrankVsNB)
  
  #step2: select the as ill classified and classfy cll vs non cll
  ind_NonNB=which(Cls_KrankVsNB>=2)
  CLLvsOthers= TrainClsPerPatient[ind_NonNB]
  CLLvsOthers[CLLvsOthers>2]=3
  CLLvsOthers_List=TreeBasedClassifiers::GenerateDecisionTree(Data = ALPODS_Populations[ind_NonNB,],Cls = CLLvsOthers,PlotIt=F,...)
  Cls_CLLvsOther_predicted=TreeBasedClassifiers::PredictFromTreelist(CLLvsOthers_List$TreeList,ALPODS_Populations[ind_NonNB,])
  
  #classify maybnb
  ind_maybeNB=which(Cls_KrankVsNB==1)
  MaybeNBTrain= TrainClsPerPatient[ind_maybeNB]
 
  MaybeNB_List=TreeBasedClassifiers::GenerateDecisionTree(Data = ALPODS_Populations[ind_maybeNB,],Cls = MaybeNBTrain,PlotIt=F,...)
  MaybeNB_predicted=TreeBasedClassifiers::PredictFromTreelist(MaybeNB_List$TreeList,ALPODS_Populations[ind_maybeNB,])
  
  #table(Cls_CLLvsOther_predicted,CLLvsOthers)
  
  #step3: select non cll and classify nb, cll and all others
  ind_NonNB_NonCLL=which(Cls_CLLvsOther_predicted>=3)
  OtherIllness_List=TreeBasedClassifiers::GenerateDecisionTree(Data = ALPODS_Populations[ind_NonNB[ind_NonNB_NonCLL],],Cls = TrainClsPerPatient[ind_NonNB[ind_NonNB_NonCLL]],PlotIt=F,...)
  Cls_OtherIllness=TreeBasedClassifiers::PredictFromTreelist(OtherIllness_List$TreeList,ALPODS_Populations[ind_NonNB[ind_NonNB_NonCLL],])
  table(Cls_OtherIllness,TrainClsPerPatient[ind_NonNB[ind_NonNB_NonCLL]])
  
  #combine cls
  #first non ill
  ClsCombined=rep(NaN,nrow(ALPODS_Populations))
  #second cll
  ClsCombined[ind_NonNB]=Cls_CLLvsOther_predicted
  #class 1
  ClsCombined[ind_maybeNB]=MaybeNB_predicted
  #third others illness last
  ClsCombined[ind_NonNB[ind_NonNB_NonCLL]]=Cls_OtherIllness
  
  if(requireNamespace("FCPS"))
   MCC=FCPS::ClusterMCC(TrainClsPerPatient,ClsCombined)
  else
    MCC=NULL
  
  contingency=table(ClsCombined,TrainClsPerPatient)
  contingency
  
  #baum fuer test set
  # BalancedInd=FCPS::ClusterEqualWeighting(KrankVsNB)$BalancedInd
  # KrankVsNB_List=TreeBasedClassifiers::GenerateDecisionTree(Data = ALPODS_Populations[BalancedInd,],Cls = KrankVsNB{BalancedInd},PlotIt=F,...)
  # 
  # 
  SumTrainClsPerPatients=apply(as.matrix(contingency),2,sum)
  contingency_norm=contingency
  for(i in 1:ncol(contingency_norm))
    contingency_norm[,i]=round( contingency_norm[,i]/SumTrainClsPerPatients[i],3)*100
  
  return(list(MCC=MCC,TreeLists=list(KrankVsNB_List=KrankVsNB_List,MaybeNB_List=MaybeNB_List,CLLvsOthers_List=CLLvsOthers_List,OtherIllness_List=OtherIllness_List),
              ModelClsPerPatient=ClsCombined,Contingency=contingency,NormalizedContigency=contingency_norm))
  
}