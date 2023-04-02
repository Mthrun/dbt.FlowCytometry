SelectOptimalPopulations=function(Vars,PopNo,Pops,ClsDiag,Iterations=200,cl,PlotIt=FALSE){
  #Optimum is defined by highest MCC and highes Population percentage overall patients given a target number of populations
  # that should be in the result
  #Weightin of  MCC is 3 times larger than for Population percentage
  
  ResEval=parLapply(cl = cl,X = 1:Iterations,function(i,PopNo,Vars,Pops,ClsDiag){
    if(length(Vars)>PopNo){
      Vars_Cur=sample(Vars,PopNo)
    }else{
      draw=PopNo-length(Vars)
      AllVars=colnames(Pops)
      AllVars[which(match(Vars,table = AllVars,nomatch = 0)==0)]
      Vars_Cur=c(Vars,sample(AllVars,size = draw))
    }
    rankfolge=match(Vars_Cur,table = Vars,nomatch = 0)
    indcols2=match(Vars_Cur,colnames(Pops))
    trainedmodel =TreeBasedClassifiers::GenerateDecisionTree(Data =Pops[,indcols2],ClsDiag)
    trained =TreeBasedClassifiers::PredictFromTreelist(trainedmodel$TreeList,Pops[,indcols2])
    mcc=XAI::QualityMeasureXAI(trained ,ClsDiag)$MCC
    return(list(MCC=mcc,Vars=Vars_Cur,SumRang=sum(rankfolge),rankfolge=rankfolge))
  },PopNo,Vars ,Pops,ClsDiag)
  
  MCC =sapply(ResEval, "[[",1)
  if(isTRUE(PlotIt)){
    MDplot(MCC )
  }
  Vars_V=lapply(ResEval, "[[",2)
  RankSum =sapply(ResEval, "[[",3)
  Ranks=lapply(ResEval, "[[",4)
  
  T1_ind=which.min(MCC +(RankSum /max(RankSum ))/3)
  Vars_Final=Vars_V[[T1_ind]]
  Vars_Final=sort(Vars_Final)
  return(list(OptimalPops=Vars_Final,MCC=MCC [T1_ind],Ranks=Ranks[[T1_ind]]))
}
