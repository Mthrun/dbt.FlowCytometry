ApplyRuleClassifier=function(DataOrDataList,Cls,FUN,cl=NULL){

  if(is.character(FUN))
    FUN=get(FUN)

  if(is.list(DataOrDataList)){
    if(is.null(cl)){
      out=lapply(DataOrDataList, FUN)
    }else{
      clusterExport(cl, varlist = "findAttrCol")
      out=parallel::parLapply(cl = cl,X = DataOrDataList, fun = FUN)
    }
  RulesNo=names(out[[1]]$IndList)
  PopsInds=lapply(out, function(x) x$IndList)
  Pops=mapply(function(x,y){
    Pop=c()
    for(i in 1:length(x))
      Pop=c(Pop,list(length(x[[i]])/nrow(y)))

    return(Pop)
  }  ,PopsInds,DataOrDataList,SIMPLIFY = F)


  Pops
  PopsMat=matrix(NaN,nrow =length(Pops) ,ncol = length(Pops[[1]]))
  for(i in 1:length(Pops)){
    PopsMat[i,]=unlist(Pops[[i]])
  }

  colnames(PopsMat)=RulesNo
  PopsMat
  thresholdsPops=EmpiricalBayesDecisionLimitPerFeature(Data = PopsMat,Cls = Cls)
  thresholdsPops


  count=PopsMat*0
  for(i in 1:ncol(PopsMat)){
    for(j in 1:nrow(PopsMat))
      count[j,i]=PopsMat[j,i]>thresholdsPops[i]
  }
  count


  Class1=apply(count, 1, function(x) sum(x==0))

  Class2=apply(count, 1, function(x) sum(x==1))

  Prediction=cbind(Class1,Class2)/ncol(count)
  Prediction

  TestCls=apply(Prediction, 1, which.max)
  TestCls

  #Nun wissen wir nicht welche Klasse hier welche ist, also waehlen wir die permutation mit dem besten ergebnis
  Permute=function(Cls,TestCls){
    allPossiblePermutations <- pracma::perms(unique(TestCls))
    nrOfPermutations <- nrow(allPossiblePermutations)
    nrOfStdClasses <- ncol(allPossiblePermutations)
    givenClasses <- sort(na.last = T, unique(Cls))
    nrOfGivenClasses <- length(givenClasses)
    renamedCls <- TestCls
    bestAccuracy <- 0
    whichbest=c()

    for (i in 1:nrOfPermutations) {
      tryRenameCls <- TestCls
      newClassNames <- c(1:nrOfGivenClasses)
      newClassNames[1:nrOfStdClasses] <- allPossiblePermutations[i, ]
      for (j in 1:nrOfGivenClasses) {
        tryRenameCls[which(TestCls == givenClasses[j])] <- newClassNames[j]
      }
      accuracy <- sum(tryRenameCls == Cls)
      if (accuracy > bestAccuracy) {
        renamedCls <- tryRenameCls
        bestAccuracy <- accuracy
        whichbest=i
      }
    }
    return(renamedCls)
  }
  TestCls=Permute(Cls,TestCls)
  V=dbt.ClassAnalysis::BiClass_SensitivitySpecifity(Cls,TestCls)
  Sensitivity = V$Sensitivity
  Specifity  = V$Specifity
  Accuracy = V$Accuracy
  ClassifierAnalysis=cbind(Sensitivity,Specifity,Accuracy)

  ClsMat=apply(count, 2, function(x,y) Permute(y,x+1),Cls)

  ClassAnalysis=apply(ClsMat,2,function(x,y){
    V=dbt.ClassAnalysis::BiClass_SensitivitySpecifity(y,x)
    Sensitivity = V$Sensitivity
    Specifity  = V$Specifity
    Accuracy = V$Accuracy
    return(cbind(Sensitivity,Specifity,Accuracy))
  },Cls)


  rownames(ClassAnalysis)=c("Sensitivity","Specifity","Accuracy")
  ClassAnalysis

  return(list(ClassifierAnalysis=ClassifierAnalysis,RuleCls=TestCls,Prediction=Prediction,  ClassAnalysis=t(ClassAnalysis),PopulationMatrix=PopsMat,ThresholdsPops=thresholdsPops,ClsMatrixPerRule=ClsMat))
  }else{
    warning("For Non List Data not implemented yet")
  }
}
