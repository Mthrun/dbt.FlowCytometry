RF_Lime=function(TrainData,TestData,TrainCls,PriorTestCls, n_labels = 1, n_features = 4,PlotIt=TRUE,NumberOfTrees=100,cl){
#RF_Lime(TrainData,TestData,TrainCls,PriorTestCls, n_labels = 1, n_features = 4,PlotIt=TRUE)
  requireNamespace("lime")
  requireNamespace("FCPS")
  requireNamespace("stringr")
  requireNamespace("dbt.ClassAnalysis")
  #source('~/Subversion/PUB/dbt/Classifiers/R/RandomForestClassifier.R')
  ModelV=Classifiers::RandomForestClassifier(TrainData = TrainData,TrainCls = TrainCls,TestData = TestData,PlotIt = F,NumberOfTrees = NumberOfTrees,Fast = T)
  #Model4Lime=lime::as_classifier(ModelV$Forest)
  Model4Lime=ModelV$Forest
  # predict_model.rfsrc <- function(x, newdata, type, ...) {
  #   res <- predict(x, newdata = newdata, ...)
  #   x=res$predicted
  #   switch(
  #     type,
  #     raw = data.frame(Response = apply(predicted, 1, which.max), stringsAsFactors = FALSE),
  #     prob = as.data.frame(x, check.names = FALSE)
  #   )
  # }
  # model_type.rfsrc<- function(x, ...) 'classification'
  #source('~/Subversion/PRO/Research/BlutVsKMA2020/21MarburgBlutVsKMA/08AnalyseProgramme/04rfLIME/rfsrc_pred.R')
  Data=as.data.frame(rbind(TrainData,TestData))
  out_esplanations=lime::lime(x=Data,model=Model4Lime)

  #F1=dbt.ClassAnalysis::F1score(ModelV$TestCls,PriorTestCls)
  #Acc=FCPS::ClusterAccuracy(ModelV$TestCls,PriorTestCls)
  n=nrow(Data)
  if(n>1000){
  indfull=1:n#
  Chunks=split(indfull, ceiling(seq_along(indfull)/1000))
  explList=parLapply(cl,Chunks,function(i,Data,out_esplanations,n_labels,n_features){
    #source('~/Subversion/PRO/Research/BlutVsKMA2020/21MarburgBlutVsKMA/08AnalyseProgramme/04rfLIME/rfsrc_pred.R')
    library(lime)
    #library(randomForest)
    library(randomForestSRC)
    return(lime::explain(Data[unlist(i),], out_esplanations, n_labels = n_labels, n_features =n_features))
  },Data,out_esplanations,n_labels,n_features)
  tibble_casewiseExplanations=do.call(rbind,explList)

  }else{
    tibble_casewiseExplanations=lime::explain(Data, out_esplanations, n_labels = n_labels, n_features =n_features)
  }

  if(isTRUE(PlotIt)){
    ggobj=lime::plot_explanations(tibble_casewiseExplanations,ncol=1)
    print(ggobj)
  }
  Df_casewiseExplanations=as.data.frame(tibble_casewiseExplanations)
  num_cases <- unique(suppressWarnings(as.numeric(Df_casewiseExplanations$case)))
  if (!anyNA(num_cases)) {
    Df_casewiseExplanations$case <- factor(Df_casewiseExplanations$case, levels = as.character(sort(num_cases)))
  }

  V=FCPS::ClusterCreateClassification(Df_casewiseExplanations$label)
  UniqueClasses=V$ClusterNames
  NumCase=as.numeric(Df_casewiseExplanations$case)
  if(sum(is.finite(NumCase))==length(NumCase)){
    Filtered_DF=data.frame(Class=V$Cls,Case=NumCase,Desc=Df_casewiseExplanations$feature_desc,Weight=Df_casewiseExplanations$feature_weight)
  }else{
    Filtered_DF=data.frame(Class=V$Cls,Case=Df_casewiseExplanations$case,Desc=Df_casewiseExplanations$feature_desc,Weight=Df_casewiseExplanations$feature_weight)
  }

  ClasswiseList=list()
  Rules=list()
  NoConditions=list()
  k=0
  for(i in UniqueClasses){
    k=k+1
    V=unstack(Filtered_DF[Filtered_DF$Class==k,], form = Case ~ Desc)
    ruleschar=names(V)
    Rules[[i]]=ruleschar

    NoConditions[[i]]=sapply(ruleschar, function(x){
      counts=stringr::str_count(x,"<")+stringr::str_count(x,">")#+stringr::str_count(x,">=")+stringr::str_count(x,"<=")
    })
    ClasswiseList[[i]]=V
  }
  names(ClasswiseList)=UniqueClasses
  names(Rules)=UniqueClasses
  names(NoConditions)=UniqueClasses
  NoRules=sapply(Rules, length)
  rf_lime_results=list(Rules=Rules,NoConditions=NoConditions,NoRules=NoRules,TestCls=ModelV$TestCls,ClasswiseRules2Cases=ClasswiseList,Filtered_Explanations=Filtered_DF,CasewiseExplanations=Df_casewiseExplanations,LimeModel=out_esplanations,RFmodel=Model4Lime,ClassifierList=ModelV)
  return(rf_lime_results)
}


