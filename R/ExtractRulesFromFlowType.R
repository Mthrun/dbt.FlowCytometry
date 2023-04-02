ExtractRulesFromFlowType=function(FlowTypePhenoTypes,DataListed,ClsPerPatient,Pvalues=NULL,RuleDirectory=getwd(),FunctionName="FlowTypeclassify",digits=2,Silent=FALSE,cl=NULL,MatlabStyle=TRUE){
  #note: "*" is not allowed in column names otherwise function will break
  if(isFALSE(Silent))
    message("Preparing data...")

if(length(DataListed)!=length(ClsPerPatient)) stop("Length of List 'DataListed' does not equal length of 'ClsPerPatient'.")
    #anpassung der colnamen sodass kein stern * steht
  DataListed=lapply(DataListed, function(x) {
    names_x=colnames(x)
    names_x=gsub("\\*","",names_x)
    colnames(x)=names_x
    return(x)
  })

  DataFull=do.call(rbind,DataListed)
  if(!is.null(Pvalues)){
    Importance=-log10(Pvalues)
    model=ABCanalysis::calculatedABCanalysis(Importance)
    Relevant=model$Aind
    FlowTypePhenoTypes=FlowTypePhenoTypes[Relevant]
  }
  if(is.null(names(FlowTypePhenoTypes))) names(FlowTypePhenoTypes)=FlowTypePhenoTypes

  PreRules=gsub("-","\\*",FlowTypePhenoTypes)
  PreRules=gsub("\\+","\\*",PreRules)
  Cols=strsplit(x = PreRules,"\\*")
  ns=sapply(DataListed,nrow)
  ClsCasesWise=mapply(ClsPerPatient, FUN = function(i,y) return(rep(i,y)),ns,SIMPLIFY = F)
  if(isFALSE(Silent))
    message("EmpiricalBayesDecisionLimitPerFeature computing...")

  thresholds=EmpiricalBayesDecisionLimitPerFeature(Data =DataFull, Cls = unlist(ClsCasesWise),cl=cl)

  if(isFALSE(Silent))
    message("Generating Rules...")

  Tlist=lapply(Cols, function(x,y) y[match(x,names(y),nomatch = 0)],thresholds)

  Conds=gsub("\\+",">*",names(Cols))
  Conds=gsub("\\-","<*",Conds)
  Conditions=strsplit(x = Conds,"\\*")
  RulesWithConditions=mapply(function(x,y) paste0(x,y),Conditions, Tlist)
  names(RulesWithConditions)=names(Cols)
  RulesWithConditions

  LIST2Fun=c()

  Rules2Fun = vector(mode = 'character',length = length(RulesWithConditions))
  for(num in 1:length(RulesWithConditions)){
    LIST2Fun = c(LIST2Fun,paste0("Ind",num))
    Rules2Fun[num] = paste0("Ind",num," = which(")
    Rules2Fun[num] =paste0( Rules2Fun[num],paste(RulesWithConditions[[num]],collapse = " & "),")")
  }
  Rules2Fun
  NamesInRuleSet=unique(unlist(Cols))

  Vliste=RuleGenerationPreClassifier(Rules2Fun,NamesInRuleSet,LIST2Fun,RuleDirectory = RuleDirectory,FunctionName = FunctionName,digits = digits,MatlabStyle = MatlabStyle)
  return(list(RulesWithConditions=RulesWithConditions,Rules2Fun=Rules2Fun,NamesInRuleSet=NamesInRuleSet,Thresholds=thresholds,RuleSet=Vliste$RuleSet,Prefix=Vliste$Prefix, Names2Var=Vliste$Names2Var,NamesInRuleSet = Vliste$NamesInRuleSet,FunctionAsfString=Vliste$FunctionAsfString))
}
