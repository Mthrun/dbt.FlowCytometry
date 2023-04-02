FaustRuleSet2Rules = function(ProjectPath, Set="V1", digits = 3){
  if(substr(ProjectPath, nchar(ProjectPath), nchar(ProjectPath)) != "/"){
    ProjectPath = paste0(ProjectPath, "/")
  }
  RootPath = paste0(ProjectPath, "faustData/faustCountMatrix.rds")
  GatePath = paste0(ProjectPath, "faustData/gateData/root_resList.rds")

  count_df <- as.data.frame(readRDS(RootPath))
  RuleNames = colnames(count_df)
  UniqueRuleNames = unique(RuleNames)
  if(length(RuleNames) != length(UniqueRuleNames)){
    warning("You have redundant rules in your set.")
  }
  Thresholds = readRDS(GatePath)
  TableThresholds = do.call(rbind, Thresholds)
  TableThresholds = TableThresholds[,Set]
  V = helpFaustRuleSet2Rules(UniqueRuleNames,
                             TableThresholds,
                             digits = digits)
  return(list("RuleSet"        = V$OurRuleSet,
              "LIST2Fun"       = V$LIST2Fun,
              "NamesInRuleSet" = V$NamesInRuleSet,
              "RuleClsString"  = V$RuleClsString,
              "UniqueRuleNames"= UniqueRuleNames))
}
