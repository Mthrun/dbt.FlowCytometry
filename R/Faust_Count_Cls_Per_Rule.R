Faust_Count_Cls_Per_Rule = function(Cls, UniqueRuleNames){
  NumRules = length(UniqueRuleNames)
  Range = 0:(NumRules-1)
  tmpTable = table(Cls)
  tmpClsCount = as.numeric(lapply(Range, function(x) if(!is.na(tmpTable[paste0(x)])) tmpTable[paste0(x)] else 0))
  tmpClsCount = c(tmpClsCount[2:length(tmpClsCount)], tmpClsCount[1])
  tmpMatrix = t(as.matrix(tmpClsCount))
  colnames(tmpMatrix) = UniqueRuleNames
  return(list("ClsCountInOrder" = tmpClsCount,
              "CountsPerRule"   = tmpMatrix))
}
