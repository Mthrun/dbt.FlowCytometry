ClsCountFaustRulesOnTraining = function(ProjectPath, UniqueRuleNames){
  # Get rules which classify each datapoint from the Gatingset V1
  ListRules   = read.delim(paste0(ProjectPath, "faustData/sampleData/V1/faustAnnotation.csv"), header = FALSE)
  VectorRules = unlist(ListRules)
  # Translate the special symbolic encoding from faust annotations (e.g., ~1~2) to the medical rules (e.g., -)
  ExtractedFaustRules2 = TranslateFaustAnnotations(VectorRules)
  # Get the Cls assignment of the Gatingset based on the Faust rules stored in faustAnnotation.csv
  FaustCls = unlist(lapply(ExtractedFaustRules2, function(x) which(UniqueRuleNames == x)))
  return((as.numeric(table(FaustCls))))
}
