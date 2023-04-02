helpFaustRuleSet2Rules = function(UniqueRuleNames, TableThresholds, digits = 3){
  #
  #
  #
  #
  OurRuleSet = rbind()
  LIST2Fun = c()
  RuleClsString = c()
  UniqueVariablesOverall = c()
  for(i in 1:length(UniqueRuleNames)){
    tmpAllVariables = c()
    # Prepare new rule from foreign rule set
    tmpNewRule = paste0("Ind", i, " = which(")
    # Split the Rule after the special symbol and keep it (to indicate the rule!)
    tmpThresholds = gsub("Dim", ".", UniqueRuleNames[i])
    tmpThresholds = gsub("Bright", ",", tmpThresholds)
    tmpThresholds = gsub("\\+\\+", ";", tmpThresholds)
    tmpThresholds = gsub("Med\\+", ":", tmpThresholds)
    tmpThresholds = gsub("Med\\-", "#", tmpThresholds)
    tmpThresholds = unlist(strsplit(tmpThresholds, "(?<=[-+.,;:#])", perl = TRUE))
    if(length(grep("0", tmpThresholds)) == 0){
      # Loop through all parts of the foreign rule to extract variables, symbol and
      # threshold
      for(j in 1:length(tmpThresholds)){
        tmpSymbol = tmpThresholds[j]
        # Extract the variable name
        tmpVarName = substr(tmpSymbol, 1, nchar(tmpSymbol) - 1)
        tmpAllVariables = c(tmpAllVariables, tmpVarName)
        # Extract the Threshold based on the variables
        tmpVar = as.numeric(unlist(TableThresholds[tmpVarName]))
        tmpThreshold = round(tmpVar, digits = digits)
        # Extract the rule symbol ("+" == ">", "-" == "<")
        tmpRuleSym = substr(tmpSymbol, nchar(tmpSymbol), nchar(tmpSymbol))
        tmpNewRuleSym = ""
        if(tmpRuleSym == "+"){
          tmpNewRule = paste0(tmpNewRule, tmpVarName, " > ", tmpThreshold[1])
        }else if(tmpRuleSym == "-"){
          tmpNewRule = paste0(tmpNewRule, tmpVarName, " < ", tmpThreshold[1])
        }else if(tmpRuleSym == "."){
          tmpNewRule = paste0(tmpNewRule, tmpVarName, " < ", tmpThreshold[2])
          tmpNewRule = paste0(tmpNewRule, " & ")
          tmpNewRule = paste0(tmpNewRule, tmpVarName, " > ", tmpThreshold[1])
        }else if(tmpRuleSym == ","){
          tmpNewRule = paste0(tmpNewRule, tmpVarName, " > ", tmpThreshold[2])
          #tmpNewRule = paste0(tmpNewRule, " & ")
          #tmpNewRule = paste0(tmpNewRule, tmpVarName, " < ", tmpThreshold[1])
        }else if(tmpRuleSym == ";"){
          tmpNewRule = paste0(tmpNewRule, tmpVarName, " > ", tmpThreshold[3])
          #tmpNewRule = paste0(tmpNewRule, " & ")
          #tmpNewRule = paste0(tmpNewRule, tmpVarName, " < ", tmpThreshold[1])
        }else if(tmpRuleSym == ":"){
          tmpNewRule = paste0(tmpNewRule, tmpVarName, " > ", tmpThreshold[2])
          tmpNewRule = paste0(tmpNewRule, " & ")
          tmpNewRule = paste0(tmpNewRule, tmpVarName, " < ", tmpThreshold[3])
        }else if(tmpRuleSym == "#"){
          tmpNewRule = paste0(tmpNewRule, tmpVarName, " > ", tmpThreshold[1])
          tmpNewRule = paste0(tmpNewRule, " & ")
          tmpNewRule = paste0(tmpNewRule, tmpVarName, " < ", tmpThreshold[2])
        }else{
          tmpNewRule = ""
        }
        if(j != length(tmpThresholds)){
          tmpNewRule = paste0(tmpNewRule, " & ")
        }
      }
      # Close the new rule statement
      tmpNewRule = paste0(tmpNewRule, ")")
      # Save the new rule
      OurRuleSet = rbind(OurRuleSet, tmpNewRule)
      LIST2Fun = c(LIST2Fun, paste0("Ind", i))
      RuleClsString = c(RuleClsString, paste0("RuleCls[Ind", i,"] = ", i))
      UniqueVariablesOverall = unique(c(UniqueVariablesOverall, tmpAllVariables))
    }
  }
  rownames(OurRuleSet) = NULL
  return(list("OurRuleSet"     = OurRuleSet,
              "LIST2Fun"       = LIST2Fun,
              "NamesInRuleSet" = UniqueVariablesOverall,
              "RuleClsString"  = RuleClsString))
}
