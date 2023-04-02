TranslateFaustAnnotations = function(FaustAnnotations){
  newRuleNames <- as.character(sapply(FaustAnnotations, function(x) {
    gsub("~1~[[:digit:]]~", "-", x)
  }))
  newRuleNames <- as.character(sapply(newRuleNames, function(x) {
    gsub("~2~2~", "+", x)
  }))
  newRuleNames <- as.character(sapply(newRuleNames, function(x) {
    gsub("~3~3~", "Bright", x)
  }))
  newRuleNames <- as.character(sapply(newRuleNames, function(x) {
    gsub("~4~4~", "++", x)
  }))
  newRuleNames <- as.character(sapply(newRuleNames, function(x) {
    gsub("~2~3~", "Dim", x)
  }))
  newRuleNames <- as.character(sapply(newRuleNames, function(x) {
    gsub("~2~4~", "Med-", x)
  }))
  newRuleNames <- as.character(sapply(newRuleNames, function(x) {
    gsub("~3~4~", "Med+", x)
  }))
}