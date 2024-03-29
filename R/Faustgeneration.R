Faustgeneration=function(FaustRuleSet,RuleDirectory=getwd(),FunctionName="Faustclassify",digits=2){
  #[RuleSet,Prefix,Names2Var,NamesInRuleSet] = ALPODSgeneration(DecisionTree,Names,RuleDirectory,FunctionName) 
  # a matlab function  [RuleCls,PopulationNumber] = ALPODSClassify(Data,Header) 
  # is written containing seperate descriptions of the populations in form of:
  # PopolationIndex = find(Conditions)
  #
  # INPUT
  # DecisionTree         a binary DecisionTree where each non-terminal node is a
  #                      condition on a variable (clolumn of Data)
  #                      left son, means condition satisfied
  # Names                names of the Variables in the tree
  #
  # OPTIONAL
  # RuleDirectory        where to write the function default: 
  #                      RuleDirectory='D:\Subversion\PUB\Mdbt\Classifiers'
  #
  # FunctionName         the name of the matlab function for AlPODS, default:
  #                      'ALPODSclassify' 
  # OUTPUT 
  # a Matlab function   [RuleCls,PopulationNumber] = ALPODSClassify(Data,Header) 
  #                      is written into ALPODSclassify.m
  #                      RuleCls        returns the classification in terms of Cls
  #                      PopulationNumber  returns the number of the Rule used for the classification
  # 
  # the function's text  can be constructed using the following text variables
  # RuleSet              Character Array: containing the Rules one in each line
  # Prefix               the header of the function, e.g.: RuleCls = zeros(n,1)  PopulationNumber =zeros(n,1) 
  # Names2Var            How to name the variablee e.g.: CD13=findAttrCol('CD13',Header,Data)   
  # NamesInRuleSet       character array containing all the unique Names that are used in the RuleSet
  
  # ALPODSgeneration(DecisionTree,RuleDirectory,FunctionName) 
  #                       1          2         3          4
  
  RuleSet        = FaustRuleSet$RuleSet
  NamesInRuleSet = FaustRuleSet$NamesInRuleSet
  LIST2Fun       = FaustRuleSet$LIST2Fun
  RuleClsStr     = FaustRuleSet$RuleCls
  
  ## MT nun die Regeln als andwendbare Funktion ausschreiben:
  NamesInRuleSet=unique(NamesInRuleSet)
  
  FirstLine=paste0(FunctionName,"=function(Data,Header=NULL){")
  
  ReturnLine="return(list(RuleCls=RuleCls,RulePopNumber=RulePopNumber,IndList=Indlist))"
  LastLine="}"
  
  FileName=paste0(FunctionName,".R")
  if(dir.exists(FileName)){
    file.remove(FileName)
  }
  
  cat(FirstLine, file=FileName, append=FALSE)
  cat("\n", file=FileName, append=TRUE)
  
  Doku = c(paste('# V = ',FunctionName,'(Data,Header)\n'),
           '# RuleCls=V$RuleCls;PopulationNumber=V$PopulationNumber;Indlist=V$Indlist;\n',
           '# rule based classifier and subpopulations corresponding to RuleCls\n',
           '# INPUT',
           '# Data[1:d,1:n]        data array of d cases with n variables\n',
           '# Optional\n',
           '# Header[1:d]          Header of data if given, otherwise colnames(Data) per default\n',
           '# OUTPUT\n',
           '# List V of\n',
           '# RuleCls[1:n]             vector of classes, n integer numbers, number k indicates class_k of k number of classes used for training the tree\n',
           '# PopulationNumber[1:n]    vector of classes, d integer numbers, number i indicates subpopulation_i of m subpupulations equal to the m leafs of the tree\n',
           '# Indlist[1:m]             list of inds for each population such that RuleCls[Indlist[[i]]] = class_k and PopulationNumber[Indlist[[i]]]=subpopulation_i \n',
           '# \n',
           '# V 1.0\n',
           '# generated by ALPODSgeneration.R\n')
  
  Prefix="RulePopNumber = rep(0,nrow(Data));RuleCls = rep(0,nrow(Data));"
  OptionalHeader1="if(!is.null(Header))"
  OptionalHeader2="   colnames(Data)=Header"
  
  cat(Doku, file=FileName, append=TRUE)
  cat(paste0(Prefix,"\n"), file=FileName, append=TRUE)
  #cat("\n", file=FileName, append=TRUE)
  #cat("\n", file=FileName, append=TRUE)
  cat(paste0(OptionalHeader1,"\n"), file=FileName, append=TRUE)
  #cat("\n", file=FileName, append=TRUE)
  cat(paste0(OptionalHeader2,"\n"," \n"), file=FileName, append=TRUE)
  #cat("\n", file=FileName, append=TRUE)
  #cat("\n", file=FileName, append=TRUE)
  
  Lines = vector(mode = 'character',length = length(NamesInRuleSet))
  
  for(i in 1:length(NamesInRuleSet)){
    
    Lines[i]=paste0(NamesInRuleSet[i]," = Data[,'",NamesInRuleSet[i],"']")
    if(i==1)
      writeout=paste0(Lines[i],"\n")
    else
      writeout=paste0(writeout,Lines[i],"\n")
    #cat(Lines[i], file=FileName, append=TRUE)
    #cat("\n", file=FileName, append=TRUE)
  }
  cat(paste0(writeout," \n"), file=FileName, append=TRUE)
  
  writeout=NULL
  for(i in 1:length(RuleSet)){
    if(i==1)
      writeout=paste0(RuleSet[i],"\n")
    else
      writeout=paste0(writeout,RuleSet[i],"\n")
    # cat(RuleSet[i], file=FileName, append=TRUE)
    # cat("\n", file=FileName, append=TRUE)
  }
  cat(writeout, file=FileName, append=TRUE)
  writeout=NULL
  
  PopNo=gsub("Ind","",LIST2Fun)
  ordered_pop=order(PopNo)
  cat("\n", file=FileName, append=TRUE)
  for(i in ordered_pop){
    rawline=paste0("RulePopNumber[",LIST2Fun[i],"]"," = ",PopNo[i])
    if(i==1)
      writeout=paste0(rawline,"\n")
    else
      writeout=paste0(writeout,rawline,"\n")
    
    # cat(rawline, file=FileName, append=TRUE)
    # cat("\n", file=FileName, append=TRUE)
  }
  cat(paste0(writeout," \n"), file=FileName, append=TRUE)
  
  #cat("\n", file=FileName, append=TRUE)
  writeout=NULL
  for(i in 1:length(RuleClsStr)){
    if(i==1)
      writeout=paste0(RuleClsStr[i],"\n")
    else
      writeout=paste0(writeout,RuleClsStr[i],"\n")
    # cat(RuleClsStr[i], file=FileName, append=TRUE)
    # cat("\n", file=FileName, append=TRUE)
  }
  cat(paste0(writeout," \n"), file=FileName, append=TRUE)
  
  LIST2FunLine=paste0("Indlist = list(",paste(LIST2Fun,collapse = ","),")")
  cat(paste0(LIST2FunLine,"\n"), file=FileName, append=TRUE)
  #cat("\n", file=FileName, append=TRUE)
  cat(paste0(ReturnLine,"\n"), file=FileName, append=TRUE)
  #cat("\n", file=FileName, append=TRUE)
  cat(LastLine, file=FileName, append=TRUE)
  return(list(RuleSet=RuleSet,Prefix=Prefix,Names2Var=Lines,NamesInRuleSet=NamesInRuleSet))
}