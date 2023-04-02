MatrixList2GatingSet <- function(MatrixList){
  # V = MatrixList2GatingSet(Data, VarIdentifiers, VarNames, Cls)
  # 
  # 
  # INPUT
  # MatrixList    List of matrices [1:n, 1:d]. Matrix  with n observations and d 
  #               features carrying flowcytometry data values.
  # 
  # OUTPUT
  # GatingSet
  #
  # Author: QS 2021
  
  if(!is.list(MatrixList)){
    message("Parameter MatrixList is must be of type list.")
    return()
  }
  
  MyFlowSet = list()
  for(i in 1:length(MatrixList)){
    MyFlowSet[[i]] = LRN2FlowFrame(MatrixList[[i]])
  }
  return(flowWorkspace::GatingSet(flowCore::flowSet(MyFlowSet)))
}