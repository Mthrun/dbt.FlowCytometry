WriteParquet = function(Data, FileName, FilePath = ""){
  # DESCRIPTION
  # Input routine for reading parquet files
  # 
  # INPUT
  # Data        List of numerical matrices
  # FileName    String name of parquet file
  # FilePath    String path to parquet file
  # 
  # OUTPUT
  # List V with elements
  #     ListData              List with numerical matrix elements
  #     NumberListElements    Integer with number of list elements
  #     FeatureDim            Integer with dimension of all matrix elements
  # 
  NumEntries = length(Data)
  #Index = c()
  #for(i in 1:NumEntries){
  #  Index = c(Index, rep(i, dim(Data[[i]])[1]))
  #}
  N = sapply(Data, FUN=function(x) dim(x)[1])
  Len = sum(N)
  #Index = vector(Len, mode="factor") # Never use factor, inefficient!
  #Index = vector(Len, mode="character")
  Index = vector(Len, mode="numeric")
  Index[1:dim(Data[[1]])[1]] = 1
  From = 1
  To = N[1]
  for(i in 1:(length(N)-1)){
    Index[From:To] = i
    From = From + N[i]
    To = To + N[i+1]
  }
  #Index = as.factor(Index) # Factor here efficient for writing, but not for reading!
  DataFrame = data.frame(cbind(do.call(rbind, Data), Index))
  arrow::write_parquet(x=DataFrame, sink=FileName)
}
