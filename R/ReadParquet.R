ReadParquet = function(FileName, FilePath = ""){
  # DESCRIPTION
  # Input routine for reading parquet files
  # 
  # INPUT
  # FileName    String name of parquet file
  # FilePath    String path to parquet file
  # 
  # OUTPUT
  # List V with elements
  #     ListData              List with numerical matrix elements
  #     NumberListElements    Integer with number of list elements
  #     FeatureDim            Integer with dimension of all matrix elements
  # 
  if(is.null(FileName)){
    stop('Filename is null/does not exist, please use a string as a filename')
  }
  if(FilePath != ""){
    if(substr(FilePath, nchar(FilePath), nchar(FilePath)) != "/"){
      FilePath = paste0(FilePath, "/")
    }
    file = paste0(FilePath, FileName)
  }else{
    file = FileName
  }
  Parquet = arrow::read_parquet(file=file, as_data_frame = T)

  Ncol = dim(Parquet)[2]
  #Nrow = dim(Parquet)[1]
  NumListElements = max(Parquet[,Ncol])
  #all(unique(Parquet[,Ncol]) == 1:NumListElements)
  
  #tic()
  #SplitRes = by(Parquet[,1:Ncol], Parquet[,Ncol], identity)
  #toc()
  # 31 sec

  #tic()
  #SplitRes = sapply(Parquet[,1:Ncol-1], FUN = function(x) split(x, f = Parquet[,Ncol]))
  #toc()
  # 179.12 sec elapsed

  #system.time(SplitRes <- sapply(Parquet[,1:Ncol-1], FUN = function(x) split(x, f = Parquet[,Ncol])))
  
  
  #system.time(SplitRes <- base::split(x = Parquet[,1:Ncol-1], f = Parquet[,Ncol]))
  
  SplitRes = base::split(x = Parquet[,1:Ncol-1], f = Parquet[,Ncol])

  #library(parallel)
  #cl <- parallel::makeCluster(getOption("cl.cores", 4))
  #tic()
  #SplitRes = parallel::parApply(cl, X = Parquet[,1:Ncol-1],
  #                              MARGIN = Parquet[,Ncol],
  #                              FUN = base::split(x = Parquet[,1:Ncol-1], f = Parquet[,1:Ncol-1]))
  #toc()
  #parallel::stopCluster(cl)
  # 31 sec
  return(list("ListData"=SplitRes, "NumberListElements"=NumListElements, "FeatureDim"=Ncol))
}
