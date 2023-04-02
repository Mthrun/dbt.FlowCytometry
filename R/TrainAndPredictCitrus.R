TrainAndPredictCitrus=function(TrainIndices,TestIndices,Filenames,ClsTrain,InDirectory,ClusteringColumns,SampleSize=1000,Transform=FALSE,Scale=FALSE){
  library(citrus)
  # List of files to be clustered
  fileList_train = data.frame("unstim"=Filenames[TrainIndices])

  # List of files to be mapped
  fileList_test = data.frame("unstim"=Filenames[TestIndices])


  # List of columns to be used for clustering
  if(missing(ClusteringColumns)){
    citrus.combinedFCSSet_sample = citrus::citrus.readFCSSet(InDirectory,data.frame("unstim"=Filenames[TrainIndices[1:2]]))
    ClusteringColumns=citrus.combinedFCSSet_sample$fileChannelNames[[1]][[1]]
  }
  if(isTRUE(Transform)){
    transformColumns=ClusteringColumns
  }else{
    transformColumns=NULL
  }
  if(isTRUE(Scale)){
    scaleColumns =ClusteringColumns
  }else{
    scaleColumns =NULL
  }
  # Read the data, optional transform and scale
  citrus.combinedFCSSet_train = citrus::citrus.readFCSSet(InDirectory,fileList_train,transformColumns = transformColumns,scaleColumns = scaleColumns,fileSampleSize = SampleSize)
  citrus.citrus.combinedFCSSet_test = citrus::citrus.readFCSSet(InDirectory,fileList_test,transformColumns = transformColumns,scaleColumns = scaleColumns,fileSampleSize = SampleSize)

  # Cluster first dataset
  citrus.clustering = citrus::citrus.cluster(citrus.combinedFCSSet_train,ClusteringColumns)

  # Map new data to exsting clustering
  citrus.mapping = citrus::citrus.mapToClusterSpace(citrus.combinedFCSSet.new=citrus.citrus.combinedFCSSet_test,citrus.combinedFCSSet.old=citrus.combinedFCSSet_train,citrus.clustering)

  # Large Enough Clusters
  largeEnoughClusters = citrus::citrus.selectClusters(citrus.clustering)

  # Clustered Features and mapped features
  clusteredFeatures = citrus::citrus.calculateFeatures(citrus.combinedFCSSet_train,clusterAssignments=citrus.clustering$clusterMembership,clusterIds=largeEnoughClusters)
  mappedFeatures = citrus::citrus.calculateFeatures(citrus.citrus.combinedFCSSet_test,clusterAssignments=citrus.mapping$clusterMembership,clusterIds=largeEnoughClusters)

  # Labels
  #labels = c(rep("Blood",14),rep("BoneMarrow",14))
  labels=as.factor(ClsTrain)
  # Build Endpoint Model
  citrus.endpointModel = citrus::citrus.buildEndpointModel(clusteredFeatures,labels)
  Model=list(citrus.endpointModel=citrus.endpointModel,mappedFeatures=mappedFeatures,clusteredFeatures=clusteredFeatures,
             largeEnoughClusters=largeEnoughClusters,citrus.clustering=citrus.clustering,citrus.mapping=citrus.mapping)
  # Predict
  PredictionTrain_raw=citrus::citrus.predict(citrus.endpointModel,newFeatures=clusteredFeatures)
  PredictionTest_raw=citrus::citrus.predict(citrus.endpointModel,newFeatures=mappedFeatures)

  CountPerClass=function(x,Uniques){

    counts=suppressWarnings(ClusterCount(x)$CountPerCluster)
    uniquesGiven=names(counts)
    ind=match(uniquesGiven,Uniques)
    CountsAll=vector(length = length(Uniques))
    CountsAll[ind]=counts
    return(CountsAll)
  }
  ProbTest=NULL
  ProbTrain=NULL
  try({
  UniqueClasses=unique(unlist(apply(as.matrix(PredictionTrain_raw), 1,unique)))
  })
  try({
  ProbTest=t(apply(PredictionTest_raw, 1, CountPerClass,UniqueClasses))
  colnames(ProbTest)=UniqueClasses
  })
  try({
  ProbTrain=t(apply(PredictionTrain_raw, 1, CountPerClass,UniqueClasses))
  colnames(ProbTrain)=UniqueClasses
  })

  return(list(ProbTest=ProbTest,ProbTrain=ProbTrain,PredictionTest_raw=PredictionTest_raw,PredictionTrain_raw=PredictionTrain_raw,Model=Model))
}
