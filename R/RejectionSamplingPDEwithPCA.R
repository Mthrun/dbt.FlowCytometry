RejectionSamplingPDEwithPCA=function(Data,SampleSize=1000){
# DataSample=RejectionSamplingPDEwithPCA(x,10000)
# Samples Cluster consistent using Rejection sampling with a combination of PCA and PDE
# Cluster consistent in a sense that besides outliers all FCPS structures can be sampled correctly
# INPUT
# Data              [1:n,1:d] datamatrix
# SampleSize        SampleSize, usually lower than n, if higher than n, then only d=3 data is currently possible
#
# OUTPUT
# takesample        [1:SampleSize,1:d] sample of datamatrix
#
# Author MT 05/2020

  n=nrow(Data)

  if(mode(Data)!='numeric'){
    warning('Data is not numeric.. Calling "mode(Data)=numeric"')
    mode(Data)='numeric'
  }
  res <- prcomp(x = Data, retx = T, scale = FALSE, tol = 0,
                center = FALSE)
  TransData = as.matrix(res$x)
  HighestVariance = TransData[, 1]

  kernels=seq(from=min(HighestVariance,na.rm = T),to=max(HighestVariance,na.rm = T),length.out=SampleSize)
  pde=DataVisualizations::ParetoDensityEstimation(HighestVariance,kernels = kernels)
  maxdens=max(pde$paretoDensity)

  dens=function(y,pde,maxdens){
    ind=which.min(abs(pde$kernels-y))
    return(pde$paretoDensity[ind]/maxdens)
  }
  sampleind=c()
  i=0
  while(length(sampleind)<SampleSize){
    x=sample(HighestVariance,1)
    y=runif(1,0,1)
    d=dens(x,pde,maxdens)
    if(y<=d){
      i=i+1
      #funktioniert nur wenn man wirklich alle indizes mitnimmt und nicht nur den ersten
      sampleind=c(sampleind,which(HighestVariance==x))
    }
  }
  takesample=Data[sampleind,]
  if(SampleSize>n){
    Mnull=as.matrix(parallelDist::parDist(Data))
    d=ncol(Data)
    diag(Mnull)=NaN
    x=min(Mnull,na.rm = T)
    y=max(Mnull,na.rm = T)
    Min=1-x/y
    Max=1+x/y
    diff=Max-Min
    M=as.matrix(parallelDist::parDist(takesample))
    M[upper.tri(M)]=NaN
    #dubletten
    ind=which(M==0,arr.ind = TRUE)

    for(i in 1:nrow(ind)){
      v=takesample[ind[i,1],,drop=FALSE]
      w=takesample[ind[i,2],,drop=FALSE]
      ##muss ich noch verallgemeinern von 3d
      if(dist2center_ext(v)>10e-3){
        #a <- v * runif_ext(d=d,n=1, min=Min, max=Max)

      }else{
        #a <- v + runif_ext(d=d,n=1, min=-diff, max=diff)
      }
      if(dist2center_ext(v)>10e-3){
        #b <- w * runif_ext(d=d,n=1,  min=Min, max=Max)

      }else{
        #b <- w + runif_ext(d=d,n=1, min=-diff, max=diff)
      }
      takesample[ind[i,1],]=v#a
      takesample[ind[i,2],]=w#b
    }

  }

  return(takesample)
}
