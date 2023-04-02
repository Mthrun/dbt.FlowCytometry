RejectionSamplingIntBin=function(Data,SampleSize=1000){
  # DataSample=RejectionSamplingIntBin(x,10000)
  # Samples Cluster consistent using Rejection sampling in combination with DataDensityEstimationByIntBin
  # Cluster consistent in a sense that besides outliers all FCPS structures can be sampled correctly (ToDO waere das zu testen)
  # INPUT
  # Data              [1:n,1:d] datamatrix
  # SampleSize        SampleSize, usually lower than n, if higher than n, then only d=3 data is currently possible
  #
  # OUTPUT
  # takesample        [1:SampleSize,1:d] sample of datamatrix
  #
  # Author MT 06/2020

  n=nrow(Data)

  if(mode(Data)!='numeric'){
    warning('Data is not numeric.. Calling "mode(Data)=numeric"')
    mode(Data)='numeric'
  }

  dens=DataDensityEstimationByIntBin(Data)

  maxdens=max(dens$DensityEsimation)

#normierungsfunktion der dichte
  densnorm=function(y,dens,maxdens){
    ind=which.min(abs(dens$AnzPerUniqCV-y))
    return(dens$DensityEsimation[ind]/maxdens)
  }

  sampleind=c()
  i=0
  IDs=1:nrow(Data)
  while(length(sampleind)<SampleSize){
    x=sample(IDs,1) #ziehe zufaellige IDE
    y=runif(1,0,1) #ziehe zufalls zahl aus uniform verteilung 0-1
    d=densnorm(x,dens,maxdens) #geje sicher das dichte zwischen 0 und 1
    if(y<=d){ #wenn zufallszahl kleiner als dichte
      i=i+1
      #funktioniert nur wenn man wirklich alle indizes mitnimmt und nicht nur den ersten
      sampleind=c(sampleind,which(IDs==x)) #fuege zahl zu sample hinzu
    }#ansonsten rejection
  }#ende rejection sampling
  takesample=Data[sampleind,]

## Sample> Anzahl Datenpinkte ----
  # im Falle dass das sample groesser als der Datensatz ist jittern wir die punkte
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
    #hier jittern wir die punkte, geht aber nur in 2D und 3D als fuer FCPS
    for(i in 1:nrow(ind)){
      v=takesample[ind[i,1],,drop=FALSE]
      w=takesample[ind[i,2],,drop=FALSE]
      ##muss ich noch verallgemeinern von 3d
      if(dist2center_ext(v)>10e-3){
        a <- v * runif_ext(d=d,n=1, min=Min, max=Max)

      }else{
        a <- v + runif_ext(d=d,n=1, min=-diff, max=diff)
      }
      if(dist2center_ext(v)>10e-3){
        b <- w * runif_ext(d=d,n=1,  min=Min, max=Max)

      }else{
        b <- w + runif_ext(d=d,n=1, min=-diff, max=diff)
      }
      takesample[ind[i,1],]=a
      takesample[ind[i,2],]=b
    }

  }

  return(list(takesample,sampleind))
}

#hifsfunktionen fuer sample > anzahl datenpunkte
runif3d=function(...){
  x=runif(...)
  y=runif(...)
  z=runif(...)
  return(t(as.matrix(c(x,y,z))))
}
dist2center3d=function(Data){
  d=sqrt((Data[,1]-0)^2+ (Data[,2]-0)^2+(Data[,3]-0)^2)
}
runif2d=function(...){
  x=runif(...)
  y=runif(...)
  return(t(as.matrix(c(x,y))))
}
dist2center2d=function(Data){
  d=sqrt((Data[,1]-0)^2+ (Data[,2]-0)^2)
}

runif_ext=function(d=3,...){
  if(d==2) return(runif2d(...))
  if(d==3) return(runif3d(...))
}

dist2center_ext=function(Data){
  cc=ncol(Data)
  if(cc==2) return(dist2center2d(Data))
  if(cc==3) return(dist2center3d(Data))
}
