ReadScatter=function(Filename,Dir){
  V1=ReadCLS(Filename,Dir)
  V2=ReadLRN(Filename,Dir)
  if(isFALSE(TheSameKey(V1$Key,V2$ClsKey)))
    return(NULL)

  Data=V2$Data
  return(cbind(Cls=V1$Cls,Data))
}
