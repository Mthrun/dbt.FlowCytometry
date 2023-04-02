SubsampleDensityProp=function(Data,SampleSize,MinDensity=9/1000){
# V =  SubsampleDensityProp(Data,SampleSize)
# subsample with aprobability proportional to the (intBin) density
# points are ignored with a lower density than  MinDensity (in percentage of the Maximum denisity)
# up to SampleSize points, this may be less if Data contains fewer than SampleSize points
#
# INPUT
# Data(1:n,)            n cases,  d variables
# SampleSize            size of Sammpled data
#
# OPTIONAL
# MinDensity            points are ignored with a lower density than  MinDensity
#                       default: MinDensity = 9/1000  #(in percentage of the Maximum density)
#
# OUTPUT
# SampledData(1:s,)    where s is min(n,SampleSize)
# Ind                   SampledData=Data(Ind,)
#author MT+ALU

AnzDaten  = dim(Data)[1]
MaxAnzSamples = min(SampleSize,AnzDaten)      # sicherstellen dass SampleSize <= AnzDaten
# DichteSchaetzung durch IntBins
DensityEsimation = DataDensityEstimationByIntBin(Data)$DensityEsimation                              # schaetzung der Dichte durch IntBins
DensityEsimation = DensityEsimation / max(DensityEsimation,na.rm = T)                      # auf 0..1 normieren
DensityEsimation[DensityEsimation<MinDensity] = 0                                   # Outlier weg
Uniform =  runif(min = 0,max = 1,n = AnzDaten)
# rejection sampling# uniform verteilte fuers rejecten
DensInd = which((DensityEsimation>Uniform)==TRUE)

#Sample ziehen durch permutieren
Ind=sample(DensInd)[1:min(SampleSize,length(DensInd))]       # subsampeln fuer n = SampleSize

SampledData=Data[Ind,]     # die daten sampeln

if(nrow(SampledData)<SampleSize)
  warning('SubsampleDensityProp: Dense Regions found are smaller then the SampleSize set. Hence, the sample is smaller than SampleSize.')
return(list(SampledData=SampledData,Ind=Ind))
}
