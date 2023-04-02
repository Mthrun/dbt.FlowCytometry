SubsampleDenseData=function(Data,SampleSize){
# SampledData =  SubsampleDenseData(Data,SampleSize)
# subsample the A & B denstity classes of Data, density measured by IntBin
# up to SampleSize points, this may be less if Data contains fewer than SampleSize or less than  SampleSize dense points
#
# INPUT
# Data(1:n,:)           n cases,  d variables
# SampleSize            size of Sammpled data
#
# OUTPUT
# SampledData(1:s,:)    where s is min(n,SampleSize)
# Ind                   SampledData=Data(Ind,:)
# reimplemented from ALUs matlab versiomn by MT

V = dim(Data)
AnzDaten = V[1]
AnzVariablen = V[2]

MaxAnzSamples = min(SampleSize,AnzDaten)      # sicherstellen dass SampleSize <= AnzDaten
# DichteSchaetzung durch IntBins
DensityEsimation = DataDensityEstimationByIntBin(Data)$DensityEsimation                              # schaetzung der Dichte durch IntBins
V= ABCanalysis::ABCanalysis(DensityEsimation,PlotIt = F)  # bestimmung BClimit
BClimit=V$BCLimit
#[Ax,Ay,Bx,By,Cx,Cy,Aind,Bind,Cind,ABlimit,BClimit]

ReducedInd = which(DensityEsimation>BClimit)                                         # index der Daten mit A & B dichte
#Ind = subsampleUpToN(ReducedInd,SampleSize)                                         # subsampeln fuer n = SampleSize
#Sample ziehen durch permutieren
Ind=sample(ReducedInd)[1:SampleSize]
SampledData=Data[Ind,]                                                             # die daten sampeln

return(list(SampledData=SampledData,Ind = Ind))
}
