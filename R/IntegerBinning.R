 IntegerBinning=function(Data){
# Assign an d-ary integer number for each data point of a d-dimensional Data set such that
# each dimension is divided into 10 bins numbered 0 to 9
# NOTE: typically matlab operates up to d = 15 !
#
# INPUT
# Data(1:n,1:d)                   Data
#
# OUTPUT
# CaseIntegerValues(1:n)         d-ary integer number for each data point

# werte in den bereich 0...9.4 umskalieren und Runden => integer
   #rescale <- function(x,MIN,MAX) (x-MIN)/(MAX - MIN)
  # MIN=0
  # MAX=9.4
#ScaledData =round(torange(Data , 0, 9.4))
   #ScaledData=apply(Data, 2, rescale,MIN,MAX)

  ScaledData =round(toRange(Data , lower = 0, upper = 9.4))
  # werte zu einer integer Zahl kombinieren
  CaseIntegerValues = CombineCls(ScaledData)

return(CaseIntegerValues)
}
