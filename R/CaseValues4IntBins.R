CaseValues4IntBins=function(CaseIntegerValues,Data){
# the mean case data value within the bin determined by CaseIntegerValues
#
# INPUT
# CaseIntegerValues(1:n)          d-ary integer number for each data point
# Data(1:n,1:d)                   Data
#
# OUTPUT
#  CaseValues(1:n,1:d)            mean value of data within the same CaseIntegerValue for each case


V =ClassMean(Data,CaseIntegerValues)
CaseValues=V$MeanPerClass
return(CaseValues)
}
