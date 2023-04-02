XcellGate=function(Data,Header,Key,Cls){
# [XcellData,XcellKey,XcellCls,PercentXcells] =  XcellGate(Data,Header,Key,Cls)
# select Xcells from data
# Xcells are ether CD5&CD19 both high or both low and  Low in FS and SS
#
# INPUT
# Data,Header
#
# OPTIONAL
# Key,Cls
  #authror: matlab ALU, reimplemented in R by MCT
V= dim(Data)
n=V[1]
d=V[2]
if(missing(Header)){
  if(!is.null(colnames(Data))){
    Header=colnames(Data)
  }else{
    warning('Header is missing and colnames data is null.')
    return('Header is missing and colnames data is null.')
  }
}

if(missing(Key)) Key=seq_len(n)

if(missing(Cls)) Cls=rep(1,n)

# xzellen selektieren
FS1=Data[,which(Header=='FS1')]
SS1= Data[,which(Header=='SS1')]
CD2orFMC7= Data[,which(Header=='CD2orFMC7')]
CD79orCD4= Data[,which(Header=='CD79orCD4')]
CD5= Data[,which(Header=='CD5')]
CD7lambda= Data[,which(Header=='CD7lambda')]
CD8kappa= Data[,which(Header=='CD8kappa')]
CD19= Data[,which(Header=='CD19')]
CD20orCD3= Data[,which(Header=='CD20orCD3')]
CD23= Data[,which(Header=='CD23')]
CD38= Data[,which(Header=='CD38')]
CD45= Data[,which(Header=='CD45')]


 	CCInd = which((FS1<3)&(SS1<2)&((CD5<3)&(CD19<3)|(CD5>3)&(CD19>3)))
	XcellData = Data[CCInd,]
	XcellKey  = Key[CCInd]
	XcellCls  = Cls[CCInd]
	PercentXcells = round(length(CCInd)/n*100,-1)

	return(list(XcellData=XcellData,XcellKey=XcellKey,XcellCls=XcellCls,PercentXcells=PercentXcells))
}

