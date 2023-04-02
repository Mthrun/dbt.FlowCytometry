 DeltaPxy=function(Data,Cls) {
# AllDeltaP = DeltaPxy(Data,Cls)
# welche Bivariate kombination trennt Cls==1 am besten vom rest
# benutzt ProbabilityDifferenceXY(X,Y,Cls)
# INPUT
# Data(1:n,1:d)             data points
#
# OUTPUT
# AllDeltaP(1:d,1:d)         AllDeltaP(x,y) = ProbabilityDifferenceXY(X,Y,Cls)

V = dim(Data)
AnzCases=V[1]
AnzVar=V[2]
AllDeltaP = matrix(0, AnzVar, AnzVar)
#Tacho = waitbar(0,['DeltaPxy 0 /', num2str(AnzVar)])                                           # open tacho
for(i in 1: AnzVar){
  #waitbar(i/AnzVar,Tacho,['DeltaPxy: ',num2str(i),' /', num2str(AnzVar)])  # update tacho
  if((i+1)<=AnzVar){
    for(j in ((i+1):AnzVar)){
  	X = Data[,i]
  	Y = Data[,j]
  	V = ProbabilityDifferenceXY(X,Y,Cls)  # overall difference in 2dimensional probabilities
  	DeltaP=V$DeltaP
  	#Xkernels
  	#Ykernels
  	#Density1
  	#Density2

  	AllDeltaP[i,j] = DeltaP   # AllDeltaP(j,i) = DeltaP
    }  # for j
  }
} # for i

#AllDeltaP = AllDeltaP+AllDeltaP  # symmetrisieren

AllDeltaP[lower.tri(AllDeltaP)]=AllDeltaP[upper.tri(AllDeltaP)]

#close(Tacho)
 return(AllDeltaP)
}
