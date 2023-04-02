ProbabilityDifferenceXY=function(X,Y,Cls,MinPercentage=0.02,nbins=200,lambda=20,Xkernels,Ykernels) {
# DeltaP = ProbabilityDifferenceXY(X,Y,Cls)                                        # overall difference in 2dimensional probabilities
# [DeltaP,Xkernels,Ykernels,Density1,Density2] = ProbabilityDifferenceXY(X,Y,Cls)  # overall difference in 2dimensional probabilities
# [DeltaP,Xkernels,Ykernels,Density1,Density2] = ProbabilityDifferenceXY(X,Y,Cls,MinPercentage,nbins,lambda,Xkernels,Ykernels)
# overall difference in 2dimensional probabilities: prob(X,Y,Cls~=1) - prob(X,Y,Cls==1)
# NOTE for diplay use:
# Delta = Density2 - Density1                          # differenz der Wahrscheinlichkeit
# AbsDelta = abs(Delta)                                # abs(p2-P1)


#
# INPUT
# X(1:n),Y(1:n)     a set of 2D points
# Cls(1:n)          classification of these points
#
# OPTIONAL
# MinPercentage     below this the XY histograms which are noomalized to [0,1} are set to zero  default: MinPercentag= 0.02
# nbins             number of bins  for nbins=[], => nbins =200 (default)
#                   nbins= nxy      => the nr of bins in x and y is nxy
#                   nbins = [nx,ny] => the nr of bins in x is nx and for y is ny
#
# lambda            smoothing factor used by the density estimator
#                   default: lambda = 20 which roughly
#                   means that the smoothing is over 20 bins around a given point.
# Xkernels,Ykernels bin kernels in x and y directions are given
#
# OUTPUT
# DeltaP                  overall difference in 2dimensional probabilieites
# Xkernels,Ykernels,      such that mesh(Xkernels,Ykernels,Densities) respectively [Xm,Ym]= meshgrid(Xkernels,Ykernels)  plot3(Xm,Ym,DensityI) are the densities
# Density1,Density2       the densities of XY wcaluated at [Xm,Ym]= meshgrid(Xkernels,Ykernels)

# MinPercentage = 0.02  # alles under diesen prozenten wird das auf 1 normierte Histogramm auf Null gesetzt
# nbins = 200      # Anzahl bins in X, Y direction
# lambda = 20     end  # smoothing factor used by the density estimator
  if(missing(Xkernels)&missing(Xkernels)){# gemeinsames 2DHistogram fuer Xkernels,Ykernels
   V=SmoothedDensitiesXY(X,Y,nbins)
   Xkernels=V$Xkernels
   Ykernels=V$Ykernels
  }else if(missing(Xkernels)){
    V=SmoothedDensitiesXY(X,Y,nbins)
    Xkernels=V$Xkernels

  }else if(missing(Ykernels)){
    V=SmoothedDensitiesXY(X,Y,nbins)

    Ykernels=V$Ykernels
  }else{

  }

Cls1ind = which(Cls==1)                              # Cls1 vs
Cls2ind = which(Cls!=1)                              #  alle anderen
X1 = X[Cls1ind]
Y1 = Y[Cls1ind]                    # XY Cls1
X2 = X[Cls2ind]
Y2 = Y[Cls2ind]                    # XY alle anderen Klassen


V= SmoothedDensitiesXY(X1,Y1,nbins,lambda,Xkernels,Ykernels)  # 2DHistogram 1
XYdens4Data1 = V$Densities
Xkernels1 = V$Xkernels
Ykernels1 = V$Ykernels
Hist1 = V$hist_F_2D

V = SmoothedDensitiesXY(X2,Y2,nbins,lambda,Xkernels,Ykernels)  # 2DHistogram 2
XYdens4Data2 = V$Densities
Xkernels2 = V$Xkernels
Ykernels2 = V$Ykernels
Hist2 = V$hist_F_2D

Hist1[Hist1<MinPercentage] = 0
Hist2[Hist2<MinPercentage] = 0

AnzBinPoints = nbins *nbins

Integral1 = sum(as.vector(Hist1))/ AnzBinPoints              # das integral bezogen auf die Anz Bins damit die nachfolgende Normierung nicht zu klein wird
Integral2 = sum(as.vector(Hist2))/ AnzBinPoints              # das integral bezogen auf die Anz Bins damit die nachfolgende Normierung nicht zu klein wird

Density1= Hist1/Integral1                            # Wahrscheinlichkeit des Auftretens von Cls1 *  AnzBinPoints = p1
Density2= Hist2/Integral2                            # Wahrscheinlichkeit des Auftretens von Cls2 *  AnzBinPoints = p2

Delta = Density2 - Density1                          # differenz der Wahrscheinlichkeit
AbsDelta = abs(Delta)                                # abs(p2-P1)

DeltaP = sum(as.vector(AbsDelta))/AnzBinPoints               # Differenz der wahrscheinlichkeiten

return(list(DeltaP=DeltaP,Xkernels=Xkernels,Ykernels=Ykernels,Density1=Density1,Density2=Density2))
}
