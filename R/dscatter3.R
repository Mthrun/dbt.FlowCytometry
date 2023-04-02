dscatter3=function(X,Y, Cls){
# dscatter3(X,Y)       # 3D plot of dcatter
# dscatter3(X,Y,Cls)   # ClassPlot3(X,Y,Density, Cls)
# [Density,Xkernels,Ykernels,F] = dscatter3(X,Y, Cls)  # calculate and display 2Dimensional density
# INPUT
# X,Y               set of points at which the Densities (SDH)are calculated and displayed as surface colored with the dots
#                   Z-axis is such that highest density ==1
#
# OPTIONAL
# Cls               if given ClassPlot3(X,Y,Density, Cls)  is performed
#
# OUTPUT
# Density,
# [Xkernels,Ykernels,F]             a mesch such that F[Xkernels,Ykernels] is a mesh of Denisties

if(missing(Cls)){ # do a 3D surface plot
  V = dscatter(X,Y,'PLOTTYPE','surf')
  hAxes=V$hAxes
  nbins=V$nbins
  Density=V$Density
  Xkernels=V$Xkernels
  Ykernels=V$Ykernels
  Fsmooth1D=V$Fsmooth1D
}else{ # CLS given
  V= SmoothedDensitiesXY(X,Y)  # Dichte Ausrechnen using Smothed Histograms
  Density=V$Density
  Xkernels=V$Xkernels
  Ykernels=V$Ykernels
  Fsmooth1D=V$Fsmooth1D
  ind==V$ind
  ClassPlot3(X,Y,Density, Cls)
} # if nargin < 3

return(list(Density=Density,Xkernels=Xkernels,Ykernels=Ykernels,Fsmooth1D=Fsmooth1D))
 }
