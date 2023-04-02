SmoothDensHist2D= function(x,lambda=NULL){

# SDH = SmoothDensHist2D(H) 
# SDH = SmoothDensHist2D(H,lambda) 
# Smoothed Density Histogams in 2 Dimension
#
# INPUT
# H[1:l,1:c]        two dimensional histogram matrix
#
# OPTIONAL
# lambda or [] or 0   smoothing factor used by the density estimator
#                     default:  lambda = max(size(H))
#
# OUTPUT
# SDH[1:l,1:c]        Smoothed Density Histogam for H

#Autor: Tim Schreier
#uebertragen von SmoothDensHist2D.m



  nbins =  dim(x)
  MaxAnzBins = max(nbins,na.rm=T)

  if(is.null(lambda) || lambda==0){
    lambda= MaxAnzBins
  }

  G = smooth1D(x,nbins[2]/lambda)

  SDH = smooth1D(t(G), nbins[1]/lambda)
  return (SDH)

}
