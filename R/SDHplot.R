SDHplot = function(x,PlotColor,PlotSymbol,Weight,Kernels=NULL,LineWidth,Overlay=FALSE,...){
# [Kernels,SDH] = SDHplot(x,PlotSymbol,Weight,Kernels,LineWidth)
# plot smoothed density estimation  SDH
# uses PDEstimationFuerGauss and ParetoRadiusfuerGMM
#
# INPUT
# x                  Vector of Data to be plotted
#
# OPTIONAL
# PlotColor          Parameter for plotting PDE see Function plot, blue if omitted
# PlotSymbol         Parameter for plotting PDE see Function plot,      '-' if omitted
# Weight             Weight*ParetoDensity  is plotted,            Weight==1, if omitted
# Kernels            the x points of the PDE function  or ==0(default), => need to calculate Kernels
# LineWidth          linewith of the PDE plot, see plot(...,'LineWidth',...I)
# Overlay             FALSE: calls plot, TRUE: calls points
# OUTPUT
# Kernels            the x points of the PDE function
# SDH                the SDH(x)

# Author: Alfred Ultsch (matlab), reeimplemented in R by Michael Thrun, 2019

  x = x[is.finite(x)]
  if (missing(PlotColor)) PlotColor="blue"

  if (missing(LineWidth))
    LineWidth = 1   # linienbreite 1
  if (is.null(Kernels) ||
      length(Kernels) == 0 ||
      sum(Kernels) == 0)
    Kernels = 200 # Kernels muessen bestimmt werden
  if (missing(Weight))
    Weight = 1
  if (missing(PlotSymbol)) {
    PlotSymbol = 'b-'
    warning('Not implemented')
  } else{
    warning('Not implemented')
  }

  if (length(Kernels) == 1) {
    # nur Zahl geg. Kernels werden erst bestimmt
    nbins = Kernels
  } else{
    # Kernels als Vektor gegeben
    nbins = length(Kernels)
  }  #if length(Kernels) ==1 #
  if (nbins > 150) {
    lambda = 20  #default lambda  # evenuuell mal bessere heuristik 200 -> 20 100 -> 40
  } else{
    lambda = 40
  }  # if nbins > 150

  SDHinPercent = 0  # make it a PDF
  V = SmoothDensHist1D(x, Kernels, SDHinPercent, lambda)
  Kernels = V$Kernels
  SDH = V$SDH
  SDH = SDH * Weight
  # Now plot it all
  if(isTRUE(Overlay)){
    plot.new()
    plot(
      Kernels,
      SDH,
      type = 'l',
      col = PlotColor,
      lwd = LineWidth,
      ylab = 'SDH',
      xlab = 'data',
      main = 'SDH(x)',...
    )
  }else{
    plot.new()
    points(
      Kernels,
      SDH,
      type = 'l',
      col = PlotColor,
      lwd = LineWidth,
      ylab = 'SDH',
      xlab = 'data',
      main = 'SDH(x)',...
    )
  }
  #ax = axis   yaxis(0,ax(4)) #in R ??

  return(invisible(list(Kernels = Kernels, SDH = SDH)))

}
