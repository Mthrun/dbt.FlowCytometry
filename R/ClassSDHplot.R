ClassSDHplot= function(Data,Cls,ColorSequence,ColorSymbSequence,PlotIt=TRUE,PlotLegend,LineWidth,xlim,ylim,...){
# [Kernels,ClassSDHdensities] = ClassSDHplot(Data,Cls,ColorSequence,ColorSymbSequence,PlotLegend)
# SDHplot the data for allclasses, weight the Plot with priors
# INPUT
# Data                 the Data to be plotted
# Cls                  vector of class identifiers can be integers or
#                      NaN's, need not be consecutive nor positive
# OPTIONAL
# ColorSequence        the sequence of colors used, if ==0 r not given: DefaultColorSequence
# ColorSymbSequence    the plot symbols used
#                      if there are less than 7 classes only the first
#                      symbol is used, otherwise the ColorSymbSequence is: '.sdv<>ph+*xo'
# PlotLegend           ==1 (default) add a legent to plot
# LineWidth           Line width for the PDF  (default:==1)
# OUTPUT
# Kernels,ClassSDHdensities         die PDFs
# Author: Alfred Ultsch (matlab), reeimplemented in R by Michael Thrun, 2019

  if(missing(LineWidth))
    LineWidth =1
  if (missing(PlotLegend))
    PlotLegend =PlotIt
  if (missing(ColorSymbSequence)) {
    ColorSymbSequence =  DataVisualizations::DefaultColorSequence#DefaultColorSymbSequence()#paste('-',DefaultColorSymbSequence)
  } else{
    warning('Not implemented')
  }
  if (missing(ColorSequence)){
    ColorSequence     = DataVisualizations::DefaultColorSequence                     # set default
  }else{
    if (ColorSequence == 0)
      ColorSequence = DataVisualizations::DefaultColorSequence               # set default
  }
  #if nargout >0  SameKernelsAndRadius =1  else  SameKernelsAndRadius =0   end   #inR ??


  V = FCPS::ClusterCount(Cls)  # KlassenZaehlen
  UniqueClasses=V$UniqueClusters
  CountPerClass=V$CountPerCluster
  NrOfClasses=V$NumberOfClusters
  ClassPercentages=V$ClusterPercentages
  BMClassPlotSymbols = dbt.ColorScale::BMClassColor(NrOfClasses, ColorSequence,ColorSymbSequence)  # Klassenfarben erzeugen


  UniqAnzPerClass=CountPerClass
  MaxWeight=max(UniqAnzPerClass)
  MaxWeightInd=which.max(UniqAnzPerClass)
  # MaxWeightInd= min(MaxWeightInd)  # falls es 2 gibt
  # Ind = find(Cls==UniqueClasses(MaxWeightInd))  # nur die Datenpunkte aus der Verteilung mit maxWeight
  # [Kernels,SDH] = SmoothDensHist1D(Data(Ind),200)  # Kernels bestimmen

  V = SmoothDensHist1D(Data, 200)  # Kernels bestimmen
  Kernels =V$Kernels
  SDH = V$SDH
  ClassSDHdensities = matrix(0, length(Kernels), NrOfClasses)
  # fuer alle Klassen
  if(missing(xlim)) xlim=c(min(Kernels),max(Kernels))
  if(missing(ylim)) ylim=c(0,max(SDH))
  for (c in 1:NrOfClasses) {
    Class = UniqueClasses[c]
    ClassInd = which(Cls == Class)
    Weight = ClassPercentages[c] / 100  # gewichtet mit a prioris
    if(isTRUE(PlotIt)){
    if (c == 1)
      V = SDHplot(
        Data[ClassInd],
        ColorSequence[c],
        BMClassPlotSymbols[c, ],
        Weight,
        Kernels,
        LineWidth,
        Overlay = FALSE,
        xlim=xlim,
        ylim=ylim,
        ...
      )
    else
      V = SDHplot(
        Data[ClassInd],
        ColorSequence[c],
        BMClassPlotSymbols[c, ],
        Weight,
        Kernels,
        LineWidth,
        Overlay = TRUE,
        xlim=xlim,
        ylim=ylim,
        ...
      )
    }
    SDH = V$SDH

    ClassSDHdensities[, c] = SDH
  }  # for c

  # hold off
  # axis tight
  if (isTRUE(PlotLegend)) {
    legend(
      'topright',
      col = ColorSequence[1:NrOfClasses],
      text.col = ColorSequence[1:NrOfClasses],
      legend = as.character(UniqueClasses)
    )
    #legend(char(num2str(UniqueClasses)))
  }  # if PlotLegend ==1
  return(list(Kernels = Kernels, ClassSDHdensities = ClassSDHdensities))
}
