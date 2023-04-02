PlotBestDeltaP=function(Data,Header,Cls,AllDeltaP,XPrimeNames,YPrimeNames,FN,AnzPlotRows=3,AnzPlotColums=4){
# PlotBestDeltaP(Data,Header,Cls,AllDeltaP,XPrimeNames,YPrimeNames,FN)
#  Plot the best DeltaP and first XPrimeNames vs YPrimeNames
#
# INPUT
# Data,Header,Cls
#
# OPTIONAL
# AllDeltaP                            see AllDeltaP = DeltaPxy(Data,Cls)
# XPrimeNames,YPrimeNames              diese Variablen werden als erstes dargestellt
# FN                                   dies wird als ueberschrift auf den ersten tile gesetzt
# AnzPlotColums,AnzPlotRows            for tileplot(AnzPlotRows,AnzPlotColums,w)
#
# OUTPUT
# PlottedXNames(1:9)                  diese Variablen wurden geplotted
# PlottedYNames(1:9)

# Data,Header,Cls,AllDeltaP,XPrimeNames,YPrimeNames,FN,AnzPlotRows,AnzPlotColum
# 1       2   3    4             5          6       7      8          9
#author MT 07/2020, reimplemented from matlab of ALU

  #multiplot ist vermutlich schlechter als facet_wrap
  ## p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  ## p
  ## p + facet_wrap(vars(class))

  warning('This functions was not tested so far')

  if(missing(Header))
    Header = colnames(Data)

  if(missing(AllDeltaP))
     AllDeltaP = DeltaPxy(Data,Cls)
  if(missing(XPrimeNames))
    XPrimeNames = c()
  if(missing(YPrimeNames))
    YPrimeNames = c()
  if(missing(FN))
    FN = c()


AllTiles      = AnzPlotColums *AnzPlotRows
AnzPrimeNames=dim(XPrimeNames)[1]
AnzYPrimeNames=dim(YPrimeNames)[1]

w = 0    #  tilenumer fuer plot
AllXind = c()
AllYind = c()  # aufsammeln zum spaeteren verhindern von doubletten
PlottedXNames = ' '
PlotedYNames = ' '  # zum spaeteren erweitern
plothandle=list()
for(v in 1:AnzPrimeNames){
	Xind = which(Header==XPrimeNames[v])
	Yind = which(Header==YPrimeNames[v])

	AllXind = c(AllXind,Xind)
	AllYind = c(AllYind ,Yind)

 	X = Data[,Xind]
 	Y = Data[,Yind]
 	XN = Header[Xind]
  YN =  Header[Yind]
    w=w+1

    #tileplot(AnzPlotRows,AnzPlotColums,w)
    Ind = which((X>0)& (Y>0))  # dumme weglassen
    nzX = X[Ind]
    nzY=Y[Ind]
    NZcls = Cls[Ind]
    if(w==1 & length(FN)>0)
      title=FN
    else
      title=paste0(AllDeltaP[Xind,Yind])

    plothandle[v]=DataVisualizations::Classplot(nzX,nzY,NZcls,Colors = c("blue","red"),main =title,xlab = XN,ylab = YN,Plotter="ggplot")+ggplot2::xlim(c(0,6))+ggplot2::ylim(c(0,6))
    #axis([0,6,0,6])#no legend
    #drawnow
	PlottedXNames = c(PlottedXNames,XN)
	PlottedYNames = c(PlottedYNames,YN)
}  # for v
# AllXind  und AllYind =enhalten jetzt die bereits gezeichneten indices
dbt.Plot::multiplot(plotlist =plothandle,cols= AnzPlotColums)

# jetzt noch die restlichen tileplots auf die Groessten AllDeltaP aufteilen
	DeltaPVector = pracma::squareform(AllDeltaP)
	SortedDeltaPVector = sort(DeltaPVector,decreasing = TRUE)
i=0
LastTile = AllTiles
plothandle2=list()
for(v in 1:AllTiles+w){
  i=i+1
   if(i <= LastTile){
   ind = which(AllDeltaP==SortedDeltaPVector[i],arr.ind = T)
   Xind=ind[,1]
   Yind=ind[,2]
   # naechst kleineres DeltaP ausproieren
    Xind=Xind[2]#???
    Yind=Yind[2]#???
	if(is.element(Xind, AllXind) & is.element(Yind, AllYind)){    # schaun obs schon gezeichet war
	 LastTile=LastTile+1                                    # bereits gezeichnet, es darf also noch ein tile gezeichnet werden
	}else{                                                    # noch nicht gezeichnet also Zeichnen
	  X = Data[,Xind]
	  Y = Data[,Yind]
	  XN = Header[Xind]
	  YN =  Header[Yind]
	  w=w+1
	  if(w <= AllTiles){
		#tileplot(AnzPlotRows,AnzPlotColums,w)

	    Ind = which((X>0)& (Y>0))  # dumme weglassen
	    nzX = X[Ind]
	    nzY=Y[Ind]
	    NZcls = Cls[Ind]

		if(w==1 & length(FN)>0)
		  title=FN
		else
		  title=paste0(AllDeltaP[Xind,Yind])

		#ClassDotPlot(nzX,nzY,NZcls,'br') legend off xlabel(XN) ylabel(YN)  grid on axis([0,6,0,6])
	   plothandle2[v]=DataVisualizations::Classplot(nzX,nzY,NZcls,Colors = c("blue","red"),main =title,xlab = XN,ylab = YN,Plotter="ggplot")+ggplot2::xlim(c(0,6))+ggplot2::ylim(c(0,6))

		#drawnow
		PlottedXNames = c(PlottedXNames,XN)
		PlottedYNames = c(PlottedYNames,YN)
	  }  # if w <= AllTiles
	}  # if ismember(Xind, AllXind) & ismember(Yind, AllYind)
   } # v < LastTile
} # for v

dbt.Plot::multiplot(plotlist =plothandle2,cols= AnzPlotColums)

	PlottedXNames = PlottedXNames[2:length(PlottedXNames)]
	PlottedYNames = PlottedYNames[2:length(PlottedYNames)]

return(list(PlottedXNames,PlottedYNames))
}
