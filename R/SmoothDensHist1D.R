SmoothDensHist1D= function(x,KernelsOrNbins=NULL,SDHinPercent,lambda,PlotIt=FALSE){
# [Kernels,SDH] = SmoothDensHist1D(x,KernelsOrNbins,SDHinPercent,lambda)
# Smoothed Density Histogams in 1 Dimension
# deault SDH is a probability distribution i.e integral (SDH) ==1
# optional: ax(SDH) ==1
#
# INPUT
# x(1:n)            one dimensional data, NaN are ignored
#
# OPTIONAL
# KernelsOrNbins    either c>1 Kernels at which SDH is calculated or
#                   KernelsOrNbins = nbins number of bins
#                   if KernelsOrNbins = [] or == 0 then
#                   default: KernelsOrNbins =200
#
# SDHinPercent      ==1 means max(SDH)  default SDHinPercent =0
#
# lambda            smoothing factor used by the density estimator
#                   default: lambda = 20 which roughly
#                   means that the smoothing is over 20 bins around a given point.
#
# OUTPUT
# SDH(1:nbins)      Smoothed Density Histogam for x at
# Kernels(1:nbins) locations  such that plot(Kernels,SDH) gives the
#                   Smoothed Density Histogam
# Author: Alfred Ultsch (matlab), reeimplemented in R by Michael Thrun and Tim Schreier, 2019, improved by J.Loetsch in 2021
  if(!requireNamespace('pracma'))
    stop("SmoothDensHist1D: Please install pracma")

  if(!requireNamespace('caTools'))
    stop("SmoothDensHist1D: Please install caTools")

  if(!requireNamespace('DataVisualizations'))
    stop("SmoothDensHist1D: Please install DataVisualizations")

  if(!requireNamespace('dbt.RetroMAT'))
    stop("SmoothDensHist1D: Please install dbt.RetroMAT")

  smooth1D= function(...,f=DataVisualizations:::smooth1D){
    return(f(..., na.rm = TRUE,Silent=TRUE))
  }

  # end function Z = smooth1D(Y,lambda)
  if (length(x) == 0) {
    warning("SmoothDensHist1D: Size of x is zero.", call. = FALSE)
    if (is.null(KernelsOrNbins) == TRUE)
      Kernels = 1
    else
      Kernels = KernelsOrNbins
    SDH = Kernels * 0
  } else {
    if (missing(lambda))
      lambda = 20             # smoothing factor
    if (missing(SDHinPercent))
      SDHinPercent = 0        # SDH is a PDF

    if (is.null(KernelsOrNbins)) {
      KernelsOrNbins = 200   # Anz Bins
    }
	 if(length(KernelsOrNbins) < 1) KernelsOrNbins = 200

    x   = x[is.finite(x)]#colvector(noNaN(x))   # leave nan out
    n   = length(x)  # Anzahl Daten
    minx = min(x, na.rm = T)
    maxx = max(x, na.rm = T)

    # Kernnels oder nbins
    if (length(KernelsOrNbins) == 1) {
      # nur anzahl bins gegeben
      nbins = KernelsOrNbins
      edges1 = pracma::linspace(minx, maxx, nbins + 1)
      end = length(edges1)
      Kernels = c(edges1[1:(end - 1)] + 0.5 * diff(edges1))#colvector(edges1(1:end-1) + .5*diff(edges1))
      InInd = c()  # fuer die Buchhaltung
    } else{
      # Kernels gegeben, berechne nbins
      Kernels = c(KernelsOrNbins) #colvector
    } #  end if length(KernelsOrNbins) = 1
      # Wertebereiche Beachten
      InInd = which((Kernels >= minx) &
                      (Kernels <= maxx))  # Index der Kernels im datenbereich

      if (length(InInd) == 0) {
        # keine Kernels im Datenbereich
        SDH = Kernels * 0
        nbins = 1
      } else{

        DataInd = which((x >= min(Kernels[InInd])) &
                          (x <= max(Kernels[InInd])))
        if (length(DataInd) < 2) {
          if (length(DataInd) == 0) {
            SDH = Kernels * 0
            nbins = 1
          } else {
            SDH = Kernels * 0
            SDH[InInd] <- 1
            nbins = 1
          }
        } else {
        x = x[DataInd]
        edges1  =  sort(Kernels[InInd],decreasing = FALSE)  # NUr die Kernels im Datenereich,must be a monotonically non-decreasing vector for hist c
        nbins = length(edges1)

      # ab hier gibts Kernels,edges1,nbins

      # # Histogramm rechnen
      ende = length(edges1)
      if(length(edges1)>3){
        edges1 = c(-Inf, edges1[2:(ende - 1)], Inf)
      }else if(length(edges1)==3){
        edges1 = c(-Inf, edges1[2], Inf)
      }else{
        #MT keine ahnung was man hier machen soll
        warning("SmoothDensHist1D: Two or less than two Kernels are in the data. Density will not be estimated correctly.")
        edges1 = c(-Inf,edges1,Inf)
      }

      V = pracma::histc(x, edges = edges1)   # histogram in x(i) gehoert in welches bin
      dummy = V$cnt

      bin = V$bin
      #   H ist die Histogrammdichteschätzung
      # dauert ggf lange fuer grosse datenmengen

      #H = accumarray(bin,1,[nbins,1]) ./ n; #matlab
      #doku: C:\Subversion\PUB\dbt\RetroMAT\R\accumarray.R
      #funktioniert nicht.
      #H = pracma::accumarray(bin, 1,c(1,nbins)) / n
      #funktioiert, kann aber falsch sein
      # H=pracma::accumarray(bin,rep(1,length(bin)))/n
      #TIM:
      H = dummy/ n

      # smoothig
      SDH = smooth1D(H, nbins / lambda) #oder der call ist falsch

      SDH = as.vector(SDH)
        }# end if  if (length(DataInd) < 2) {
      if (length(InInd) > 1) {
        # Wertebereich der Kernels berucksichtigen
        sdh = SDH
        SDH = Kernels * 0
        SDH[InInd] = sdh
      }  # if  Wertebereich der Kernels berucksichtigen
    }#end length(InInd) == 0)
    if (SDHinPercent == 0) {
      # SDH is a PDF
      # Flaeche auf 1 Normieren
      if (sum(SDH) == 0) {
        Area = 0
      } else {
      Area = caTools::trapz(Kernels, SDH)  # Fläche unter SDH
      }
      if (Area < 1e-10) {
        # praktisch null
        SDH = rep(x = 0,nbins)
      } else{
        SDH = SDH / Area   # SDH approximierte Wahrscheinlichkeits Dichten
      }
    } else{
      # in Prozent
      SDH = SDH / dbt.RetroMAT::nanmax(SDH)
    }  #if SDHinPercent> 0

      if (isTRUE(PlotIt)) {
        plot(
          Kernels,
          SDH,
          type = 'l',
          main = 'RAW SDH rplot',
          xaxs = 'i',
          yaxs = 'i',
          xlab = 'Data',
          ylab = 'SDH',
          ylim=c(0,max(SDH)*1.1)
        )
      }

    return(list(Kernels = Kernels, SDH = SDH))
  }
}

# V1 Function is buggy. V 2. Version is written here
# Und in der DBT unter /PUB/dbt/pareto/R/SmoothDensHist1Dv2.R
# Here is an analysis of the bugs
#
# Debugging:
#   /PUB/dbt/pareto/R/SmoothDensHist1D.R
#
# smooth1D (Unterfunktion){ #MT: nun eigene funktion
#   -Zeile 37:
#     Hier sollte eine mxm Einheitsmatrix erstellt werden.
#     Ich habe
#     E = diag(1,m,n)
#     durch
#     E = diag(200)
#     ersetzt.
#   -Zeile 38 & 39:
#     D1 und D2 müssen Matrizen bleiben.
#     Ich habe in beiden Zeilen den as.vector() Aufruf entfernt.
#   -Zeile 42:
#     Beim Übertragen von Matlab wurde das Transponieren von D2 und D1 vergessen.
#     Neue Zeile:
#     P = lambda ^ 2 * ( t(D2) %*% D2) + 2 * lambda * ( t(D1) %*% D1)
#   -Zeile 43:
#     Hier muss nicht durch Y geteilt werden, sondern ein LGS gelöst werden.
#     ( matlab Operartor ist \ ). Auch das as.numeric() habe ich weggelassen.
#     alte Zeile:
#     Z = (E + as.numeric(P)) / Y
#     neue Zeile:
#     Z= solve((E+P),Y)
#   }
#
#   -Zeile 100:
#     hier fehlt ein Paar Klammern.
#     Alt:
#     edges1 = c(-Inf, edges1[2:end - 1], Inf)
#     Neu:
#     edges1 = c(-Inf, edges1[(2:end - 1)], Inf)
#
#   -Außerdem habe ich den Aufruf (Zeile:114)
#       H=pracma::accumarray(bin,rep(1,length(bin)))/n
#       durch die Zeile :
#       H = dummy[1:200]/n
#       ersetzt.
#       Dummy wurde bereits berechnet und liefert exakt das gleiche Ergebnis für H.
#       (Kein Fehler, aber hier kann man Laufzeit sparen)
#
#       → Auch in SmoothDensHist1.m (Matlab version) kann man sich den accumarry Aufruf sparen.
#       Hier könnte man Zeile 55 durch
#       H = Dummy(1:200) ./n
#       ersetzten um Laufzeit zu sparen.
