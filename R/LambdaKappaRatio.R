LambdaKappaRatio=function(Data,PlotIt=FALSE,digits=1,Polygon,GateVar1,GateVar2) {

  #Ratio nur sinnvoll auf Bcellen, d.h. Daten muessen vorher entsprechend gegated sein
  ##INPUT
  #Data       Datenmit den Variablen Lambda und Kappa
  # Optional
  # digits angabe der rundung nachkomme stellen von y
  # Polygon: wenn nicht gegeben dann per defaullt dreieck aus koordianten 0,0, 0,6 und 6,6
  # GateVar1, GateVar2 optional andere variablenbezeichnungen fuer kapp und lambda
  #PlotIt
  #OUTPUT
  # y     named vector mit Lambda zellen in % und Kappa Zellen in %
  #Wenn Lambda >50% -> krankheit vermutet
  # wenn kappa >70% -> krankheit vermutet
  #bem.: gesunde haben 10-20% der bcellen der kranken
  #author MT
  n=nrow(Data)

  if(missing(Polygon))
    Polygon=matrix(c(0,0,6,0,6,6),ncol = 2)

  if(missing(GateVar1))
    GateVar1 = "Kappa"

  if(missing(GateVar2))
    GateVar2 = "Lambda"

  #requireNamespace("dbt.FlowCytometry")
  requireNamespace("Classifiers")
  #requireNamespace("secr")
  out=dbt.FlowCytometry::ApplyGate(Data,GateVar1 =GateVar1,GateVar2 = GateVar2,Polygon =Polygon ,PlotIt = PlotIt)
  InLambda=out$InGateInd
  Lambda=length(InLambda)/n*100
  Kappa=length(setdiff(1:n,InLambda))/n*100
  y=round(c(Lambda,Kappa),digits)
  names(y)=c("LambdaInPercent","KappaInPercent")
  return(y)
}
