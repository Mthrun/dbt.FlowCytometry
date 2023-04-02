AutoGate_FlowDensity=function(Data,Position,Header,PlotIt=FALSE,...){
#V=    AutoGate_FlowDensity(Data, Position, Header, PlotIt = FALSE)
#AutoGate using FlowDensity
#automatially gates flow cytometry data using flow density algorithm [Malek et al., 2015].
#INPUT
# Data
#       [1:n,1:d] numerical matrix of n cases and d features defining a dataset. Has to have either columnnames and the user has to specify header or d==2.
#
# Position
#       [1:2] vector of two logical values specifying the position of the cell subset of interest on the 2D plot.
#
# Header
#       [1:2] character vector defining the relevant column names if d>2
#
# PlotIt
#       TRUE: Generate a scatter dot plot with colors based on the distribution of the density of the provided channels with the polygon
#
# \dots
#     optional arguments for flow density, i.e., see details in *.RD documentation by caling ?AutoGate_FlowDensity
#
# OUTPUT
# PolygonXY       [1:p,1:2] xy cartesian coordinates of gate stored within a numerical matrix}
# GateModel      a CellPopulation object of flowdensity package}
#
# author: Michael Thrun
#
# [Malek et al., 2015]  Malek, M., Taghiyar, M. J., Chong, L., Finak, G., Gottardo, R., & Brinkman, R. R.: flowDensity: reproducing manual gating of flow cytometry data by automated density-based cell population identification, Bioinformatics, Vol. 31(4), pp. 606-607, 2015.
#
#EXAMPLE
#data(XY)
#V=AutoGate_FlowDensity(XY,Position = c(F,T),PlotIt = T)

  if(!requireNamespace("flowDensity")){
    warning("Please install the package flowDensity because it is missing. returning null")
    return(list(PolygonXY=NULL,GateModel=NULL))
  }

  if(!is.matrix(Data)){
    warning("AutoGate_FlowDensity: Data is not matrix, calling as.matrix()")
    Data=as.matrix(Data)
  }
  nc=dim(Data)
  if(nc[2]<2){
    stop("AutoGate_FlowDensity: data has to have at least 2 columns")
  }
  if(is.null(colnames(Data))){
    if(nc==2){
      colnames(Data)=c("X","Y")
    }
  }

  if(missing(Header)){
    if(!is.null(colnames(Data))){#in that case two columns data, d=2
      Header=colnames(Data)[1:2]
    }else{#no headaer ond d!=2
      stop("AutoGate_FlowDensity: data has no colnames and no header is given. Please either provide data with colnames or a header.")
    }
  }else{
    if(length(Header)>2){
      warning("AutoGate_FlowDensity: Header has more than two elements. Using the first two.")
      Header=Header[1:2]
    }

    if(length(Header)<2){
      warning("AutoGate_FlowDensity: Header has less than two elements. Using the first two columns of data")
      if(!is.null(colnames(Data))){
        Header=colnames(Data)[1:2]
      }else{
        stop("AutoGate_FlowDensity: data has no colnames and no valid header is given. Please either provide data with colnames or a header.")
      }
    }
  }


  if(length(Position)>2){
    warning("AutoGate_FlowDensity: Position has more than two elements. Using the first two.")
    Position=Position[1:2]
  }
  if(length(Position)<2){
    stop("AutoGate_FlowDensity: Position less than two elements.")
  }
  if(sum(Position)>2){
    stop("AutoGate_FlowDensity: Position os not bolean.")
  }
  if(sum(Position)<0){
    stop("AutoGate_FlowDensity: Position os not bolean.")
  }
  indvar1=which(colnames(Data)==Header[1])
  indvar2=which(colnames(Data)==Header[2])

  if(length(indvar1)!=1){
    stop("AutoGate_FlowDensity: Header[1] was not identified in colnames of data. Please provide eithe no Header or a valid Header.")
  }
  if(length(indvar2)!=1){
    stop("AutoGate_FlowDensity: Header[2] was not identified in colnames of data. Please provide eithe no Header or a valid Header.")
  }
  frame=flowCore::flowFrame(Data)

  Gate_Model <- flowDensity::flowDensity(frame,channels = c(Header),position = Position,...)

  if(isTRUE(PlotIt)){
    flowDensity::plotDens(frame,c(indvar1,indvar2))
    lines(Gate_Model@filter,type="l")
  }
  Polygon=Gate_Model@filter

  return(list(PolygonXY=Polygon,GateModel=Gate_Model))
}
