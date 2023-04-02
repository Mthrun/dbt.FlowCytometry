\name{ApplyGate}
\alias{ApplyGate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ApplyGate
}
\description{
A specific Gate defined by xy coordinates that result in a closed polygon is applied to the flowcytometry data.
}
\usage{
ApplyGate(Data, GateVar1, GateVar2, Polygon, PlotIt = FALSE, PlotSampleSize = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
numerical matrix n x d
}
  \item{GateVar1}{
either column index in Data of X coordinate of gate or its variable name as string
}
  \item{GateVar2}{
either column index in Data of X coordinate of gate or its variable name as string
}
  \item{Polygon}{
numerical marix of two columns defining the coordiantes of the polygon. polygon assumed to be closed, i.e.,last coordinate connects to first coordinate.
}
  \item{PlotIt}{
if TRUE: plots a sample of data in the two selected variables and marks point inside the gate as yellow and outside as magenta
}
  \item{PlotSampleSize}{
size pof the plottet sample
}
}
\details{
Gates are alwaxs two dimensional, i.e., require two filters, although all dimensions of data are filted by the gates. Only high-dimensional points inside the polygon (gate) are given back
}
\value{
list of
\item{DataInGate }{m x d numerical matrix with m<=n of data points inside the gate}
\item{InGateInd }{index of length m for the datapoints in original matrix}

}
\author{
Michael Thrun
}
\note{
if GateVar1 or GateVar2 is not found a text is given back which will state this issue
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[Classifiers]{PointsInPolygon}}
}
\examples{
Data <- matrix(runif(1000), ncol = 10)
colnames(Data)=paste0("GateVar",1:ncol(Data))
poly <- cbind(x = c(0.2,0.5,0.8), y = c(0.2,0.8,0.2))
#set plotit TRUE for understanding the example

#Select indect
V=ApplyGate(Data,5,8,poly,PlotIt=FALSE,100)

#select var name
V=ApplyGate(Data,"GateVar5","GateVar8",poly,PlotIt=FALSE,100)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
