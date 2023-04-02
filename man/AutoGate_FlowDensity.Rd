\name{AutoGate_FlowDensity}
\alias{AutoGate_FlowDensity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AutoGate using FlowDensity
}
\description{
automatially gates flow cytometry data using flow density algorithm [Malek et al., 2015].
}
\usage{
AutoGate_FlowDensity(Data, Position, Header, PlotIt = FALSE, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
[1:n,1:d] numerical matrix of n cases and d features defining a dataset. Has to have either columnnames and the user has to specify header or d==2.
}
  \item{Position}{
[1:2] vector of two logical values specifying the position of the cell subset of interest on the 2D plot.
}
  \item{Header}{
[1:2] character vector defining the relevant column names if d>2
}
  \item{PlotIt}{
TRUE: Generate a scatter dot plot with colors based on the distribution of the density of the provided channels with the polygon
}
  \item{\dots}{
optional arguments for flow density, i.e., see details.
}
}
\details{
use.percentile' if TRUE, returns the 'percentile'th threshold.

'percentile' a value in [0,1] that is used as the percentile if 'use.percentile' is TRUE.

'upper' if 'TRUE', it finds the change in the slope after the peak with index 'peak.ind'.

'use.upper' if 'TRUE', forces to return the inflection point based on the first (last) peak if upper=F (upper=T)

'twin.factor' a value in [0,1] that is used to exclude twinpeaks.

'bimodal' If TRUE, it returns a cutoff that splits population closer to 50-50, when there are more than two peaks.

'after.peak' If TRUE, it returns a cutoff that is after the maximum peaks, when there are more than two peaks.

'sd.threshold' if TRUE, it uses 'n.sd' times standard deviation for gating.

'n.sd' an integer that is multiplied to the standard deviation to determine the place of threshold if 'sd.threshold' is 'TRUE'.

'tinypeak.removal' a vector of length 2, for sensitivity of peak finding for each channel. See deGate() for more information.

'filter' If provided it uses the given filter to gate the population.

'use.control' if TRUE, it finds the threshold using a matched control population and uses it for gating.

'control' a 'flowFrame' or 'CellPopulation' object used for calculating the gating threshold when 'use.control' is set to TRUE. If a control population is used, the other arguments ('upper', 'percentile', etc.) are applied to the control data when finding the threshold (i.e. not to 'obj').

'alpha' a value in [0,1) specifying the significance of change in the slope which would be detected. This is by default 0.1, and typically need not be changed.

'ellip.gate' if TRUE, it fits an ellipse on the data as a gate, otherwise the rectangle gating results are returned

'scale' a value in [0,1) that scales the size of ellipse to fit if 'ellip.gate' is TRUE

}
\value{
List with
\item{PolygonXY }{[1:p,1:2] xy cartesian coordinates of gate stored within a numerical matrix}
\item{GateModel }{a CellPopulation object of flowdensity package}

}
\references{
[Malek et al., 2015]  Malek, M., Taghiyar, M. J., Chong, L., Finak, G., Gottardo, R., & Brinkman, R. R.: flowDensity: reproducing manual gating of flow cytometry data by automated density-based cell population identification, Bioinformatics, Vol. 31(4), pp. 606-607, 2015.
}
\author{
Michael Thrun
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(XY)
\donttest{
V=AutoGate_FlowDensity(XY,Position = c(F,T),PlotIt = T)
}
\dontshow{
V=AutoGate_FlowDensity(XY[1:100,],Position = c(F,T),PlotIt = F)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{gating}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{auto gate}
% \concept{ ~cpt2 }
% Use only one concept per line.
