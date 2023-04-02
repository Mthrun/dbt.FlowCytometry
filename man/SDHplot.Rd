\name{SDHplot}
\alias{SDHplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SDHplot
}
\description{
plot smoothed density estimation  SDH
}
\usage{
SDHplot(x, PlotColor, PlotSymbol, Weight, Kernels = NULL, LineWidth, Overlay = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Vector of Data to be plotted
}
  \item{PlotColor}{
 Parameter for plotting PDE see Function plot, blue if omitted
}
  \item{PlotSymbol}{
Parameter for plotting PDE see Function plot,      '-' if omitted
}
  \item{Weight}{
 Weight*ParetoDensity  is plotted,            Weight==1, if omitted
}
  \item{Kernels}{
the x points of the PDE function  or ==0(default), => need to calculate Kernels
}
  \item{LineWidth}{
linewith of the PDE plot, see plot(...,'LineWidth',...I)
}
  \item{Overlay}{
FALSE: calls plot, TRUE: calls points
}
  \item{\dots}{
Further arguments passed on to either \code{\link{plot}} or \code{\link{points}}
}
}
\details{
none
}
\value{
List with
\item{SDH }{(1:nbins)      Smoothed Density Histogam for x at}
\item{Kernels }{(1:nbins) locations  such that plot(Kernels,SDH) gives the Smoothed Density Histogam}
in mode invisible
}
\references{
none so far
}
\author{
Alfred Ultsch (matlab), reeimplemented in R by Michael Thrun, 2019
}
\note{
PlotSymbol Not implemented yet.
}

\seealso{
 \code{\link{SmoothDensHist1D}}
}
\examples{
x=runif(100)
\donttest{SDHplot(x)}
}

\keyword{SDHplot}% use one of  RShowDoc("KEYWORDS")
\keyword{SmoothDensHist1D}% __ONLY ONE__ keyword per line
\keyword{Smoothed Density}% __ONLY ONE__ keyword per line
