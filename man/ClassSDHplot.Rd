\name{ClassSDHplot}
\alias{ClassSDHplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Class-wise SDH plot
}
\description{
SDHplot the data for allclasses, weight the Plot with priors
}
\usage{
ClassSDHplot(Data, Cls, ColorSequence, ColorSymbSequence, PlotLegend, LineWidth, ...)
}
\arguments{
  \item{Data}{
the Data to be plotted
}
  \item{Cls}{
 vector of class identifiers can be integers or NaN's, need not be consecutive nor positive
}
  \item{ColorSequence}{
the sequence of colors used, if ==0 r not given: DefaultColorSequence
}
  \item{ColorSymbSequence}{
the plot symbols used
if there are less than 7 classes only the first
symbol is used, otherwise the ColorSymbSequence is: '.sdv<>ph+*xo'
}
  \item{PlotLegend}{
==TRUE (default) add a legent to plot
}
  \item{LineWidth}{
Line width for the PDF  (default:==1)
}
  \item{\dots}{
Further arguments passed on to \code{\link{plot}} and \code{\link{points}}
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
ColorSymbSequence not implemented yet.
}

\seealso{
\code{\link{SDHplot}}
\code{\link{SmoothDensHist1D}}
}
\examples{
Data=c(rnorm(100,0,1),rnorm(100,5,1))
Cls=c(rep(1,100),rep(2,100))
\donttest{
ClassSDHplot(Data, Cls)
}
}
\keyword{SDHplot}% use one of  RShowDoc("KEYWORDS")
\keyword{SmoothDensHist1D}% __ONLY ONE__ keyword per line
\keyword{Smoothed Density}% __ONLY ONE__ keyword per lin
\keyword{class-wise Smoothed Density}
