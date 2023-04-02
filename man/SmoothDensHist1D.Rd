\name{SmoothDensHist1D}
\alias{SmoothDensHist1D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Smoothed Density Histogams in 1 Dimension published in [Eilers/Goeman, 2004].
}
\description{
deault SDH is a probability distribution i.e integral (SDH) ==1
}
\usage{
SmoothDensHist1D(x, KernelsOrNbins = NULL, SDHinPercent, lambda,PlotIt=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 one dimensional data, NaN are ignored
}
  \item{KernelsOrNbins}{
 either c>1 Kernels at which SDH is calculated or
                  KernelsOrNbins = nbins number of bins
                   if KernelsOrNbins = [] or == 0 then
                   default: KernelsOrNbins =200
}
  \item{SDHinPercent}{
==1 means max(SDH)  default SDHinPercent =0
}
  \item{lambda}{
smoothing factor used by the density estimator
                   default: lambda = 20 which roughly
                   means that the smoothing is over 20 bins around a given point.
}
  \item{PlotIt}{
  TRUE: raw plot
  }
}
\details{
none
}
\value{
List with
\item{SDH }{(1:nbins)      Smoothed Density Histogam for x at}
\item{Kernels }{(1:nbins) locations  such that plot(Kernels,SDH) gives the Smoothed Density Histogam}

}
\references{
[Eilers/Goeman, 2004]  Eilers, P. H., & Goeman, J. J.: Enhancing scatterplots with smoothed densities, Bioinformatics, Vol. 20(5), pp. 623-628. 2004.
}
\author{
Alfred Ultsch (matlab), reeimplemented in R by Michael Thrun, 2019, improved by Joern Loetsch
}
\note{
 optional: ax(SDH) ==1 does not work yetg
}

\seealso{
\code{\link{SDHplot}}
}
\examples{
x=runif(100)
SmoothDensHist1D(x)

\dontshow{
data(iris)
x=as.vector(iris[,2])
V=SmoothDensHist1D(x)
V2=SmoothDensHist1D(c(3,3,3),KernelsOrNbins=V$Kernels)
V3=SmoothDensHist1D(c(3,NaN),KernelsOrNbins=V$Kernels)

V=SmoothDensHist1D(x,Kernels=seq(from=3,to=4,by=0.1))


}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{SmoothDensHist1D}% use one of  RShowDoc("KEYWORDS")
\keyword{Smoothed Density}% __ONLY ONE__ keyword per line
