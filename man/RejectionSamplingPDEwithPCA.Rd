\name{RejectionSamplingPDEwithPCA}
\alias{RejectionSamplingPDEwithPCA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
RejectionSamplingPDEwithPCA
}
\description{
Samples Cluster consistent using Rejection sampling with a combination of PCA and PDE
}
\usage{
RejectionSamplingPDEwithPCA(Data, SampleSize = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
 [1:n,1:d] datamatrix
}
  \item{SampleSize}{
SampleSize, usually lower than n,
}

}
\details{
if SampleSize is higher than n, then only d=3 data is currently possible.

Cluster consistent in a sense that besides outliers all FCPS structures can be sampled correctly [Thrun/Ultsch, 2020].
}
\value{
  [1:SampleSize,1:d] sample of datamatrix
}
\references{
[Thrun/Ultsch, 2020]  Thrun, M. C., & Ultsch, A.: Clustering Benchmark Datasets Exploiting the Fundamental Clustering Problems, Data in Brief, Vol. in press, pp. 105501, \doi{10.1016/j.dib.2020.105501}, 2020.

}
\author{
Michael Thrun
}

\examples{
#ToDo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{RejectionSamplingPDEwithPCA}% use one of  RShowDoc("KEYWORDS")
\keyword{RejectionSampling}% __ONLY ONE__ keyword per line
