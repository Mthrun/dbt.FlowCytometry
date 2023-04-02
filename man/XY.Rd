\name{XY}
\alias{XY}
\docType{data}
\title{
XY sample file
}
\description{
two columns of flow cytometry sample file 01-Well-A10_auto_gated
}
\usage{data("XY")}
\format{
  The format is:
 num [1:29817, 1:2] 5.03 5.07 4.93 4.95 5 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:2] "SSC-A" "CD45-KO_V405-525BP-H"
 - attr(*, "na.action")= 'omit' num 2646
}
\details{
compensated and logarithmized with two columns of cell events
}
\source{
taken from E:/Subversion/PRO/Research/AutoGating/90RawData
}

\examples{
data(XY)
## maybe str(XY) ; plot(XY) ...
}
\keyword{datasets}
