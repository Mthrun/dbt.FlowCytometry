\name{WriteFCS_Anonymized}
\alias{WriteFCS_Anonymized}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
WriteFCS Anonymized
}
\description{
Anonymizes data by writing out fcs data with a specific selection of content
}
\usage{
WriteFCS_Anonymized(Filename, RawFrame, AnnotatedDF,

Header = NULL,OutDirectory = getwd(), shiny=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Filename}{
name for *.fcs file
}
  \item{RawFrame}{object of class flowFrame, see \code{\link[flowCore]{read.FCS}} in \pkg{flowCore}}
  \item{AnnotatedDF}{
see \pkg{Biobase} AnnotatedDataFrame
}
  \item{Header}{
1:d names for the features of the data stored in RawFrame
}
  \item{OutDirectory}{
Optional, directory where to write out the *.fcs  (default ==  getwd() )
}
  \item{shiny}{
Optional, shiny =TRUE disabled several checks not useful in interaxtive application
}
}
\details{
AnnotatedDF is internally stored in RawFrame, but using it externally, allowes the user to change the parameter names manually in AnnotatedDF
}
\value{
an *.fcs file is written out
}
\references{
the manual
}
\author{
Michael Thrun
}
\note{
none
}

\seealso{
\code{\link{WriteFCS_Anonymized}},\code{\link[flowCore]{read.FCS}}
}
\examples{
##TODo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{WriteFCS_Anonymized}% use one of  RShowDoc("KEYWORDS")
\keyword{FCS}% __ONLY ONE__ keyword per line
\keyword{LMD}% __ONLY ONE__ keyword per line
\keyword{write}% __ONLY ONE__ keyword per line
