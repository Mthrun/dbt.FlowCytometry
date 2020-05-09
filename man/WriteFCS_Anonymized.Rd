\name{WriteFCS_Anonymized}
\alias{WriteFCS_Anonymized}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
WriteFCS Anonymized
}
\description{
Anonymizes data by writing out fcs data with a specific selection of content.
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
Optional, a 1:d character vector containing the names of the features of the data stored in the RawFrame
}
  \item{OutDirectory}{
Optional, Output directory for the newly created *.fcs  (default ==  getwd() )
}
  \item{shiny}{
Optional, shiny =TRUE disabled several checks that are not useful in the interactive application.
}
}
\details{
The AnnotatedDF is stored internally in RawFrame, but using it externally, allows the user to change the parameter names manually in AnnotatedDF.
}
\value{
an *.fcs file is written out.
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{WriteFCS_Anonymized}% use one of  RShowDoc("KEYWORDS")
\keyword{FCS}% __ONLY ONE__ keyword per line
\keyword{LMD}% __ONLY ONE__ keyword per line
\keyword{write}% __ONLY ONE__ keyword per line
