\name{AnonymizeFACSdata}
\alias{AnonymizeFACSdata}

\title{
Anonymize FACS data
}
\description{
Interactive tool for Anonymization of FACS data (either *.fcs or *.lmd)
}
\usage{
AnonymizeFACSdata()
}

\details{
If *.lmd format is chosen, marburg style is assumed. This means the spillover matrix and the data is in the second dataset but the names of the parameters in the first dataset. Otherwise please use \code{\link{ReadFCS_FlowCompensated}} and \code{\link{WriteFCS_Anonymized}} manually.
}
\value{
a *.fcs file is written out
}
\references{
\url{http://www.deepbionics.org}
}
\author{
Michael Thrun
}

\note{
technical note: Due to security reasons the output file path cannot be the file input path with usual shiny. However, in a future version shinyFiles could be used so that the choice for output file paht is automatically the same that input file path
}

\seealso{
\code{\link{ReadFCS_FlowCompensated}},\code{\link{WriteFCS_Anonymized}}
}
\examples{
##ToDo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{AnonymizeFACSdata}% use one of  RShowDoc("KEYWORDS")
\keyword{Anonymize}% __ONLY ONE__ keyword per line
\keyword{FCS}% __ONLY ONE__ keyword per line
\keyword{LMD}% __ONLY ONE__ keyword per line
