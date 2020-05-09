\name{AnonymizeFACSdata}
\alias{AnonymizeFACSdata}

\title{
Anonymize FACS data
}
\description{
An interactive tool for the anonymization of FACS data (either *.fcs or *.lmd)
}
\usage{
AnonymizeFACSdata()
}

\details{
If the *.lmd format is chosen, the Marburg style is assumed. This means the spillover matrix and the data are in the second dataset but the names of the parameters are in the first dataset. Otherwise please use \code{\link{ReadFCS_FlowCompensated}} and \code{\link{WriteFCS_Anonymized}} manually.
}
\value{
a *.fcs file is written out.
}
\references{
\url{http://www.deepbionics.org}
}
\author{
Michael Thrun
}

\note{
Due to security reasons, the output file path cannot be the same as the input file path when using standard shiny. However, in a future version shinyFiles could be used so that the choice of the output file path is automatically the same that input file path.
}

\seealso{
\code{\link{ReadFCS_FlowCompensated}},\code{\link{WriteFCS_Anonymized}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{AnonymizeFACSdata}% use one of  RShowDoc("KEYWORDS")
\keyword{Anonymize}% __ONLY ONE__ keyword per line
\keyword{FCS}% __ONLY ONE__ keyword per line
\keyword{LMD}% __ONLY ONE__ keyword per line
