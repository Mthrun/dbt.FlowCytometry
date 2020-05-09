\name{ReadFCS_FlowCompensated}
\alias{ReadFCS_FlowCompensated}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ReadFCS_FlowCompensated reads *.fcs or *.lmd file
}
\description{
This method automatically selects the appropriate function the read a file, according to its extension.  It can read many FCS files on a lmd at once.
}
\usage{
ReadFCS_FlowCompensated(Filename, InDirectory = getwd(),
Extension, shiny=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Filename}{
Name of *.fcs file or *.lmd file
}
  \item{InDirectory}{
Optional, directory where *.fcs or lmd files are located (default ==  getwd() )
}
  \item{Extension}{
Optional, can be set by the user to either *.fcs or *.lmd. By default, automatic detection is applied.
}
  \item{shiny}{
Optional, shiny =TRUE disabled several checks that are not useful in the interactive application.
}
}
\details{
"FlowCompensated" is an internal hint that automatic compensation is disabled in this data io function, because it varies depending on the files and the main goal of this function was to provide a method for data anonymization.
For reading files with automatic compansation, please see the aultsch/DataIO package on github.

Requires flowCore package to be installed to work
}
\value{
 if the input is a *.fcs file, then a LIST containing the following items is returned:
\item{Data}{[1:n,1:d] data matrix of n cases and d features}
\item{LaserDescriptions}{1:n character vector of laser descriptions}
\item{Header}{1:n character vector consisting of names of biomarkers/parameters}
\item{Comments}{all accessible information}
\item{Spillover}{[1:d,1:d]  spillovermatrix used for compensation}
\item{AnnotatedDataFrame}{see \pkg{Biobase} AnnotatedDataFrame}
\item{RawData}{object of class flowFrame, see \code{\link[flowCore]{read.FCS}} in \pkg{flowCore}}
\item{Extension}{used in shiny application otherwise irrelevant}

if *.lmd then a list element per stored file with the content above is generated
}
\references{
the manual
}
\author{
Michael Thrun
}

\seealso{
\code{\link{ReadFCS_FlowCompensated}},\code{\link[flowCore]{read.FCS}}
}
\examples{
}

\note{
In case of one *.fcs, the compensation function of flowCore package can be used which was certified on the dresden files. However I dont know if it works for non lmd marburg files). Compensation with spillover in a lmd file is usuallynot performed with the flowCore package..
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ReadFCS_FlowCompensated}% use one of  RShowDoc("KEYWORDS")
\keyword{FCS}% __ONLY ONE__ keyword per line
\keyword{LMD}% __ONLY ONE__ keyword per line
\keyword{read}% __ONLY ONE__ keyword per line
