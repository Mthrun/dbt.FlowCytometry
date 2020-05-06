\name{ReadFCS_FlowCompensated}
\alias{ReadFCS_FlowCompensated}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ReadFCS_FlowCompensated reads *.fcs or *.lmd file
}
\description{
Uses several read in function depending on extension and can read many fcs files on a lmd at once.

}
\usage{
ReadFCS_FlowCompensated(Filename, InDirectory = getwd(),

Extension, shiny=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Filename}{
name of *.fcs file
}
  \item{InDirectory}{
Optional, directory where *.fcs or lmd file is  (default ==  getwd() )
}
  \item{Extension}{
Optional, can be set by the user to either *.fcs or *.lmd otherwise autoatic detection
}
  \item{shiny}{
Optional, shiny =TRUE disabled several checks not useful in interaxtive application
}
}
\details{
uses in case of one *.fcs compensation function of flowsom package which was certified on the dresden files (but I dont know if it works for non lmd marburg files)

Requires flowCore package to be installed to work
}
\value{
 if *.fcs then is a LIST of
\item{Data}{nXd data matrix of n cases and d features}
\item{LaserDescriptions}{n laser descriptions}
\item{Header}{n names of biomarkes/parameters}
\item{Comments}{all accessible information}
\item{Spillover}{spillovermatrix dxd used of kompensation}
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
##ToDo
}

\note{
Kompensation with spillover in a lmd file is an opnen topic
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ReadFCS_FlowCompensated}% use one of  RShowDoc("KEYWORDS")
\keyword{FCS}% __ONLY ONE__ keyword per line
\keyword{LMD}% __ONLY ONE__ keyword per line
\keyword{read}% __ONLY ONE__ keyword per line
