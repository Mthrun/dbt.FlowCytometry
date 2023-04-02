\name{ClassifyByEvent_SVM}
\alias{ClassifyByEvent_SVM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ClassifyByEvent_SVM
}
\description{
Performs Eventbased SVM in order to classify datasets by majority
}
\usage{
ClassifyByEvent_SVM(DataListTrain, Cls, DataListTest, type = "C-classification", probability = T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataListTrain}{
[1:p] list of datasets, each dataset has exactly [1:n,1:d] cases and features
}
  \item{Cls}{
[1:p] numerical vector defining the class for each dataset
}
  \item{DataListTest}{
[1:l] list of datasets, each dataset has exactly [1:n,1:d] cases and features
}
  \item{type}{
svm type
}
  \item{probability}{
true or false setting for svm
}
  \item{\dots}{
further parameters for svm model
}
}
\details{
...
}
\value{
List with
\item{ClsTest}{[1:l] probability of a dataset belonging to a class}
\item{Model}{vent-based SVM model}
\item{ClsTrain}{[1:p] numerical vector defining the probability for each dataset beloning to a class}
}
\references{
to be published
}
\author{
Michael Thrun
}
\examples{
#ToDo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ClassifyByEvent_SVM}% use one of  RShowDoc("KEYWORDS")
\keyword{svm}% __ONLY ONE__ keyword per line
