\name{ReadParquet}
\alias{ReadParquet}
\title{
ReadParquet
}
\description{
Reads Parquet files.
}
\usage{
ReadParquet(FileName, FilePath = "")
}
\arguments{
\item{FileName}{
Character Sting with name of file.
}
\item{FilePath}{
Character Sting with name of directory.
}
}
\value{
List with
\item{ListData}{List with numerical matrix elements}
\item{NumberListElements}{Integer with number of list elements}
\item{FeatureDim}{Integer with dimension of all matrix elements}
}
\author{
QS
}
\examples{

}
\keyword{parquet}
\keyword{DataIO}
