\name{OneScatter}
\alias{OneScatter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generates a ggobject of one scater plot
}
\description{
Generates a ggobject of one scater plot for either plotting directy or plotting a panel via multiplot
}
\usage{
OneScatter(ClsAndData, Xvar, Yvar, Clsvar = "Cls", zero.rm = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ClsAndData}{
numerical matrix [1:n,1:d+1] with d features and one classification vector as a column of this matrix
}
  \item{Xvar}{
string for column name of first feauture
}
  \item{Yvar}{
string for column name of second feauture
}
  \item{Clsvar}{
otional, string for column name of classification, default "\code{Cls}"
}
  \item{zero.rm}{
TRUE: removes zeros
}
  \item{\dots}{
further parameters for \code{\link[DataVisualizations]{Classplot}}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
ggobject defining a two-dimensional scatter plot
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
michael thrun
}


\seealso{
\code{\link[DataVisualizations]{Classplot}},

\code{\link[dbt.Plot]{multiplot}}
}
\examples{
\dontrun{
DataMZL_T1=ReadScatter(nameMZL_T1,path)
DataMZL_T2=ReadScatter(nameMZL_T2,path)

gg1=OneScatter(DataMZL_T1,"CD5","CD19",Size=1,main="")
gg2=OneScatter(DataMZL_T1,"CD10","CD19",Size=1,main="")
gg3=OneScatter(DataMZL_T1,"CD23","CD20",Size=1,main="")
gg4=OneScatter(DataMZL_T1,"FMC7","CD20",Size=1,main="")
gg5=OneScatter(DataMZL_T2,"Kappa","Lambda",Size=1,main="")
gg6=OneScatter(DataMZL_T1,"CD79b","IgM",Size=1,main="")
#panel visualization
dbt.Plot::multiplot(gg1,gg2,gg3,gg4,gg5,gg6,cols = 3)#+ggtitle("MZL 12920")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
