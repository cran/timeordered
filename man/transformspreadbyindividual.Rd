\name{transformspreadbyindividual}
\alias{transformspreadbyindividual}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A helper function to assess differences in spreading potential by vertex.
}
\description{
Converts a data frame of spreading samples into a data frame that is grouped by vertex identity.
}
\usage{
transformspreadbyindividual(sa)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sa}{
A data frame returned by \code{spreadanalysis}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame whose columns are the identities of vertices and whose rows are the mean fraction of vertices reached by the seed vertex at each time delay, averaged over all samples beginning at this vertex.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Benjamin Blonder \email{bblonder@email.arizona.edu}.}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{spreadanalysis}}
}
\examples{
data(ants)
allindivs <- c(union(ants$VertexFrom, ants$VertexTo), "NULL1", "NULL2")
g <- generatetonetwork(ants, allindivs)
sa <- spreadanalysis(g, seq(0,1000,by=50), 20)
b <- transformspreadbyindividual(sa)
plot(ts(b),plot.type="single",col=rainbow(ncol(b)),xlab="Time",ylab="Fraction reached")
legend("bottomright",colnames(b),lwd=1,col=rainbow(ncol(b)),bg="white")
## The function is currently defined as
function(sa)
{
	# returns mean by individual at each timepoint
	return(sapply(by(sa, sa$startvertex, function(x) { apply(x[,-1], 2, mean) }), function(x){x}))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line