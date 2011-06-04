\name{generatetimeaggregatednetwork}
\alias{generatetimeaggregatednetwork}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Constructs a weighted time-aggregated network from a time-ordered network by aggregating interactions occurring between a start and stop time.
}
\description{
-
}
\usage{
generatetimeaggregatednetwork(g, starttime, stoptime)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
The time-ordered network to be aggregated
}
  \item{starttime}{
The time at which to begin aggregating interactions.
}
  \item{stoptime}{
The time at which to stop aggregating interactions.
}
}

\value{
A weighted time-aggregated network whose edge weights equal the number of interactions between those vertices in the time window.
}

\author{Benjamin Blonder \email{bblonder@email.arizona.edu}.}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{generatenetworkslices}}
}
\examples{
data(ants)
allindivs <- c(union(ants$VertexFrom, ants$VertexTo), "NULL1", "NULL2")
g <- generatetonetwork(ants, allindivs)
tan500 <- generatetimeaggregatednetwork(g, 0, 500)
plottanet(tan500)

## The function is currently defined as
function(g, starttime, stoptime)
{
	sg <- subgraph(g, V(g)[Time >= starttime & Time < stoptime])

	newedgelist <- data.frame(VertexFrom=E(sg)$VertexFrom,VertexTo=E(sg)$VertexTo,stringsAsFactors=FALSE)
	newedgelist <- newedgelist[newedgelist[,1] != newedgelist[,2],]
	newedgelist$EdgePair <- as.numeric(factor(paste(newedgelist$VertexFrom, 	newedgelist$VertexTo)))
	tabcounts <- tabulate(newedgelist$EdgePair)
	newedgelist$Count <- tabcounts[newedgelist$EdgePair]
	uniqueedgelist <- unique(newedgelist)
	uniqueedgelist <- uniqueedgelist[,c("VertexFrom","VertexTo","Count")]
	allvertices <- as.data.frame(unique(V(g)$Name))
	
	timeaggregatednetwork <- graph.data.frame(d=uniqueedgelist, vertices=allvertices)
	
	return(timeaggregatednetwork)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
