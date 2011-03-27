\name{shortesttimepath}
\alias{shortesttimepath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determines a path (shortest by the least time) between a vertex at a start time and another vertex at any later time.

}
\description{
-
}
\usage{
shortesttimepath(g, startvertexname, startvertextime, stopvertexname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
The time-ordered network on which to find paths.
}
  \item{startvertexname}{
The name of the start vertex.
}
  \item{startvertextime}{
The time of the start vertex. Must be a time at which an interaction has occurred involving this vertex.
}
  \item{stopvertexname}{
The name of the stop vertex.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vertex list containing all the events on the shortest-time path between the start vertex at the start time and the stop vertex at a later time.

}
\references{
%% ~put references to the literature/web site here ~
}
\author{Benjamin Blonder \email{bblonder@email.arizona.edu}.}
\note{
May generate warning messages - don't worry!
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{shortesthoppath}}
}
\examples{
data(ants)
allindivs <- c(union(ants$VertexFrom, ants$VertexTo), "NULL1", "NULL2")
g <- generatetonetwork(ants, allindivs)
stp <- shortesttimepath(g, "WBGG", 927, "Q")
plottonet(g, stp)
title(paste(diff(range(stp$Time)), "time elapsed"))

## The function is currently defined as
function(g, startvertexname, startvertextime, stopvertexname)
{
	if (length(startvertexname) != 1 | length(stopvertexname) != 1)
	{
		stop("must provide single startvertex and stop vertex")	
	}
	startvertex <- V(g)[Name==startvertexname & Time==startvertextime]
	stopvertices <- V(g)[Name==stopvertexname & Time>=startvertextime]	
	paths <- get.shortest.paths(g, startvertex, stopvertices, mode="out", weights=E(g)$TimeCost)
	
	shortestpath  <- paths[[which.min(lapply(paths, function(x) { tail(V(g)[x]$Time,1) })) ]]
	return(V(g)[shortestpath])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
