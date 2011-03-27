\name{shortesthoppath}
\alias{shortesthoppath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determines a path (shortest by the least number of unique vertices) between two vertices at two times.
}
\description{
-
}
\usage{
shortesthoppath(g, startvertexname, startvertextime, stopvertexname, stopvertextime)
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
  \item{stopvertextime}{
The time of the stop vertex. Must be a time at which an interaction has occurred involving this vertex.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vertex list containing all the events on the shortest-hop path between the start and stop vertices/times.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Benjamin Blonder \email{bblonder@email.arizona.edu}.}
\note{
Multiple shortest-hop paths may exist; returns only one of them.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{shortesttimepath}}
}
\examples{
data(ants)
allindivs <- c(union(ants$VertexFrom, ants$VertexTo), "NULL1", "NULL2")
g <- generatetonetwork(ants, allindivs)
shp <- shortesthoppath(g, "WBGG", 927, "GYGG", 1423)
plottonet(g, shp)
title(paste(length(unique(shp$Name))," hops"))


## The function is currently defined as
function(g, startvertexname, startvertextime, stopvertexname, stopvertextime)
{
	if (length(startvertexname) != 1 | length(stopvertexname) != 1)
	{
		stop("must provide single startvertex and stop vertex")	
	}
	startvertex <- V(g)[Name==startvertexname & Time==startvertextime]
	stopvertex <- V(g)[Name==stopvertexname & Time==stopvertextime]
	vertices <- get.shortest.paths(g, startvertex, stopvertex, mode="out", weights=E(g)$HopCost)
	
	return(V(g)[vertices[[1]] ])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
