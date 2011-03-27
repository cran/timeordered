\name{randomizeidentities}
\alias{randomizeidentities}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Resamples data based on vertex identity.
}
\description{
Produces a new event list from an existing event list with resampled vertex identities given certain constraints on randomization. Effectively re-orders pairs of From/To vertices between different times.
}
\usage{
randomizeidentities(raw, withinvertexfrom, byvertexfrom, withreplacement)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{raw}{
A raw event list to be resampled. Contains four columns: VertexFrom, VertexTo, TimeStart, TimeStop
}
  \item{withinvertexfrom}{
If true, resamples within data subsets where VertexFrom is fixed; otherwise resamples within all data.
}
  \item{byvertexfrom}{
If true, subsets of data for withinvertexfrom are obtained using VertexFrom; if false, using VertexTo.
}
  \item{withreplacement}{
Samples with or without replacement.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An event list of the same size or smaller as raw. The returned event list will be smaller only if resampling produces events that connect a vertex to itself; these are removed.
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
\code{\link{randomizetimes}},\code{\link{rarefy}}
}
\examples{
data(ants)
allindivs <- c(union(ants$VertexFrom, ants$VertexTo), "NULL1", "NULL2")
ri <- randomizeidentities(ants,withinvertexfrom=TRUE,byvertexfrom=TRUE,withreplacement=TRUE)
g <- generatetonetwork(ri, allindivs)
plottonet(g)

## The function is currently defined as
function(raw, withinvertexfrom, byvertexfrom, withreplacement=TRUE)
{
	if (byvertexfrom)
	{
		if (withinvertexfrom)
		{
			list<-by(raw,raw$VertexFrom, function(x) { neworder <- sample(nrow(x), replace= withreplacement); x$VertexTo <- x$VertexTo[neworder]; return(x)})
			outdf <- do.call(rbind.fill, list)	
		}
		else
		{
			neworder <- sample(nrow(raw),replace=withreplacement)
			outdf <- raw
			outdf$VertexTo <- outdf$VertexTo[neworder]
		}
	}
	else
	{
		if (withinvertexfrom)
		{
			list<-by(raw,raw$VertexTo, function(x) { neworder <- sample(nrow(x), replace= withreplacement); x$VertexFrom <- x$VertexFrom[neworder]; return(x)})
			outdf <- do.call(rbind.fill, list)	
		}
		else
		{
			neworder <- sample(nrow(raw), replace= withreplacement)
			outdf <- raw
			outdf$VertexFrom <- outdf$VertexFrom[neworder]
		}		
	}
	
	# remove loops
	outdf$VertexFrom <- as.character(outdf$VertexFrom)
	outdf$VertexTo <- as.character(outdf$VertexTo)

	return(subset(outdf,outdf$VertexFrom!=outdf$VertexTo))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line