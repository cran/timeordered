\name{generatelatencies}
\alias{generatelatencies}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generates vector-clock latencies for each individual at each time.
}
\description{
Vector clock latencies describe the minimum time delay between one individual broadcasting a signal and another individual receiving it, at a given time, through any causally permitted path in the time-ordered network. Smaller values indicate individuals that are connected by shorter causally-permitted paths at a given time.
}
\usage{
generatelatencies(raw, allindivs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{raw}{
An event list, consisting of a data frame with four columns: VertexFrom, VertexTo, TimeStart, and TimeStop. Each row in this data frame represents a single directed interaction event between VertexFrom and VertexTo beginning at TimeStart and ending at TimeStop. Assumes that no event begins at a time less than zero.
}
  \item{allindivs}{
A list of all possible vertices including ones not observed interacting during the range of time reported in \code{raw}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A n x n x m array, where n is the number of vertices and m is the maximum start time in the raw event list. The [i,j,k] entry of the array describes the latency from i to j at time k. NA is returned if there is not causally permitted path between i and j by time k. 
}
\references{
Kossinets et al. The structure of information pathways in a social communication network. KDD '08: Proceeding of the 14th ACM SIGKDD international conference on Knowledge discovery and data mining (2008)
}
\author{Benjamin Blonder \email{bblonder@email.arizona.edu}.}
\note{
Return value can require large memory allocation depending on the data set. Ensure that data contains no times < 0 before running.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}
}
\examples{
data(ants)
allindivs <- c(union(ants$VertexFrom, ants$VertexTo), "NULL1", "NULL2")
l <- generatelatencies(ants, allindivs)
image(l[,,1000],axes=FALSE,frame=TRUE,col=rainbow(100))
axis(1, at = (1:ncol(l))/ncol(l), labels=colnames(l),tick=FALSE,las=2,cex.axis=0.2)
axis(2, at = (1:nrow(l))/nrow(l), labels=rownames(l),tick=FALSE,las=2,cex.axis=0.2)

## The function is currently defined as
function(raw, allindivs=union(raw$VertexFrom, raw$VertexTo))
{
	maxTime <- max(c(raw$TimeStart,raw$TimeStop))	
	
	vectorClocks = array(-10*maxTime,dim=c(length(allindivs),length(allindivs),maxTime+1),dimnames=list(allindivs,allindivs,0:maxTime))

	timeOrderedEvents <- data.frame(Actor=raw$VertexFrom, Target=raw$VertexTo, Time=raw$TimeStart)
	timeOrderedEvents <- timeOrderedEvents[order(timeOrderedEvents$Time),]

	for (t in 1:(maxTime+1))
	{
		if (t > 1)
		{
			vectorClocks[,,t] <- vectorClocks[,,t-1]
		}
	
		# do the diagonal
		for (i in 1:length(allNames))
		{
			vectorClocks[i,i,t] <- (t-1)	# t=0 at index one
		}
	
		theseEvents <- timeOrderedEvents[timeOrderedEvents$Time == (t-1),]
		if (nrow(theseEvents) > 0)
		{
			for (i in 1:nrow(theseEvents))
			{
				# put in the observed event
				thisEvent <- theseEvents[i,]
				actorID <- as.vector(thisEvent$Actor)
				targetID <- as.vector(thisEvent$Target)
			
				# put in the interaction
				vectorClocks[actorID, targetID, t] <- (t-1)
				# update the clocks
				actorClock <- vectorClocks[actorID, , t]
				targetClock <- vectorClocks[targetID, , t]
				vectorClocks[actorID, , t] <- pmax(actorClock, targetClock)
			

				# the reciprocal interaction
				vectorClocks[targetID, actorID, t] <- (t-1)
				actorClock <- vectorClocks[actorID, , t]
				targetClock <- vectorClocks[targetID, , t]
				vectorClocks[targetID, , t] <- pmax(actorClock, targetClock)
			
			}
		}
	}
	
	vectorClocks[which(vectorClocks < 0)] <- NA
	latencies <- vectorClocks
	for (t in 0:maxTime)
	{
		latencies[,,(t+1)] <- t - vectorClocks[,,(t+1)]
	}
	
	return(latencies)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
