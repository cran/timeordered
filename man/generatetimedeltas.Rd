\name{generatetimedeltas}
\alias{generatetimedeltas}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Constructs matrix of sequential time windows suitable for slicing time ordered networks
}
\description{
-
}
\usage{
generatetimedeltas(starttime, stoptime, delta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{starttime}{
The starting time of the first time window.
}
  \item{stoptime}{
The stopping time of the last time window.
}
  \item{delta}{
The size of each time window.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A n x 2 matrix. Each row contains the start and stop time of a window with width delta.
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
\code{\link{generatetimelags}} ~~~
}
\examples{
td100 <- generatetimedeltas(0,1500,100)
boxplot(t(td100))

## The function is currently defined as
function(starttime, stoptime, delta)
{
	ts <- seq(starttime,stoptime,by=delta)
	return(cbind(head(ts,-1),tail(ts,-1)))	
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
