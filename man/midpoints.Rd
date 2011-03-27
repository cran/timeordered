\name{midpoints}
\alias{midpoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determines the mean value of each row of a matrix; used as a convenience function for plotting.
}
\description{
-
}
\usage{
midpoints(td)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{td}{
A n x 2 matrix describing a set of start and stop times.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A mean value for each of n rows of td
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
\code{\link{generatetimelags}},\code{\link{generatetimedeltas}} ~~~
}
\examples{
tl100 <- generatetimelags(0,1500,100)
boxplot(t(midpoints(tl100)))

## The function is currently defined as
function(td)
{
	return(apply(td, 1, mean))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
