\name{sge.split}
\alias{sge.split}
\title{Splits an object and returns a list containing those segments}
\description{
This functions splits an object into a number of pieces as specified by ncl. 
}
\usage{
sge.split(x, ncl)
}
\arguments{
  \item{x}{Object to be split (only supports matrix, array, data.frame, list, and vector)}
  \item{ncl}{Number of segments that the object should be split into.}
}
\details{
  This method is an internal helper method that is used by parApply to split the objects into segments that are distributed to nodes of an SGE cluster.
  This function relies on the splitIndices method from the snow package to determine how many rows each segment should have.

}
\value{
  List containing all split data segments.
}
 
\author{Dan Bode}

\examples{

}
\keyword{interface}
