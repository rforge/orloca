\name{rloca.p}
\alias{rloca.p}

\title{rloca.p random instances generator of loca.p class object}
\description{
  It returns a random instance of loca.p class object in a given
  rectangular region.
}
\usage{
rloca.p(n, xmin=0, xmax=1, ymin=0, ymax=1)
}
\arguments{
  \item{n}{The number of demand points.}
  \item{xmin}{Minimun value for the x coordinates of the demand points.}
  \item{xmax}{Maximun value for the x coordinates of the demand points.}
  \item{ymin}{Minimun value for the y coordinates of the demand points.}
  \item{ymax}{Maximun value for the y coordinates of the demand points.}
}
\details{
  \code{n} must be at least 1.

  \code{xmin} must be less or equal than \code{xmax}.

  \code{ymin} must be less or equal than \code{ymax}.
}
\value{
  If the arguments are valid values, it returns a new object of class
  \code{loca_p}, else it returns an error.
}
\examples{
# A random loca.p object at unit square with 5 demand points
rloca.p(5)

# At another region
rloca.p(10, xmin=-2, xmax=2, ymin=-2, ymax=2)
}
\keyword{datagen}
\seealso{See also \code{\link{orloca-package}} and \code{loca.p}}
