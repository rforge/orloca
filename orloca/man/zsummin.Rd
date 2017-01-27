\name{zsummin}
\alias{zsummin}
\alias{zsummin,loca.p-method}

\title{zsummin at orloca package}
\description{
  Solve the min-sum location problem for a given loca.p class object.
}
\usage{
zsummin(o, x=0, y=0, n=100, eps=1.e-3, verbose=FALSE, algorithm="weiszfeld")
}
\arguments{
  \item{o}{An object of loca.p class.}
  \item{x}{The x coordinate of the starting point.}
  \item{y}{The y coordinate of the starting point.}
  \item{n}{Maximun number of iterations allowed.}
  \item{eps}{The norm of the gradient in the stop rule.}
  \item{verbose}{If TRUE the function produces detailed output.}
  \item{algorithm}{The method to be use. At the moment, the valid values
    are: "gradient" or "g" for a gradient based method, "search" or "s"
    for local search method, and "weiszfeld" or "w" for the weiszfeld
    method. "weiszfeld" is the default value.}
}
\value{
  \code{zsummin} returns an array with the coordinates of the solution point.
}
\examples{
# An unweigthed new loca.p object
loca <- new("loca.p", x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# Compute the minimun
sol<-zsummin(loca)

# Show the results
sol

# Evaluation of the objective function at solution point
zsum(loca, sol[1], sol[2])
}
\keyword{classes}
\keyword{optimize}
\seealso{
  See also \code{\link{orloca-package}}, \code{\link[orloca]{loca.p}} and \code{\link[orloca]{zsum}}.
  }