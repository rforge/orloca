\name{zsum}
\alias{zsum}
\alias{zsum,loca.p-method}
\alias{zsumgra}
\alias{zsumgra,loca.p-method}

\title{zsum and zsumgra at orloca package}
\description{
  The objective function and the gradient function for the mini-sum
  objective location problem are given.
}
\usage{
zsum(o, x=0, y=0)
zsumgra(o, x=0, y=0)
}
\arguments{
  \item{o}{An object of \code{loca.p} class.}
  \item{x}{The x coordinate of the point to be evaluated.}
  \item{y}{The y coordinate of the point to be evaluated.}
}
\value{
  \code{zsum} returns the objective function of the mini-sum location
  problem, \eqn{\sum_{a_i \in o} w_i d(a_i, (x,y))}, where \eqn{d(a_i, (x,y))} gives the
  euclidean distances between \eqn{a_i} and \eqn{(x,y)}. 

  \code{zsumgra} returns the gradient vector of the function \code{zsum}.
}
\examples{
# An unweigthed new loca.p object
loca <- new("loca.p", x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# Evaluation of zsum at (0, 0)
zsum(loca)

# Evaluation of zsum at (1, 3)
zsum(loca, 1, 3)

# The gradient function
zsumgra(loca, 1, 3)
}
\keyword{classes}
\keyword{optimize}
\seealso{
  See also \code{\link{orloca-package}} and \code{\link[orloca]{zsummin}}.
  }