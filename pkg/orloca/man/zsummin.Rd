\name{zsummin}
\alias{zsummin}
\alias{zsummin,loca.p-method}

\title{zsummin at orloca package}
\description{
  Solve the min-sum location problem for a given \code{loca.p} class
  object.
}
\usage{
zsummin(o, x=0, y=0, lp=numeric(0), max.iter=100, eps=1.e-3, 
   verbose=FALSE, algorithm="weiszfeld", \ldots)
}
\arguments{
  \item{o}{An object of loca.p class.}
  \item{x}{The x coordinate of the starting point.}
  \item{y}{The y coordinate of the starting point.}
  \item{lp}{If given, the \eqn{l_p} norm will be used instead of the
    Euclidean norm.}
  \item{max.iter}{Maximum number of iterations allowed.}
  \item{eps}{The module of the gradient in the stop rule.}
  \item{verbose}{If TRUE the function produces detailed output.}
  \item{algorithm}{The algorithm to be use. For this version of the package, the valid values
    are: "gradient" or "g" for a gradient based method, "search" or "s"
    for local search method, "ucminf" or "u" for optimization with
    ucminf from ucminf package, and "weiszfeld" or "w" for the Weiszfeld
    method or any of the valid method for optim function, now
    "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN". "weiszfeld" is the
    default value.}
  \item{\ldots}{Other options for optimization algorithms.}
}
\details{If \eqn{p < 1} thus \eqn{l_p} is not a norm, so, only \eqn{p
    \ge 1} are valid values.
  }
\value{
  \code{zsummin} returns an array with the coordinates of the solution point.
}
\examples{
# A new unweighted loca.p object
loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# Compute the minimum
sol<-zsummin(loca)

# Show the result
sol

# Evaluation of the objective function at solution point
zsum(loca, sol[1], sol[2])
}
\keyword{classes}
\keyword{optimize}
\seealso{
  See also \code{\link{orloca-package}}, \code{\link{loca.p}} and \code{\link{zsum}}.
  }