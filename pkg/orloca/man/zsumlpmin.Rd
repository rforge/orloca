\name{zsumlpmin}
\alias{zsumlpmin}
\alias{zsumlpmin,loca.p-method}

\title{zsumlpmin at orloca package}
\description{\code{zsummin} function with \eqn{l_p} norm. Mainly for
  internal use.}
\usage{
zsumlpmin(o, x=0, y=0, p=2, max.iter=100, eps=1.e-3, 
   verbose=FALSE, algorithm="weiszfeld")
}
\arguments{
  \item{o}{An object of loca.p class.}
  \item{x}{The x coordinate of the starting point.}
  \item{y}{The y coordinate of the starting point.}
  \item{p}{p value for \eqn{l_p} norm.}
  \item{max.iter}{Maximum number of iterations allowed.}
  \item{eps}{The module of the gradient in the stop rule.}
  \item{verbose}{If TRUE, then the function produces detailed output.}
  \item{algorithm}{The method to be use. For this version of the package, the valid values
    are: "gradient" or "g" for a gradient based method, "search" or "s"
    for local search method, and "weiszfeld" or "w" for the weiszfeld
    method. "weiszfeld" is the default value.}
}
\details{
If \eqn{p<1} then \eqn{l_p} is not a norm, so only \eqn{p>=1} are valid values.
}
\value{
  \code{zsumlpmin} returns an array with the coordinates of the solution point.
}
\note{Since \eqn{l_2} norm is the Euclidean norm, when \eqn{p=2}
  \code{zsumlpmin} are equal to \code{zsummin}. But the computations involved are greater for the
first form.
  }
\keyword{classes}
\keyword{optimize}
\seealso{
  See also \code{\link{zsummin}}, \code{\link{orloca-package}}, \code{\link{loca.p}} and \code{\link{zsum}}.
  }