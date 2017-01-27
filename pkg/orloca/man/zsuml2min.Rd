\name{zsuml2min}
\alias{zsuml2min}
\alias{zsuml2min,loca.p-method}

\title{zsuml2min at orloca package}
\description{\code{zsummin} function for the Euclidean norm
  (\eqn{l_2}). Mainly for internal use.}
\usage{
zsuml2min(o, x=0, y=0, max.iter=100, eps=1.e-3, verbose=FALSE,
   algorithm="weiszfeld")
}
\arguments{
  \item{o}{An object of loca.p class.}
  \item{x}{The x coordinate of the starting point.}
  \item{y}{The y coordinate of the starting point.}
  \item{max.iter}{Maximum number of iterations allowed.}
  \item{eps}{The module of the gradient in the stop rule.}
  \item{verbose}{If TRUE the function produces detailed output.}
  \item{algorithm}{The method to be use. For this version of the package, the valid values
    are: "gradient" or "g" for a gradient based method, "search" or "s"
    for local search method, and "weiszfeld" or "w" for the weiszfeld
    method. "weiszfeld" is the default value.}
}
\value{
  \code{zsuml2min} returns an array with the coordinates of the solution point.
}
\keyword{classes}
\keyword{optimize}
\seealso{
  See also \code{\link{orloca-package}}, \code{\link{zsummin}}, \code{\link{loca.p}} and \code{\link{zsum}}.
  }