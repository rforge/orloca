\name{czsummin}
\alias{czsummin}
\alias{czsummin,loca.p-method}

\title{czsummin at orloca package}
\description{\code{czsummin} is the cluster version of the function \code{zsummin}.}
\usage{
czsummin(o, cluster, x=0, y=0, max.iter=100, eps=1.e-3, 
   verbose=FALSE, algorithm="gradient")
}
\arguments{
  \item{o}{An object of loca.p class.}
  \item{cluster}{An snow cluster object.}
  \item{x}{The x coordinate of the starting point.}
  \item{y}{The y coordinate of the starting point.}
  \item{max.iter}{Maximum number of iterations allowed.}
  \item{eps}{The module of the gradient in the stop rule.}
  \item{verbose}{If TRUE then the function produces detailed output.}
  \item{algorithm}{The algorithm to be use. For this version of the package, the valid values
    are: "gradient" or "g" for a gradient based algorithm, and "search" or "s"
    for local search method. "gradient" is the default value.}
}
\value{
  \code{czsummin} returns an array with the coordinates of the solution point.

  These computation are made at the given snow cluster.
}
%\examples{
%# A new unweighted loca.p object
%loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
%
%# Compute the minimum
%sol<-zsummin(loca)
%
%# Show the results
%sol
%
%# Evaluation of the objective function at solution point
%zsum(loca, sol[1], sol[2])
%}
\keyword{classes}
\keyword{optimize}
\seealso{
  See \url{http://www.stat.uiowa.edu/~luke/R/cluster/cluster.html} for information about computers clusters.

  See also \code{\link{zsummin}}, \code{\link{loca.p}} and \code{\link{zsum}}.
  }