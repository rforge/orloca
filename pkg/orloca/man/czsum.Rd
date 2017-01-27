\name{czsum}
\alias{czsum}
\alias{czsum,loca.p-method}
\alias{czsumgra}
\alias{czsumgra,loca.p-method}

\title{czsum and czsumgra at orloca package}
\description{
  The objective function and the gradient function for the min-sum location problem.
}
\usage{
czsum(o, cluster, x=0, y=0)
czsumgra(o, cluster, x=0, y=0)
}
\arguments{
  \item{o}{An object of \code{loca.p} class.}
  \item{cluster}{An snow cluster object.}
  \item{x}{The x coordinate of the point to be evaluated.}
  \item{y}{The y coordinate of the point to be evaluated.}
}
\value{
  \code{czsum} returns the objective function of the mini-sum location
  problem, \eqn{\sum_{a_i \in o} w_i d(a_i, (x,y))}, where \eqn{d(a_i, (x,y))} gives the
  euclidean distances between \eqn{a_i} and \eqn{(x,y)}. 

  \code{czsumgra} returns the gradient vector of the function \code{zsum}.

  The computations are made at the given cluster.
}
%\examples{
%# An unweighted new loca.p object
%loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
%
%# Evaluation of zsum at (0, 0)
%zsum(loca)
%
%# Evaluation of zsum at (1, 3)
%zsum(loca, 1, 3)
%
%# The gradient function
%zsumgra(loca, 1, 3)
%}
\keyword{classes}
\keyword{optimize}
\seealso{
  See \url{http://www.stat.uiowa.edu/~luke/R/cluster/cluster.html} for information about computers clusters.

  See also \code{\link[orloca]{zsum}}, \code{\link[orloca]{zsumgra}} and \code{\link[orloca]{zsummin}}
  }