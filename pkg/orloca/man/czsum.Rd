\name{czsum}
\alias{czsum}
\alias{czsum,loca.p-method}
\alias{czsumgra}
\alias{czsumgra,loca.p-method}

\title{czsum and czsumgra at orloca package}
\description{
  zsum and zsumgra functions computed at the given snow cluster.
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
  \code{czsum} returns the objective function of the min-sum location
  problem, \eqn{\sum_{a_i \in o} w_i d(a_i, (x,y))}, where \eqn{d(a_i, (x,y))} gives the
  euclidean distances between \eqn{a_i} and the point \eqn{(x,y)}. 

  \code{czsumgra} returns the gradient vector of the function \code{zsum}.

  The computations are made at the given snow cluster.
}
%\examples{
%# A new unweighted loca.p object
%loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
%
%# Evaluation of zsum at (0, 0)
%zsum(loca)
%
%# Evaluation of zsum at (1, 3)
%zsum(loca, 1, 3)
%
%# The gradient function at (1,3)
%zsumgra(loca, 1, 3)
%}
\keyword{classes}
\keyword{optimize}
\seealso{
  See \url{http://www.stat.uiowa.edu/~luke/R/cluster/cluster.html} for information about computers clusters.

  See also \code{\link{zsum}}, \code{\link{zsumgra}} and \code{\link{zsummin}}.
  }