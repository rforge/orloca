\name{plot.zsum}
\alias{plot.zsum}
\alias{contour.loca.p}
\alias{contour,loca.p-method}
\alias{persp.loca.p}
\alias{persp,loca.p-method}

\title{Plots of the min-sum objective function}
\description{
  \code{contour} and \code{persp} provide two graphical representations of min-sum function (\code{zsum}).
}
\usage{
\method{contour}{loca.p}(x, lp=numeric(0), xmin=min(x@x), xmax=max(x@x),
   ymin=min(x@y), ymax=max(x@y), n=100, \ldots)
\method{persp}{loca.p}(x, lp=numeric(0), xmin=min(x@x), xmax=max(x@x),
   ymin=min(x@y), ymax=max(x@y), n=100, \ldots)
}
\arguments{
  \item{x}{The loca.p object to compute the objective.}
  \item{lp}{If given, then \eqn{l_p} norm will be used instead of the Euclidean norm.}
  \item{xmin}{The minimum value for x axis.}
  \item{xmax}{The maximum value for x axis.}
  \item{ymin}{The minimum value for y axis.}
  \item{ymax}{The maximum value for y axis.}
  \item{n}{The number of divisions for grid.}
  \item{\ldots}{Other options.}
}
\value{
  \code{contour.loca.p} plots a contour like graphics and \code{persp.loca.p} a 3D plot.
}
\details{
If \eqn{p<1} then \eqn{l_p} ara not a norm, so only \eqn{p>=1} are valid values.
}

\examples{
# A new unweighted loca.p object
loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# The contour plot of min-sum function for loca (a loca.p object) 
contour(loca)

# The 3D graphics
persp(loca)
}
\keyword{classes}
\keyword{hplot}
\seealso{See also \code{\link{orloca-package}}, \code{\link{plot.loca.p}} and \code{\link{loca.p}}.}
