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
\method{contour}{loca.p}(x, xmin=min(x@x), xmax=max(x@x), ymin=min(x@y), ymax=max(x@y), n=100, \ldots)
\method{persp}{loca.p}(x, xmin=min(x@x), xmax=max(x@x), ymin=min(x@y), ymax=max(x@y), n=100, \ldots)
}
\arguments{
  \item{x}{The loca.p object to compute the objective.}
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
\examples{
# An unweighted new loca.p object
loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# The contour plot of loca object
contour(loca)

# The 3D graphics
persp(loca)
}
\keyword{classes}
\keyword{hplot}
\seealso{See also \code{\link{orloca-package}}, \code{\link[orloca]{plot.loca.p}} and \code{\link[orloca]{loca.p}}.}
