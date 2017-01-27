\name{plot-methods}
\docType{methods}
\alias{plot-methods}
\alias{plot,loca.p-method}
\alias{plot.loca.p}


\title{plot of loca.p class objects}
\description{
  This method provides a graphical representations of an object of class \code{loca.p}.
}
\usage{
\method{plot}{loca.p}(x, xlab="", ylab="", main="Plot of loca.p object", \ldots)
}
\arguments{
  \item{x}{The loca.p object to plot.}
  \item{xlab}{The label for x axis.}
  \item{ylab}{The label for y axis.}
  \item{main}{The main title for the plot.}
  \item{...}{Other graphical options.}
}
\details{
  The function plots the demand points with automatic limits evaluation.
}
\value{
  The function plot the graphics.
}
\examples{
# An unweigthed new loca.p object
loca <- new("loca.p", x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# The plot of loca object
plot(loca)
}
\keyword{classes}
\keyword{hplot}
\seealso{See also \code{\link{orloca-package}},
  \code{\link[orloca]{loca.p}} and \code{\link[orloca]{plot.zsum}}.}
