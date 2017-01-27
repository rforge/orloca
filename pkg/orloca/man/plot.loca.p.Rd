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
\method{plot}{loca.p}(x, xlab="", ylab="", main=gettext("Plot of loca.p object"),
   img=NULL, xlim=c(min(xleft, min(x@x)), max(xright, max(x@x))),
   ylim=c(min(ybottom, min(x@y)), max(ytop, max(x@y))),
   xleft=min(x@x), ybottom=min(x@y), xright=max(x@x),
   ytop=max(x@y), \ldots)
}
\arguments{
  \item{x}{The \code{loca.p} object to plot.}
  \item{xlab}{The label for x axis.}
  \item{ylab}{The label for y axis.}
  \item{main}{The main title for the plot.}
  \item{img}{A raster image to plot on background.}
  \item{xlim}{Limit over the x axes of the plot.}
  \item{ylim}{Limit over the y axes of the plot.}
  \item{xleft}{The left position of the image.}
  \item{ybottom}{The bottom position of the image.}
  \item{xright}{The right position of the image.}
  \item{ytop}{The top position of the image.}
  \item{\ldots}{Other graphical options.}
}
\details{
  The function plots the demand points with automatic limits evaluation.
}
\value{
  The function plots the required graphics.
}
\examples{
# A new unweighted loca.p object
loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# The plot of loca object
plot(loca)
}
\keyword{classes}
\keyword{hplot}
\seealso{See also \code{\link{orloca-package}},
  \code{\link{loca.p}} and \code{\link{plot.zsum}}.}
