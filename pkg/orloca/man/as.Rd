\name{as-methods}
\docType{methods}
\alias{as-methods}
\alias{as.loca.p}
\alias{as.data.frame}
\alias{as.matrix}
\alias{as,loca.p-method}
\alias{as.loca.p,data.frame-method}
\alias{as.loca.p,matrix-method}
\alias{as.matrix,loca.p-method}
\alias{as.data.frame,loca.p-method}
\alias{as.loca.p.data.frame}
\alias{as.loca.p.matrix}
\alias{as.matrix.loca.p}
\alias{as.data.frame.loca.p}

\title{Conversions between loca.p class and some others}
\description{
  Methods to convert from and to \code{loca.p} class.
}
\usage{
%  \method{as}{loca.p}(from, "data.frame")
%  \method{as}{loca.p}(from, "matrix")
%  \method{as}{data.frame}(from, "loca.p")
%  \method{as}{matrix}(from, "loca.p")
  \method{as.loca.p}{data.frame}(x, ...)
  \method{as.loca.p}{matrix}(x, ...)
  \method{as.data.frame}{loca.p}(x, row.names = NULL, optional = FALSE, ...)
  \method{as.matrix}{loca.p}(x, ...)
}
\arguments{
%  \item{data.frame}{is a data.frame to convert to loca.p.}
%  \item{matrix}{is a matrix to convert to loca.p.}
%  \item{loca.p}{is a loca.p object to convert to\ldots}
  \item{x}{is the object to convert to the new class object.}
  \item{row.names}{Unused.}
  \item{optional}{Unused}
  \item{...}{Other arguments, unused.}
}
\details{
  NA's  values are not allowed in any of the arguments.
  
  The \code{matrix} to convert into \code{loca.p} must have at least two columns. 
  The first column will be consider as the x coordinates, the second as the y
  coordinates, and the third (if available) as the values of w.

  The \code{data.frame} to convert into \code{loca.p} must have at least
  an \code{x} column for x coordinates, and an \code{y} column for y
  coordinates. Optionally, it can have \code{w} column, as the values of w.
}

\value{
  If the arguments have valid values, it returns a new object of new class.
  }

\examples{
# An unweighted new loca.p object
loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# Conversion to matrix
m <- as.matrix(loca)
m

# Conversion from matrix
as.loca.p(m)
}
\keyword{classes}
\keyword{methods}
\seealso{See also \code{\link[orloca]{loca.p}}.}
