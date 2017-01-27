\name{loca.p-class}
\docType{class}
\alias{loca.p-class}
\alias{loca.p}
\alias{print.loca.p}
\alias{print,loca.p-method}
\alias{summary-method}
\alias{summary,loca.p-method}
\alias{initialize,loca.p-method}

\title{loca.p class for Operations Research LOCational Analysis}
\description{
  An object of class \code{loca.p} represents a weighted location
  problem with a demand points set. 
 The \code{\link[orloca]{orloca-package}} is mainly devoted to deals with location problems.
}
\section{Generators}{
   The main generator is \code{loca.p(x, y, w = numeric(0), label =
     "")}. An alternative form is \code{new("loca.p", x, y, w = numeric(0), label =
     "")}.
\describe{
  \item{x}{is a vector of the x coordinates of the demand points.}
  \item{y}{is a vector of the y coordinates of the demand points.}
  \item{w}{is a vector of weights of the demand points. If w is
    omitted then all weights are considered as 1.}
  \item{label}{If given, it is the label of the new object.}
}
}
\details{
  The lengths of \code{x} and \code{y} vector must be equals. The length
  of \code{w} must be equal to the previous ones or must be 0. NA's
  values are not allowed at any of the arguments.
}

\value{
  If the arguments have valid values, it returns a new object of class
  \code{loca.p}, else it returns an error.
  \code{summary(x)} returns a summary of the \code{x} \code{loca.p} object and
    \code{print(x)} prints a summary of the \code{x} \code{loca.p} object.
  }

\examples{
# An unweighted new loca.p object
loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
# or
loca <- new("loca.p", x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# An example with weights and name
locb <- new("loca.p", x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1), w = c(1, 2, 1, 2),
   label = "Weighted case")
}
\keyword{classes}
\keyword{optimize}
\seealso{See also \code{\link{orloca-package}}.}
