\name{orloca-package}
\alias{orloca-package}
\alias{orloca}
\docType{package}
\encoding{UTF-8}
\title{
  The package deals with Operations Research LOCational Analysis models}
\description{
  This version of the package deals with the min-sum location problem, also known as
  Fermat--Weber problem. 
 The min-sum location problem look for a point such that the weighted sum of the distances to the demand points are minimized.
}
\details{
\preformatted{
Package:   orloca
Type:      Package
Version:   4.0
Date:      2012-05-08
License:   GPL (>= 2)
}
The packages provides a class (\code{loca.p}) that represents a location
problem with a finite set of demand points over the plane. Also, it is possible to plot
the points and the objective function. Such objective function is the
total weighted distances travel by all the customers to the service.

For a demo, load the package with \code{library(orloca)}, and use \code{demo(orloca)}.

The package is ready for internationalization. The authors ask for translated version of the .mo file to include in the package.
}
\author{
Fernando Fernandez-Palacin <fernando.fernandez@uca.es> and Manuel
 Munoz-Marquez <manuel.munoz@uca.es>
 
Maintainer: Manuel Munoz-Marquez <manuel.munoz@uca.es>
}
\references{
[1] Love, R. F., Morris, J. G., Wesolowsky, G. O. \emph{Facilities Location: Chapter 2: Introduction to Single-Facility Location}, 1988, North-Holland

[2] \url{http://knuth.uca.es/orloca}
}
\keyword{package}
\keyword{optimize}

\section{Index:}{
  \tabular{ll}{
\code{\link{loca.p}}: \tab loca.p class description.\cr
\code{\link{rloca.p}}: \tab generates random instances of loca.p class objects.\cr
\code{\link{zsum}}: \tab computes the objective function
  for min-sum location models.\cr
\code{\link{zsummin}}: \tab looks for the minimum for the optimization
problem.\cr
\code{\link{plot.loca.p}}: \tab makes plots of loca.p objects.\cr
\code{\link{plot.zsum}}: \tab makes plots of objective function.\cr
}
}

\seealso{Para la \enc{versión}{version} en \enc{español}{espanol},
  instale el paquete orloca.es  y consulte la ayuda sobre
  \code{\link[orloca.es]{orloca.es-package}}. (For the spanish version,
  install the package orloca.es and see the help about \code{\link[orloca.es]{orloca.es-package}}).
}
\examples{
# A new unweighted loca.p object
o <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# Compute the objective function at point (3, 4)
zsum(o, 3, 4)

# Compute the objective function at point (3, 4) using lp norm
zsum(o, 3, 4, lp=2.5)

# Solve the optimization problem
zsummin(o)

# Contour plot
contour.loca.p(o)

# Make a demo of the package
demo(orloca)
}
