\name{orloca-package}
\alias{orloca-package}
\alias{orloca}
\docType{package}
\encoding{UTF-8}
\title{
  The package deals with Operations Research LOCational Analysis models}
\description{
  This version of the package deals with the min-sum or center location problems. 
 The min-sum location problem search for a point such that the weighted sum of the distances to the demand points are minimized.
}
\details{
\preformatted{
Package:   orloca
Type:      Package
Version:   1.0
Date:      2008-06-12
License:   GPL (>= 2)
}
The packages provides a class (loca.p) that represents a location
problem with a finite set of demand points over the plane. Also, it is possible to plot
the points and the objective function. Such objective function is the
total distances travel by all the customers to the service.

For a demo, load the package with \code{library(orloca)}, and use \code{demo(orloca)}.
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
\code{\link[orloca]{loca.p}}: \tab loca.p class description.\cr
\code{\link[orloca]{rloca.p}}: \tab random instances of loca.p class objects.\cr
\code{\link[orloca]{zsum}}: \tab function to compute objective function for min-sum models.\cr
\code{\link[orloca]{zsummin}}: \tab to find minimum for the optimization
problem.\cr
\code{\link[orloca]{plot.loca.p}}: \tab to make plots of loca.p objects.\cr
\code{\link[orloca]{plot.zsum}}: \tab to make plots of objective function.\cr
}
}

\seealso{Para la \enc{versión}{version} en \enc{español}{espanol} del paquete instale y consulte
  el paquete \code{\link[orloca-es]{orloca-es-package}}. (For the spanish version of the packages install and look at \code{\link[orloca-es]{orloca-es-package}}).
}
\examples{
# An unweighted new loca.p object
o <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# Compute the objective function at point c(3, 4)
zsum(o, 3, 4)

# Solve the optimization problem
zsummin(o)

# Contour plot
contour.loca.p(o)

# Execute a demo of the package
demo(orloca)
}
