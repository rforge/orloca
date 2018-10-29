\name{RcmdrPlugin.orloca-package}
\title{A GUI for Planar Location Problems}
\alias{RcmdrPlugin.orloca-package}
\alias{RcmdrPlugin.orloca}
\docType{package}
\encoding{UTF-8}
\description{
  RcmdrPlugin.orloca provides a GUI for the orloca package as a Rcmdr
  plug-in. The package deals with location problems on the plane.

  In order to use the full power of the Rcmdr interface, the data for
  location problems are stored as \code{data.frame} objects. These
  \code{data.frame} objects are converted on the fly to \code{loca.p}
  objects (the type of objects that are handle by \code{orloca}
  package). The \code{data.frame} should have at least three columns:
  \code{x}, \code{y}, and \code{w}, without NA's values. See \code{\link[orloca]{loca.p}} for details.

  The main functions of \code{orloca} packages are available throughout the
  GUI. These functions include: data manipulation, plot of the demand 
  points set and the objective function, and also the resolution of the location
  problem.
}
\details{
\tabular{ll}{
Package: \tab RcmdrPlugin.orloca\cr
Type: \tab Package\cr
Version: \tab 4.7\cr
Date: \tab 2018-10-29\cr
License: \tab GPL version 3 or newer\cr
}
}

\author{
Manuel Munoz-Marquez <manuel.munoz@uca.es>

Maintainer: Manuel Munoz-Marquez <manuel.munoz@uca.es>
}

\keyword{package}
\seealso{
For more information see \code{\link[Rcmdr]{Rcmdr-package}} and
\code{\link[orloca]{orloca-package}}. 

Para ayuda en \enc{español}{espanol}, \enc{véase}{vease}
\link{RcmdrPlugin.orloca.es}. (For Spanish help see \link{RcmdrPlugin.orloca.es}.) 
}

