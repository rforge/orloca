\name{RcmdrPlugin.orloca-package}
\title{RcmdrPlugin.orloca: A GUI for orloca-package}
\alias{RcmdrPlugin.orloca-package}
\alias{RcmdrPlugin.orloca}
\docType{package}
\encoding{UTF-8}
\description{
  This package provides a GUI (Graphical User Interface) for the
  orloca-package. It is a Plug-in for Rcmdr (R Commander).

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

  Parallel computation functions provided by \code{orloca} package are not available
  throughout the GUI.   
}
\details{
\tabular{ll}{
Package: \tab RcmdrPlugin.orloca\cr
Type: \tab Package\cr
Version: \tab 4.1-0\cr
Date: \tab 2013-01-20\cr
License: \tab GPL version 3 or newer\cr
}
}

\author{
Fernando Fernandez-Palacin <fernando.fernandez@uca.es> and Manuel
Munoz-Marquez <manuel.munoz@uca.es>

Maintainer: Manuel Munoz-Marquez <manuel.munoz@uca.es>
}

\keyword{package}
\seealso{
For more information see \code{\link[Rcmdr]{Rcmdr-package}} and
\code{\link[orloca]{orloca-package}}. 

Para ayuda en \enc{español}{espanol}, \enc{véase}{vease}
\link{RcmdrPlugin.orloca.es}. (For Spanish help see \link{RcmdrPlugin.orloca.es}.) 
}

