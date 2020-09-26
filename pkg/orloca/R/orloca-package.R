#' Operations Research LOCational Analysis Models
#' 
#' Objects and methods to handle and solve the min-sum location problem, also known as Fermat-Weber problem.
#'
#' The min-sum location problem search for a point such that the weighted sum of the distances to the demand points are minimized. See "The Fermat-Weber location problem revisited" by Brimberg, Mathematical Programming, 1, pg. 71-76, 1995, DOI:10.1007/BF01592245.
#'
#' General global optimization algorithms are used to solve the problem, along with the adhoc Weiszfeld method, see "Sur le point pour lequel la Somme des distances de n points donnes est minimum", by E. Weiszfeld, Tohoku Mathematical Journal, First Series, 43, pg. 355-386, 1937 or "On the point for which the sum of the distances to n given points is minimum", by E. Weiszfeld and F. Plastria, Annals of Operations Research, 167, pg. 7-41, 2009, DOI:10.1007/s10479-008-0352-z.
#'
#' @aliases orloca-package
#' @docType package
#' @name orloca-package
#' @details
#' \preformatted{
#' 
#' Package:   orloca
#' 
#' Type:      Package
#' 
#' Version:   4.10
#' 
#' Date:      2020-09-23
#' 
#' License:   GPL (>= 3)
#' }
#'
#' The package provides a class (\code{loca.p}) that represents a location problem with a finite set of demand points over the plane.
#' Also, it is possible to plot the points and the objective function.
#' Such objective function is the total weighted distances travelled by all the customers to the service.
#'
#'
#' Non-planar location problems could be handle in future versions of the package.
#'
#' 
#' For a demo, load the package with \code{library(orloca)}, and use \code{demo(orloca)}.
#'
#' 
#' The package is ready for internationalization. The author ask for translated version of the .mo file to include in the package.
#'
#' @author Manuel Munoz-Marquez <manuel.munoz@@uca.es>
#' 
#' Mantainer: Manuel Munoz-Marquez <manuel.munoz@@uca.es>
#' @references
#' [1] Brimberg, J. \emph{The Fermat-Weber location problem revisited}, Mathematical Programming, 1, pg. 71-76, 1995. \url{https://doi.org/10.1007/BF01592245}.
#'
#' [2] Love, R. F., Morris, J. G., Wesolowsky, G. O. \emph{Facilities Location: Chapter 2: Introduction to Single-Facility Location}, 1988, North-Holland. ISBN: 0-444-01031-9.
#'
#' [3] Weiszfeld, E. and Plastria, F. \emph{On the point for which the sum of the distances to n given points is minimum}, Annals of Operations Research, 167, pg. 7-41, 2009, \url{https://doi.org/10.1007/s10479-008-0352-z}.
#' 
#' [4] \url{http://knuth.uca.es/orloca}
#' 
#' @keywords package optimize
#' @seealso
#' Para la version en espanol, instale el paquete orloca.es y consulte la ayuda sobre \code{\link[orloca.es]{orloca.es-package}}.
#' (For the spanish version, install the orloca.es package and see the help about \code{\link[orloca.es]{orloca.es-package}}).
#' @examples
#' # A new unweighted loca.p object
#' o <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
#' 
#' # Compute the sum of distances to point (3, 4)
#' distsum(o, 3, 4)
#' 
#' # Compute the sum of distances to point (3, 4) using lp norm
#' distsum(o, 3, 4, lp=2.5)
#'
#' # Solve the optimization problem
#' distsummin(o)
#' # Contour plot
#' contour(o)
#'
#' # Make a demo of the package
#' demo(orloca)
#'
#' @import graphics
#' @import grDevices
#' @import methods
#' @import png
#' @import stats
#' @import ucminf
#'
#' @export as.loca.p
#' @export as.loca.p.matrix
#' @export as.matrix.loca.p
#' @export contour.loca.p
#' @export loca.p
#' @export persp.loca.p
#' @export plot.loca.p
#' @export distsum
#' @export distsumgra
#' @export distsummin
#'
NULL
