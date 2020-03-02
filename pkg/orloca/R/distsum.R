#' Computes distsum function
#'
#' The objective function and the gradient function for the min-sum location problem.
#'
#' @name distsum
#' @aliases distsum distsum,loca.p-method
#' @docType methods
#' @keywords classes optimize
#' @param o An object of \code{loca.p} class.
#' @param x The x coordinate of the point to be evaluated.
#' @param y The y coordinate of the point to be evaluated.
#' @param lp If given, then \eqn{l_p} norm will be used instead of the Euclidean norm.
#' @return 
#' \code{distsum} returns the objective function of the min-sum location problem, \eqn{\sum_{a_i \in o} w_i d(a_i, (x,y))}, where \eqn{d(a_i, (x,y))} gives the euclidean or the \eqn{l_p} distances between \eqn{a_i} and the point \eqn{(x,y)}.
#'
#' @examples
#' # A new unweighted loca.p object
#' loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
#' # Evaluation of distsum at (0, 0)
#' distsum(loca)
#'
#' # Evaluation of distsum at (1, 3)
#' distsum(loca, 1, 3)
#' # Compute the objective function at point (3, 4) using lp norm and p = 2.5
#' distsum(loca, 3, 4, lp=2.5)
#' # The gradient function at (1,3)
#' distsumgra(loca, 1, 3)
#'
#' @seealso See also \code{\link{orloca-package}} and \code{\link{distsummin}}.
#'
#' @details
#' The function zsum is deprecated and will be removed from new versions of the package.
#' 
# distsum method definition
#   Evaluation of objective function
setGeneric("distsum", function(o, x=0, y=0, lp=numeric(0)) standardGeneric("distsum"))

# Definition of distsum for class loca.p
#' @export
setMethod("distsum", "loca.p", function(o, x=0, y=0, lp=numeric(0))
   {
     if (length(lp) == 0) return(distsuml2(o=o, x=x, y=y))
     else if (lp >= 1) return(distsumlp(o=o, x=x, y=y, p=lp))
     else stop(paste(lp, gettext("is not a valid value for lp, use 1 <= lp", domain = "R-orloca")))
   }
)

#' @rdname zsum
#' @name zsum
#' @aliases zsum
#' @title zsum
#' @docType methods
#' @keywords deprecated
#' @description
#' The function zsum is deprected and could be removed in next version of the package. Use \link{distsum} instead.
#' @param \ldots Parameters passed to distsum
#' @export
zsum <- function(...) {
    warning('The function zsum is deprected and could be removed in next version of the package. Use distsum instead.')
    distsum(...)
    }
