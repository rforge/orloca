#' Computes zsum function
#'
#' The objective function and the gradient function for the min-sum location problem.
#'
#' @name zsum
#' @docType methods
#' @aliases zsum zsum,loca.p-method
#' @keywords classes optimize
#' @param o An object of \code{loca.p} class.
#' @param x The x coordinate of the point to be evaluated.
#' @param y The y coordinate of the point to be evaluated.
#' @param lp If given, then \eqn{l_p} norm will be used instead of the Euclidean norm.
#' @return 
#' \code{zsum} returns the objective function of the min-sum location problem, \eqn{\sum_{a_i \in o} w_i d(a_i, (x,y))}, where \eqn{d(a_i, (x,y))} gives the euclidean or the \eqn{l_p} distances between \eqn{a_i} and the point \eqn{(x,y)}.
#'
#' @examples
#' # A new unweighted loca.p object
#' loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
#' # Evaluation of zsum at (0, 0)
#' zsum(loca)
#'
#' # Evaluation of zsum at (1, 3)
#' zsum(loca, 1, 3)
#' # Compute the objective function at point (3, 4) using lp norm and p = 2.5
#' zsum(loca, 3, 4, lp=2.5)
#' # The gradient function at (1,3)
#' zsumgra(loca, 1, 3)
#'
#' @seealso See also \code{\link{orloca-package}} and \code{\link{zsummin}}.
#' 
# zsum method definition
#   Evaluation of objective function
setGeneric("zsum", function(o, x=0, y=0, lp=numeric(0)) standardGeneric("zsum"))

# Definition of zsum for class loca.p
#' @export
setMethod("zsum", "loca.p", function(o, x=0, y=0, lp=numeric(0))
   {
     if (length(lp) == 0) return(zsuml2(o=o, x=x, y=y))
     else if (lp >= 1) return(zsumlp(o=o, x=x, y=y, p=lp))
     else stop(paste(lp, gettext("is not a valid value for lp, use 1 <= lp", domain = "R-orloca")))
   }
)
