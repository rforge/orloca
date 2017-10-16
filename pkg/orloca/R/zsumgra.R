#' Computes the gradient of zsum function
#'
#' The gradient function for the min-sum location problem.
#'
#' @name zsumgra
#' @docType methods
#' @aliases zsumgra zsumgra,loca.p-method
#' @keywords classes optimize
#' @param o An object of \code{loca.p} class.
#' @param x The x coordinate of the point to be evaluated.
#' @param y The y coordinate of the point to be evaluated.
#' @param lp If given, then \eqn{l_p} norm will be used instead of the Euclidean norm.
#' @param partial If (x,y) is a demand point \code{partial=T} means ignore such point to compute the gradient. This option is mainly for internal use.
#' @return 
#' \code{zsumgra} returns the gradient vector of the function of the min-sum location problem, \eqn{\sum_{a_i \in o} w_i d(a_i, (x,y))}, where \eqn{d(a_i, (x,y))} gives the euclidean or the \eqn{l_p} distances between \eqn{a_i} and the point \eqn{(x,y)}.
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
#' @seealso See also \code{\link{orloca-package}} and \code{\link{zsum}}.
#' 

# zsumgra method definition
#   Evaluation of the gradient
setGeneric("zsumgra", function(o, x=0, y=0, lp=numeric(0), partial=F) standardGeneric("zsumgra"))

# Definition os zsumgra for class loca.p
#' @export
setMethod("zsumgra", "loca.p", function(o, x=0, y=0, lp=numeric(0), partial=F)
   {
     if (length(lp) == 0) return(zsuml2gra(o=o, x=x, y=y, partial=partial))
     else if (lp >= 1) return(zsumlpgra(o=o, x=x, y=y, p=lp, partial=partial))
     else stop(paste(lp, gettext("is not a valid value for lp, use 1 <= lp", domain = "R-orloca")))
   }
)
