#' zsumlp and zsumlpgra at orloca package
#'
#' zsum and zsumgra functions with \eqn{l_p} norm. Mainly for internal use.
#'
#' @details
#'
#' If \eqn{p<1} then \eqn{l_p} are not a norm, so only \eqn{p>=1} are valid values.
#'
#' Since \eqn{l_2} norm is the Euclidean norm, when \eqn{p=2} \code{zsumlp} are equal to \code{zsum}, and \code{zsumlpgra} are equal to \code{zsumgra}.
#' But the computations involved are greater for the firsts form.
#' 
#' @name zsumlp
#' @keywords internal classes optimize
#' @aliases zsumlp zsumlp,loca.p-method zsumlpgra zsumlpgra,loca.p-method
#' @param o An object of \code{loca.p} class.
#' @param x The x coordinate of the point to be evaluated.
#' @param y The y coordinate of the point to be evaluated.
#' @param p The \eqn{l_p} norm to use.
#' @param partial If (x,y) is a demand point \code{partial=T} means ignore such point to compute the gradient. This option is mainly for internal use.
#' @return
#' \code{zsumlp} returns the objective function of the min-sum location problem with \eqn{l_p} norm, \eqn{\sum_{a_i \in o} w_i d(a_i, (x,y))}, where \eqn{d(a_i, (x,y))} gives the distances between \eqn{a_i} and the point \eqn{(x,y)} using \eqn{l_p} norm.
#'
#' \code{zsumlpgra} returns the gradient vector of the function \code{zsumlp}.
#' 
#' @seealso See also \code{\link{zsum}}, \code{\link{orloca-package}} and \code{\link{zsumlpmin}}.
#' 
# Functions for evaluation of objective function
# 
# zsum method definition
#   Evaluation of objective function
setGeneric("zsumlp", function(o, x=0, y=0, p=2) standardGeneric("zsumlp"))

# Definition of zsum for class loca.p with lp norm
#' @export
setMethod("zsumlp", "loca.p", function(o, x=0, y=0, p=2)
   {
     if (p>=1) return(sum(o@w*(abs(o@x-x)^p+abs(o@y-y)^p)^(1/p)))
     else stop(paste(p, gettext("is not a valid value for p, use 1 <= p", domain = "R-orloca")))
   }
)

# zsumlpgra method definition
#   Evaluation of the gradient
setGeneric("zsumlpgra", function(o, x=0, y=0, p=2, partial=F) standardGeneric("zsumlpgra"))

# Definition os zsumlpgra for class loca.p with lp
#' @export
setMethod("zsumlpgra", "loca.p", function(o, x=0, y=0, p=2, partial=F)
   {
     if (p>=1) {
       n<- o@w*(abs(x-o@x)^p+abs(y-o@y)^p)^(1/p-1)
       c(sum(sign(x-o@x)*abs(x-o@x)^(p-1)*n, na.rm=partial), sum(sign(y-o@y)*abs(y-o@y)^(p-1)*n, na.rm=partial))
     }
     else stop(paste(p, gettext("is not a valid value for p, use 1 <= p", domain = "R-orloca")))
   }
)
