#' distsumlp and distsumlpgra at orloca package
#'
#' distsum and distsumgra functions with \eqn{l_p} norm. Mainly for internal use.
#'
#' @details
#'
#' If \eqn{p<1} then \eqn{l_p} is not a norm, so only \eqn{p>=1} are valid values.
#'
#' Since \eqn{l_2} norm is the Euclidean norm, when \eqn{p=2} \code{distsumlp} are equal to \code{distsum}, and \code{distsumlpgra} are equal to \code{distsumgra}.
#' But the computations involved are greater for the firsts form.
#' 
#' @name distsumlp
#' @keywords internal classes optimize
#' @aliases distsumlp distsumlp,loca.p-method distsumlpgra distsumlpgra,loca.p-method
#' @param o An object of \code{loca.p} class.
#' @param x The x coordinate of the point to be evaluated.
#' @param y The y coordinate of the point to be evaluated.
#' @param p The \eqn{l_p} norm to use.
#' @param partial If (x,y) is a demand point \code{partial=T} means ignore such point to compute the gradient. This option is mainly for internal use.
#' @return
#' \code{distsumlp} returns the objective function of the min-sum location problem with \eqn{l_p} norm, \eqn{\sum_{a_i \in o} w_i d(a_i, (x,y))}, where \eqn{d(a_i, (x,y))} gives the distances between \eqn{a_i} and the point \eqn{(x,y)} using \eqn{l_p} norm.
#'
#' \code{distsumlpgra} returns the gradient vector of the function \code{distsumlp}.
#' 
#' @seealso See also \code{\link{distsum}}, \code{\link{orloca-package}} and \code{\link{distsumlpmin}}.
#' 
# Functions for evaluation of objective function
# 
# distsum method definition
#   Evaluation of objective function
setGeneric("distsumlp", function(o, x=0, y=0, p=2) standardGeneric("distsumlp"))

# Definition of distsum for class loca.p with lp norm
#' @export
setMethod("distsumlp", "loca.p", function(o, x=0, y=0, p=2)
   {
     if (p>=1) return(sum(o@w*(abs(o@x-x)^p+abs(o@y-y)^p)^(1/p)))
     else stop(paste(p, gettext("is not a valid value for p, use 1 <= p", domain = "R-orloca")))
   }
)

# distsumlpgra method definition
#   Evaluation of the gradient
setGeneric("distsumlpgra", function(o, x=0, y=0, p=2, partial=F) standardGeneric("distsumlpgra"))

# Definition os distsumlpgra for class loca.p with lp
#' @export
setMethod("distsumlpgra", "loca.p", function(o, x=0, y=0, p=2, partial=F)
   {
     if (p>=1) {
       n<- o@w*(abs(x-o@x)^p+abs(y-o@y)^p)^(1/p-1)
       c(sum(sign(x-o@x)*abs(x-o@x)^(p-1)*n, na.rm=partial), sum(sign(y-o@y)*abs(y-o@y)^(p-1)*n, na.rm=partial))
     }
     else stop(paste(p, gettext("is not a valid value for p, use 1 <= p", domain = "R-orloca")))
   }
)
