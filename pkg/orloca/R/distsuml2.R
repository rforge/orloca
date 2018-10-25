#' distsuml2 and distsuml2gra at orloca package
#'
#' \code{distsum} and \code{distsumgra} functions for the Euclidean norm (\eqn{l_2}). Mainly for internal use.
#'
#' @name distsuml2
#' @aliases distsuml2 distsuml2,loca.p-method distsuml2gra distsuml2gra,loca.p-method
#' @keywords internal classes optimize
#' @param o An object of \code{loca.p} class.
#' @param x The x coordinate of the point to be evaluated.
#' @param y The y coordinate of the point to be evaluated.
#' @param partial If (x,y) is a demand point \code{partial=T} means ignore such point to compute the gradient. This option is mainly for internal use.
#' @return \code{distsuml2} returns the objective function of the min-sum location problem, \eqn{\sum_{a_i \in o} w_i d(a_i, (x,y))}, where \eqn{d(a_i, (x,y))} gives the euclidean distances between \eqn{a_i} and the point \eqn{(x,y)}.  \code{distsumgra} returns the gradient vector of the function \code{distsum}.
#' @seealso See also \code{\link{orloca-package}}, \code{\link{distsum}}, \code{\link{distsumgra}} and \code{\link{distsummin}}.
#' 
# distsuml2 method definition
#   Evaluation of objective function
setGeneric("distsuml2", function(o, x=0, y=0) standardGeneric("distsuml2"))

# Definition of distsum for class loca.p
#' @export
setMethod("distsuml2", "loca.p", function(o, x=0, y=0)
   {
   sum(o@w*sqrt((o@x-x)^2+(o@y-y)^2))
   }
)

# distsuml2gra method definition
#   Evaluation of the gradient
setGeneric("distsuml2gra", function(o, x=0, y=0, partial=F) standardGeneric("distsuml2gra"))

# Definition os distsumgra for class loca.p
#' @export
setMethod("distsuml2gra", "loca.p", function(o, x=0, y=0, partial=F)
   {
   n<- o@w/sqrt((x-o@x)^2+(y-o@y)^2)
   c(sum((x-o@x)*n, na.rm=partial), sum((y-o@y)*n, na.rm=partial))
   }
)
