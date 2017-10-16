#' zsuml2 and zsuml2gra at orloca package
#'
#' \code{zsum} and \code{zsumgra} functions for the Euclidean norm (\eqn{l_2}). Mainly for internal use.
#'
#' @name zsuml2
#' @aliases zsuml2 zsuml2,loca.p-method zsuml2gra zsuml2gra,loca.p-method
#' @keywords internal classes optimize
#' @param o An object of \code{loca.p} class.
#' @param x The x coordinate of the point to be evaluated.
#' @param y The y coordinate of the point to be evaluated.
#' @param partial If (x,y) is a demand point \code{partial=T} means ignore such point to compute the gradient. This option is mainly for internal use.
#' @return \code{zsuml2} returns the objective function of the min-sum location problem, \eqn{\sum_{a_i \in o} w_i d(a_i, (x,y))}, where \eqn{d(a_i, (x,y))} gives the euclidean distances between \eqn{a_i} and the point \eqn{(x,y)}.  \code{zsumgra} returns the gradient vector of the function \code{zsum}.
#' @seealso See also \code{\link{orloca-package}}, \code{\link{zsum}}, \code{\link{zsumgra}} and \code{\link{zsummin}}.
#' 
# zsuml2 method definition
#   Evaluation of objective function
setGeneric("zsuml2", function(o, x=0, y=0) standardGeneric("zsuml2"))

# Definition of zsum for class loca.p
#' @export
setMethod("zsuml2", "loca.p", function(o, x=0, y=0)
   {
   sum(o@w*sqrt((o@x-x)^2+(o@y-y)^2))
   }
)

# zsuml2gra method definition
#   Evaluation of the gradient
setGeneric("zsuml2gra", function(o, x=0, y=0, partial=F) standardGeneric("zsuml2gra"))

# Definition os zsumgra for class loca.p
#' @export
setMethod("zsuml2gra", "loca.p", function(o, x=0, y=0, partial=F)
   {
   n<- o@w/sqrt((x-o@x)^2+(y-o@y)^2)
   c(sum((x-o@x)*n, na.rm=partial), sum((y-o@y)*n, na.rm=partial))
   }
)
