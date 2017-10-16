#' zsummin at orloca package returns the solution of the minimization problem
#'
#' Solve the min-sum location problem for a given \code{loca.p} class object.
#' 
#' @name zsummin
#' @aliases zsummin zsummin,loca.p-method
#' @keywords classes optimize
#' @details
#' If \eqn{p < 1} thus \eqn{l_p} is not a norm, so, only \eqn{p \ge 1} are valid values.
#' @param o An object of loca.p class.
#' @param x The x coordinate of the starting point.
#' @param y The y coordinate of the starting point.
#' @param lp If given, the \eqn{l_p} norm will be used instead of the Euclidean norm.
#' @param max.iter Maximum number of iterations allowed.
#' @param eps The module of the gradient in the stop rule.
#' @param verbose If TRUE the function produces detailed output.
#' @param algorithm The algorithm to be use. For this version of the package, the valid values are: "gradient" or "g" for a gradient based method, "search" or "s" for local search method, "ucminf" or "u" for optimization with ucminf from ucminf package, and "weiszfeld" or "w" for the Weiszfeld method or any of the valid method for optim function, now "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN". "weiszfeld" is the default value.
#' @param \ldots Other options for optimization algorithms. 
#' @return \code{zsummin} returns an array with the coordinates of the solution point.
#' @seealso See also \code{\link{orloca-package}}, \code{\link{loca.p}} and \code{\link{zsum}}.
#' @examples
#' # A new unweighted loca.p object
#' loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
#' # Compute the minimum
#' sol<-zsummin(loca)
#'
#' # Show the result
#' sol
#'
#' # Evaluation of the objective function at solution point
#' zsum(loca, sol[1], sol[2])
#' 
setGeneric("zsummin",
           function (o, x=0, y=0, lp=numeric(0), max.iter=100, eps=1.e-3, verbose=FALSE, algorithm="weiszfeld", ...) standardGeneric("zsummin")
)

# Check lp value and call to the specific function
#' @export
setMethod("zsummin", "loca.p",
function (o, x=0, y=0, lp=numeric(0), max.iter=100, eps=1.e-3, verbose=FALSE, algorithm="weiszfeld", ...)
   {
     if (length(lp) == 0) return(zsuml2min(o=o, x=x, y=y, max.iter=max.iter, eps=eps, verbose=verbose, algorithm=algorithm, ...))
     else if (lp >= 1) return(zsumlpmin(o=o, x=x, y=y, p=lp, max.iter=max.iter, eps=eps, verbose=verbose, algorithm=algorithm, ...))
     else stop(paste(lp, gettext("is not a valid value for lp, use 1 <= lp", domain = "R-orloca")))
   }
)

warning.max.iter <- function(max.iter)
  {
    warning(paste(gettext("zsummin: Maximun number of iteration reached", domain = "R-orloca"), " (max.iter = ", max.iter, ")\n", gettext("The solution may be non-optimal.\nPerhaps, you could try increasing max.iter value.", domain = "R-orloca"), sep=""), call. = F)
  }
