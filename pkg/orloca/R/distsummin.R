#' Returns the solution of the minimization problem
#'
#' Solve the min-sum location problem for a given \code{loca.p} class object.
#' 
#' @name distsummin
#' @aliases distsummin distsummin,loca.p-method
#' @keywords classes optimize
#' @details
#' The algorithms Weiszfeld and gradient include and optimality test for demand points.
#' The Weiszfeld version of the algorithm also implements slow convergence test and accelerator procedure.
#' 
#' If \eqn{p < 1} thus \eqn{l_p} is not a norm, so, only \eqn{p \ge 1} are valid values.
#'
#' Since \eqn{l_2} norm is the Euclidean norm, when \eqn{p=2} \code{distsumlpmin} are equal to \code{distsummin}.
#' But the computations involved are greater for the first form.
#' 
#' max.iter for SANN algorithm is the number of evaluation of objective function, so this methos usually requires large values of max.iter to reach optimal value
#' @param o An object of loca.p class.
#' @param x The x coordinate of the starting point. It's default value is 0.
#' @param y The y coordinate of the starting point. It's default value is 0.
#' @param lp If given, the \eqn{l_p} norm will be used instead of the Euclidean norm.
#' @param max.iter Maximum number of iterations allowed. It's default value is 100000.
#' @param eps The module of the gradient in the stop rule. It's default value is 1e-3.
#' @param verbose If TRUE the function produces detailed output. It's default value is FALSE.
#' @param algorithm The method to be use. For this version of the package, the valid values are: "gradient" for a gradient based method, "search" for local search method (this option is deprecated), "ucminf" for optimization with ucminf from ucminf package, and "Weiszfeld" for the Weiszfeld method or any of the valid method for optim function, now "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN". "Weiszfeld" is the default value.
#' @param \ldots Other options for optimization algorithms. 
#' @return \code{distsummin} returns an array with the coordinates of the solution point.
#' @seealso See also \code{\link{orloca-package}}, \code{\link{loca.p}} and \code{\link{distsum}}.
#' @examples
#' # A new unweighted loca.p object
#' loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
#' # Compute the minimum
#' sol<-distsummin(loca)
#'
#' # Show the result
#' sol
#'
#' # Evaluation of the objective function at solution point
#' distsum(loca, sol[1], sol[2])
#'
#' @details
#' The function zsummin is deprecated and will be removed from new versions of the package.
#' 
setGeneric("distsummin",
           function (o, x=0, y=0, lp=numeric(0), max.iter=100000, eps=1.e-3, verbose=FALSE, algorithm="Weiszfeld", ...) standardGeneric("distsummin")
)

# Check lp value and call to the specific function
#' @export
setMethod("distsummin", "loca.p",
function (o, x=0, y=0, lp=numeric(0), max.iter=100000, eps=1.e-3, verbose=FALSE, algorithm="Weiszfeld", ...)
   {
     if (length(lp) == 0) return(distsuml2min(o=o, x=x, y=y, max.iter=max.iter, eps=eps, verbose=verbose, algorithm=algorithm, ...))
     else if (lp >= 1) return(distsumlpmin(o=o, x=x, y=y, p=lp, max.iter=max.iter, eps=eps, verbose=verbose, algorithm=algorithm, ...))
     else stop(paste(lp, gettext("is not a valid value for lp, use 1 <= lp", domain = "R-orloca")))
   }
)

warning.max.iter <- function(max.iter)
  {
    warning(paste(gettext("distsummin: Maximun number of iteration reached", domain = "R-orloca"), " (max.iter = ", max.iter, ")\n", gettext("The solution may be non-optimal.\nPerhaps, you could try increasing max.iter value.", domain = "R-orloca"), sep=""), call. = F)
  }

#' @rdname zsummin
#' @name zsummin
#' @aliases zsummin
#' @title zsummin
#' @docType methods
#' @keywords deprecated
#' @description
#' The function zsummin is deprected and could be removed in next version of the package. Use \link{distsummin} instead.
#' @param \ldots Parameters passed to distsummin
#' @export
zsummin <- function(...) {
    warning('The function zsum is deprected and could be removed in next version of the package. Use distsum instead.')
    distsummin(...)
    }
