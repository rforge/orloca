#' Plots of the min-sum objective function
#'
#' \code{persp} provides a graphical representations of min-sum function (\code{distsum}).
#'
#' @name persp.loca.p
#'
#' @details
#' If \eqn{p<1} then \eqn{l_p} is not a norm, so only \eqn{p>=1} are valid values.
#'
#' @keywords classes hplot
#' @param x The loca.p object to compute the objective.
#' @param lp If given, then \eqn{l_p} norm will be used instead of the Euclidean norm.
#' @param xmin The minimum value for x axis.
#' @param xmax The maximum value for x axis.
#' @param ymin The minimum value for y axis.
#' @param ymax The maximum value for y axis.
#' @param n The number of divisions for grid.
#' @param \ldots Other options.
#' @return A plot a 3D plot or min-sum function.
#' @seealso See also \code{\link{orloca-package}}, \code{\link{plot.loca.p}} and \code{\link{loca.p}}.
#' @examples
#' # A new unweighted loca.p object
#' loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
#'
#' # The 3D graphics
#' persp(loca)
#'
#' @export
persp.loca.p <- function(x, lp=numeric(0), xmin=min(x@x), xmax=max(x@x), ymin=min(x@y), ymax=max(x@y), n=10, ...)
   {
   .x<-seq(xmin, xmax, length.out=n)
   .y<-seq(ymin, ymax, length.out=n)
   .z<-matrix(1, nrow=n, ncol=n, byrow=TRUE)
   if (length(lp) == 0)
     {
       for(i in 1:n)
         for(j in 1:n)
           .z[i,j] <- distsum(x, .x[i], .y[j])
     }
   else if (lp >= 1)
     {
       for(i in 1:n)
         for(j in 1:n)
           .z[i,j] <- distsumlp(x, .x[i], .y[j], p=lp)
    }
   else stop(paste(lp, gettext("is not a valid value for lp, use 1 <= lp", domain = "R-orloca")))
   persp(.x, .y, .z, ...)
   invisible(1)
   }

