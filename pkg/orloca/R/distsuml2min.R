#' distsuml2min at orloca package
#' 
#' \code{distsuml2min} is the \code{distsummin} function for the Euclidean norm (\eqn{l_2}).
#' This function returns the solution of the minimization problem.
#' Mainly for internal use.
#'
#' @name distsuml2min
#' @aliases distsuml2min distsuml2min,loca.p-method
#' @keywords internal classes optimize
#' @inherit distsummin 
setGeneric("distsuml2min",
           function (o, x = 0, y = 0, max.iter = 100, eps = 1.e-3, verbose = FALSE, algorithm = "Weiszfeld", ...) standardGeneric("distsuml2min")
           )

## General distsuml2min function
## L-BFGS-B seems to be the best similar to Weiszfeld
## Take into account that Weiszfeld is completely implemented in R
#' @export
setMethod("distsuml2min", "loca.p",
          function (o, x = 0, y = 0, max.iter = 100, eps = 1.e-3, verbose = FALSE, algorithm = "Weiszfeld", control = list(maxit = max.iter), ...) {
              algorithm <- match.arg(algorithm, c('Weiszfeld', 'gradient', 'ucminf', 'Nelder-Mead', 'BFGS', 'CG', 'L-BFGS-B', 'SANN'))
              if (algorithm == "gradient") distsuml2mingradient.loca.p(o, x, y, max.iter, eps, verbose)
              else if (algorithm == "Weiszfeld") distsuml2minWeiszfeld.loca.p(o, x, y, max.iter, eps, verbose, ...)
              else if (algorithm == "ucminf") distsuml2minucminf.loca.p(o, x, y, max.iter, eps, verbose)
              else {
                  zdistsummin <- function(x) distsum(o, x[1], x[2])
                  par <- c(sum(o@x*o@w)/sum(o@w), sum(o@y*o@w)/sum(o@w))
                  optim(par, zdistsummin, method = algorithm, control = list(maxit = max.iter))$par
              }
          }
          )

## Optimization by ucminf function from ucminf package
distsuml2minucminf.loca.p <- function (o, x = 0, y = 0, max.iter = 100, eps = 1.e-3, verbose = FALSE) {
    zdistsum <- function(xx) distsum(o, xx[1], xx[2])
    sol <- ucminf(par = c(x, y), fn = zdistsum, control = list(maxeval = max.iter, trace = verbose))
    if (verbose) cat(gettext(sol$message))
    return(sol$par)
}

## Gradient Method
distsuml2mingradient.loca.p <- function (o, x = 0, y = 0, max.iter = 100, eps = 1.e-3, verbose = FALSE) {
    lambda <- 1;
    eps2 <- eps^2
    u<-c(x,y)
    z <- distsum(o, u[1], u[2])
    for (i in 0:max.iter) {
        if (verbose) cat(paste(gettext("Iter", domain = "R-orloca"), ".", i, ": (", u[1], ",", u[2], ") ", z, "\n", sep = ""))
        g<-distsumgra(o, u[1], u[2])
        mg <- sum(g^2)
        ## Check stop rule
        if (is.na(mg)) {
            ## A demand point stop rule
            g<-distsumgra(o, u[1], u[2], partial = T)
            mg <- sum(g^2)
            ii <- which.min((o@x-u[1])^2+(o@y-u[2])^2)
            if (mg < sum(o@w[ii]^2)) {
                if(verbose) cat(gettext("Optimality condition reached at demand point.", domain = "R-orloca"));
                break
            }
        } else if (mg < eps2) {
            if(verbose) cat(gettext("Optimality condition reached.", domain = "R-orloca"));
            break;
        }
        nu <- u - lambda*g
        nz <- distsum(o, nu[1], nu[2])
        if (nz < z) {
            u<-nu
            z<-nz
            lambda <- lambda*2.2
        } else {
            lambda <- lambda/2
        }
    }
    if (i == max.iter) warning.max.iter(max.iter)
    u
}

distsuml2minWeiszfeld.loca.p <- function (o, x = 0, y = 0, max.iter = 100, eps = 1.e-3, verbose = FALSE, csmooth = .9) {
    ## Check smooth value
    if (!identical(csmooth >= 0 && csmooth < 1, TRUE)) {
        warning(paste(gettext("Value for smooth parameter non valid:", domain = "R-orloca"), smooth, gettext("Reseting to its default value.", domain = "R-orloca")))
        csmooth <- .5
    }
    eps2 <- eps^2
    u<-c(x,y)
    ## Begin iterations in non smooth mode
    .smooth = 0
    i.i = 0
    i.s = round(max.iter*.5)
    for (j in 1:2) {
        for (i in i.i:i.s) {
            if (verbose) cat(paste(gettext("Iter", domain = "R-orloca"), ". ", i, ": (", u[1], ",", u[2], ") ", distsum(o, u[1], u[2]), "\n", sep = ""))
            ## Compute the distances to demand points
            n <- sqrt((u[1]-o@x)^2+(u[2]-o@y)^2)
            ## Check for demand point proximities
            ii <- (n > eps)
            ## Compute the numerator of iteration
            n <- o@w/n;
            ## Compute the gradient
            g <- c(sum((u[1]-o@x[ii])*n[ii]), sum((u[2]-o@y[ii])*n[ii]))
            mg <- sum(g^2)
            ## Check stop rule
            if (!all(ii)) {
                ## A demand point stop rule
                if (mg < sum(o@w[!ii]^2) || mg < eps2) {
                    if(verbose) cat(gettext("Optimality condition reached at demand point.", domain = "R-orloca"));
                    break
                }
            } else if (mg <eps2) { ## Generic stop rule
                if(verbose) cat(gettext("Optimality condition reached.", domain = "R-orloca"));
                break
            }
            s <- sum(n[ii])
            nx <- n*o@x
            ny <- n*o@y
            u <- .smooth * u + (1-.smooth) * c(sum(nx[ii]), sum(ny[ii]))/s
        }
        ## Check if optimality condition had been reached
        if (i != i.s) break
        ## Changing to smooth version
        .smooth = csmooth
        if (j == 1) warning(gettext("The algorithm seems converges very slowly. Trying now with the smooth version.", domain = "R-orloca"))
        i.i = i.s
        i.s = max.iter
    }
    if (i == max.iter) warning.max.iter(max.iter)
    u
}
