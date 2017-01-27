# This function returns the solution of the minimization problem
setGeneric("czsummin",
           function (o, cluster, x=0, y=0, max.iter=100, eps=1.e-3, verbose=FALSE, algorithm="gradient") standardGeneric("czsummin")
)

# Gradient Method
setMethod("czsummin", "loca.p",
function (o, cluster, x=0, y=0, max.iter=100, eps=1.e-3, verbose=FALSE, algorithm="gradient")
   {
   if (algorithm=="gradient" || algorithm=="g") czsummingradient.loca.p(o, cluster, x, y, max.iter, eps, verbose)
   else if (algorithm=="search" || algorithm=="s") czsumminsearch.loca.p(o, cluster, x, y, max.iter, eps, verbose)
#   else if (algorithm=="weiszfeld" || algorithm=="w") czsumminweiszfeld.loca.p(o, cluster, x, y, max.iter, eps, verbose)
   else stop(paste(algorithm, gettext("is not a valid value for algorithm parameter.\n")))
   }
)

czsummingradient.loca.p <- function (o, cluster, x=0, y=0, max.iter=100, eps=1.e-3, verbose=FALSE)
   {
   # Split loca.p object
   l <- split.loca.p(o, length(cluster))
   # Spread over the nodes
   parLapply(cluster, l, function(o) assign(".orloca.loca.p", o, envir = sys.frame()))
   # Continue as zsummingradient
   lambda = 1;
   eps2 <- eps^2
   u <- c(x, y)
   z <- czsumaux(cluster, u)
   for (i in 0:max.iter)
      {
      g<-czsumgraaux(cluster, u)
      if (is.na(g[1]) || is.na(g[2]))
         {
         warning(paste(gettext("Singular point found at"), ": (", u[1], ",", u[2], "). ", gettext("Randomizing"), ".", sep=""))
         u<-u+runif(2)
         next
         }
      if (sum(g^2)<eps2) break;
      nu <- u - lambda*g
      nz <- czsumaux(cluster, nu)
      if (nz < z)
         {
         u<-nu
         z<-nz
         lambda <- lambda*2.2
         }
      else
         {
         lambda <- lambda/2
         }
      if (verbose) cat(paste(gettext("Iter."),i, ": (", u[1], ",", u[2], ") ", z, "\n", sep=""))
      }
   u
   }

czsumminsearch.loca.p <- function (o, cluster, x=0, y=0, max.iter=100, eps=1.e-3, verbose=FALSE)
   {
    # Split loca.p object
   l <- split.loca.p(o, length(cluster))
   # Spread over the nodes
   parLapply(cluster, l, function(o) assign(".orloca.loca.p", o, envir = sys.frame()))
   # Continue as zsumminsearch
   eps2 <- eps^2
   lambda <- c(1, 1)
   u <- c(x, y)
   z <- czsumaux(cluster, u)
   nu <- u
   for(i in 0:max.iter)
      {
      for (j in 1:2)
         {
         nu[j] <- u[j] + lambda[j]
         nz <- czsumaux(cluster, nu)
         if (nz < z)
            {
            u <- nu
            z <- nz
            lambda[j] <- 2.2 * lambda[j]
            }
         else
            {
            nu[j] <- u[j] - lambda[j]
            nz <- czsumaux(cluster, nu)
            if (nz < z)
               {
               u <- nu
               z <- nz
               lambda[j] <- -2.2 * lambda[j]
               }
            else lambda[j] <- lambda[j]/2
            }

         }
      if (verbose) cat(paste(gettext("Iter."),i, ": (", u[1], ",", u[2], ") ", z, "\n", sep=""))
      if (sum(lambda^2) < eps2) break;
      }
   u
   }

#czsumminweiszfeld.loca.p <- function (o, cluster, x=0, y=0, max.iter=100, eps=1.e-3, verbose=FALSE)
#   {
#   # Split loca.p object
#   l <- split.loca.p(o, length(cluster))
#   # Spread over the nodes
#   parLapply(cluster, l, function(o) assign(".orloca.loca.p", o, envir = sys.frame()))
#   # Continue as zsumminweiszfeld
#   error("Not implemented")
#   lambda = 1;
#   eps2 <- eps^2
#   u<-c(x,y)
#   for (i in 0:max.iter)
#      {
#      n <- o@w/sqrt((u[1]-o@x)^2+(u[2]-o@y)^2)
#      g <- c(sum((u[1]-o@x)*n), sum((u[2]-o@y)*n))
#      if (is.na(g[1]) || is.na(g[2]))
#         {
#         warning(paste(gettext("Singular point found at"), ": (", u[1], ",", u[2], "). ", gettext("Randomizing"), ".", sep=""))
#         u<-u+runif(2)
#         next
#         }
#      u[1] <- sum(n*o@x)/sum(n)
#      u[2] <- sum(n*o@y)/sum(n)
#      if (sum(g^2)<eps2) break;
#      if (verbose) cat(paste(gettext("Iter."),i, ": (", u[1], ",", u[2], ") ", zsum(o, u[1], u[2]), "\n", sep=""))
#      }
#   u
#   }
#

czsumaux <- function(cluster, u) {
  m <- rep(u, length(cluster))
  m <- matrix(m, ncol = 2, byrow=TRUE)
  l <- parRapply(cluster, m, zsumaux)
  sl <- 0
  for (i in 1:length(cluster)) sl <- sl + l[[i]]
}

zsumaux <- function(u) zsum(.orloca.loca.p, u[1], u[2])

czsumgraaux <- function(cluster, u) {
  m <- rep(u, length(cluster))
  m <- matrix(m, ncol = 2, byrow=TRUE)
  l <- parRapply(cluster, m, zsumgraaux)
  sl <- c(0,0)
  for (i in 1:2) sl[i] <- sum(l[seq(i, 2*length(cluster), 2)])
  return(sl)
}

zsumgraaux <- function(u) zsumgra(.orloca.loca.p, u[1], u[2])
