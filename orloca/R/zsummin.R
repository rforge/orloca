# This function returns the solution of the minimization problem
setGeneric("zsummin",
           function (o, x=0, y=0, n=100, eps=1.e-3, verbose=FALSE, algorithm="weiszfeld") standardGeneric("zsummin")
)

# Gradient Method
setMethod("zsummin", "loca.p",
function (o, x=0, y=0, n=100, eps=1.e-3, verbose=FALSE, algorithm="weiszfeld")
   {
   if (algorithm=="gradient" || algorithm=="g") zsummingradient.loca.p(o, x, y, n, eps, verbose)
   else if (algorithm=="search" || algorithm=="s") zsumminsearch.loca.p(o, x, y, n, eps, verbose)
   else if (algorithm=="weiszfeld" || algorithm=="w") zsumminweiszfeld.loca.p(o, x, y, n, eps, verbose)
   else stop(paste(algorithm, gettext("is not a valid value for algorithm parameter.\n")))
   }
)

zsummingradient.loca.p <- function (o, x=0, y=0, n=100, eps=1.e-3, verbose=FALSE)
   {
   lambda = 1;
   eps2 <- eps^2
   u<-c(x,y)
   z <- zsum(o, u[1], u[2])
   for (i in 1:n)
      {
      g<-zsumgra(o, u[1], u[2])
      if (is.na(g[1]) || is.na(g[2]))
         {
         warning(paste(gettext("Singular point found at"), ": (", u[1], ",", u[2], "). ", gettext("Randomizing"), ".", sep=""))
         u<-u+runif(2)
         next
         }
      if (sum(g^2)<eps2) break;
      nu <- u - lambda*g
      nz <- zsum(o, nu[1], nu[2])
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
      if (verbose) cat(paste("Iter.",i, ": (", u[1], ",", u[2], ") ", z, "\n", sep=""))
      }
   u
   }

zsumminsearch.loca.p <- function (o, x=0, y=0, n=100, eps=1.e-3, verbose=FALSE)
   {
   eps2 <- eps^2
   lambda <- c(1, 1)
   u <- c(x, y)
   z <- zsum(o, x, y)
   nu <- u
   for(i in 1:n)
      {
      for (j in 1:2)
         {
         nu[j] <- u[j] + lambda[j]
         nz <- zsum(o, nu[1], nu[2])
         if (nz < z)
            {
            u <- nu
            z <- nz
            lambda[j] <- 2.2 * lambda[j]
            }
         else
            {
            nu[j] <- u[j] - lambda[j]
            nz <- zsum(o, nu[1], nu[2])
            if (nz < z)
               {
               u <- nu
               z <- nz
               lambda[j] <- -2.2 * lambda[j]
               }
            else lambda[j] <- lambda[j]/2
            }

         }
      if (verbose) cat(paste("Iter.",i, ": (", u[1], ",", u[2], ") ", z, "\n", sep=""))
      if (sum(lambda^2) < eps2) break;
      }

   u
   }

zsumminweiszfeld.loca.p <- function (o, x=0, y=0, n=100, eps=1.e-3, verbose=FALSE)
   {
   lambda = 1;
   eps2 <- eps^2
   u<-c(x,y)
   for (i in 1:n)
      {
      n <- o@w/sqrt((u[1]-o@x)^2+(u[2]-o@y)^2)
      g <- c(sum((u[1]-o@x)*n), sum((u[2]-o@y)*n))
      if (is.na(g[1]) || is.na(g[2]))
         {
         warning(paste(gettext("Singular point found at"), ": (", u[1], ",", u[2], "). ", gettext("Randomizing"), ".", sep=""))
         u<-u+runif(2)
         next
         }
      u[1] <- sum(n*o@x)/sum(n)
      u[2] <- sum(n*o@y)/sum(n)
      if (sum(g^2)<eps2) break;
      if (verbose) cat(paste("Iter.",i, ": (", u[1], ",", u[2], ") ", zsum(o, u[1], u[2]), "\n", sep=""))
      }
   u
   }
