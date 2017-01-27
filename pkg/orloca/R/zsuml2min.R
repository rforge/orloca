# This function returns the solution of the minimization problem
setGeneric("zsuml2min",
           function (o, x=0, y=0, max.iter=100, eps=1.e-3, verbose=FALSE, algorithm="weiszfeld") standardGeneric("zsuml2min")
)

# Gradient Method
setMethod("zsuml2min", "loca.p",
function (o, x=0, y=0, max.iter=100, eps=1.e-3, verbose=FALSE, algorithm="weiszfeld")
   {
   if (algorithm=="gradient" || algorithm=="g") zsuml2mingradient.loca.p(o, x, y, max.iter, eps, verbose)
   else if (algorithm=="search" || algorithm=="s") zsuml2minsearch.loca.p(o, x, y, max.iter, eps, verbose)
   else if (algorithm=="weiszfeld" || algorithm=="w") zsuml2minweiszfeld.loca.p(o, x, y, max.iter, eps, verbose)
   else stop(paste(algorithm, gettext("is not a valid value for algorithm parameter.\n")))
   }
)

zsuml2mingradient.loca.p <- function (o, x=0, y=0, max.iter=100, eps=1.e-3, verbose=FALSE)
   {
   lambda <- 1;
   eps2 <- eps^2
   u<-c(x,y)
   z <- zsum(o, u[1], u[2])
   for (i in 0:max.iter)
      {
      if (verbose) cat(paste(gettext("Iter."),i, ": (", u[1], ",", u[2], ") ", z, "\n", sep=""))
      g<-zsumgra(o, u[1], u[2])
      mg <- sum(g^2)
      # Check stop rule
      if (is.na(mg))
        {
        # A demand point stop rule
        g<-zsumgra(o, u[1], u[2], partial=T)
        mg <- sum(g^2)
        ii <- which.min((o@x-u[1])^2+(o@y-u[2])^2)
        if (mg < sum(o@w[ii]^2)) 
        	  {
        	  if(verbose) cat(gettext("Optimality condition reached at demand point.\n"));
        	  break
        	  }
        }
      else if (mg < eps2)
         {
         if(verbose) cat(gettext("Optimality condition reached.\n"));
         break;
         }
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
      }
   if (verbose && i == max.iter) cat(gettext("Maximun number of itereation reached.\n"));
   u
   }

zsuml2minsearch.loca.p <- function (o, x=0, y=0, max.iter=100, eps=1.e-3, verbose=FALSE)
   {
   eps2 <- eps^2
   lambda <- c(1, 1)
   u <- c(x, y)
   z <- zsum(o, x, y)
   nu <- u
   for(i in 0:max.iter)
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
      if (verbose) cat(paste(gettext("Iter."),i, ": (", u[1], ",", u[2], ") ", z, "\n", sep=""))
      if (sum(lambda^2) < eps2)
        {
        if(verbose) cat(gettext("Optimality condition reached.\n"));
        break;
        }
      }
   if (verbose && i == max.iter) cat(gettext("Maximun number of itereation reached.\n"));
   u
   }

zsuml2minweiszfeld.loca.p <- function (o, x=0, y=0, max.iter=100, eps=1.e-3, verbose=FALSE)
   {
   eps2 <- eps^2
   u<-c(x,y)
   for (i in 0:max.iter)
      {
      if (verbose) cat(paste(gettext("Iter."),i, ": (", u[1], ",", u[2], ") ", zsum(o, u[1], u[2]), "\n", sep=""))
      n <- o@w/sqrt((u[1]-o@x)^2+(u[2]-o@y)^2)
      ii <- is.finite(n)
      # Compute the gradient
      g <- c(sum((u[1]-o@x)*n), sum((u[2]-o@y)*n))
      mg <- sum(g^2)
      # Check stop rule
      if (is.na(mg))
         {
         # A demand point stop rule
         g <- c(sum((u[1]-o@x)*n, na.rm=T), sum((u[2]-o@y)*n, na.rm=T))
         mg <- sum(g^2)
         if (mg < sum(o@w[!ii]^2))
           {
           if(verbose) cat(gettext("Optimality condition reached at demand point.\n"));
           break
           }
         }
      else if (mg <eps2)
        {
        if(verbose) cat(gettext("Optimality condition reached.\n"));
        break
        }
      
      s <- sum(n[ii])
      nx <- n*o@x
      ny <- n*o@y
      u <- c(sum(nx[ii]), sum(ny[ii]))/s
      }
   if (verbose && i == max.iter) cat(gettext("Maximun number of itereation reached.\n"));
   u
   }
