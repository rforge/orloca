# This function returns the solution of the minimization problem
setGeneric("zsumlpmin",
           function (o, x=0, y=0, p=2, max.iter=100, eps=1.e-3, verbose=FALSE, algorithm="weiszfeld") standardGeneric("zsumlpmin")
)

# Gradient Method
setMethod("zsumlpmin", "loca.p",
function (o, x=0, y=0, p=2, max.iter=100, eps=1.e-3, verbose=FALSE, algorithm="weiszfeld")
   {
     if (p>=1) {
       if (algorithm=="gradient" || algorithm=="g") zsumlpmingradient.loca.p(o, x, y, p, max.iter, eps, verbose)
       else if (algorithm=="search" || algorithm=="s") zsumlpminsearch.loca.p(o, x, y, p, max.iter, eps, verbose)
       else if (algorithm=="weiszfeld" || algorithm=="w") zsumlpminweiszfeld.loca.p(o, x, y, p, max.iter, eps, verbose)
   else stop(paste(algorithm, gettext("is not a valid value for algorithm parameter.\n")))
     }
   else stop(paste(p, gettext("is not a valid value for p, use 1 <= p")))
   }
)

zsumlpmingradient.loca.p <- function (o, x=0, y=0, p=2, max.iter=100, eps=1.e-3, verbose=FALSE)
   {
   lambda = 1;
   eps2 <- eps^2
   u<-c(x,y)
   z <- zsumlp(o, u[1], u[2], p)
   for (i in 0:max.iter)
      {
      if (verbose) cat(paste(gettext("Iter."),i, ": (", u[1], ",", u[2], ") ", z, "\n", sep=""))
      g <- zsumlpgra(o, u[1], u[2], p)
      mg <- sum(g^2)
      if (is.na(mg))
         {
         # A demand point stop rule
         g <- zsumlpgra(o, u[1], u[2], p, partial=T)
         q <- p/(p-1)
         mg <- sum(abs(g)^q)^(1/q)
         ii <- which.min((o@x-u[1])^2+(o@y-u[2])^2)
         if (mg < sum(o@w[ii]))
        	  {
        	  if(verbose) cat(gettext("Optimality condition reached at demand point.\n"));
        	  break
        	  }
         }
      else if (mg<eps2)
         {
         if(verbose) cat(gettext("Optimality condition reached.\n"));   
         break;
         }
      nu <- u - lambda*g
      nz <- zsumlp(o, nu[1], nu[2], p)
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

zsumlpminsearch.loca.p <- function (o, x=0, y=0, p=2, max.iter=100, eps=1.e-3, verbose=FALSE)
   {
   eps2 <- eps^2
   lambda <- c(1, 1)
   u <- c(x, y)
   z <- zsumlp(o, x, y, p)
   nu <- u
   for(i in 0:max.iter)
      {
      for (j in 1:2)
         {
         nu[j] <- u[j] + lambda[j]
         nz <- zsumlp(o, nu[1], nu[2], p)
         if (nz < z)
            {
            u <- nu
            z <- nz
            lambda[j] <- 2.2 * lambda[j]
            }
         else
            {
            nu[j] <- u[j] - lambda[j]
            nz <- zsumlp(o, nu[1], nu[2], p)
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
        break
        }
      }
   if (verbose && i == max.iter) cat(gettext("Maximun number of itereation reached.\n"));
   u
   }

zsumlpminweiszfeld.loca.p <- function (o, x=0, y=0, p=2, max.iter=100, eps=1.e-3, verbose=FALSE)
   {
   lambda = 1;
   eps2 <- eps^2
   u<-c(x,y)
   for (i in 0:max.iter)
      {
      if (verbose) cat(paste(gettext("Iter."),i, ": (", u[1], ",", u[2], ") ", zsumlp(o, u[1], u[2], p), "\n", sep=""))
      n <- o@w*(abs(u[1]-o@x)^p+abs(u[2]-o@y)^p)^(1/p-1)
      ii <- is.finite(n)
      # Compute the gradient
      g <- c(sum(sign(u[1]-o@x)*abs(u[1]-o@x)^(p-1)*n), sum(sign(u[2]-o@y)*abs(u[2]-o@y)^(p-1)*n))
      mg <- sum(g^2)
      # Check stop rule
      if (is.na(mg))
         {
         # A demand point stop rule
         g <- c(sum((u[1]-o@x)*n, na.rm=T), sum((u[2]-o@y)*n, na.rm=T))
         q <- p/(p-1)
         mg <- sum(abs(g)^q)^(1/q)
         if (mg < sum(o@w[!ii]))
           {
           if(verbose) cat(gettext("Optimality condition reached at demand point.\n"));
           break
           }
         }
      else if (mg<eps2)
        {
        if(verbose) cat(gettext("Optimality condition reached.\n"));
        break;
        }
      dx <- n*abs(u[1]-o@x)^(p-2)
      nx <- dx*o@x
      dy <- n*abs(u[2]-o@y)^(p-2)
      ny <- dy*o@y
      u[1] <- sum(nx[ii])/sum(dx[ii])
      u[2] <- sum(ny[ii])/sum(dy[ii])
      }
   if (verbose && i == max.iter) cat(gettext("Maximun number of itereation reached.\n"))
   u
   }
