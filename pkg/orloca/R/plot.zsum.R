#
# Plots for objective function
#

contour.loca.p <- function(x, lp=numeric(0), xmin=min(x@x), xmax=max(x@x), ymin=min(x@y), ymax=max(x@y), n=100, ...)
   {
   .x<-seq(xmin, xmax, length.out=n)
   .y<-seq(ymin, ymax, length.out=n)
   .z<-matrix(1, nrow=n, ncol=n, byrow=TRUE)
   if (length(lp) == 0)
     {
       for(i in 1:n)
         for(j in 1:n)
           .z[i,j] <- zsum(x, .x[i], .y[j])
     }
   else if (lp >= 1)
     {
       for(i in 1:n)
         for(j in 1:n)
           .z[i,j] <- zsumlp(x, .x[i], .y[j], p=lp)
    }
   else stop(paste(lp, gettext("is not a valid value for lp, use 1 <= lp")))
   contour(.x, .y, .z, ...)
   invisible(1)
   }

persp.loca.p <- function(x, lp=numeric(0), xmin=min(x@x), xmax=max(x@x), ymin=min(x@y), ymax=max(x@y), n=100, ...)
   {
   .x<-seq(xmin, xmax, length.out=n)
   .y<-seq(ymin, ymax, length.out=n)
   .z<-matrix(1, nrow=n, ncol=n, byrow=TRUE)
   if (length(lp) == 0)
     {
       for(i in 1:n)
         for(j in 1:n)
           .z[i,j] <- zsum(x, .x[i], .y[j])
     }
   else if (lp >= 1)
     {
       for(i in 1:n)
         for(j in 1:n)
           .z[i,j] <- zsumlp(x, .x[i], .y[j], p=lp)
    }
   else stop(paste(lp, gettext("is not a valid value for lp, use 1 <= lp")))
   persp(.x, .y, .z, ...)
   invisible(1)
   }

