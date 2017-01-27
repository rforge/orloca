#
# Plots for objective function
#

contour.loca.p <- function(x, xmin=min(x@x), xmax=max(x@x), ymin=min(x@y), ymax=max(x@y), n=100, ...)
   {
   .x<-seq(xmin, xmax, length.out=n)
   .y<-seq(ymin, ymax, length.out=n)
   .z<-matrix(1, nrow=n, ncol=n, byrow=TRUE)
   for(i in 1:n)
      for(j in 1:n)
         .z[i,j] <- zsum(x, .x[i], .y[j])
   contour(.x, .y, .z, ...)
   invisible(1)
   }

persp.loca.p <- function(x, xmin=min(x@x), xmax=max(x@x), ymin=min(x@y), ymax=max(x@y), n=100, ...)
   {
   .x<-seq(xmin, xmax, length.out=n)
   .y<-seq(ymin, ymax, length.out=n)
   .z<-matrix(1, nrow=n, ncol=n, byrow=TRUE)
   for(i in 1:n)
      for(j in 1:n)
         .z[i,j] <- zsum(x, .x[i], .y[j])
   persp(.x, .y, .z, ...)
   invisible(1)
   }

