#
# Random instances generator of loca.p class object
#
rloca.p <- function (n, xmin=0, xmax=1, ymin=0, ymax=1)
   {
   new("loca.p", x=runif(n, xmin, xmax), y=runif(n, ymin, ymax))
   }
