#
# Graphics for loca.p
#
plot.loca.p <- function(x, xlab="", ylab="", main=gettext("Plot of loca.p object"), ...)
   {
   plot(x@x, x@y, xlab=xlab, ylab=ylab, main=main, ...)
   }
