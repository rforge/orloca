#
# Functions for evaluation of objective function at clusters
# 
# czsum method definition
#   Evaluation of objective function
setGeneric("czsum", function(o, cluster, x=0, y=0) standardGeneric("czsum"))

# Definition of czsum for class loca.p
setMethod("czsum", "loca.p", function(o, cluster, x=0, y=0)
   {
#   sum(o@w*sqrt((o@x-x)^2+(o@y-y)^2))
     l <- split.loca.p(loca.p(x=o@x-x, y=o@y-y, w=o@w), length(cluster))
#   sum(apply(m, 1, l2))
     sum(unlist(parLapply(cluster, l, zsum)))
   }
)

# czsumgra method definition
#   Evaluation of the gradient
setGeneric("czsumgra", function(o, cluster, x=0, y=0) standardGeneric("czsumgra"))

# Definition os czsumgra for class loca.p
setMethod("czsumgra", "loca.p", function(o, cluster, x=0, y=0)
   {
   l <- split.loca.p(loca.p(x=o@x-x, y=o@y-y, w=o@w), length(cluster))
   sum(unlist(parLapply(cluster, l, zsumgra)))
   l <- parLapply(cluster, l, zsumgra)
   g <- c(0, 0)
   for (i in 1:length(cluster)) g <- g + l[[i]]
   return(g)
   }
)

# Auxiliary functions
l2 <- function(x) x[1]*sqrt(x[2]^2+x[3]^2)
