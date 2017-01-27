#
# Functions for evaluation of objective function
# 
# zsum method definition
#   Evaluation of objective function
setGeneric("zsum", function(o, x=0, y=0) standardGeneric("zsum"))

# Definition of zsum for class loca.p
setMethod("zsum", "loca.p", function(o, x=0, y=0)
   {
   sum(o@w*sqrt((o@x-x)^2+(o@y-y)^2))
   }
)

# zsumgra method definition
#   Evaluation of the gradient
setGeneric("zsumgra", function(o, x=0, y=0) standardGeneric("zsumgra"))

# Definition os zsumgra for class loca.p
setMethod("zsumgra", "loca.p", function(o, x=0, y=0)
   {
   n<- o@w/sqrt((x-o@x)^2+(y-o@y)^2)
   c(sum((x-o@x)*n), sum((y-o@y)*n))
   }
)
