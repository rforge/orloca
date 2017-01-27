#
# Class loca.p definition
#
setClass("loca.p",
	representation(x="numeric", y="numeric", w="numeric", label="character")
	)

#
# loca.p Validity method
#
setValidity("loca.p", 
   function(object)
      {
      if(length(object@x)==length(object@y))
         {
	 if (length(object@x)==length(object@w) || length(object@w)==0)
	    {
            if (sum(is.na(object@x))+sum(is.na(object@y)+sum(is.na(object@w)))==0) TRUE
            else paste("Valores NA no permitidos", sep="")
	    }
	 else paste("La longitud de w (", length(object@w), ") debe ser la de x, y (", length(object@x) ,") o 0")
         }
      else paste("Diferentes longitudes de x, y: ", length(object@x), ", ", length(object@y), sep="")
      }
   )

#
# loca.p initialize method
#
setMethod("initialize", "loca.p",  
   function(.Object, x, y, w = numeric(0), label="")
      {
      .Object@x <- x
      .Object@y <- y
      if (length(w) == 0) .Object@w <- rep(1,length(x))
      else .Object@w <- w
      .Object@label <- label
      validObject(.Object)
      .Object
      }
)

loca.p <- function(x, y, w = numeric(0), label="") new("loca.p", x, y, w, label)


#
# loca.p summary method
#
setMethod("summary", "loca.p",
   function(object, ...)
          {
            c("label"=object@label, "n"=length(object@x), "xmin"=min(object@x), "xmean"=mean(object@x), "xmax"=max(object@x), "ymin"=min(object@y), "ymean"=mean(object@y), "ymax"=max(object@y))
            }
          )
          

#
# loca.p print method
#
setMethod("print", "loca.p",
   function(x, ...)
      {
      print(summary(x), ...)
      invisible(x)
      }
)
