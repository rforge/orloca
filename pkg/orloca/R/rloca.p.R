#
# Random instances generator of loca.p class object
#
rloca.p <- function (n, xmin=0, xmax=1, ymin=0, ymax=1, groups=0, xgmin=xmin, xgmax=xmax, ygmin=ymin, ygmax=ymax)
   {
     if (!is.numeric(groups)) stop(paste(gettext("Parameter groups must be numeric.\n")))
     if (identical(groups, 0))
       {
         new("loca.p", x=runif(n, xmin, xmax), y=runif(n, ymin, ymax))
       }
     else if (identical(length(groups),  1))
       {
         x = numeric(0)
         y = numeric(0)
         w = numeric(0)
         for(i in 1:groups)
           {
             gx <- runif(1, xmin, xmax)
             gy <- runif(1, xmin, xmax)
             gn <- floor(n/(groups-i+1))
             n <- n - gn
             x <- c(x, gx + runif(gn, xgmin, xgmax))
             y <- c(y, gy + runif(gn, ygmin, ygmax))
#             w <- c(w, rep(i,gn))
           }
#         new("loca.p", x=x, y=y, w=w)
         new("loca.p", x=x, y=y)
       }
     else
       {
         x = numeric(0)
         y = numeric(0)
         w = numeric(0)
         i <- 1
         for(gn in groups)
           {
             gx <- runif(1, xmin, xmax)
             gy <- runif(1, xmin, xmax)
             x <- c(x, gx + runif(gn, xgmin, xgmax))
             y <- c(y, gy + runif(gn, ygmin, ygmax))
#             w <- c(w, rep(i,gn))
             i <- i + 1
           }
#         new("loca.p", x=x, y=y, w=w)
         new("loca.p", x=x, y=y)
       }
   }
