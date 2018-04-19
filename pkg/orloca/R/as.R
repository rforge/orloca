#' Conversions between loca.p class and some others
#'
#' Methods to convert from and to \code{loca.p} class.
#' @aliases as.data.frame as.data.frame.loca.p as.data.frame,loca.p-method as.loca.p as.loca.p.data.frame as.loca.p,data.frame-method as.loca.p.matrix as.loca.p,matrix-method as.matrix as.matrix.loca.p as.matrix,loca.p-method as-methods
#' @docType methods
#' @name as-methods
#' @title as-methods
#' @details
#' NA's  values are not allowed in any of the arguments.
#'
#' The \code{matrix} to convert into \code{loca.p} must have at least two columns.
#' The first column will be consider as the x coordinates, the second as the y coordinates, and the third (if given) as the values of w.
#'
#' The \code{data.frame} to convert into \code{loca.p} must have at least an \code{x} column for x coordinates, and an \code{y} column for y coordinates. Optionally, it can have \code{w} column, as the values of w.
#'
#' @return If the arguments have valid values, it returns a new object of the new class.
#' @keywords classes methods
#' @seealso See also \code{\link{loca.p}}
#' @examples
#' # A new unweighted loca.p object
#' loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
#'
#' # Conversion to matrix
#' m <- as.matrix(loca)
#'
#' # Show matrix
#' m
#'
#' # Conversion from matrix
#' as.loca.p(m)
#' @param x is the object to convert to the new class object.
#' @param row.names Unused.
#' @param optional Unused.
#' @param ... Other arguments, unused.

setAs("loca.p", "data.frame",
      function(from, to) {
          df <- data.frame("w" = from@w, "x" = from@x, "y" = from@y)
          attr(df, 'label') <- from@label
          df
      }
      )

setAs("data.frame", "loca.p",
      function(from, to)
          new("loca.p", w = from$w, x = from$x, y = from$y, label = ifelse(is.null(attr(from, 'label')), '', attr(from, 'label')))
      )


setAs("loca.p", "matrix",
      function(from, to) {
          m <- cbind(from@x, from@y, from@w)
          attr(m, 'label') <- from@label
          m
      }
      )

setAs("matrix", "loca.p",
      function(from, to) {
          label <- ifelse(is.null(attr(from, 'label')), '', attr(from, 'label'))
          if (dim(from)[2] == 2) loca.p(x=from[,1], y=from[,2], label = label)
          else if (dim(from)[2] == 3) loca.p(x=from[,1], y=from[,2], w=from[,3], label = label)
          else stop(gettext("The second dimension of matrix must be 2 or 3.", domain = "R-orloca"))
      }
      )

#
# The following is for S3 compatibility, mainly for documentation check
#
setGeneric("as.loca.p", function(x, ...) standardGeneric("as.loca.p"))
#' @S3method as.loca.p matrix
as.loca.p.matrix <- function(x,...) as(x, "loca.p")
setMethod("as.loca.p", "matrix", as.loca.p.matrix)
#' @S3method as.loca.p data.frame
as.loca.p.data.frame <- function(x, ...) as(x, "loca.p")
setMethod("as.loca.p", "data.frame", as.loca.p.data.frame)
#' @S3method as.matrix loca.p
as.matrix.loca.p <- function(x, rownames.force = NA, ...) as(x, "matrix")
#setMethod("as.matrix", "loca.p", as.matrix.loca.p)
#' @S3method as.data.frame loca.p
as.data.frame.loca.p <- function(x, row.names = NULL, optional = FALSE, ...) as(x, "data.frame")
#setMethod("as.data.frame", "loca.p", as.data.frame.loca.p)
