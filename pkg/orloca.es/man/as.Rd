\name{as-methods}
\docType{methods}
\alias{as-methods}
\alias{as.loca.p}
\alias{as.data.frame}
\alias{as.matrix}
\alias{as,loca.p-method}
\alias{as.loca.p,data.frame-method}
\alias{as.loca.p,matrix-method}
\alias{as.matrix,loca.p-method}
\alias{as.data.frame,loca.p-method}
\alias{as.loca.p.data.frame}
\alias{as.loca.p.matrix}
\alias{as.matrix.loca.p}
\alias{as.data.frame.loca.p}
\encoding{UTF-8}


\title{Conversion entre objetos de la clase loca.p y otros objetos}
\description{
  \enc{Métodos}{Metodos} para convertir desde y hacia la clase \code{loca.p}.
}
\usage{
%  \method{as}{loca.p}(from, "data.frame")
%  \method{as}{loca.p}(from, "matrix")
%  \method{as}{data.frame}(from, "loca.p")
%  \method{as}{matrix}(from, "loca.p")
  \method{as.loca.p}{data.frame}(x, ...)
  \method{as.loca.p}{matrix}(x, ...)
  \method{as.data.frame}{loca.p}(x, row.names = NULL, optional = FALSE, ...)
  \method{as.matrix}{loca.p}(x, ...)
}
\arguments{
%  \item{data.frame}{is a data.frame to convert to loca.p.}
%  \item{matrix}{is a matrix to convert to loca.p.}
%  \item{loca.p}{is a loca.p object to convert to\ldots}
  \item{x}{es el objeto para convertir a una nueva clase.}
  \item{row.names}{No usado.}
  \item{optional}{No usado.}
  \item{...}{Otros argumentos, no usados.}
}
\details{
  Valores NA's no \enc{están}{estan} permitidos en ninguno de los argumentos.
  
  La \code{matrix} a convertir en \code{loca.p} debe tener al menos dos columnas. 
  la primera columna \enc{será}{sera} considerada como la coordenada x,
  la segunda como la coordenada y, y la tercera (si existe) como los
  valores de w.

  El \code{data.frame} para convertir en \code{loca.p} debe tener al menos
  una columna \code{x} para la coordenada x, y una columna \code{y} para
  la coordenada y. Opcionalmente, puede tener una columna \code{w}, como
  los valores de w.
}

\value{
  Si los argumentos tienen valores \enc{válidos}{validos}, devuelve un
  nuevo objeto de la nueva clase.
  }

\examples{
# Un objeto loca.p no ponderado
loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# Conversión a matriz
m <- as.matrix(loca)
m

# Conversión desde una matriz
as.loca.p(m)
}
\keyword{classes}
\keyword{methods}
\seealso{Ver \enc{también}{tambien} \code{\link{loca.p}}.}
