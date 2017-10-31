\name{zsuml2}
\alias{zsuml2}
\alias{zsuml2,loca.p-method}
\alias{zsuml2gra}
\alias{zsuml2gra,loca.p-method}
\encoding{UTF-8}

\title{zsuml2 y zsuml2gra del paquete orloca}
\description{Las funciones \code{zsum} y \code{zsumgra} para la norma
  \enc{euclídea}{euclidea} (\eqn{l_2}). Principalmente para uso interno.}
\usage{
zsuml2(o, x=0, y=0)
zsuml2gra(o, x=0, y=0, partial=F)
}
\arguments{
  \item{o}{Un objeto de la clase \code{loca.p}.}
  \item{x}{La coordenada x del punto a ser evaluado.}
  \item{y}{La coordenada y del punto a ser evaluado.}
  \item{partial}{Si (x,y) es un punto de demanda \code{partial=T}
    significa que se ignore dicho punto para el \enc{cálculo}{calculo}
    del gradiente. Principalmente para uso interno.}
}
\value{
  \code{zsuml2} devuelve la \enc{función}{funcion} objetivo para el problema de \enc{localización}{localizacion} min-sum, \eqn{\sum_{a_i \in o} w_i d(a_i, (x,y))}, donde \eqn{d(a_i, (x,y))} es la distancia \enc{euclídea}{euclidea} entre \eqn{a_i} y \eqn{(x,y)}. 

  \code{zsumgra} devuelve el vector gradiente de la \enc{función}{funcion} \code{zsum}.
}
\keyword{classes}
\keyword{optimize}
\seealso{
  \enc{Véase}{Vease} \enc{también}{tambien} \code{\link{orloca.es-package}}, \code{\link{zsum}}, \code{\link{zsumgra}} y \code{\link{zsummin}}.
}
