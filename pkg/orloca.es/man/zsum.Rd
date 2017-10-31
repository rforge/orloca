\name{zsum}
\alias{zsum}
\alias{zsum,loca.p-method}
\alias{zsumgra}
\alias{zsumgra,loca.p-method}
\encoding{UTF-8}

\title{zsum y zsumgra del paquete orloca}
\description{La \enc{función}{funcion} objetivo y la
  \enc{función}{funcion} gradiente para el problema de
  \enc{localización}{localizacion} min-sum. Para uso interno.
}
\usage{
zsum(o, x=0, y=0, lp=numeric(0))
zsumgra(o, x=0, y=0, lp=numeric(0), partial=F)
}
\arguments{
  \item{o}{Un objeto de la clase \code{loca.p}.}
  \item{x}{La coordenada x del punto a ser evaluado.}
  \item{y}{La coordenada y del punto a ser evaluado.}
  \item{lp}{Si se proporciona, entonces se usa la norma \eqn{l_p} en vez
    de la \enc{euclídea}{euclidea}.}
  \item{partial}{Si (x,y) es un punto de demanda \code{partial=T}
    significa que se ignore dicho punto para el \enc{cálculo}{calculo}
    del gradiente. Principalmente para uso interno.}
}
\value{
  \code{zsum} devuelve la \enc{función}{funcion} objetivo para el
  problema de \enc{localización}{localizacion} min-sum, \eqn{\sum_{a_i
      \in o} w_i d(a_i, (x,y))}, donde \eqn{d(a_i, (x,y))} es la
  distancia \enc{euclídea}{euclidea} o la distancia \eqn{l_p} entre \eqn{a_i} y \eqn{(x,y)}. 

  \code{zsumgra} devuelve el vector gradiente de la \enc{función}{funcion} \code{zsum}.
}
\keyword{classes}
\keyword{optimize}
\seealso{
  \enc{Véase}{Vease} \enc{también}{tambien} \code{\link{orloca.es-package}} y \code{\link{zsummin}}.
  }