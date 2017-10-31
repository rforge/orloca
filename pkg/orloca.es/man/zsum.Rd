\name{zsum}
\alias{zsum}
\alias{zsum,loca.p-method}
\alias{zsumgra}
\alias{zsumgra,loca.p-method}
\encoding{UTF-8}

\title{zsum y zsumgra del paquete orloca}
\description{La \enc{función}{funcion} objetivo y la \enc{función}{funcion} gradiente para el problema de \enc{localización}{localizacion} min-sum.
}
\usage{
zsum(o, x=0, y=0)
zsumgra(o, x=0, y=0)
}
\arguments{
  \item{o}{Un objeto de la clase \code{loca.p}.}
  \item{x}{La coordenada x del punto a ser evaluado.}
  \item{y}{La coordenada y del punto a ser evaluado.}
}
\value{
  \code{zsum} devuelve la \enc{función}{funcion} objetivo para el problema de \enc{localización}{localizacion} min-sum, \eqn{\sum_{a_i \in o} w_i d(a_i, (x,y))}, donde \eqn{d(a_i, (x,y))} es la distancia \enc{euclídea}{euclidea} entre \eqn{a_i} y \eqn{(x,y)}. 

  \code{zsumgra} devuelve el vector gradiente de la \enc{función}{funcion} \code{zsum}.
}
\examples{
# Un objeto loca.p no ponderado
loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# Evaluación de zsum en (0, 0)
zsum(loca)

# Evaluación de zsum en (1, 3)
zsum(loca, 1, 3)

# La función gradiente
zsumgra(loca, 1, 3)
}
\keyword{classes}
\keyword{optimize}
\seealso{
  \enc{Véase}{Vease} \enc{también}{tambien} \code{\link{orloca-package}} y \code{\link{zsummin}}.
  }