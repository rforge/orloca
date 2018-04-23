\name{zsum}
\alias{zsum}
\alias{zsum,loca.p-method}
\encoding{UTF-8}

\title{zsum y zsumgra del paquete orloca}
\description{La \enc{función}{funcion} objetivo para el problema de
  \enc{localización}{localizacion} min-sum.
}
\section{Uso}{
zsum(o, x=0, y=0, lp=numeric(0))
}
\section{Argumentos}{
  \describe{
  \item{o:}{Un objeto de la clase \code{loca.p}}
  \item{x:}{La coordenada x del punto a ser evaluado}
  \item{y:}{La coordenada y del punto a ser evaluado}
  \item{lp:}{Si se proporciona, entonces se usa la norma \eqn{l_p} en vez
    de la \enc{euclídea}{euclidea}}
  }
}
\section{Valor}{
  \code{zsum} devuelve la \enc{función}{funcion} objetivo para el
  problema de \enc{localización}{localizacion} min-sum, \eqn{\sum_{a_i
      \in o} w_i d(a_i, (x,y))}, donde \eqn{d(a_i, (x,y))} es la
  distancia \enc{euclídea}{euclidea} o la distancia \eqn{l_p} entre \eqn{a_i} y \eqn{(x,y)}. 
}
\section{\enc{Véase}{Vease} \enc{también}{tambien}}{
  \enc{Véase}{Vease} \enc{también}{tambien} \code{\link{orloca.es-package}} y \code{\link{zsummin}}.
  }
\section{Ejemplos}{
# Un nuevo objeto loca.p sin pesos

loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# Evaluacion de zsum en (0, 0)

zsum(loca)

# Evaluacion de zsum at (1, 3)

zsum(loca, 1, 3)

# Calculo de la fucion objetivo en el punto (3, 4) usando la norma lp con p = 2.5

zsum(loca, 3, 4, lp=2.5)

# La funcion gradiente en (1,3)

zsumgra(loca, 1, 3)
}
\keyword{classes}
\keyword{optimize}
