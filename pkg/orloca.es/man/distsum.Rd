\name{distsum}
\alias{distsum}
\alias{distsum,loca.p-method}
\encoding{UTF-8}

\title{distsum y distsumgra del paquete orloca}
\description{La \enc{función}{funcion} objetivo para el problema de
  \enc{localización}{localizacion} min-sum.
}
\section{Uso}{
distsum(o, x=0, y=0, lp=numeric(0))
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
  \code{distsum} devuelve la \enc{función}{funcion} objetivo para el
  problema de \enc{localización}{localizacion} min-sum, \eqn{\sum_{a_i
      \in o} w_i d(a_i, (x,y))}, donde \eqn{d(a_i, (x,y))} es la
  distancia \enc{euclídea}{euclidea} o la distancia \eqn{l_p} entre \eqn{a_i} y \eqn{(x,y)}. 
}
\section{Detalles}{
  La \enc{función}{funcion} zsum \enc{está}{esta} marcada como obsoleta
  y \enc{será}{sera} borrada de nuevas versiones del paquete.
}
\section{\enc{Véase}{Vease} \enc{también}{tambien}}{
  \enc{Véase}{Vease} \enc{también}{tambien} \code{\link{orloca.es-package}} y \code{\link{distsummin}}.
  }
\section{Ejemplos}{
# Un nuevo objeto loca.p sin pesos
loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# Evaluacion de distsum en (0, 0)
distsum(loca)

# Evaluacion de distsum at (1, 3)
distsum(loca, 1, 3)

# Calculo de la fucion objetivo en el punto (3, 4) usando la norma lp con p = 2.5
distsum(loca, 3, 4, lp=2.5)

# La funcion gradiente en (1,3)
distsumgra(loca, 1, 3)
}
\keyword{classes}
\keyword{optimize}
