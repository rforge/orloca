\docType{methods}
\name{distsumgra}
\alias{distsumgra}
\alias{distsumgra,loca.p-method}
\encoding{UTF-8}

\title{Calculo del gradiente de la funcion distsum}

\description{\code{distsumgra} calcula el
gradiente de la fuci\enc{ó}{o}n distsum}

\section{Uso}{
distsumgra(o, x = 0, y = 0, lp = numeric(0), partial = F)
}

\section{Argumentos}{
  \describe{
    \item{o}{Un objeto de clase \code{loca.p}.}
    \item{x}{La coordenada x del punto a evaluar.}
    \item{y}{La coordenada y del punto a evaluar.}
    \item{lp}{Si se proporciona, la norma \eqn{l_p} ser\enc{á}{a} usada en vez de
  la norma eucl\enc{í}{i}dea.}
    \item{partial}{Si (x,y) es un punto de demanda \code{partial=T}
  significa que se ignore dicho punto para el c\enc{á}{a}lculo del
  gradiente. Esta opci\enc{ó}{o}n es principalmente para uso interno.}
}
}

\section{Valor}{
\code{distsumgra} devuelve el vector gradiente de la funci\enc{ó}{o}n
min-sum del problema de localizaci\enc{ó}{o}n, \eqn{\sum_{a_i \in o} w_i
  d(a_i, (x,y))}, donde \eqn{d(a_i, (x,y))} da la distancia
eucl\enc{í}{i}dea o la distancia \eqn{l_p} entre \eqn{a_i} y el punto \eqn{(x,y)}.
}
\section{Detalles}{
  La \enc{función}{funcion} zsumgra \enc{está}{esta} marcada como obsoleta
  y \enc{será}{sera} borrada de nuevas versiones del paquete.
}
\section{Ejemplos}{
# Un nuevo objeto loca.p no ponderado
loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# Evaluacion de distsum en (0, 0)
distsum(loca)

# Evaluacion de distsum en (1, 3)
distsum(loca, 1, 3)

# Calculo de la funcion objetivo en el punto (3, 4) usando la norma lp con p = 2.5
distsum(loca, 3, 4, lp=2.5)

# El gradiente de la funcion en el punto (1,3)
distsumgra(loca, 1, 3)

}
\seealso{
V\enc{é}{e}ase \code{\link{orloca-package}} y \code{\link{distsum}}.
}
\keyword{classes}
\keyword{optimize}
