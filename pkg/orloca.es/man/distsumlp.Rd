\name{distsumlp}
\alias{distsumlp}
\alias{distsumlp,loca.p-method}
\alias{distsumlpgra}
\alias{distsumlpgra,loca.p-method}
\encoding{UTF-8}

\title{distsumlp y distsumlpgra del paquete orloca}
\description{Las funciones distsum y distsumgra con norma
  \eqn{l_p}. Principalmente para uso interno.}
\section{Uso}{
distsumlp(o, x=0, y=0, p=2)
distsumlpgra(o, x=0, y=0, p=2, partial=F)
}
\section{Argumentos}{
  \describe{
  \item{o}{Un objeto de clase \code{loca.p}.}
  \item{x}{La coordenada x del punto a ser evaluado.}
  \item{y}{La coordenada y del punto a ser evaluado.}
  \item{p}{La norma \eqn{l_p} a usar.}
  \item{partial}{Si (x,y) es un punto de demanda, \code{partial=T}
    significa que se ignore dicho punto para el \enc{cálculo}{calculo} del
    gradiente. Principalmente para uso interno.}
  }
}
\section{Valor}{
  \code{distsumlp} devuelve el valor de la \enc{función}{funcion} objetivo
  del problema de \enc{localización}{localizacion} min-sum con norma
  \eqn{l_p}, \eqn{\sum_{a_i \in o} w_i d(a_i, (x,y))}, donde \eqn{d(a_i,
    (x,y))} es la distancia entre \eqn{a_i} y el punto \eqn{(x,y)}
  usando la norma \eqn{l_p}. 

  \code{distsumlpgra} devuelve el vector gradiente de la \enc{función}{funcion} \code{distsumlp}.
}
\section{Details}{
Si \eqn{p<1} entonces \eqn{l_p} no es una norma, por tanto, \enc{sólo}{solo} valores
\eqn{p>=1} son \enc{válidos}{validos}.

Dado que \eqn{l_2} es la norma \enc{euclídea}{euclidea}, cuando \eqn{p=2}
  \code{distsumlp} es igual a \code{distsum}, y \code{distsumlpgra} es igual a
  \code{distsumgra}. Pero los \enc{cálculos}{calculos} necesarios son
  mayores para la primera forma.

  La \enc{función}{funcion} zsumlp \enc{está}{esta} marcada como obsoleta
  y \enc{será}{sera} borrada de nuevas versiones del paquete.
}

\section{\enc{Véase}{Vease} \enc{también}{tambien}}{
  \enc{Véase}{Vease} \enc{también}{tambien} \code{\link{distsum}}, \code{\link{orloca.es-package}} y \code{\link{distsumlpmin}}.
}

\keyword{classes}
\keyword{optimize}
