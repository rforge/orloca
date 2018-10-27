\name{distsuml2min}
\alias{distsuml2min}
\alias{distsuml2min,loca.p-method}
\encoding{UTF-8}

\title{distsuml2min en el paquete orloca}
\description{La \enc{función}{funcion} \code{distsummin} para la norma
  \enc{euclídea}{euclidea} (\eqn{l_2}). Principalmente para uso interno.}
\section{Uso}{
distsuml2min(o, x=0, y=0, max.iter=100, eps=0.001, verbose=FALSE,
   algorithm="Weiszfeld", ...)
}
\section{Argumentos}{
  \describe{
  \item{o}{Un objeto de la clase \code{loca.p}.}
  \item{x}{La coordenada x del punto inicial.}
  \item{y}{La coordenada y del punto inicial.}
  \item{max.iter}{\enc{Número}{Numero} \enc{máximo}{maximo} de iteraciones permitido.}
  \item{eps}{La norma del gradiente en la regla de parada.}
  \item{verbose}{Si es TRUE la \enc{función}{funcion} proporciona salida
    detallada.}
  \item{algorithm}{El algoritmo a utilizar. Los valores \enc{válidos}{validos} son: "gradient" para un algoritmo de gradiente, "search" para un algoritmo de \enc{búsqueda}{busqueda} local (esta
    \enc{opción}{opcion} \enc{está}{esta} obsoleta), "Weiszfeld" para el algoritmo de Weiszfeld o cualquiera de los \enc{métodos}{metodos} \enc{válidos}{validos} para la
    \enc{función}{funcion} optim, a saber, "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN". 
    "Weiszfeld" es el valor por defecto.}
  \item{...}{Otras opciones para los algoritmos de
    optimizaci\enc{ó}{o}n.}
  }
}
\section{Valor}{
  \code{distsummin} devuelve un vector con las coordenadas del punto \enc{solución}{solucion}.
}
\section{Detalles}{
  Los algoritmos de Weiszfeld y gradiente incluyen un test de
  optimalidad para los puntos de demanda. El algoritmo de Weiszfeld
  \enc{también}{tambien} implementa un test de convergencia lenta y un
  procedimiento acelerador.
  
  Si \eqn{p < 1} entonces \eqn{l_p} no es una norma, por ello, solo se
  admiten valores \eqn{p \ge 1}.

  \eqn{l_2} es la norma \enc{euclídea}{euclidea}, cuando \eqn{p=2}
  \code{distsumlpmin} es igual a \code{distsuml2min}. Pero los
  \enc{cálculos}{calculos} involucrados en la primera forma son mucho
  mayores.

  max.iter en el algoritmo SANN es el \enc{número}{numero} de
  evaluaciones de la función objetivo, por lo que este
  \enc{método}{metodo} requiere de valores grandes de max.iter para
  alcanzar el \enc{óptimo}{optimo}.

    La \enc{función}{funcion} zsuml2min \enc{está}{esta} marcada como obsoleta
  y \enc{será}{sera} borrada de nuevas versiones del paquete.
}
\section{\enc{Véase}{Vease} \enc{también}{tambien}}{
  Vea \enc{también}{tambien} \code{\link{orloca.es-package}}, \code{\link{loca.p}} y \code{\link{distsum}}.
  }
\keyword{classes}
\keyword{optimize}
