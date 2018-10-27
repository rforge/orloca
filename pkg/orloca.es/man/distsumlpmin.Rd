\name{distsumlpmin}
\alias{distsumlpmin}
\alias{distsumlpmin,loca.p-method}
\encoding{UTF-8}

\title{distsumlpmin en el paquete orloca}
\description{La \enc{función}{funcion} \code{distsummin} con norma
  \eqn{l_p}. Principalmente para uso interno.}
\section{Uso}{
distsumlpmin(o, x=0, y=0, p=2, max.iter=100, eps=1.e-3, 
   verbose=FALSE, algorithm="Weiszfeld", ...)
}
\section{Argumentos}{
  \describe{
  \item{o}{Un objeto de la clase \code{loca.p}.}
  \item{x}{La coordenada x del punto inicial.}
  \item{y}{La coordenada y del punto inicial.}
  \item{p}{Valor de p para la norma \eqn{l_p}.}
  \item{max.iter}{\enc{Número}{Numero} \enc{máximo}{maximo} de iteraciones permitido.}
  \item{eps}{El \enc{módulo}{modulo} del gradiente para la regla de
    parada.}
  \item{verbose}{Si es TRUE la \enc{función}{funcion} proporciona salida
    detallada.}
  \item{algorithm}{El algoritmo a utilizar. Para esta
    \enc{versión}{version} del paquete, los valores
    \enc{válidos}{validos} son: "gradient" para un algoritmo de
    gradiente, "search" para un algoritmo de
    \enc{búsqueda}{busqueda} local (esta \enc{opción}{opcion}
    \enc{está}{esta} obsoleta), "Weiszfeld" para el algoritmo de
    Weiszfeld o cualquiera de los \enc{métodos}{metodos}
    \enc{válidos}{validos} para la \enc{función}{funcion} optim, a
    saber, "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN". "Weiszfeld"
    es el valor por defecto.}
  \item{...}{Otras opciones para los algoritmos de
    optimizaci\enc{ó}{o}n.}
  }
}
\section{Detalles}{
Si \eqn{p<1} entonces \eqn{l_p} no es una norma, por tanto,
\enc{sólo}{solo} valores \eqn{p>=1} son \enc{válidos}{validos}.

Dado que \eqn{l_2} es la norma \enc{euclídea}{euclidea}, para \eqn{p=2}
  \code{distsumlpmin} es equivalente a \code{distsummin}. Pero los
  \enc{cálculos}{calculos} involucrados son mayores en la primera forma.

  La \enc{función}{funcion} zsumlpmin \enc{está}{esta} marcada como obsoleta
  y \enc{será}{sera} borrada de nuevas versiones del paquete.
  }

\section{Valor}{
  \code{distsummin} devuelve un vector con las coordenadas del punto \enc{solución}{solucion}.
}

\section{Ejemplos}{
# Un nuevo objeto loca.p
loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# Calcular el minimo
sol<-distsummin(loca)

# Mostrar el resultado
sol

# Evaluar la funcion en el punto solucion
distsum(loca, sol[1], sol[2])
}

\section{\enc{Véase}{Vease} \enc{también}{tambien}}{
  \code{Véase}{Vease} \enc{también}{tambien} \code{\link{distsummin}}, \code{\link{orloca.es-package}}, \code{\link{loca.p}} y \code{\link{distsum}}.
  }

\keyword{classes}
\keyword{optimize}
