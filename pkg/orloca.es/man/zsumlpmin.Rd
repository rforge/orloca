\name{zsumlpmin}
\alias{zsumlpmin}
\alias{zsumlpmin,loca.p-method}
\encoding{UTF-8}

\title{zsumlpmin en el paquete orloca}
\description{La \enc{función}{funcion} \code{zsummin} con norma
  \eqn{l_p}. Principalmente para uso interno.}
\usage{
zsumlpmin(o, x=0, y=0, p=2, max.iter=100, eps=1.e-3, 
   verbose=FALSE, algorithm="weiszfeld", ...)
}
\arguments{
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
    \enc{válidos}{validos} son: "gradient" o "g" para un algoritmo de
    gradiente, "search" o "s" para un algoritmo de
    \enc{búsqueda}{busqueda} local (esta \enc{opción}{opcion}
    \enc{está}{esta} obsoleta), "weiszfeld" o "w" para el algoritmo de
    Weiszfeld o cualquiera de los \enc{métodos}{metodos}
    \enc{válidos}{validos} para la \enc{función}{funcion} optim, a
    saber, "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN". "Weiszfeld"
    es el valor por defecto.}
  \item{...}{Otras opciones para los algoritmos de optimizaci\enc{ó}{o}n.}
}
\details{
Si \eqn{p<1} entonces \eqn{l_p} no es una norma, por tanto, \enc{sólo}{solo} valores \eqn{p>=1} son \enc{válidos}{validos}.
}
\value{
  \code{zsummin} devuelve un vector con las coordenadas del punto \enc{solución}{solucion}.
}
\note{Dado que \eqn{l_2} es la norma \enc{euclídea}{euclidea}, para \eqn{p=2}
  \code{zsumlpmin} es equivalente a \code{zsummin}. Pero los \enc{cálculos}{calculos} involucrados son mayores en la primera forma.
  }
\keyword{classes}
\keyword{optimize}
\seealso{
  \code{Véase}{Vease} \enc{también}{tambien} \code{\link{zsummin}}, \code{\link{orloca.es-package}}, \code{\link{loca.p}} y \code{\link{zsum}}.
  }