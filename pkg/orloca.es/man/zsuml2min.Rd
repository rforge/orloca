\name{zsuml2min}
\alias{zsuml2min}
\alias{zsuml2min,loca.p-method}
\encoding{UTF-8}

\title{zsuml2min en el paquete orloca}
\description{La \enc{función}{funcion} \code{zsummin} para la norma
  \enc{euclídea}{euclidea} (\eqn{l_2}). Principalmente para uso interno.}
\usage{
zsuml2min(o, x=0, y=0, max.iter=100, eps=0.001, verbose=FALSE,
   algorithm="weiszfeld", ...)
}
\arguments{
  \item{o}{Un objeto de la clase \code{loca.p}.}
  \item{x}{La coordenada x del punto inicial.}
  \item{y}{La coordenada y del punto inicial.}
  \item{max.iter}{\enc{Número}{Numero} \enc{máximo}{maximo} de iteraciones permitido.}
  \item{eps}{La norma del gradiente en la regla de parada.}
  \item{verbose}{Si es TRUE la \enc{función}{funcion} proporciona salida
    detallada.}
  \item{algorithm}{El algoritmo a utilizar. Los valores \enc{válidos}{validos} son: "gradient" o "g" para un algoritmo de gradiente, "search" o "s" para un algoritmo de \enc{búsqueda}{busqueda} local (esta
    \enc{opción}{opcion} \enc{está}{esta} obsoleta), "weiszfeld" o
    "w" para el algoritmo de Weiszfeld o cualquiera de los
    \enc{métodos}{metodos} \enc{válidos}{validos} para la
    \enc{función}{funcion} optim, a saber, "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN". 
    "Weiszfeld" es el valor por defecto.}
  \item{...}{Otras opciones para los algoritmos de optimizaci\enc{ó}{o}n.}
}
\value{
  \code{zsummin} devuelve un vector con las coordenadas del punto \enc{solución}{solucion}.
}
\keyword{classes}
\keyword{optimize}
\seealso{
  Vea \enc{también}{tambien} \code{\link{orloca.es-package}}, \code{\link{loca.p}} y \code{\link{zsum}}.
  }