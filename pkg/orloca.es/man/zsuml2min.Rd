\name{zsuml2min}
\alias{zsuml2min}
\alias{zsuml2min,loca.p-method}
\encoding{UTF-8}

\title{zsuml2min en el paquete orloca}
\description{La \enc{función}{funcion} \code{zsummin} para la norma \enc{euclídea}{euclidea} (\eqn{l_2}).}
\usage{
zsuml2min(o, x=0, y=0, max.iter=100, eps=1.e-3, verbose=FALSE,
   algorithm="weiszfeld")
}
\arguments{
  \item{o}{Un objeto de la clase \code{loca.p}.}
  \item{x}{La coordenada x del punto inicial.}
  \item{y}{La coordenada y del punto inicial.}
  \item{max.iter}{\enc{Número}{Numero} \enc{máximo}{maximo} de iteraciones permitido.}
  \item{eps}{La norma del gradiente en la regla de parada.}
  \item{verbose}{Si es TRUE la \enc{función}{funcion} proporciona salida
    detallada.}
  \item{algorithm}{El algoritmo a utilizar. Los valores \enc{válidos}{validos} son: "gradient" o "g" para un algoritmo de gradiente, "search" o "s"
    para un algoritmo de \enc{búsqueda}{busqueda} local, y "weiszfeld" o "w" para el algoritmo de Weiszfeld. 
   "Weiszfeld" es el valor por defecto.}
}
\value{
  \code{zsummin} devuelve un vector con las coordenadas del punto \enc{solución}{solucion}.
}
\examples{
# Un objeto loca.p no ponderado
loca <- new("loca.p", x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# Calcula el mínimo
sol<-zsummin(loca)

# Muestra los resultados
sol

# Evaluación de la función objetivo en el punto solución
zsum(loca, sol[1], sol[2])
}
\keyword{classes}
\keyword{optimize}
\seealso{
  Vea también \code{\link{orloca.es-package}}, \code{\link{loca.p}} y \code{\link{zsum}}.
  }