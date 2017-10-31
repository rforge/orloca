\name{zsummin}
\alias{zsummin}
\alias{zsummin,loca.p-method}
\encoding{UTF-8}

\title{zsummin en el paquete orloca}
\description{
  Reuselve el problema de \enc{localización}{localizacion} min-sum para un objeto dado de la clase \code{loca.p}.
}
\usage{
zsummin(o, x=0, y=0, max.iter=100, eps=1.e-3, verbose=FALSE,
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
  \item{algorithm}{El \enc{método}{metodo} a utilizar. Los valores \enc{válidos}{validos} son: "gradient" o "g" para el \enc{método}{metodo} basado en el gradiente, "search" o "s"
    para \enc{método}{metodo} de busca local, y "weiszfeld" o "w" para el \enc{método}{metodo} de Weiszfeld. 
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
  Vea también \code{\link{orloca-package}}, \code{\link{loca.p}} y \code{\link{zsum}}.
  }