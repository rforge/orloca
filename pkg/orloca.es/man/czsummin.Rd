\name{czsummin}
\alias{czsummin}
\alias{czsummin,loca.p-method}
\encoding{UTF-8}

\title{czsummin en el paquete orloca}
\description{
  Resuelve el problema de \enc{localización}{localizacion} min-sum para
  un objeto de la clase loca.p.
}
\usage{
czsummin(o, cluster, x=0, y=0, max.iter=100, eps=1.e-3,
   verbose=FALSE, algorithm="gradient")
}
\arguments{
  \item{o}{Un objeto de la clase loca.p.}
  \item{cluster}{Un objeto snow cluster.}
  \item{x}{La coordenada x del punto inicial.}
  \item{y}{La coordenada y del punto inicial.}
  \item{max.iter}{\enc{Máximo}{Maximo} \enc{número}{numero} de iteraciones permitido.}
  \item{eps}{El \enc{módulo}{modulo} del gradiente en la regla de parada.}
  \item{verbose}{Si es TRUE la \enc{función}{funcion} proporciona una salida
    detallada.}
  \item{algorithm}{El \enc{método}{metodo} que se \enc{usará}{usara}. Los valores \enc{válidos}{validos} son: "gradient" o "g"
    para el \enc{método}{metodo} del gradiente, y "search" o "s"
    para el \enc{método}{metodo} de busca local. "gradient" es el valor
    por defecto.}
}
\value{
  \code{czsummin} devuelve una matriz con las coordenadas del punto \enc{solución}{solucion}.

  Los \enc{cálculos}{calculos} se realizan en el cluster dado.
}
%\examples{
%# Un objeto loca.p no ponderado
%loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
%
%# Calcula el mínimo
%sol<-zsummin(loca)
%
%# Muestra los resultados
%sol
%
%# Evaluación de la función objetivo en el punto solución
%zsum(loca, sol[1], sol[2])
%}
\keyword{classes}
\keyword{optimize}
\seealso{
  Ver \url{http://www.stat.uiowa.edu/~luke/R/cluster/cluster.html} para
  \enc{información}{informacion} sobre \enc{computación}{computacion} en clusters.

  Ver \enc{también}{tambien} \code{\link{zsummin}}, \code{\link{loca.p}} y \code{\link{zsum}}.
  }