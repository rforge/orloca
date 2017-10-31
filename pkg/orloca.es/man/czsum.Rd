\name{czsum}
\alias{czsum}
\alias{czsum,loca.p-method}
\alias{czsumgra}
\alias{czsumgra,loca.p-method}
\encoding{UTF-8}


\title{czsum y czsumgra en el paquete orloca}
\description{
 Funciones \code{zsum} y \code{zsumgra} calculadas en el \enc{clúster}{cluster} snow dado.
}
\usage{
czsum(o, cluster, x=0, y=0)
czsumgra(o, cluster, x=0, y=0)
}
\arguments{
  \item{o}{Un objeto de la clase \code{loca.p}.}
  \item{cluster}{Un objeto snow cluster.}
  \item{x}{La coordenada x del punto para ser evaluada.}
  \item{y}{La coordenada y del punto para ser evaluada.}
}
\value{
  \code{czsum} devuelve la \enc{función}{funcion} objetivo del problema
  de \enc{localización}{localizacion} min-sum, \eqn{\sum_{a_i \in o}
    w_i d(a_i, (x,y))}, donde \eqn{d(a_i, (x,y))} representa la
  distancia \enc{euclídea}{euclidea} entre \eqn{a_i} y \eqn{(x,y)}. 

  \code{czsumgra} devuelve el vector gradiente de la \enc{función}{funcion} \code{zsum}.

  El \enc{cálculo}{calculo} se realiza en el \enc{clúster}{cluster} snow dado.
}
%\examples{
%# An unweighted new loca.p object
%loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
%
%# Evaluation of zsum at (0, 0)
%zsum(loca)
%
%# Evaluation of zsum at (1, 3)
%zsum(loca, 1, 3)
%
%# The gradient function
%zsumgra(loca, 1, 3)
%}
\keyword{classes}
\keyword{optimize}
\seealso{
  Ver \url{http://www.stat.uiowa.edu/~luke/R/cluster/cluster.html} para
  \enc{información}{informacion} sobre \enc{computación}{computacion} en clusters.

  \enc{Véase}{Vease} \enc{también}{tambien} \code{\link{zsum}}, \code{\link{zsumgra}} y \code{\link{zsummin}}
  }