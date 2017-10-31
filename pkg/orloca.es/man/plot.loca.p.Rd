\name{plot-methods}
\docType{methods}
\alias{plot-methods}
\alias{plot,loca.p-method}
\alias{plot.loca.p}
\encoding{UTF-8}

\title{Grafico de un objeto de la clase loca.p}
\description{
  Este \enc{método}{metodo} proporciona una \enc{representación}{representacion} \enc{gráfica}{grafica} de un objeto de la clase \code{loca.p}.
}
\usage{
\method{plot}{loca.p}(x, xlab="", ylab="", main=gettext("Plot of loca.p object"),
   img=NULL, xlim=c(min(xleft, min(x@x)), max(xright, max(x@x))),
   ylim=c(min(ybottom, min(x@y)), max(ytop, max(x@y))),
   xleft=min(x@x), ybottom=min(x@y), xright=max(x@x),
   ytop=max(x@y), \ldots)
}
\arguments{
  \item{x}{El objeto \code{loca.p} a representar.}
  \item{xlab}{La etiqueta para el eje x.}
  \item{ylab}{La etiqueta para el eje y.}
  \item{main}{El \enc{título}{titulo} principal del \enc{gráfico}{grafico}.}
  \item{img}{Una imagen en formato raster para el fondo.}
  \item{xlim}{Limite sobre el eje x del \enc{gráfico}{grafico}.}
  \item{ylim}{Limite sobre el eje y del \enc{gráfico}{grafico}.}
  \item{xleft}{\enc{Posición}{Posicion} del borde izquierdo de la imagen.}
  \item{ybottom}{\enc{Posición}{Posicion} del borde inferior de la imagen.}
  \item{xright}{\enc{Posición}{Posicion} del borde derecho de la imagen.}
  \item{ytop}{\enc{Posición}{Posicion} del borde superior de la imagen.}
  \item{\ldots}{Otras opciones gráficas.}
}
\details{
  Gráfico de los puntos de demanda con \enc{límites}{limitex} de \enc{evaluación}{evaluacion} \enc{automáticos}{automaticos}.
}
\value{
  La \enc{representación}{representacion} \enc{gráfica}{grafica} de los puntos de demanda.
}
\examples{
# Un objeto de la clase loca.p no ponderado
loca <- new("loca.p", x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# El gráfico del objeto loca.p
plot(loca)
}
\keyword{classes}
\keyword{hplot}
\seealso{\enc{Véase}{Vease} \enc{también}{tambien} \code{\link{orloca.es-package}},
  \code{\link{loca.p}} y \code{\link{plot.zsum}}.}
