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
\method{plot}{loca.p}(x, xlab="", ylab="", main="Plot of loca.p object", ...)
}
\arguments{
  \item{x}{El objeto \code{loca.p} a representar.}
  \item{xlab}{La etiqueta para el eje x.}
  \item{ylab}{La etiqueta para el eje y.}
  \item{main}{El \enc{título}{titulo} principal del \enc{gráfico}{grafico}.}
  \item{...}{Otras opciones gráficas.}
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
