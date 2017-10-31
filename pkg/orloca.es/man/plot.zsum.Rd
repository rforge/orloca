\name{plot.zsum}
\alias{plot.zsum}
\alias{contour.loca.p}
\alias{contour,loca.p-method}
\alias{persp.loca.p}
\alias{persp,loca.p-method}
\encoding{UTF-8}

\title{Grafica de la funcion objetivo min-sum}
\description{
  Proporciona dos representaciones \enc{gráficas}{graficas} de la \enc{función}{funcion} objetivo del problema min-sum (\code{zsum}).
}
\usage{
\method{contour}{loca.p}(x, xmin=min(x@x), xmax=max(x@x), ymin=min(x@y), ymax=max(x@y), n=100, \ldots)
\method{persp}{loca.p}(x, xmin=min(x@x), xmax=max(x@x), ymin=min(x@y), ymax=max(x@y), n=100, \ldots)
}
\arguments{
  \item{x}{El objeto \code{loca.p} para calcular el objetivo.}
  \item{xmin}{El valor mínimo del eje x.}
  \item{xmax}{El valor máximo del eje x.}
  \item{ymin}{El valor mínimo del eje y.}
  \item{ymax}{El valor máximo del eje y.}
  \item{n}{El número de divisiones para la rejilla.}
  \item{\ldots}{Otras opciones.}
}
\value{
  La \enc{función}{funcion} \code{contour} representa un \enc{gráfico}{grafico} de curvas de nivel y la \enc{función}{funcion} \code{persp} un \enc{gráfico}{grafico} 3D.
}
\examples{
# Un objeto loca.p no ponderado
loca <- new("loca.p", x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# El gráfico de curvas de nivel del objeto loca
contour(loca)

# El gráfico 3D del objeto loca
persp(loca)
}
\keyword{classes}
\keyword{hplot}
\seealso{\enc{Véase}{Vease} \enc{también}{tambien} \code{\link{orloca-package}}, \code{\link[orloca]{plot.loca.p}} y \code{\link[orloca]{loca.p}}.}
