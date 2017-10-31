\name{plot.zsum}
\alias{plot.zsum}
\alias{contour.loca.p}
\alias{contour,loca.p-method}
\alias{persp.loca.p}
\alias{persp,loca.p-method}
\encoding{UTF-8}

\title{Grafica de la funcion objetivo min-sum}
\description{
  \code{contour} y \code{persp} proporcionan dos representaciones \enc{gráficas}{graficas} de la \enc{función}{funcion} objetivo del problema min-sum (\code{zsum}).
}
\usage{
\method{contour}{loca.p}(x, lp=numeric(0), xmin=min(min(x@x), xleft),
   xmax=max(max(x@x), xright), ymin=min(min(x@y), ybottom),
   ymax=max(max(x@y), ytop), n=100, img=NULL, xleft=min(x@x),
   ybottom=min(x@y), xright=max(x@x), ytop=max(x@y), \ldots)
\method{persp}{loca.p}(x, lp=numeric(0), xmin=min(x@x), xmax=max(x@x),
   ymin=min(x@y), ymax=max(x@y), n=100, \ldots)
}
\arguments{
  \item{x}{El objeto \code{loca.p} para calcular el objetivo.}
  \item{lp}{Si se proporciona, entonces se usa la norma \eqn{l_p} en vez de la \enc{euclídea}{euclidea}.}
  \item{xmin}{El valor mínimo del eje x.}
  \item{xmax}{El valor máximo del eje x.}
  \item{ymin}{El valor mínimo del eje y.}
  \item{ymax}{El valor máximo del eje y.}
  \item{n}{El número de divisiones para la rejilla.}
  \item{img}{Una imagen en formato raster para el fondo.}
  \item{xleft}{\enc{Posición}{Posicion} del borde izquierdo de la imagen.}
  \item{ybottom}{\enc{Posición}{Posicion} del borde inferior de la imagen.}
  \item{xright}{\enc{Posición}{Posicion} del borde derecho de la imagen.}
  \item{ytop}{\enc{Posición}{Posicion} del borde superior de la imagen.}
  \item{\ldots}{Otras opciones.}
}
\value{
  La \enc{función}{funcion} \code{contour} representa un \enc{gráfico}{grafico} de curvas de nivel y la \enc{función}{funcion} \code{persp} un \enc{gráfico}{grafico} 3D.
}
\details{Si \eqn{p < 1} entonces \eqn{l_p} no es norma, por tanto, \enc{sólo}{solo} \eqn{p \ge 1} es \enc{válido}{valido}.}
\examples{
# Un objeto loca.p no ponderado
loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# El gráfico de curvas de nivel del objeto loca.p
contour(loca)

# El gráfico 3D del objeto loca.p
persp(loca)
}
\keyword{classes}
\keyword{hplot}
\seealso{\enc{Véase}{Vease} \enc{también}{tambien} \code{\link{orloca.es-package}}, \code{\link{plot.loca.p}} y \code{\link{loca.p}}.}
