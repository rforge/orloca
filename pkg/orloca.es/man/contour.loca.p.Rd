\name{contour.loca.p}
\alias{contour,loca.p-method}
\alias{contour.loca.p}
\encoding{UTF-8}

\title{Grafica de la funcion objetivo min-sum}
\description{
  \code{contour} proporcionan la \enc{representación}{representacion} \enc{gráfica}{grafica} de la \enc{función}{funcion} del problema min-sum (\code{zsum}).
}
\section{Uso}{
  ## \enc{Método}{Metodo} S3 para la clase 'loca.p'
  
  contour(x, lp = numeric(0), xmin = min(min(x@x), xleft),
  xmax = max(max(x@x), xright), ymin = min(min(x@y), ybottom),
  ymax = max(max(x@y), ytop), n = 100, img = NULL, xleft = min(x@x),
  ybottom = min(x@y), xright = max(x@x), ytop = max(x@y), ...)
}
\section{Argumentos}{
  \describe{
  \item{x:}{El objeto \code{loca.p} para calcular el objetivo.}
  \item{lp:}{Si se proporciona, entonces se usa la norma \eqn{l_p} en vez de la \enc{euclídea}{euclidea}.}
  \item{xmin:}{El valor mínimo del eje x.}
  \item{xmax:}{El valor máximo del eje x.}
  \item{ymin:}{El valor mínimo del eje y.}
  \item{ymax:}{El valor máximo del eje y.}
  \item{n:}{El número de divisiones para la rejilla.}
  \item{img:}{Una imagen en formato raster para el fondo.}
  \item{xleft:}{\enc{Posición}{Posicion} del borde izquierdo de la imagen.}
  \item{ybottom:}{\enc{Posición}{Posicion} del borde inferior de la imagen.}
  \item{xright:}{\enc{Posición}{Posicion} del borde derecho de la imagen.}
  \item{ytop:}{\enc{Posición}{Posicion} del borde superior de la imagen.}
  \item{\ldots:}{Otras opciones.}
  }
}

\section{Detalles}{Si \eqn{p < 1} entonces \eqn{l_p} no es norma, por
  tanto, \enc{sólo}{solo} \eqn{p \ge 1} es \enc{válido}{valido}.}

\section{Valor}{
  La \enc{función}{funcion} \code{contour.loca.p} representa un
  \enc{gráfico}{grafico} de curvas de nivel de la \enc{función}{funcion}
  min-sum (\code{zsum}).
}

\section{\enc{Véase}{Vease} \enc{también}{tambien}}{\enc{Véase}{Vease} \enc{también}{tambien} \code{\link{orloca.es-package}}, \code{\link{plot.loca.p}} y \code{\link{loca.p}}.}

\section{Ejemplos}{
# Un objeto loca.p sin pesos
loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# El grafico de curvas de nivel de la fucion min-sum para el objeto
contour(loca)

}
\keyword{classes}
\keyword{hplot}
