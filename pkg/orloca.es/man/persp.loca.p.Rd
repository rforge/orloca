\name{plot.zsum}
\alias{plot.zsum}
\alias{persp.loca.p}
\alias{persp,loca.p-method}
\encoding{UTF-8}

\title{Grafica de la funcion objetivo min-sum}
\description{
  \code{persp} proporciona la \enc{representación}{representacion} \enc{gráfica}{grafica} de la \enc{función}{funcion} objetivo del problema min-sum (\code{zsum}).
}
\section{Uso}{
  ## Metodo S3 para la clase \code{loca.p}
  
  persp(x, lp=numeric(0), xmin=min(x@x), xmax=max(x@x),
   ymin=min(x@y), ymax=max(x@y), n=100, \ldots)
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
  \item{\ldots}{Otras opciones.}
  }
}

\section{Detalles}{Si \eqn{p < 1} entonces \eqn{l_p} no es norma, por tanto, \enc{sólo}{solo} \eqn{p \ge 1} es \enc{válido}{valido}.}


\section{Valor}{
  Un \enc{gráfico}{grafico} 3D de la \enc{función}{funcion} min-sum.
}

\section{\enc{Véase}{Vease} \enc{también}{tambien}}{\enc{Véase}{Vease} \enc{también}{tambien} \code{\link{orloca.es-package}}, \code{\link{plot.loca.p}} y \code{\link{loca.p}}.}

\section{Ejemplos}{
# Un objeto loca.p sin pesos
loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# El grafico 3D del objeto loca.p
persp(loca)
}
\keyword{classes}
\keyword{hplot}
