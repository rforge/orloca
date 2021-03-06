\name{loca.p-class}
\docType{class}
\alias{loca.p}
\alias{print}
\alias{summary}
\alias{initialize}
\encoding{UTF-8}

\title{Clase de objetos loca.p para Localizacion en Investigacion Operativa}
\description{
  Un objeto de la clase \code{loca.p} representa un problema de
  \enc{localización}{localizacion} ponderado con un conjunto finito de
  puntos de demanda en el plano. 
 El \code{\link{orloca.es-package}} \enc{está}{esta} principalmente
 dedicado a abordar problemas de \enc{localización}{localizacion} plana.
}

\section{Argumentos}{
\describe{
  \item{x:}{es un vector con las coordenadas x de los puntos de demanda}
  \item{y:}{es un vector con las coordenadas y de los puntos de demanda}
  \item{w:}{es un vector de pesos de los puntos de demanda. Si \code{w} se omite
    entonces todos los pesos se consideran iguales a 1}
  \item{label:}{Si se explicita, es la etiqueta del nuevo objeto}
}
}
\section{Detalles}{
  El principal generador es \code{loca.p(x, y, w = numeric(0), label = "")} o alternativamente  \code{new("loca.p", x, y, w = numeric(0), label = "")}.

  Las longitudes de los vectores \code{x} e \code{y} deben ser iguales. La longitud de \code{w} debe ser igual a los anteriores o 0. Los valores NA no \enc{están}{estan} permitidos en ninguno de los argumentos.
}

\section{Valor}{
  Si los argumentos son valores \enc{válidos}{validos}, devuelve un objeto de la clase
  \code{loca.p}, en caso contrario devuelve un error.
  \code{summary(x)} devuelve un resumen del objeto \code{x} de la clase \code{loca.p} y
    \code{print(x)} imprime el objeto \code{x} de la clase \code{loca.p}
  en formato tabla.
  }
  
\section{\enc{Véase}{Vease} \enc{también}{tambien}}{\enc{Véase}{Vease} \enc{también}{tambien} \code{\link{orloca.es-package}}.}

\section{Ejemplos}{
# Un objeto loca.p sin pesos
loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# o
loca <- new("loca.p", x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# Un ejemplo con pesos y nombre
locb <- new("loca.p", x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1), w = c(1, 2, 1, 2), label = "Caso Ponderado")
}
\keyword{classes}
\keyword{optimize}
