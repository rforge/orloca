\docType{methods}
\name{as-methods}
\alias{as-methods}
\alias{as.data.frame}
\alias{as.data.frame,loca.p-method}
\alias{as.data.frame.loca.p}
\alias{as.loca.p}
\alias{as.loca.p,data.frame-method}
\alias{as.loca.p,matrix-method}
\alias{as.loca.p.data.frame}
\alias{as.loca.p.matrix}
\alias{as.matrix}
\alias{as.matrix,loca.p-method}
\alias{as.matrix.loca.p}
\encoding{UTF-8}

\title{as-methods}
\section{Argumentos}{
  \describe{
  \item{x:}{es el objeto a convertir a la nueva clase.}

  \item{row.names:}{Sin uso.}

  \item{optional:}{Sin uso.}

  \item{...:}{Otros argumentos, sin uso.}
  }
}
\section{Valor}{
Si el argumento tiene un valor \enc{válido}{valido} devuelve un nuevo
objeto de la nueva clase.
}
\description{Conversiones entre la clase loca.p y algunas otras}
\section{Detalles}{Métodos para convertir desde y a la clase \code{loca.p}.

Valores NA no son permitidos en ningún argumento.

La \code{matrix} a convertir en \code{loca.p} debe tener al menos dos columnas. 
La primera \enc{será}{sera} considerada como la coordenada x, y la segunda como la coordenada y, y la tercera (si se ha suministrado) \enc{serán}{seran} los valores de w.

El \code{data.frame} a convertir a \code{loca.p} debe tener al menos una columna \code{x} para la coordenada x, y una columna \code{y} para la coordenada y.
Opcionalmente, puede tener una columna \code{w} para los valores de w.
}
\section{Ejemplos}{
# Un nuevo objeto loca.p

loca <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# Conversion a matrix

m <- as.matrix(loca)

# Muestra la matrix

m

# Conversion desde matrix

as.loca.p(m)
}
\section{\enc{Véase}{Vease} \enc{también}{tambien}}{
\enc{Véase}{Vease} \enc{también}{tambien} \code{\link{loca.p}}
}
\keyword{classes}
\keyword{methods}

