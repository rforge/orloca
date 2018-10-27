\name{as.matrix.loca.p}
\alias{as.matrix.loca.p}
\encoding{UTF-8}

\title{as.matrix.loca.p M\enc{é}{e}todo para convertir de loca.p a matriz}
\section{Argumentos}{
  \describe{
  \item{x:}{es el objeto a convertir a la nueva clase.}

  \item{row.names:}{Sin uso.}

  \item{optional:}{Sin uso.}

  \item{...:}{Otros argumentos, sin uso.}
  }
}
\section{Valor}{
Si el argumento tiene un valor v\enc{á}{a}lido devuelve un nuevo objeto de la nueva clase.
}
\description{Conversiones entre la clase loca.p y algunas otras}
\section{Detalles}{M\enc{é}{e}todos para convertir desde y a la clase \code{loca.p}.

No se permiten valores NA en ning\enc{ú}{u}n argumento.

La \code{matrix} a convertir en \code{loca.p} debe tener al menos dos columnas. 
La primera ser\enc{á }{a } considerada como la coordenada x, y la segunda como la coordenada y, y la tercera (si se ha suministrado) ser\enc{á}{a}n los valores de w.

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
\section{V\enc{é}{e}ase tambi\enc{é}{e}n}{
V\enc{é}{e}ase tambi\enc{é}{e}n \code{\link{loca.p}}
}
\keyword{classes}
\keyword{methods}

