\name{rloca.p}
\alias{rloca.p}
\encoding{UTF-8}

\title{Generador de instancias aleatorias de objetos de la clase loca.p}
\description{
  Devuelve una instancia aleatoria de un objeto de la clase
  \code{loca.p} en una \enc{región}{region} dada.
}
\section{Uso}{
rloca.p(n, xmin = 0, xmax = 1, ymin = 0, ymax = 1, groups = 0,
   xgmin = xmin, xgmax = xmax, ygmin = ymin, ygmax = ymax)
}
\section{Argumentos}{
  \describe{
  \item{n:}{El \enc{número}{numero} de puntos de demanda.}
  \item{xmin:}{\enc{Mínimo}{Minimo} valor para la coordenada x de los puntos de demanda.}
  \item{xmax:}{\enc{Máximo}{Maximo} valor para la coordenada x de los puntos de demanda.}
  \item{ymin:}{\enc{Mínimo}{Minimo} valor para la coordenada y de los puntos de demanda.}
  \item{ymax:}{\enc{Máximo}{Maximo} valor para la coordenada y de los puntos de demanda.}
  \item{groups:}{El \enc{número}{numero} de grupos (de aproximadamente
    igual \enc{tamaño}{tamano}) o una lista con los \enc{tamaños}{tamanos} de los
    grupos a generar. En el segundo caso \code{n} se ignora.}
  \item{xgmin:}{\enc{Mínimo}{Minimo} valor para la coordenada x de los
    puntos de demanda respecto del punto referencia del grupo.}
  \item{xgmax:}{\enc{Máximo}{Maximo} valor para la coordenada x de los
    puntos de demanda respecto del punto referencia del grupo.}
  \item{ygmin:}{\enc{Mínimo}{Minimo} valor para la coordenada y de los
    puntos de demanda respecto del punto referencia del grupo.}
  \item{ygmax:}{\enc{Máximo}{Maximo} valor para la coordenada y de los
    puntos de demanda respecto del punto referencia del grupo.}
  }
}
\section{Detalles}{
  \code{n} debe ser al menos 1.

  \code{xmin} debe ser menor o igual que \code{xmax}.

  \code{ymin} debe ser menor o igual que \code{ymax}.

  Cuando se suministra valor no nulo para \code{groups} los puntos se generan en
  dos fases, en la primera se genera un punto de referencia, en la
  segunda se genera un desplazamiento sobre dicho punto de referencia
  que se suma a \enc{éste}{este}.

  \enc{Obsérvese}{Observese} que \code{groups = 1} no es equivalente a
  \code{groups = 0}, debido a que en el primer caso se genera
  un punto de referencia en la primera etapa.
}

\section{Valor}{
  Si los argumentos son valores \enc{válidos}{validos}, devuelve un nuevo objeto de la clase
  \code{loca_p}, en otro caso informa de un error.
}

\section{\enc{Véase}{Vease} \enc{también}{tambien}}{\enc{Véase}{Vease} \enc{también}{tambien} \code{\link[orloca]{orloca-package}} y \code{\link{loca.p}}}

\section{Ejemplos}{
# Un objeto aleatorio loca.p en el cuadrado unidad con 5 puntos de demanda

rloca.p(5)

# En otra region

rloca.p(10, xmin=-2, xmax=2, ymin=-2, ymax=2)

# Cinco grupos

rloca.p(48, groups=5)

# Tres grupos de distinto tamano

rloca.p(1, groups=c(10, 7, 2))
}
\keyword{datagen}
