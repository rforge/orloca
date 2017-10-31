\name{rloca.p}
\alias{rloca.p}
\encoding{UTF-8}

\title{Generador de instancias aleatorias de objetos de la clase loca.p}
\description{
  Devuelve una instancia aleatoria de un objeto de la clase \code{loca.p} en una determinada \enc{región}{region}.
}
\usage{
rloca.p(n, xmin=0, xmax=1, ymin=0, ymax=1)
}
\arguments{
  \item{n}{El \enc{número}{numero} de puntos de demanda.}
  \item{xmin}{\enc{Mínimo}{Minimo} valor para la coordenada x de los puntos de demanda.}
  \item{xmax}{\enc{Máximo}{Maximo} valor para la coordenada x de los puntos de demanda.}
  \item{ymin}{\enc{Mínimo}{Minimo} valor para la coordenada y de los puntos de demanda.}
  \item{ymax}{\enc{Máximo}{Maximo} valor para la coordenada y de los puntos de demanda.}
}
\details{
  \code{n} debe ser al menos 1.

  \code{xmin} debe ser menor o igual que \code{xmax}.

  \code{ymin} debe ser menor o igual que \code{ymax}.
}
\value{
  Si los argumentos son valores \enc{válidos}{validos}, devuelve un nuevo objeto de la clase
  \code{loca_p}, en otro caso informa de un error.
}
\examples{
# Un objeto aleatorio loca.p en el cuadrado unidad con 5 puntos de demanda
rloca.p(5)

# En otra región
rloca.p(10, xmin=-2, xmax=2, ymin=-2, ymax=2)
}
\keyword{datagen}
\seealso{\enc{Véase}{Vease} \enc{también}{tambien} \code{\link{orloca-package}} y \code{loca.p}}
