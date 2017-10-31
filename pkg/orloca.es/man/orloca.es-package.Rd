\name{orloca.es-package}
\alias{orloca.es-package}
\alias{orloca}
\alias{orloca.es}
\docType{package}
\encoding{UTF-8}

\title{
  Spanish version of orloca package - Version espanola del paquete orloca}
\description{
  El paquete ofrece modelos de \enc{Análisis}{Analisis} de \enc{Localización}{Localizacion}.
  Esta \enc{versión}{version} aborda problemas de \enc{localización}{localizacion} min-sum. 
 El problema min-sum, \enc{también}{tambien} conocido como problema de Weber, localiza un punto tal que la suma ponderada de las distancias entre dicho punto y los puntos de demanda sea \enc{mínima}{minima}.
}
\details{
\preformatted{
Package:   orloca.es
Type:      Package
Version:   3.2
Date:      2010-05-22
License:   GPL (>= 2)
}
El paquete proporciona una clase (\code{loca.p}) que representa un problema de \enc{localización}{localizacion} con un conjunto finito de puntos de demanda sobre el plano. \enc{También}{Tambien} es posible representar los puntos y la \enc{función}{funcion} objetivo. Dicha \enc{función}{funcion} objetivo representa la suma de los desplazamientos de los usuarios a un servicio.

Para una \enc{demostración}{demostracion}, cargue el paquete con \code{library(orloca.es)} y use \code{demo(orloca)}.

El paquete está preparado para su internacionalización. Las traducciones
de los ficheros .mo recibidas serán añadidas en próximas versiones del paquete.
}
\author{
Fernando Fernandez-Palacin <fernando.fernandez@uca.es> and Manuel
 Munoz-Marquez <manuel.munoz@uca.es>
 
Maintainer: Manuel Munoz-Marquez <manuel.munoz@uca.es>
}
\references{
[1] Love, R. F., Morris, J. G., Wesolowsky, G. O. \emph{Facilities Location: Chapter 2: Introduction to Single-Facility Location}, 1988, North-Holland

[2] \url{http://knuth.uca.es/orloca}
}
\keyword{package}
\keyword{optimize}

\section{Index:}{
  \tabular{ll}{
\code{\link{loca.p}}: \tab \enc{descripción}{descripcion} de la clase loca.p.\cr
\code{\link{rloca.p}}: \tab instancias aleatorias de objetos de la clase \code{loca.p}.\cr
\code{\link{zsum}}: \tab \enc{función}{funcion} para evaluar la función objetivo para modelos min-sum.\cr
\code{\link{zsummin}}: \tab para encontrar el \enc{mínimo}{minimo} del problema de localización.\cr
\code{\link{plot.loca.p}}: \tab para hacer \enc{gráficos}{graficos} de los objetos de la clase \code{loca.p}.\cr
\code{\link{plot.zsum}}: \tab para hacer \enc{gráficos}{graficos} de la \enc{función}{funcion} objetivo.\cr
}
}

\seealso{For the English version of the package see \link[orloca]{orloca-package}.}

\examples{
# Un objeto loca.p no ponderado
o <- new("loca.p", x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# Calcula la función objetivo en el punto c(3, 4)
zsum(o, 3, 4)

# Resuelve el problema de localización
zsummin(o)

# Curvas de nivel
contour(o)

# Ejecuta una demo del paquete
demo(orloca)
}
