\name{RcmdrPlugin.orloca.es}
\title{Una interfaz grafica para localizacion en el plano}
\alias{RcmdrPlugin.orloca.es}
\docType{package}
\encoding{UTF-8}

\description{
Este paquete proporciona una interfaz \enc{gráfica}{grafica} de usuario
para el paquete orloca como un \enc{añadido}{anadido} para Rcmdr
(R-Commander). El paquete trata sobre los problemas de \enc{localización}{localizacion} en
el plano.

Para poder utilizar toda la potencia de la interfaz Rcmdr, los datos de
los problemas de \enc{localización}{localizacion} se almacenan como objetos
\code{data.frame}. Estos objetos \code{data.frame} se convierten al
vuelo en objetos \code{loca.p} (el tipo de objetos que maneja el
paquete \code{orloca}. El \code{data.frame} debe tener al menos tres
columnas: \code{x}, \code{y}, y \code{w}, sin valores de
NA. \enc{Véase}{Vease} \code{\link[orloca]{loca.p}} para \enc{más}{mas}
detalles.

Las principales funciones del paquete \code{orloca} paquetes \enc{están}{estan}
disponibles en la interfaz. Estas funciones incluyen: la
\enc{manipulación}{manipulacion} de datos, la \enc{gráfica}{grafica} de
los puntos de demanda y la \enc{función}{funcion} objetivo, y la \enc{resolución}{resolucion} del
problema de \enc{localización}{localizacion}.

Las funciones de \enc{computación}{computacion} paralela proporcionadas
por el paquete \code{orloca} no \enc{están}{estan} disponibles en la interfaz \enc{gráfica}{grafica}.
}
\details{
\tabular{ll}{
Paquete: \tab RcmdrPlugin.orloca\cr
Tipo: \tab Paquete\cr
Version: \tab 4.6\cr
Fecha: \tab 2018-07-06\cr
Licencia: \tab GPL (>= 3)\cr
}
}

\author{
Manuel \enc{Muñoz-Márquez}{Munoz-Marquez} <manuel.munoz@uca.es>

Mantenedor: Manuel \enc{Muñoz-Márquez}{Munoz-Marquez} <manuel.munoz@uca.es>
}

\keyword{package}
\seealso{
Para \enc{más}{mas} \enc{información}{informacion} \enc{véase}{vease} \code{\link[Rcmdr]{Rcmdr-package}} y \code{\link[orloca]{orloca-package}}.
}

