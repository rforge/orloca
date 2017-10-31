\name{orloca.es-package}
\alias{orloca.es-package}
\alias{orloca}
\alias{orloca.es}
\docType{package}
\encoding{UTF-8}

\title{Spanish version of orloca package - Version espanola del paquete orloca}
\description{Ayuda y demo en
  espa\enc{ñ}{n}nol del paquete orloca. 

  Objetos y m\enc{é}{e}todos para manejar y resolver el problema de
  localizaci\enc{ó}{o}n de suma m\enc{i}{i}nima, tambi\enc{é}{e}n
  conocido como problema de Fermat-Weber.}

\section{Detalles}{
  El problema de localizaci\enc{ó}{o}n de suma m\enc{i}{i}nima busca un punto tal que la suma ponderada de las distancias a los puntos de demanda se minimice. V\enc{é}{e}ase "The Fermat-Weber location problem revisited" por Brimberg, Mathematical Programming, 1, pag. 71-76, 1995. <DOI: 10.1007/BF01592245>. Los algoritmos generales de optimizaci\enc{ó}{o}n global se usan para resolver el problema, junto con el m\enc{é}{e}todo adhoc Weiszfeld, ver "Sur le point pour lequel la Somme des distance de n points donnes est minimum", por Weiszfeld, Tohoku Mathematical Journal, First Series, 43, pag. . 355-386, 1937.

\preformatted{
Package:   orloca.es
Type:      Package
Version:   4.3
Date:      2017-11-02
License:   GPL (>= 3)
}
El paquete proporciona una clase (\code{loca.p}) que representa un problema de \enc{localización}{localizacion} con un conjunto finito de puntos de demanda sobre el plano. \enc{También}{Tambien} es posible representar los puntos y la \enc{función}{funcion} objetivo. Dicha \enc{función}{funcion} objetivo representa la suma de los desplazamientos de los usuarios a un servicio.

El problema de \enc{localización}{localizacion} no plano
\enc{será}{sera} abordado en futuras versiones del paquete.

Para una \enc{demostración}{demostracion}, cargue el paquete con \code{library(orloca.es)} y use \code{demo(orloca)}.

El paquete está preparado para su internacionalización. Las traducciones
de los ficheros .mo recibidas serán añadidas en próximas versiones del paquete.
}
\section{Autor}{
Manuel  Munoz-Marquez <manuel.munoz@uca.es>
 
Mantenedor: Manuel Munoz-Marquez <manuel.munoz@uca.es>
}
\section{Referencias}{
[1] Brimberg, J. \emph{The Fermat-Weber location problem revisited}, Mathematical Programming, 1, pg. 71-76, 1995. \url{https://doi.org/10.1007/BF01592245}.
  
[2] Love, R. F., Morris, J. G., Wesolowsky, G. O. \emph{Facilities Location: Chapter 2: Introduction to Single-Facility Location}, 1988, North-Holland

[3] \url{http://knuth.uca.es/orloca}
}
\keyword{package}
\keyword{optimize}

\section{\enc{Véase}{Vease} \enc{también}{tambien}}{For the English version of the package see \link[orloca]{orloca-package}.}

\section{Ejemplos}{
# Un objeto loca.p no ponderado
o <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# Calcula la funcion objetivo en el punto (3, 4)
zsum(o, 3, 4)

# Calcula ls suma de las distancias al punto (3, 4) usando la norma lp
zsum(o, 3, 4, lp=2.5)

# Resuelve el problema de localizacion
zsummin(o)

# Curvas de nivel
contour(o)

# Ejecuta una demo del paquete
demo(orloca)
}
