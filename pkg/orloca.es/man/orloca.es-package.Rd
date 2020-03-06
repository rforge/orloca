\name{orloca.es-package}
\alias{orloca.es-package}
\alias{orloca}
\alias{orloca.es}
\docType{package}
\encoding{UTF-8}

\title{Spanish version of orloca package - Versi\enc{ó}{o}n espa\enc{ñ}{n}ola del paquete orloca}
\description{Ayuda y demo en espa\enc{ñ}{n}ol del paquete orloca. 

  Objetos y m\enc{é}{e}todos para manejar y resolver el problema de
  localizaci\enc{ó}{o}n de suma ponderada m\enc{í}{i}nima, tambi\enc{é}{e}n
  conocido como problema de Fermat-Weber.}

\section{Detalles}{
  El problema de localizaci\enc{ó}{o}n de suma m\enc{í}{i}nima busca un
  punto tal que la suma ponderada de las distancias a los puntos de
  demanda se minimice. V\enc{é}{e}ase
  "The Fermat-Weber location problem revisited" por Brimberg,
  Mathematical Programming, 1, pag. 71-76, 1995. <DOI:
  10.1007/BF01592245>. 

  Se usan algoritmos generales de optimizaci\enc{ó}{o}n global para
  resolver el problema, junto con el m\enc{é}{e}todo adhoc Weiszfeld, \enc{véase}{vease}
  "Sur le point pour lequel la Somme des distance de n points donnes est minimum", por Weiszfeld, Tohoku Mathematical Journal, First Series, 43, pag. 355-386, 1937 o "On the point for which the sum of the distances to n given points is minimum", por E. Weiszfeld y F. Plastria, Annals of Operations Research, 167, pg. 7-41, 2009. <DOI:10.1007/s10479-008-0352-z>.

\preformatted{
Package:   orloca.es
Type:      Package
Version:   4.9
Date:      2020-03-06
License:   GPL (>= 3)
}
El paquete proporciona una clase, \code{loca.p}, que representa un problema de localizaci\enc{ó}{o}n con un conjunto finito de puntos de demanda sobre el plano. Tambi\enc{é}{e}n es posible representar los puntos y la funci\enc{ó}{o}n objetivo. Dicha funci\enc{ó}{o}n objetivo representa la suma de los desplazamientos de los usuarios al servicio.

El problema de localizaci\enc{ó}{o}n no plano ser\enc{á }{a } abordado en futuras versiones del paquete.

Para una demostraci\enc{ó}{o}n, cargue el paquete con \code{library(orloca.es)} y use \code{demo(orloca)}.

El paquete est\enc{á }{a } preparado para su
internacionalizaci\enc{ó}{o}n.
Las traducciones de los ficheros .mo recibidas ser\enc{á}{a}n a\enc{ñ}{n}adidas en pr\enc{ó}{o}ximas versiones del paquete.
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

\section{V\enc{é}{e}ase tambi\enc{é}{e}n}{For the English version of the package see \link[orloca]{orloca-package}.}

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
