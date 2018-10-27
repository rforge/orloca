\name{distsummin}
\alias{distsummin}
\alias{distsummin,loca.p-method}
\encoding{UTF-8}

\title{distsummin en el paquete orloca}
\description{
  Resuelve el problema de \enc{localización}{localizacion} min-sum para un objeto dado de la clase \code{loca.p}.
}
\section{Uso}{
distsummin(o, x=0, y=0, lp=numeric(0), max.iter=100, eps=1.e-3, 
   verbose=FALSE, algorithm="weiszfeld", \ldots)
}
\section{Argumentos}{
  \describe{
  \item{o}{Un objeto de la clase \code{loca.p}.}
  \item{x}{La coordenada x del punto inicial.}
  \item{y}{La coordenada y del punto inicial.}
  \item{lp}{Si se proporciona, la norma \eqn{l_p} se usa en vez de la norma \enc{euclídea}{euclidea}.}
  \item{max.iter}{\enc{Número}{Numero} \enc{máximo}{maximo} de iteraciones permitido.}
  \item{eps}{La norma del gradiente en la regla de parada.}
  \item{verbose}{Si es TRUE la \enc{función}{funcion} proporciona salida
    detallada.}
  \item{algorithm}{El algoritmo a utilizar. En esta
    \enc{versión}{version} del paquete los valores
    \enc{válidos}{validos} son: "gradient" o "g" para el
    \enc{método}{metodo} basado en gradiente, "search" o "s"
    para el \enc{método}{metodo} de \enc{búsqueda}{busqueda} local,
    "ucminf" o "u" para usar optimizar usando ucminf del paquete ucminf,
    y "weiszfeld" o "w" para el \enc{método}{metodo} de Weiszfeld o
    cualquier otro \enc{método}{metodo} \enc{válido}{valido} para la
    \enc{función}{funcion} optim, ahora "Nelder-Mead", "BFGS", "CG",
    "L-BFGS-B", "SANN". "weiszfeld" es el valor por defecto.}
  \item{\ldots}{Otras opciones para los algoritmos de \enc{optimización}{optimizacion}.}
}
}

\section{Detalles}{
 Los algoritmos de Weiszfeld y gradiente incluyen un test de
  optimalidad para los puntos de demanda. El algoritmo de Weiszfeld
  \enc{también}{tambien} implementa un test de convergencia lenta y un
  procedimiento acelerador.
  
  Si \eqn{p < 1} entonces \eqn{l_p} no es una norma, por tanto,
  \enc{sólo}{solo} \eqn{p \ge 1} es válido.

\eqn{l_2} es la norma \enc{euclídea}{euclidea}, cuando \eqn{p=2}
  \code{distsumlpmin} es igual a \code{distsuml2min}. Pero los
  \enc{cálculos}{calculos} involucrados en la primera forma son mucho
  mayores.

  max.iter en el algoritmo SANN es el \enc{número}{numero} de
  evaluaciones de la función objetivo, por lo que este
  \enc{método}{metodo} requiere de valores grandes de max.iter para
  alcanzar el \enc{óptimo}{optimo}.
  
    La \enc{función}{funcion} zsummin \enc{está}{esta} marcada como obsoleta
  y \enc{será}{sera} borrada de nuevas versiones del paquete.
  }

\section{Valor}{
  \code{distsummin} devuelve un vector con las coordenadas del punto \enc{solución}{solucion}.
}

\examples{
# Un objeto loca.p sin pesos
loca <- new("loca.p", x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

# Calcula el minimo
sol <- distsummin(loca)

# Muestra el resultado
sol

# Evaluacion de la funcion objetivo en el punto solucion
distsum(loca, sol[1], sol[2])
}

\section{\enc{Véase}{Vease} \enc{también}{tambien}}{
\code{Véase}{Vease} \enc{también}{tambien} \code{\link{orloca.es-package}}, \code{\link{loca.p}} y \code{\link{distsum}}.
}
