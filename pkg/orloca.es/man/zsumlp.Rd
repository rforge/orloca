\name{zsumlp}
\alias{zsumlp}
\alias{zsumlp,loca.p-method}
\alias{zsumlpgra}
\alias{zsumlpgra,loca.p-method}
\encoding{UTF-8}

\title{zsumlp y zsumlpgra del paquete orloca}
\description{Las funciones zsum y zsumgra con norma \eqn{l_p}.}
\usage{
zsumlp(o, x=0, y=0, p=2)
zsumlpgra(o, x=0, y=0, p=2, partial=F)
}
\arguments{
  \item{o}{An object of \code{loca.p} class.}
  \item{x}{The x coordinate of the point to be evaluated.}
  \item{y}{The y coordinate of the point to be evaluated.}
  \item{p}{The \eqn{l_p} norm to use.}
  \item{partial}{If (x,y) is a demand point \code{partial=T} means
      ignore such point to compute the gradient. This option is mainly for internal use.}
}
\value{
  \code{zsumlp} returns the objective function of the min-sum location
  problem with \eqn{l_p} norm, \eqn{\sum_{a_i \in o} w_i d(a_i, (x,y))}, where \eqn{d(a_i, (x,y))} gives the
  distances between \eqn{a_i} and the point \eqn{(x,y)} using \eqn{l_p} norm. 

  \code{zsumlpgra} returns the gradient vector of the function \code{zsumlp}.
}
\details{
If \eqn{p<1} then \eqn{l_p} ara not a norm, so only \eqn{p>=1} are valid values.
}
\note{Since \eqn{l_2}  norm is the Euclidean norm, when \eqn{p=2}
  \code{zsumlp} are equal to \code{zsum}, and \code{zsumlpgra} are equal
to \code{zsumgra}. But the computations involved are greater for the
firsts form.}

\keyword{classes}
\keyword{optimize}
\seealso{
  \enc{Véase}{Vease} \code{\link{zsum}}, \code{\link{orloca.es-package}} y \code{\link{zsumlpmin}}.
  }