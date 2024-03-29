\name{as.function.mvp}
\alias{as.function.mvp}
\title{Functional form for multivariate polynomials}
\description{Coerces a multivariate polynomial into a function}
\usage{
\method{as.function}{mvp}(x, ...)
}
\arguments{
  \item{x}{Multivariate polynomial}
  \item{...}{Further arguments (currently ignored)}
}
\author{Robin K. S. Hankin}
\examples{

p <- as.mvp("1+a^2 + a*b^2 + c")
p
f <- as.function(p)
f

f(a=1)
f(a=1,b=2)
f(a=1,b=2,c=3)             # coerces to a scalar
f(a=1,b=2,c=3,lose=FALSE)  # formal mvp object
}

