\name{maize.individuals}
\alias{maize.individuals}
\docType{data}
\title{
    Maize_65 Individuals repartitions
}
\description{
    \code{Individuals-class} of 100 individuals in receptors fields of \code{maize.landscape}.
}
%\usage{data("maize.individuals")}
\format{
  \code{Individuals-class}.

  It contains 100 random individuals.
}
\details{
  It contains the simulation of 100 individuals in receptors fields of \code{maize.landscape}. All individuals have a life duration of 20 days within the simulation time [1-61].
  Individuals @intern_toxic come from \link{ecoToxic} function.
}
\examples{
data("maize.individuals")
plot(maize.individuals)
}
\keyword{datasets}
