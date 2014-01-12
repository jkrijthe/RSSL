\name{GenerateSlicedCookie}
\alias{GenerateSlicedCookie}
\title{Generate Sliced Cookie dataset}
\usage{
GenerateSlicedCookie(n=100,expected=FALSE)
}
\arguments{
  \item{n}{number of observations to generate}

  \item{expected}{TRUE if the large margin equals the class
  boundary, FALSE if the class boundary is perpendicular to
  the large margin}
}
\value{
A data.frame with n objects from the sliced cookie example
}
\description{
Generate a sliced cookie dataset: a circle with a large
margin in the middle.
}

