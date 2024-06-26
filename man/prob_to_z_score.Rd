% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{prob_to_z_score}
\alias{prob_to_z_score}
\title{Convert area to corresponding Z-score}
\usage{
prob_to_z_score(area, mu = 0, sd = 1, lower.tail = T, plot = T)
}
\arguments{
\item{area}{The probability}

\item{mu}{mean of the distribution}

\item{sd}{standard dev of the distribution}

\item{lower.tail}{whether to look at the positive or negative side}
}
\value{
The corresponding Z-score and a plot
}
\description{
This function returns the Z-score associated with probability. You can have
it as lower or upper tail. You can also change the distribution. It returns
a beautiful plot as well
}
