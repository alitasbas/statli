% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{two_sample.prop_test}
\alias{two_sample.prop_test}
\title{Perform a two-sample Z-test for given sample proportions}
\usage{
two_sample.prop_test(
  x1,
  x2,
  n1,
  n2,
  alternative = c("two-sided", "greater", "less"),
  alpha = 0.05,
  confint = F
)
}
\arguments{
\item{x1}{The number of favorable observations in sample 1.}

\item{x2}{The number of favorable observations in sample 2.}

\item{n1}{Size of sample 1.}

\item{n2}{Size of sample 2.}

\item{alternative}{Type of alternative hypothesis ("two-sided," "less," or "greater").}

\item{alpha}{Significance level (default is 0.05).}

\item{confint}{Whether to calculate a confidence interval (default is FALSE).}
}
\value{
The test result (reject or fail to reject null hypothesis) and an optional confidence interval.
}
\description{
This function conducts a Z-test based on sample proportions. You can specify
the sample props, alternative hypothesis, significance level, and whether to
calculate a confidence interval.
}
