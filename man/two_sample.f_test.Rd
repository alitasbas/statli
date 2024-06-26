% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{two_sample.f_test}
\alias{two_sample.f_test}
\title{Perform an F-test for two sample variances}
\usage{
two_sample.f_test(
  v1,
  v2,
  n1,
  n2,
  alternative = c("two-sided", "greater", "less"),
  confint = F,
  alpha = 0.05
)
}
\arguments{
\item{v1}{The variance of sample 1.}

\item{v2}{The variance of sample 2.}

\item{n1}{Size of sample 1.}

\item{n2}{Size of sample 2.}

\item{alternative}{Type of alternative hypothesis ("two-sided," "less," or "greater").}

\item{confint}{Whether to calculate a confidence interval (default is FALSE).}

\item{alpha}{Significance level (default is 0.05).}
}
\value{
The test result (reject or fail to reject null hypothesis) and an optional confidence interval.
}
\description{
This function conducts an F-test based on sample variances. You can specify the sample variances,
sample sizes, alternative hypothesis, significance level, and whether to
calculate a confidence interval.
}
