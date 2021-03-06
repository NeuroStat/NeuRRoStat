% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corrH.R
\name{corrH}
\alias{corrH}
\title{Correction factor h}
\usage{
corrH(Ne, Nc = NULL, type = c("one.sample", "two.sample"))
}
\arguments{
\item{Ne}{sample size of the experimental group of subjects in this study
If only one sample is used, then use Ne as the sample size in the entire study.}

\item{Nc}{sample size of the control group of subjects in this study, NULL if type = "one.sample".}

\item{type}{a character specifying either one of c("one.sample", "two.sample").
Default is one.sample as this is usually the case in fMRI.}
}
\value{
An integer h between 0 and 1.
}
\description{
Helper function for \code{\link{hedgeG}}.
Calculates the exact correction factor for Hedges g
}
