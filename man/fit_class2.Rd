% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.R
\name{fit_class2}
\alias{fit_class2}
\title{Fit and train some classification models with optional recursive feature elimination
and preprocessing steps}
\usage{
fit_class2(data, catVar, model, rfe = FALSE, preprocess = FALSE)
}
\arguments{
\item{data}{a dataset with explanatory variables and response variable}

\item{catVar}{the response variable}

\item{model}{string specifiying which classification model will be used.}

\item{rfe}{logical value whether to apply variable selection with recursive feature elimination algorythm}

\item{preprocess}{logical value whether the preprocessing steps have to be done on the explanatory variables}
}
\value{
a trained and fitted classification model
}
\description{
Fit and train some classification models with optional recursive feature elimination
and preprocessing steps
}
