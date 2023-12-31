% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbscan.R
\name{dbscan_outlier_detection}
\alias{dbscan_outlier_detection}
\title{DBSCAN outlier detection}
\usage{
dbscan_outlier_detection(datatrain, eps, minPts)
}
\arguments{
\item{datatrain}{dataset with numerical inputs}

\item{eps}{distance to be considered}

\item{minPts}{minimum number of points within the considered distance eps}
}
\value{
a dbscan object
}
\description{
DBSCAN outlier detection
}
