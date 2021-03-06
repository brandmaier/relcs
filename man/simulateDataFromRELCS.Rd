% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulateDataFromRELCS.R
\name{simulateDataFromRELCS}
\alias{simulateDataFromRELCS}
\title{Simulate Data from RELCS model}
\usage{
simulateDataFromRELCS(
  N,
  num.obs,
  selffeedback.mean = 0,
  selffeedback.variance = 0,
  residualerrorvariance = 1,
  slopevariance = 0,
  slopemu = 0,
  interceptvariance = 0,
  interceptmu = 0,
  has.slope = TRUE,
  has.icept = TRUE,
  use.openmx = FALSE,
  return.true.fb = FALSE
)
}
\description{
Simulate Data from RELCS model
}
