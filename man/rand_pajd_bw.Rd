% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RegDis_function.r
\name{rand_pajd_bw}
\alias{rand_pajd_bw}
\title{Fisher p-values adjusted bw}
\usage{
rand_pajd_bw(dataset = data, forcing_var_name = "S",
  covariates = c("sex", "HSHumanity", "HSTech", "HSOther", "hsgrade",
  "Y2004"), niter = 1000, bandwidth = c(500, 1000, 5000),
  cut_value = 15000, whichunder = 1)
}
\arguments{
\item{dataset}{The dataset with the variables}

\item{forcing_var_name}{forcing_var_name}

\item{niter}{niter}

\item{bandwidth}{bandwidth}

\item{cut_value}{cut_value}

\item{forcing_bin_var_name}{forcing_bin_var_name}

\item{Y_name}{Y_name}
}
\value{
data frame with variable and value and bandwidth
}
\description{
Fisher p-values adjusted bw
}
