% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RegDis_function.r
\name{fuzzy_fep_bw}
\alias{fuzzy_fep_bw}
\title{Fuzzy - FEP bandwidth}
\usage{
fuzzy_fep_bw(dataset = data, forcing_var_name = "S",
  Y_name = "dropout", niter = 1000, W_name = "W",
  bandwidth = c(500, 1000, 1500), cut_value = 15000, M2 = 5,
  whichunder = 1, typemod = "binary", typesided = "onesided")
}
\arguments{
\item{dataset}{The dataset with the variables}

\item{forcing_var_name}{forcing_var_name}

\item{Y_name}{Y_name}

\item{niter}{niter}

\item{bandwidth}{bandwidth}

\item{cut_value}{cut_value}

\item{forcing_bin_var_name}{forcing_bin_var_name}

\item{W}{selected}
}
\value{
data frame with variable and value and bandwidth
}
\description{
Fuzzy - FEP bandwidth
}
