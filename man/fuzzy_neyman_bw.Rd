% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RegDis_function.r
\name{fuzzy_neyman_bw}
\alias{fuzzy_neyman_bw}
\title{Fuzzy - neyman bandwidth}
\usage{
fuzzy_neyman_bw(dataset = data, forcing_var_name = "S",
  Y_name = "dropout", niter = 1000, W = "W", bandwidth = c(500,
  1000, 1500), cut_value = 15000, whichunder = 1, cin = 95)
}
\arguments{
\item{dataset}{The dataset with the variables}

\item{forcing_var_name}{forcing_var_name}

\item{Y_name}{Y_name}

\item{niter}{niter}

\item{W}{selected}

\item{bandwidth}{bandwidth}

\item{cut_value}{cut_value}

\item{forcing_bin_var_name}{forcing_bin_var_name}
}
\value{
data frame with variable and value and bandwidth
}
\description{
Fuzzy - neyman bandwidth
}
