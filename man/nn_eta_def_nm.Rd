% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nn_parmsetter_nm.R
\name{nn_eta_def_nm}
\alias{nn_eta_def_nm}
\title{Internal: Define NN ETAs in NONMEM}
\usage{
nn_eta_def_nm(number, eta_start, n_hidden = 5, time_nn = FALSE)
}
\arguments{
\item{number}{(string) Name of the NN, e.g., \dQuote{1} for NN1(...)}

\item{n_hidden}{(numeric) Number of neurons in the hidden layer, default value is 5}

\item{theta_start}{(numeric) Number with which to start the ETA count of NN parameters}
}
\value{
Vector with all NN parameter ETA definitions for a NN
}
\description{
Define random effects of NN parameters as ETAs used in the $PK section in NONMEM
}
\details{
Parameter definition in form of etaW = ETA(X) with X the next number of ETA \cr
e.g., if \cr
etaV = ETA(1) \cr
etakel = ETA(2) \cr
X is equal to 3
}
\examples{
\dontrun{
nn_parm_names <- nn_eta_def_nm("1",3)
}
}
\author{
Dominic Bräm
}
