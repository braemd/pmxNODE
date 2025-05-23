% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nn_parmsetter_nlmixr.R
\name{nn_theta_initializer_nlmixr}
\alias{nn_theta_initializer_nlmixr}
\title{Internal: Initialize typical NN parameter in nlmixr}
\usage{
nn_theta_initializer_nlmixr(
  number,
  xmini,
  xmaxi,
  n_hidden = 5,
  theta_scale = 0.1,
  pre_fixef = NULL,
  time_nn = FALSE,
  act = "ReLU",
  beta = 20
)
}
\arguments{
\item{number}{(string) Name of the NN, e.g., \dQuote{1} for NN1(...)}

\item{xmini}{(numeric) minimal activation point}

\item{xmaxi}{(numeric) maximal activation point}

\item{n_hidden}{(numeric) Number of neurons in the hidden layer, default value is 5}

\item{theta_scale}{(numeric) Scale for input-hidden-weights initialization}

\item{pre_fixef}{(named vector) Vector of pre-defined initial values}

\item{time_nn}{(boolean) Definition whether NN is time-dependent (TRUE) or not (FALSE)}
}
\value{
Vector of initial typical NN parameters for one specific NN
}
\description{
Calculate the initial typical NN parameter values, such that activation points are within the range
between \emph{min_init} and \emph{max_init} defined in the un-converter NONMEM model file, and define the 
the typical NN parameters for the \emph{ini} section of the nlmixr model
}
\details{
\itemize{
  \item \emph{theta_scale} is the scale in which the weights from input to hidden layer are initialized,
  i.e., 0.1 initializes weights between -0.3 and 0.3; 0.01 initializes weights between -0.03 and 0.03
  \item \emph{time_nn} defines whether the NN is a time-dependent NN with the restriction that all weights from
  input to hidden layer are negative
}
}
\examples{
\dontrun{
ini_values <- nn_theta_initializer_nlmixr(number="1",xmini=1,xmaxi=5)
}
}
\author{
Dominic Bräm
}
