% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/der_vs_state_nlmixr.R
\name{der_vs_state_nlmixr}
\alias{der_vs_state_nlmixr}
\title{Generate Derivative versus State (nlmixr2)}
\usage{
der_vs_state_nlmixr(
  nn_name,
  min_state,
  max_state,
  est_parms = NULL,
  fit_obj = NULL,
  length_out = 100,
  time_nn = FALSE,
  act = "ReLU",
  beta = 20
)
}
\arguments{
\item{nn_name}{(string) Name of the NN, e.g., \dQuote{c} for NNc(...)}

\item{min_state}{(numeric) Value of minimal state for which the derivative should be calculated}

\item{max_state}{(numeric) Value of maximal state for which the derivative should be calculated}

\item{est_parms}{(named vector; semi-optional) Named vector of estimated parameters form \emph{fit$fixef}. For optionality, see \strong{Details}.}

\item{fit_obj}{(nlmixr fit object; semi-optional) The fit-object from nlmixr2(...). For optionality, see \strong{Details}.}

\item{length_out}{(numeric) Number of states between min_state and max_state for derivative calculations.}

\item{time_nn}{(boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.}

\item{beta}{(numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.}
}
\value{
Dataframe with columns for the state and the corresponding derivatives
}
\description{
This functions allows to generate derivative versus state data for a neural network from a NODE in nlmixr2.
}
\details{
Either \emph{est_parms} or \emph{fit_obj} must be given. If both arguments are given, \emph{est_parms} is prioritized.
}
\examples{
\dontrun{
pop_fit <- nlmixr2(node_model,data=data,est="bobyqa")
derivative_data <- der_vs_state_nlmixr(nn="c",min_state=0,max_state=10,est_parms=pop_fit$fixef)
ggplot(derivative_data) + geom_line(aes(x=state,y=derivatives))
}
}
\author{
Dominic Bräm
}
