% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/der_vs_state.R
\name{der_state_plot_mlx}
\alias{der_state_plot_mlx}
\title{Generate Derivative versus State Plot (Monolix)}
\usage{
der_state_plot_mlx(
  nn_name,
  min_state,
  max_state,
  est_parms = NULL,
  mlx_file = NULL,
  time_nn = FALSE,
  act = "ReLU",
  plot_type = c("base", "ggplot"),
  beta = 20
)
}
\arguments{
\item{nn_name}{(string) Name of the NN, e.g., \dQuote{c} for NNc(...)}

\item{min_state}{(numeric) Value of minimal state for which the derivative should be calculated}

\item{max_state}{(numeric) Value of maximal state for which the derivative should be calculated}

\item{est_parms}{(named vector; semi-optional) Named vector of estimated parameters from the NN extracted through the \emph{pre_fixef_extractor_mlx} function. For optionality, see \strong{Details}.}

\item{mlx_file}{(string; semi-optional) (path)/name of the Monolix run. Must include ".mlxtran" and estimation bust have been run previously. For optionality, see \strong{Details}.}

\item{time_nn}{(boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.}

\item{plot_type}{(string) What plot type should be used; "base" or "ggplot"}

\item{beta}{(numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.}
}
\value{
Displaying derivative versus state plot; returns ggplot-object if \emph{plot_type="ggplot"}
}
\description{
This functions allows to generate a derivative versus state plot for a neural network from a NODE in Monolix.
}
\details{
Either \emph{est_parms} or \emph{mlx_file} must be given. If both arguments are given, \emph{est_parms} is priotized.
}
\examples{
\dontrun{
der_state_plot <- der_state_plot_mlx(nn="c",min_state=0,max_state=10,mlx_file="mlx_example1_model_mlx_file_pop.mlxtran",plot_type="ggplot")
}
}
\author{
Dominic Bräm
}
