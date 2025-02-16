% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_mlx.R
\name{indparm_extractor_mlx}
\alias{indparm_extractor_mlx}
\title{Monolix individual estimations extractor}
\usage{
indparm_extractor_mlx(model_name)
}
\arguments{
\item{model_name}{(string) Name of the Monolix run. Must include \dQuote{.mlxtran}}
}
\value{
Data frame with individual parameter estimates (EBEs)
}
\description{
When the Monolix model has been run, this function allows to extract the
estimated individual parameters (EBEs) from the Monolix run folder.
}
\examples{
\dontrun{
est_parms <- indparm_extractor_mlx("run_1_ind.mlxtran")
}
}
\author{
Dominic Bräm
}
