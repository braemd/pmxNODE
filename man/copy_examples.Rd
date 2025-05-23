% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/example_handling.R
\name{copy_examples}
\alias{copy_examples}
\title{Copy examples to your folder}
\usage{
copy_examples(
  target_folder,
  examples = NULL,
  example_nr = NULL,
  example_software = NULL,
  pkg_name = "pmxNODE"
)
}
\arguments{
\item{target_folder}{(string) Path to the folder the examples should be copied to}

\item{examples}{(vector of strings) Explicit names of example data and/or model files to be copied. Must be in
the example list obtained by \emph{get_example_list()}}

\item{example_nr}{(numeric) Number of example data and model to be copied. If \emph{example_software} is not specified,
examples with \emph{example_nr} for all software will be copied.}

\item{example_software}{(string) Software of example data and model to be copied. Either \dQuote{Monolix} or \dQuote{NONMEM}
available. If \emph{example_nr} is not specified, all examples for this software will be copied.}

\item{pkg_name}{(string) Only required in development phase}
}
\value{
Copied examples in specified folder.
}
\description{
This function allows to copy one or multiple examples (data and model files) to a directory of your choice. \cr
Either \emph{examples}, \emph{example_nr}, \emph{example_software}, or \emph{example_nr} + \emph{example_software} must be given.
}
\examples{
\dontrun{
copy_examples("path/to/target/folder",examples = c("data_example1_mlx.csv","mlx_example1_model.txt"))
copy_examples("path/to/target/folder",example_nr = 1)
copy_examples("path/to/target/folder",example_software = "Monolix")
copy_examples("path/to/target/folder",example_nr = 1, example_software = "NONMEM")
}
}
\author{
Dominic Bräm
}
