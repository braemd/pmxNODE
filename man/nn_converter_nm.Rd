% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nn_converters.R
\name{nn_converter_nm}
\alias{nn_converter_nm}
\title{NN converter for NONMEM}
\usage{
nn_converter_nm(
  ctl_path,
  pop = FALSE,
  theta_scale = 0.1,
  eta_scale = 0.001,
  pre_fixef = NULL
)
}
\arguments{
\item{ctl_path}{(string) (Path/)Name of the unconverted NONMEM model file}

\item{pop}{(boolean) If the generated NONMEM model file should be a fit without (TRUE) or with (FALSE)
inter-individual variability on NN parameters}

\item{theta_scale}{(numeric) Scale in which typical NN parameter values are initialized, default is 0.1, i.e., weights are 
initialized between -0.3 and 0.3}

\item{eta_scale}{(numeric) Initial standard deviation of random effects on NN parameters, default is 0.1}

\item{pre_fixef}{(named vector) Specific initial values for typical parameters, can be optained with the 
\emph{nn_prefix_extractor_nm} function from the results file of a previous NONMEM run}
}
\value{
Saving a converted NONMEM model file under \emph{ctl_path}_converted.ctl
}
\description{
This function converts a NONMEM model file that includes pseudo-functions for NNs as described in \strong{Details}
into a model that can be used in NONMEM. An example NONMEM model can be opened with the function \emph{open_nm_example()}.
}
\details{
An example of model file could look like following \cr
\preformatted{$SIZES LVR=80 LNP4=40000} 
\preformatted{$PROB RUN} 
\preformatted{$INPUT C ID TIME AMT DV DOSE EVID} 
\preformatted{$DATA data_example1_nm.csv IGNORE=C} 
\preformatted{$SUBROUTINES ADVAN13} 
\preformatted{$MODEL} 
\preformatted{COMP(Centr)} 
\preformatted{$PK} 
\preformatted{lV = THETA(1)} 
\preformatted{V = lV * EXP(ETA(1))} 
\preformatted{$DES}
\preformatted{DADT(1) = NNc(state=A(1),min_init=0.5,max_init=5) + DOSE * NNt(state=T,min_init=1,max_init=5,time_nn=TRUE)} 
\preformatted{$ERROR} 
\preformatted{Cc = A(1)/V} 
\preformatted{Y=Cc*(1+EPS(1)) + EPS(2)} 
\preformatted{$THETA} 
\preformatted{2 ; [V]} 
\preformatted{$OMEGA} 
\preformatted{0.1 ; [V]} 
\preformatted{$SIGMA} 
\preformatted{0.1} 
\preformatted{0.1} 
\preformatted{$ESTIMATION METHOD=1 MAXEVAL=9999 INTER PRINT=5} 
\preformatted{$TABLE ID TIME DV IPRED=CIPRED AMT NOPRINT FILE=nm_example1.tab} 
\itemize{
  \item Note that size of problem should be increased, as in the model above with \emph{$SIZES LVR=80 LNP4=40000}
  \item NN functions need to be of form NN\strong{X}(...) where X is the name of the NN so references between
  the same NN, e.g., as output of absorption compartment and input to central compartment, can be made. Arguments to NNX are
  \itemize{
    \item \emph{state=} defines the state to be used in the NN. For time, use \emph{t}.
    \item \emph{min_init=} defines the minimal activation point for the NN, i.e., minimal expected state
    \item \emph{max_init=} defines the maximal activation point for the NN, i.e., maximal expected state
    \item \emph{n_hidden=} (optional) defines the number of neurons in the hidden layer, default is 5
    \item \emph{act=} (optional) defines activation function in the hidden layer, ReLU and Softplus implemented, default is ReLU
    \item \emph{time_nn=} (optional) defines whether the NN should be assumed to be a time-dependent NN
    and consequently all weights from input to hidden layer should be strictly negative.
  }
}
Note: Converted NONMEM model file will be saved under \emph{unconverted_file}_converted.ctl
}
\examples{
\dontrun{
nn_converter_nm("nm_example_model.ctl",pop=TRUE)

est_parms <- pre_fixef_extractor_nm("nm_example_model_converted.res")

nn_converter_nm("nm_example_model.ctl",pop=FALSE,pre_fixef=est_parms)
}
}
\author{
Dominic Bräm
}
