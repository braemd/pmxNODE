#' NN converter for nlmixr
#' 
#' This function converts a nlmixr model function that includes pseudo-functions for NNs as described in \strong{Details}
#' into a model function that can be used in the \emph{nlmixr} function.
#' 
#' \itemize{
#'   \item \emph{state=} defines the state to be used in the NN. For time, use \emph{t}.
#'   \item \emph{min_init=} defines the minimal activation point for the NN, i.e., minimal expected state
#'   \item \emph{max_init=} defines the maximal activation point for the NN, i.e., maximal expected state
#'   \item \emph{n_hidden=} (optional) defines the number of neurons in the hidden layer, default is 5
#'   \item \emph{act=} (optional) defines activation function in the hidden layer, ReLU and Softplus implemented, default is ReLU
#'   \item \emph{time_nn=} (optional) defines whether the NN should be assumed to be a time-dependent NN
#'     and consequently all weights from input to hidden layer should be strictly negative.
#' }
#' 
#' @param f_ode (nlmixr model function) Model function of nlmixr type with NN functions
#' @param pop_only (boolean) If the generated nlmixr model function should be a fit without (TRUE) or with (FALSE)
#' inter-individual variability on NN parameters
#' @param theta_scale (numeric) Scale in which typical NN parameter values are initialized, default is 0.1, i.e., weights are 
#' initialized between -0.3 and 0.3
#' @param eta_scale (numeric) Initial standard deviation of random effects on NN parameters, default is 0.1
#' @param pre_fixef (named vector) Specific initial values for typical parameters, can be obtained from a run nlmixr model (e.g., run_1) through
#' \emph{run_1$fixef}
#' @param seed (numeric) Seed for random parameter initialization.
#' @return A converted nlmixr model function
#' @examples 
#' \dontrun{
#' f_ode_pop <- function(){
#' ini({
#'   lV <- 1
#'   prop.err <- 0.1
#' })
#' model({
#'   V <- lV
#'   d/dt(centr)  =  NN1(state=centr,min_init=0,max_init=300)
#'   cp = centr / V
#'   cp ~ prop(prop.err)
#'   })
#' }
#' 
#' f_new_pop <- nn_converter_nlmixr(f_ode_pop,pop_only = TRUE)
#' 
#' fit_pop <- nlmixr2(f_new_pop,data,est="bobyqa")
#' 
#' f_ode <- function(){
#'   ini({
#'     lV <- 1
#'     eta.V ~ 0.1
#'     prop.err <- 0.1
#'   })
#'   model({
#'     V <- lV * exp(eta.V)
#'     d/dt(centr)  =  NN1(state=centr,min_init=0,max_init=300)
#'     cp = centr / V
#'     cp ~ prop(prop.err)
#'   })
#' }
#' 
#' f_new <- nn_converter_nlmixr(f_ode,pop_only = FALSE, pre_fixef = fit_pop$fixef)
#' }
#' @author Dominic Bräm
#' @export
nn_converter_nlmixr <- function(f_ode,pop_only=FALSE,theta_scale=0.1,eta_scale=0.1,pre_fixef=NULL,seed=1908){
  set.seed(seed)
  pop <- pop_only
  
  f_parse <- deparse(f_ode,width.cutoff = 500)
  
  nns <- unlist(nn_extractor(f_parse))
  nn_tests <- unlist(lapply(nns,nn_tester))
  nn_errors(nn_tests)
  
  nn_lines_nr <- grep("NN\\d+\\(|NN\\w+\\(",f_parse)
  nn_lines <- f_parse[nn_lines_nr]
  
  nn_numbers_og <- unlist(lapply(nn_lines,nn_number_extractor))
  nn_duplicates <- duplicated(nn_numbers_og)
  
  nn_numbers <- nn_numbers_og[!nn_duplicates]
  nn_states <- unlist(lapply(nn_lines,nn_state_extractor))[!nn_duplicates]
  min_states <- as.numeric(unlist(lapply(nn_lines,nn_minini_extractor))[!nn_duplicates])
  max_states <- as.numeric(unlist(lapply(nn_lines,nn_maxini_extractor))[!nn_duplicates])
  nn_nhiddens <- as.numeric(unlist(lapply(nns,nn_nhidden_extractor))[!nn_duplicates])
  nn_acts <- unlist(lapply(nns,nn_act_extractor))[!nn_duplicates]
  time_nns <- unlist(lapply(nns,nn_time_nn_extractor))[!nn_duplicates]
  
  nn_code <- vector("list", length = length(nn_numbers))
  
  for(i in 1:length(nn_numbers)){
    nn_code[[i]] <- nn_generator_nlmixr(number=nn_numbers[i],state=nn_states[i],time_nn=time_nns[i],
                                        n_hidden = nn_nhiddens[i],act=nn_acts[i])
  }
  
  theta_inis <- vector("list", length = length(nn_numbers))
  
  for(i in 1:length(nn_numbers)){
    theta_inis[[i]] <- nn_theta_initializer_nlmixr(number=nn_numbers[i],xmini=min_states[i],
                                                   xmaxi=max_states[i],theta_scale=theta_scale,
                                                   time_nn=time_nns[i],pre_fixef=pre_fixef,
                                                   n_hidden=nn_nhiddens[i],act=nn_acts[i])
  }
  
  if(!pop){
    eta_inis <- vector("list", length = length(nn_numbers))
    
    for(i in 1:length(nn_numbers)){
      eta_inis[[i]] <- nn_eta_initializer_nlmixr(number=nn_numbers[i],eta_scale=eta_scale,
                                                 n_hidden=nn_nhiddens[i],time_nn=time_nns[i])
    }
  }
  
  parm_set <- vector("list", length = length(nn_numbers))
  
  for(i in 1:length(nn_numbers)){
    parm_set[[i]] <- nn_parm_setter_nlmixr(number=nn_numbers[i],pop=pop,n_hidden=nn_nhiddens[i],
                                           time_nn=time_nns[i])
  }
  
  f_parse_new <- nn_reducer(f_parse)
  
  for(i in 1:length(nn_numbers)){
    nx_line_nr <- grep(paste0("NN",nn_numbers[i]),f_parse_new)
    f_parse_new <- append(f_parse_new,unlist(nn_code[i]),after = min(as.numeric(nx_line_nr)) - 1)
  }
  
  model_line <- grep("model\\(\\{",f_parse_new)
  f_parse_new <- append(f_parse_new,unlist(parm_set),after = model_line)
  
  model_line <- grep("model\\(\\{",f_parse_new)
  f_parse_new <- append(f_parse_new,unlist(theta_inis),after = model_line - 2)
  
  if(!pop){
    model_line <- grep("model\\(\\{",f_parse_new)
    f_parse_new <- append(f_parse_new,unlist(eta_inis),after = model_line - 2)
  }
  
  f_parse_new_text <- paste(f_parse_new,collapse = "\n")
  
  f_new <- eval(str2lang(f_parse_new_text))
  
  return(f_new)
}

#' NN converter for NONMEM
#' 
#' This function converts a NONMEM model file that includes pseudo-functions for NNs as described in \strong{Details}
#' into a model that can be used in NONMEM. An example NONMEM model can be opened with the function \emph{open_nm_example()}.
#' 
#' An example of model file could look like following \cr
#' \preformatted{$SIZES LVR=80 LNP4=40000} 
#' \preformatted{$PROB RUN} 
#' \preformatted{$INPUT C ID TIME AMT DV DOSE EVID} 
#' \preformatted{$DATA data_example1_nm.csv IGNORE=C} 
#' \preformatted{$SUBROUTINES ADVAN13} 
#' \preformatted{$MODEL} 
#' \preformatted{COMP(Centr)} 
#' \preformatted{$PK} 
#' \preformatted{lV = THETA(1)} 
#' \preformatted{V = lV * EXP(ETA(1))} 
#' \preformatted{$DES}
#' \preformatted{DADT(1) = NNc(state=A(1),min_init=0.5,max_init=5) + DOSE * NNt(state=T,min_init=1,max_init=5,time_nn=TRUE)} 
#' \preformatted{$ERROR} 
#' \preformatted{Cc = A(1)/V} 
#' \preformatted{Y=Cc*(1+EPS(1)) + EPS(2)} 
#' \preformatted{$THETA} 
#' \preformatted{2 ; [V]} 
#' \preformatted{$OMEGA} 
#' \preformatted{0.1 ; [V]} 
#' \preformatted{$SIGMA} 
#' \preformatted{0.1} 
#' \preformatted{0.1} 
#' \preformatted{$ESTIMATION METHOD=1 MAXEVAL=9999 INTER PRINT=5} 
#' \preformatted{$TABLE ID TIME DV IPRED=CIPRED AMT NOPRINT FILE=nm_example1.tab} 
#' \itemize{
#'   \item Note that size of problem should be increased, as in the model above with \emph{$SIZES LVR=80 LNP4=40000}
#'   \item NN functions need to be of form NN\strong{X}(...) where X is the name of the NN so references between
#'   the same NN, e.g., as output of absorption compartment and input to central compartment, can be made. Arguments to NNX are
#'   \itemize{
#'     \item \emph{state=} defines the state to be used in the NN. For time, use \emph{t}.
#'     \item \emph{min_init=} defines the minimal activation point for the NN, i.e., minimal expected state
#'     \item \emph{max_init=} defines the maximal activation point for the NN, i.e., maximal expected state
#'     \item \emph{n_hidden=} (optional) defines the number of neurons in the hidden layer, default is 5
#'     \item \emph{act=} (optional) defines activation function in the hidden layer, ReLU and Softplus implemented, default is ReLU
#'     \item \emph{time_nn=} (optional) defines whether the NN should be assumed to be a time-dependent NN
#'     and consequently all weights from input to hidden layer should be strictly negative.
#'   }
#' }
#' Note: Converted NONMEM model file will be saved under \emph{unconverted_file}_converted.ctl
#' 
#' @param ctl_path (string) (Path/)Name of the unconverted NONMEM model file
#' @param pop_only (boolean) If the generated NONMEM model file should be a fit without (TRUE) or with (FALSE)
#' inter-individual variability on NN parameters
#' @param theta_scale (numeric) Scale in which typical NN parameter values are initialized, default is 0.1, i.e., weights are 
#' initialized between -0.3 and 0.3
#' @param eta_scale (numeric) Initial standard deviation of random effects on NN parameters, default is 0.1
#' @param pre_fixef (named vector) Specific initial values for typical parameters, can be optained with the 
#' \emph{nn_prefix_extractor_nm} function from the results file of a previous NONMEM run
#' @param seed (numeric) Seed for random parameter initialization.
#' @return Saving a converted NONMEM model file under \emph{ctl_path}_converted.ctl
#' @examples
#' \dontrun{
#' nn_converter_nm("nm_example_model.ctl",pop_only = TRUE)
#' 
#' est_parms <- pre_fixef_extractor_nm("nm_example_model_converted.res")
#' 
#' nn_converter_nm("nm_example_model.ctl",pop_only = FALSE,pre_fixef=est_parms)
#' }
#' @author Dominic Bräm
#' @export
nn_converter_nm <- function(ctl_path,pop_only=FALSE,theta_scale=0.1,eta_scale=0.001,pre_fixef=NULL,seed=1908){
  set.seed(seed)
  pop <- pop_only
  
  f_parse <- readLines(ctl_path)
  
  states_corrections <- state_correcter_nm(f_parse)
  f_parse <- states_corrections[[1]]
  
  nns <- unlist(nn_extractor(f_parse))
  nn_tests <- unlist(lapply(nns,nn_tester))
  nn_errors(nn_tests)
  
  nn_lines_nr <- grep("NN\\d+\\(|NN\\w+\\(",f_parse)
  nn_lines <- f_parse[nn_lines_nr]
  
  nn_numbers_og <- unlist(lapply(nn_lines,nn_number_extractor))
  nn_duplicates <- duplicated(nn_numbers_og)
  
  nn_numbers <- nn_numbers_og[!nn_duplicates]
  nn_states <- unlist(lapply(nn_lines,nn_state_extractor))[!nn_duplicates]
  min_states <- as.numeric(unlist(lapply(nn_lines,nn_minini_extractor))[!nn_duplicates])
  max_states <- as.numeric(unlist(lapply(nn_lines,nn_maxini_extractor))[!nn_duplicates])
  nn_nhiddens <- as.numeric(unlist(lapply(nns,nn_nhidden_extractor))[!nn_duplicates])
  nn_acts <- unlist(lapply(nns,nn_act_extractor))[!nn_duplicates]
  time_nns <- unlist(lapply(nns,nn_time_nn_extractor))[!nn_duplicates]
  
  nn_code <- vector("list", length = length(nn_numbers))
  
  for(i in 1:length(nn_numbers)){
    nn_code[[i]] <- nn_generator_nm(number=nn_numbers[i],state=nn_states[i],time_nn=time_nns[i],
                                    n_hidden=nn_nhiddens[i],act=nn_acts[i])
  }
  
  theta_defs <- vector("list", length = length(nn_numbers))
  n_thetas <- sum(grepl("[^\\$]THETA",f_parse))
  
  for(i in 1:length(nn_numbers)){
    theta_def_out <- nn_theta_def_nm(number=nn_numbers[i],theta_start=n_thetas+1,n_hidden=nn_nhiddens[i],
                                     time_nn=time_nns[i])
    theta_defs[[i]] <- theta_def_out[[1]]
    n_thetas <- n_thetas + theta_def_out[[2]]
  }
  
  theta_inis <- vector("list", length = length(nn_numbers))
  
  for(i in 1:length(nn_numbers)){
    theta_inis[[i]] <- nn_theta_initializer_nm(number=nn_numbers[i],xmini=min_states[i],
                                               xmaxi=max_states[i],theta_scale=theta_scale,
                                               time_nn=time_nns[i],pre_fixef=pre_fixef,
                                               n_hidden=nn_nhiddens[i],act=nn_acts[i])
  }
  
  eta_defs <- vector("list", length = length(nn_numbers))
  n_etas <- sum(grepl("(?<!TH)ETA",f_parse,perl = T))
  
  for(i in 1:length(nn_numbers)){
    eta_def_out <- nn_eta_def_nm(number=nn_numbers[i],eta_start=n_etas+1,n_hidden=nn_nhiddens[i],
                                 time_nn=time_nns[i])
    eta_defs[[i]] <- eta_def_out[[1]]
    n_etas <- n_etas + eta_def_out[[2]]
  }
  
  eta_inis <- vector("list", length = length(nn_numbers))
  
  for(i in 1:length(nn_numbers)){
    eta_inis[[i]] <- nn_eta_initializer_nm(number=nn_numbers[i],theta_inis=theta_inis[[i]],pop=pop,eta_scale=eta_scale,
                                           n_hidden=nn_nhiddens[i],time_nn=time_nns[i])
  }
  
  parm_set <- vector("list", length = length(nn_numbers))
  
  for(i in 1:length(nn_numbers)){
    parm_set[[i]] <- nn_parm_setter_nm(number=nn_numbers[i],pop=pop,n_hidden=nn_nhiddens[i],
                                       time_nn=time_nns[i])
  }
  
  f_parse_new <- nn_reducer(f_parse)
  
  for(i in 1:length(nn_numbers)){
    nx_line_nr <- grep(paste0("NN",nn_numbers[i]),f_parse_new)
    f_parse_new <- append(f_parse_new,unlist(nn_code[i]),after = min(as.numeric(nx_line_nr)) - 1)
  }
  
  # pk_line <- grep("\\$PK",f_parse_new)
  # f_parse_new <- append(f_parse_new,unlist(parm_set),after = pk_line)
  # f_parse_new <- append(f_parse_new,unlist(eta_defs),after = pk_line)
  # f_parse_new <- append(f_parse_new,unlist(theta_defs),after = pk_line)
  
  des_line <- grep("\\$DES",f_parse_new)
  f_parse_new <- append(f_parse_new,unlist(theta_defs),after = des_line-1)
  des_line <- grep("\\$DES",f_parse_new)
  f_parse_new <- append(f_parse_new," ",after = des_line-1)
  des_line <- grep("\\$DES",f_parse_new)
  f_parse_new <- append(f_parse_new,unlist(eta_defs),after = des_line-1)
  des_line <- grep("\\$DES",f_parse_new)
  f_parse_new <- append(f_parse_new," ",after = des_line-1)
  des_line <- grep("\\$DES",f_parse_new)
  f_parse_new <- append(f_parse_new,unlist(parm_set),after = des_line-1)
  des_line <- grep("\\$DES",f_parse_new)
  f_parse_new <- append(f_parse_new," ",after = des_line-1)
  
  if(!is.null(states_corrections[[2]])){
    des_line <- grep("\\$DES",f_parse_new)
    f_parse_new <- append(f_parse_new,unlist(states_corrections[[2]]),after = des_line)
  }
  
  omega_line <- grep("\\$OMEGA",f_parse_new)
  f_parse_new <- append(f_parse_new,unlist(theta_inis),after = omega_line-1)
  omega_line <- grep("\\$OMEGA",f_parse_new)
  f_parse_new <- append(f_parse_new," ",after = omega_line-1)
  
  sigma_line <- grep("\\$SIGMA",f_parse_new)
  f_parse_new <- append(f_parse_new,unlist(eta_inis),after = sigma_line-1)
  sigma_line <- grep("\\$SIGMA",f_parse_new)
  f_parse_new <- append(f_parse_new," ",after = sigma_line-1)
  
  ind_pop <- ifelse(pop,"pop","ind")
  file_name <- gsub("(.*)\\..*$","\\1",ctl_path)
  file_extension <- gsub(".*\\.(.*)$","\\1",ctl_path)
  file_name_new <- paste0(file_name,"_converted_",ind_pop,".",file_extension)
  
  writeLines(f_parse_new,con=file_name_new)
  return(paste0("Converted model file under: ",file_name_new))
}

#' NN converter for Monolix
#' 
#' This function converts a Monolix model file that includes pseudo-functions for NNs as described in \strong{Details}
#' into a model that can be used in Monolix. An example Monolix model can be opened with the function \emph{open_mlx_example()}.
#' In addition, it allows to generate a Monolix \emph{.mlxtran} file with automatically initialized parameters and estimation settings.
#' 
#' An example of model file could look like following \cr
#' \preformatted{DESCRIPTION:} 
#' \preformatted{A Monolix model for conversion} 
#' \preformatted{[LONGITUDINAL]} 
#' \preformatted{input = {V=2}} 
#' \preformatted{PK:} 
#' \preformatted{depot(target=C)} 
#' \preformatted{EQUATION:} 
#' \preformatted{ddt_C = NN1(state=C,min_init=1,max_init=300) + amtDose * NN2(state=t,min_init=0.5,max_init=50,time_nn=TRUE)} 
#' \preformatted{Cc = C/V} 
#' \preformatted{OUTPUT:} 
#' \preformatted{output = Cc} 
#' \itemize{
#'   \item Note that the parameters in the \emph{input} need to have an initial value
#'   \item NN functions need to be of form NN\strong{X}(...) where X is the name of the NN so references between
#'   the same NN, e.g., as output of absorption compartment and input to central compartment, can be made. Arguments to NNX are
#'   \itemize{
#'     \item \emph{state=} defines the state to be used in the NN. For time, use \emph{t}.
#'     \item \emph{min_init=} defines the minimal activation point for the NN, i.e., minimal expected state
#'     \item \emph{max_init=} defines the maximal activation point for the NN, i.e., maximal expected state
#'     \item \emph{n_hidden=} (optional) defines the number of neurons in the hidden layer, default is 5
#'     \item \emph{act=} (optional) defines activation function in the hidden layer, ReLU and Softplus implemented, default is ReLU
#'     \item \emph{time_nn=} (optional) defines whether the NN should be assumed to be a time-dependent NN
#'     and consequently all weights from input to hidden layer should be strictly negative.
#'   }
#' }
#'  \cr
#' Note: Converted Monolix model file will be saved under \emph{unconverted_file}_converted.txt
#' 
#' 
#' @param mlx_path (string) (Path/)Name of the unconverted Monolix model file
#' @param pop_only (boolean) If the generated Monolix \emph{.mlxtran} file should be a fit without (TRUE) or with (FALSE)
#' inter-individual variability on NN parameters
#' @param theta_scale (numeric) Scale in which typical NN parameter values are initialized, default is 0.1, i.e., weights are 
#' initialized between -0.3 and 0.3
#' @param eta_scale (numeric) Initial standard deviation of random effects on NN parameters, default is 0.1
#' @param pre_fixef (named vector) Specific initial values for typical parameters, can be optained with the 
#' \emph{pre_fixef_extractor_mlx} function from a previous Monolix run
#' @param gen_mlx_file (boolean) If a Monolix \emph{.mlxtran} file with already initialized parameters and estimation
#' settigs should be generated
#' @param mlx_name (string) Optional, name of the generated Monolix file (\emph{mlx_name}.mlxtran). If no name is given and 
#' \emph{gen_mlx_file}=TRUE, name of the Monolix file will be \emph{unconverted_model_name}_\strong{mlx_file}_\strong{pop/ind}.mlxtran,
#' with pop or ind depending whether \emph{pop}=TRUE or \emph{pop}=FALSE,respectively.
#' @param data_file (string) Required if \emph{gen_mlx_file}=TRUE, (Path/)Name of the data file to be used
#' @param header_types (vector) Required if \emph{gen_mlx_file}=TRUE, Vector of strings describing column types of data.\n Possible header types: 
#' ignore, id, time, observation, amount, contcov, catcov, occ, evid, mdv, obsid, cens, limit, regressor, nominaltime, admid, rate, tinf, ss, ii, addl, date
#' @param obs_types (list) List of types of observations, e.g., \dQuote{continuous}; only required if non-continuous observations
#' @param mapping (list) List of mapping between model outputs and observation IDs
#' @param seed (numeric) Seed for random parameter initialization.
#' @return Saving a converted Monolix model file under \emph{mlx_path}_converted.txt and optionally a Monolix file (\emph{mlx_name}.mlxtran)
#' if \emph{gen_mlx_file}=TRUE
#' @examples
#' \dontrun{
#' nn_converter_mlx("mlx_model2.txt",pop_only=TRUE,gen_mlx_file=TRUE,data_file="TMDD_dataset.csv",header_types=c("id","time","amount","observation"))
#' 
#' est_parms <- pre_fixef_extractor_mlx("mlx_model2_time_nn_mlx_file_pop.mlxtran")
#' 
#' nn_converter_mlx("mlx_model2.txt",pop_only=FALSE,gen_mlx_file=TRUE,data_file="TMDD_dataset.csv",header_types=c("id","time","amount","observation"),pre_fixef=est_parms)
#' }
#' @author Dominic Bräm
#' @export
nn_converter_mlx <- function(mlx_path,pop_only=FALSE,theta_scale=0.1,eta_scale=0.1,pre_fixef=NULL,
                             gen_mlx_file=FALSE,mlx_name=NULL,data_file=NULL,header_types=NULL,
                             obs_types=NULL,mapping=NULL,seed=1908){
  set.seed(seed)
  pop <- pop_only
  
  f_parse <- readLines(mlx_path)
  
  nns <- unlist(nn_extractor(f_parse))
  nn_tests <- unlist(lapply(nns,nn_tester))
  nn_errors(nn_tests)
  
  if(gen_mlx_file & is.null(data_file) & is.null(header_types)){
    stop("When generating Monolix file, at least argument data_file and header_types is required")
  }
  
  model_parms <- model_parm_extractor_mlx(f_parse)
  
  nn_lines_nr <- grep("NN\\d+\\(|NN\\w+\\(",f_parse)
  nn_lines <- f_parse[nn_lines_nr]
  
  nn_numbers_og <- unlist(lapply(nn_lines,nn_number_extractor))
  nn_duplicates <- duplicated(nn_numbers_og)
  
  nn_numbers <- nn_numbers_og[!nn_duplicates]
  nn_states <- unlist(lapply(nn_lines,nn_state_extractor))[!nn_duplicates]
  min_states <- as.numeric(unlist(lapply(nn_lines,nn_minini_extractor))[!nn_duplicates])
  max_states <- as.numeric(unlist(lapply(nn_lines,nn_maxini_extractor))[!nn_duplicates])
  nn_nhiddens <- as.numeric(unlist(lapply(nns,nn_nhidden_extractor))[!nn_duplicates])
  nn_acts <- unlist(lapply(nns,nn_act_extractor))[!nn_duplicates]
  time_nns <- unlist(lapply(nns,nn_time_nn_extractor))[!nn_duplicates]
  
  nn_code <- vector("list", length = length(nn_numbers))
  
  for(i in 1:length(nn_numbers)){
    nn_code[[i]] <- nn_generator_mlx(number=nn_numbers[i],state=nn_states[i],time_nn=time_nns[i],
                                     n_hidden=nn_nhiddens[i],act=nn_acts[i])
  }
  
  theta_defs <- vector("list", length = length(nn_numbers))
  
  for(i in 1:length(nn_numbers)){
    theta_defs[[i]] <- nn_theta_def_mlx(number=nn_numbers[i],n_hidden=nn_nhiddens[i],
                                        time_nn=time_nns[i])
  }
  
  theta_inis <- vector("list", length = length(nn_numbers))

  for(i in 1:length(nn_numbers)){
    theta_inis[[i]] <- nn_theta_initializer_mlx(number=nn_numbers[i],xmini=min_states[i],
                                                xmaxi=max_states[i],theta_scale=theta_scale,
                                                time_nn=time_nns[i],pre_fixef=pre_fixef,
                                                n_hidden=nn_nhiddens[i],act=nn_acts[i])
  }
  
  f_parse_new <- nn_reducer(f_parse)
  f_parse_new <- model_parm_updater_mlx(f_parse_new,model_parms[[1]],theta_defs)
  
  for(i in 1:length(nn_numbers)){
    nx_line_nr <- grep(paste0("NN",nn_numbers[i]),f_parse_new)
    f_parse_new <- append(f_parse_new,unlist(nn_code[i]),after = min(as.numeric(nx_line_nr)) - 1)
  }
  
  file_name <- gsub("(.*)\\..*$","\\1",mlx_path)
  file_extension <- gsub(".*\\.(.*)$","\\1",mlx_path)
  file_name_new <- paste0(file_name,"_converted.",file_extension)
  
  writeLines(f_parse_new,con=file_name_new)
  print(paste0("Converted model file under: ",file_name_new))
  
  if(gen_mlx_file){
    if(!("lixoftConnectors" %in% .packages())){
      stop("lixoftConnectors must first be initialized. Use software_initializer(...) prior to use nn_converter_mlx with gen_mlx_file=TRUE")
    }
    if(is.null(mlx_name)){
      mlx_name <- paste0(file_name[1],"_mlx_file")
    }
    
    if(pop){
      mlx_name <- paste0(mlx_name,"_pop")
    } else{
      mlx_name <- paste0(mlx_name,"_ind")
    }
    
    mlx_model_initializer(model_name=mlx_name,
                          model_file=file_name_new,
                          data_file=data_file,
                          header_types=header_types,
                          parm_names=model_parms[1],
                          parm_inis=model_parms[2],
                          theta_names=theta_defs,
                          theta_inis=theta_inis,
                          pop=pop,
                          pre_fixef=pre_fixef,
                          omega_inis=eta_scale,
                          obs_types=obs_types,
                          mapping=mapping)
  }
  
  
}
