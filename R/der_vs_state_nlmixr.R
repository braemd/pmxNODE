#' nlmixr individual estimations extractor
#' 
#' When the nlmixr model has been run, this function allows to extract the
#' estimated individual parameters for NN parameters by combining fixed effects and random effects
#' 
#' NULL
#' 
#' @param fit_obj Nlmixr fit object with random effects on NN parameters
#' @return Data frame with individual parameter estimates for NN parameters
#' @examples 
#' \dontrun{
#' fit_ind <- nlmixr(model_with_iiv,data=data,est="saem")
#' est_parms <- indparm_extractor_nlmixr(fit_ind)
#' }
#' @author Dominic Bräm
#' @export
indparm_extractor_nlmixr <- function(fit_obj){
  fit_obj_name <- deparse(substitute(fit_obj))
  if(!exists(fit_obj_name)){
    stop("Given fit object dosen't exist")
  }
  
  if(!(any(grepl("eta.W",colnames(fit_obj))) & any(grepl("eta.b",colnames(fit_obj))))){
    stop("Given fit object doesn't include random effects on NN parameters")
  }
  
  nns <- colnames(fit_obj)[grep("NN.+",colnames(fit_obj))]
  nn_names <- gsub("NN","",nns)
  nn_fix_names <- unlist(lapply(nn_names,function(x) paste0(c("lW","lb"),x)))
  nn_fix_names <- paste(nn_fix_names,collapse = "|")
  nn_ran_names <- unlist(lapply(nn_names,function(x) paste0(c("eta.W","eta.b"),x)))
  nn_ran_names <- paste(nn_ran_names,collapse = "|")
  
  est_parms <- stats::coef(fit_obj)
  fixeds <- est_parms[["fixed"]]
  nn_fixeds <- fixeds[grep(nn_fix_names,names(fixeds))]
  randoms <- est_parms[["random"]]
  nn_randoms <- randoms[,grep(nn_ran_names,colnames(randoms))]
  ind_parms <- t(apply(nn_randoms,1,function(x) nn_fixeds * exp(x)))
  colnames(ind_parms) <- names(nn_fixeds)
  
  ids <- randoms[,1]
  
  id_df <- data.frame(id = ids)
  out <- cbind(id_df,ind_parms)
  
  return(out)
}


#' Generate Derivative versus State (nlmixr2)
#' 
#' This functions allows to generate derivative versus state data for a neural network from a NODE in nlmixr2.
#' 
#' Either \emph{est_parms} or \emph{fit_obj} must be given. If both arguments are given, \emph{est_parms} is prioritized.
#' 
#' @param nn_name (string) Name of the NN, e.g., \dQuote{c} for NNc(...)
#' @param min_state (numeric) Value of minimal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param max_state (numeric) Value of maximal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param inputs (numeric vector) Vector of input values for which derivatives should be calculated (optional if min_state and max_state is given)
#' @param est_parms (named vector; semi-optional) Named vector of estimated parameters form \emph{fit$fixef}. For optionality, see \strong{Details}.
#' @param fit_obj (nlmixr fit object; semi-optional) The fit-object from nlmixr2(...). For optionality, see \strong{Details}.
#' @param length_out (numeric) Number of states between min_state and max_state for derivative calculations.
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' @param act (string) Activation function used in the NN. Currently "ReLU" and "Softplus" available.
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @return Dataframe with columns for the state and the corresponding derivatives
#' @author Dominic Bräm
#' @keywords internal
der_vs_state_nlmixr <- function(nn_name,min_state=NULL,max_state=NULL,inputs=NULL,est_parms=NULL,fit_obj=NULL,
                            length_out=100,time_nn=FALSE,act="ReLU",beta=20){
  if(is.null(inputs) & (is.null(min_state) | is.null(max_state))){
    error_msg <- "Either inputs or both, min_state and max_state, must be given"
    stop(error_msg)
  }
  if(is.null(est_parms) & is.null(fit_obj)){
    error_msg <- "Either estimated parameters or nlmixr fit object must be given"
    stop(error_msg)
  }
  if(is.null(est_parms)){
    if(!("nlmixr2FitData" %in% class(fit_obj))){
      error_msg <- "Given fit_obj is not a fit object from nlmixr2"
      stop(error_msg)
    }
    est_parms <- `$`(fit_obj,"fixef")
  } else if(!is.numeric(est_parms)){
    error_msg <- "Given estimated parameters are not numeric"
    stop(error_msg)
  }
  num_est_parms <- est_parms
  if(is.null(inputs)){
    inputs <- seq(min_state,max_state,length.out=length_out)
  }
  outputs <- derivative_calc_nm(nn_name,num_est_parms,inputs,time_nn=time_nn,act=act,beta=beta)
  out <- data.frame(state=inputs,
                    derivatives=outputs)
  return(out)
}

#' Generate Derivative versus State with individual parameters (nlmixr)
#' 
#' This functions allows to generate derivative versus state data for a neural network from a NODE in nlmixr2 with individual parameters.
#' 
#' Either \emph{est_parms} or \emph{nm_res_file} and \emph{nm_phi_file} must be given. If both arguments are given, \emph{est_parms} is prioritized.
#' 
#' @param nn_name (string) Name of the NN, e.g., \dQuote{c} for NNc(...)
#' @param min_state (numeric) Value of minimal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param max_state (numeric) Value of maximal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param inputs (numeric vector) Vector of input values for which derivatives should be calculated (optional if min_state and max_state is given)
#' @param est_parms (named vector; semi-optional) A data frame with estimated individual parameters from the NN 
#' extracted through the \emph{indparm_extractor_nlmixr} function. For optionality, see \strong{Details}.
#' @param fit_obj (nlmixr fit object; semi-optional) The fit-object from nlmixr2(...), fitted with IIV. For optionality, see \strong{Details}.
#' @param length_out (numeric) Number of states between min_state and max_state for derivative calculations.
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' @param act (string) Activation function used in the NN. Currently "ReLU" and "Softplus" available.
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @return Dataframe with columns for the state and the corresponding individual derivatives
#' @author Dominic Bräm
#' @keywords internal
ind_der_vs_state_nlmixr <- function(nn_name,min_state=NULL,max_state=NULL,inputs=NULL,est_parms=NULL,fit_obj=NULL,
                                length_out=100,time_nn=FALSE,act="ReLU",beta=20){
  if(is.null(inputs) & (is.null(min_state) | is.null(max_state))){
    error_msg <- "Either inputs or both, min_state and max_state, must be given"
    stop(error_msg)
  }
  if(is.null(est_parms) & is.null(fit_obj)){
    error_msg <- "Either estimated parameters or nlmixr fit object must be given"
    stop(error_msg)
  }
  if(is.null(est_parms)){
    if(!("nlmixr2FitData" %in% class(fit_obj))){
      error_msg <- "Given fit_obj is not a fit object from nlmixr2"
      stop(error_msg)
    }
    est_parms <- indparm_extractor_nlmixr(fit_obj)
  } else if(!is.data.frame(est_parms)){
    error_msg <- "Given estimated parameters are not numeric"
  }
  
  num_est_parms <- est_parms
  if(is.null(inputs)){
    inputs <- seq(min_state,max_state,length.out=length_out)
  }
  outputs <- apply(num_est_parms,1,function(x) {
    x <- as.numeric(x)
    names(x) <- colnames(num_est_parms)
    out <- derivative_calc_nm(nn_name,x,inputs,time_nn=time_nn,act=act,beta=beta)
  })
  colnames(outputs) <- paste0("id_",est_parms[,"id"])
  out <- data.frame(state=inputs)
  out <- cbind(out,outputs)
  
  return(out)
}


#' Generate Derivative versus State Plot (nlmixr2)
#' 
#' This functions allows to generate a derivative versus state plot for a neural network from a NODE in nlmixr2
#' 
#' Either \emph{est_parms} or \emph{fit_obj} must be given. If both arguments are given, \emph{est_parms} is prioritized.
#' 
#' @param nn_name (string) Name of the NN, e.g., \dQuote{c} for NNc(...)
#' @param min_state (numeric) Value of minimal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param max_state (numeric) Value of maximal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param inputs (numeric vector) Vector of input values for which derivatives should be calculated (optional if min_state and max_state is given)
#' @param est_parms (named vector; semi-optional) Named vector of estimated parameters from the NN extracted through \emph{fit$fixef}. For optionality, see \strong{Details}.
#' @param fit_obj (nlmixr fit object; semi-optional) The fit-object from nlmixr2(...). For optionality, see \strong{Details}.
#' @param length_out (numeric) Number of states between min_state and max_state for derivative calculations.
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' @param act (string) Activation function used in the NN. Currently "ReLU" and "Softplus" available.
#' @param plot_type (string) What plot type should be used; "base" or "ggplot"
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @return Displaying derivative versus state plot; returns ggplot-object if \emph{plot_type="ggplot"}
#' @examples 
#' \dontrun{
#' pop_fit <- nlmixr2(node_model_pop,data=data,est="bobyqa")
#' der_state_plot <- der_state_plot_nlmixr(nn="c",
#'                                         min_state=0,max_state=10,
#'                                         fit_obj=pop_fit,
#'                                         plot_type="ggplot")
#' }
#' @author Dominic Bräm
#' @export
der_state_plot_nlmixr <- function(nn_name,min_state=NULL,max_state=NULL,inputs=NULL,est_parms=NULL,fit_obj=NULL,
                              length_out=100,time_nn=FALSE,act="ReLU",plot_type=c("base","ggplot"),beta=20){
  data <- der_vs_state_nlmixr(nn_name=nn_name,min_state=min_state,max_state=max_state,
                          est_parms=est_parms,fit_obj=fit_obj,length_out=length_out,
                          time_nn=time_nn,act=act,beta=beta)
  
  if(length(plot_type)>1){
    plot_type <- "base"
  }
  if(!(plot_type %in% c("base","ggplot"))){
    error_msg <- "Please provide valid plot type, i.e., base or ggplot"
    stop(error_msg)
  }
  
  if(plot_type == "base"){
    plot(data$state,data$derivatives,xlab="State",ylab="Derivatives")
  } else if(plot_type == "ggplot"){
    if(requireNamespace("ggplot2",quietly = TRUE)){
      p <- ggplot2::ggplot(data) + ggplot2::geom_line(aes(x=state,y=derivatives)) +
        ggplot2::xlab("State") +
        ggplot2::ylab("Derivatives")
      return(p)
    } else{
      error_msg <- "To return a ggplot, the package ggplot2 must be installed"
      stop(error_msg)
    }
  }
}



#' Generate Derivative versus State Plot for individual parameter estimates (nlmixr2)
#' 
#' This functions allows to generate a derivative versus state plot for a neural network from a NODE in nlmixr2
#' with individual parameter estimates (EBEs).
#' 
#' Either \emph{est_parms} or \emph{fit_obj} must be given. If both arguments are given, \emph{est_parms} is prioritized.
#' 
#' @param nn_name (string) Name of the NN, e.g., \dQuote{c} for NNc(...)
#' @param min_state (numeric) Value of minimal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param max_state (numeric) Value of maximal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param inputs (numeric vector) Vector of input values for which derivatives should be calculated (optional if min_state and max_state is given)
#' @param est_parms (named vector; semi-optional) A data frame with estimated individual parameters from the NN 
#' extracted through the \emph{indparm_extractor_nlmixr} function. For optionality, see \strong{Details}.
#' @param fit_obj (nlmixr fit object; semi-optional) The fit-object from nlmixr2(...), fitted with IIV. For optionality, see \strong{Details}.
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' @param ribbon (boolean) Whether individual derivatives versus states should be summarise in a ribbon (TRUE) or
#' displayed as individual spaghetti plot (FALSE)
#' @param length_out (numeric) Number of points between min_state and max_state
#' @param act (string) Activation function used in the NN. Currently "ReLU" and "Softplus" available.
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @return Displaying derivative versus state plot
#' @examples 
#' \dontrun{
#' ind_fit <- nlmxir2(node_model_ind,data=data,est="saem")
#' der_state_plot <- ind_der_state_plot_nlmixr(nn="c",min_state=0,max_state=10,
#'                                             fit_obj=ind_fit)
#' }
#' @author Dominic Bräm
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr starts_with
#' @export
ind_der_state_plot_nlmixr <- function(nn_name,min_state=NULL,max_state=NULL,inputs=NULL,est_parms=NULL,fit_obj=NULL,
                                  length_out=100,time_nn=FALSE,ribbon=TRUE,act="ReLU",beta=20){
  data <- ind_der_vs_state_nlmixr(nn_name=nn_name,min_state=min_state,max_state=max_state,
                              est_parms=est_parms,fit_obj=fit_obj,
                              length_out=length_out,time_nn=time_nn,act=act,beta=beta)
  
  if(ribbon){
    mins <- data.frame(mins=apply(data[,-1],1,min))
    maxs <- data.frame(maxs=apply(data[,-1],1,max))
    medians <- data.frame(medians=apply(data[,-1],1,stats::median))
    states <- data.frame(states=data[,1])
    plot_data <- cbind(states,medians,mins,maxs)
    p <- ggplot(plot_data) + geom_ribbon(aes(x=states,ymin=mins,ymax=maxs),alpha=0.3) + 
      geom_line(aes(x=states,y=medians)) +
      xlab("State") +
      ylab("Derivatives")
    return(p)
  } else{
    plot_data <- tidyr::pivot_longer(data,
                                     cols=tidyr::starts_with("id_"),
                                     names_to = "id",
                                     names_prefix = "id_",
                                     values_to = "derivatives")
    p <- ggplot(plot_data) + geom_line(aes(x=state,y=derivatives,group=id)) +
      xlab("State") +
      ylab("Derivatives") +
      theme(legend.position = "none")
    return(p)
  }
  
}
