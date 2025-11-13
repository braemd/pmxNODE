#' Internal: Calculate the derivatives from a NN in Monolix
#' 
#' Calculate the derivatives from a NN for derivative versus state plots
#' 
#' @param nn_name (string) Name of the NN, e.g., \dQuote{c} for NNc(...)
#' @param parms (named vector) Named vector of estimated parameters from the NN; Namings are e.g. Wc_11 for the weight from input to the first hidden unit of NN called "c"
#' @param inputs (vector) Vector cointain the state values for which the derivatives should be calculated
#' @param n_hidden (numeric) Number of neurons in the hidden layer, default value is 5
#' @param time_nn (boolean) Whether the NN is a time-dependent NN and negative weights should be applied from input to hidden layer. Default values is FALSE.
#' @param act (string) Activation function used in the NN. Currently "ReLU" and "Softplus" available.
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @return A vector of derivatives of the NN for the state values
#' @examples 
#' \dontrun{
#' est_parms <- pre_fixef_extractor_mlx("mlx_example1_ind.mlxtran")
#' names(est_parms) <- gsub("_pop","",names(est_parms))
#' inputs <- seq(0,10,length.out=1000)
#' outputs <- derivative_calc_mlx(nn_name="c",parms=est_parms,inputs=inputs,n_hidden=5)
#' plot(inputs,outputs)
#' }
#' @author Dominic Bräm
derivative_calc_mlx <- function(nn_name,parms,inputs,n_hidden=5,time_nn=FALSE,act="ReLU",beta=20){
  if(!(act %in% c("ReLU","Softplus"))){
    warning(paste0("Only ReLU and Softplus are implemented yet as activation functions\n
            Activation function of NN",nn_name," was set to ReLU"))
    act <- "ReLU"
  }
  out <- lapply(inputs,function(y){
    out <- with(as.list(c(parms)),{
    if(!time_nn){
      if(act=="ReLU"){
        hs <- lapply(1:n_hidden,function(x) get(paste0("W",nn_name,"_2",x)) * pmax(0,get(paste0("W",nn_name,"_1",x)) * y + get(paste0("b",nn_name,"_1",x))))
      } else if(act=="Softplus"){
        hs <- lapply(1:n_hidden,function(x) get(paste0("W",nn_name,"_2",x)) * 1/beta * log(1 + exp(beta * get(paste0("W",nn_name,"_1",x)) * y + get(paste0("b",nn_name,"_1",x)))))
      }
      out <- sum(as.vector(unlist(hs))) + get(paste0("b",nn_name,"_21"))
    } else{
      if(act=="ReLU"){
        hs <- lapply(1:n_hidden,function(x) get(paste0("W",nn_name,"_2",x)) * pmax(0,-get(paste0("W",nn_name,"_1",x))^2 * y + get(paste0("b",nn_name,"_1",x))))
      } else if(act=="Softplus"){
        hs <- lapply(1:n_hidden,function(x) get(paste0("W",nn_name,"_2",x)) * 1/beta * log(1 + exp(beta * -get(paste0("W",nn_name,"_1",x))^2 * y + get(paste0("b",nn_name,"_1",x)))))
      }
      out <- sum(as.vector(unlist(hs)))
    }
    return(out)
  })
  })
  return(unlist(out))
}

#' Generate Derivative versus State (Monolix)
#' 
#' This functions allows to generate derivative versus state data for a neural network from a NODE in Monolix.
#' 
#' Either \emph{est_parms} or \emph{mlx_file} must be given. If both arguments are given, \emph{est_parms} is prioritized.
#' 
#' @param nn_name (string) Name of the NN, e.g., \dQuote{c} for NNc(...)
#' @param min_state (numeric) Value of minimal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param max_state (numeric) Value of maximal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param inputs (numeric vector) Vector of input values for which derivatives should be calculated (optional if min_state and max_state is given)
#' @param est_parms (named vector; semi-optional) Named vector of estimated parameters from the NN extracted through the \emph{pre_fixef_extractor_mlx} function. For optionality, see \strong{Details}.
#' @param mlx_file (string; semi-optional) (path)/name of the Monolix run. Must include ".mlxtran" and estimation bust have been run previously. For optionality, see \strong{Details}.
#' @param length_out (numeric) Number of states between min_state and max_state for derivative calculations.
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' @param act (string) Activation function used in the NN. Currently "ReLU" and "Softplus" available.
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @param transform (string) Mathematical exression as string to transform the NN output. Independent variable must be called NN, e.g.,
#' "1/(1+exp(-NN))" for sigmoidal transformation.
#' @return Dataframe with columns for the state and the corresponding derivatives
#' @examples 
#' \dontrun{
#' est_parms <- pre_fixef_extractor_mlx("mlx_example1_ind.mlxtran")
#' derivative_data <- der_vs_state_mlx(nn="c",min_state=0,max_state=10,est_parms=est_parms)
#' ggplot(derivative_data) + geom_line(aes(x=state,y=derivatives))
#' }
#' @author Dominic Bräm
der_vs_state_mlx <- function(nn_name,min_state=NULL,max_state=NULL,inputs=NULL,est_parms=NULL,mlx_file=NULL,
                             length_out=100,time_nn=FALSE,act="ReLU",beta=20,transform=NULL){
  if(is.null(inputs) & (is.null(min_state) | is.null(max_state))){
    error_msg <- "Either inputs or both, min_state and max_state, must be given"
    stop(error_msg)
  }
  if(is.null(est_parms) & is.null(mlx_file)){
    error_msg <- "Either estimated parameters or monolix file must be given"
    stop(error_msg)
  }
  if(is.null(est_parms)){
    if(!is.character(mlx_file)){
      error_msg <- "mlx_file must be path and name of mlxtran file"
      stop(error_msg)
    }
    file_path <- file.path(tools::file_path_sans_ext(mlx_file),"populationParameters.txt")
    if(!file.exists(file_path)){
      error_msg <- paste("No population estimates available for",mlx_file)
      stop(error_msg)
    }
    est_parms <- pre_fixef_extractor_mlx(mlx_file)
  }
  names(est_parms) <- gsub("_pop","",names(est_parms))
  if(is.null(inputs)){
    inputs <- seq(min_state,max_state,length.out=length_out)
  }
  outputs <- derivative_calc_mlx(nn_name,est_parms,inputs,time_nn=time_nn,act=act,beta=beta)
  if(!is.null(transform)){
    if(!is.character(transform)){
      error_msg <- "transform must be a mathematical expression as string including NN as independent variable"
      stop(error_msg)
    }
    if(!grepl("NN",transform)){
      error_msg <- "transform must be a mathematical expression as string including NN as independent variable"
    }
    transform <- gsub("NN","outputs",transform)
    t <- try(eval(parse(text=transform)))
    if("try-error" %in% class(t)){
      error_msg <- "Invalid mathematical expression in transform"
      stop(error_msg)
    }
    outputs <- t
  }
  out <- data.frame(state=inputs,
                    derivatives=outputs)
  return(out)
}


#' Generate Derivative versus State with individual parameters (Monolix)
#' 
#' This functions allows to generate derivative versus state data for a neural network from a NODE in Monolix with individual parameters.
#' 
#' Either \emph{est_parms} or \emph{mlx_file} must be given. If both arguments are given, \emph{est_parms} is prioritized.
#' 
#' @param nn_name (string) Name of the NN, e.g., \dQuote{c} for NNc(...)
#' @param min_state (numeric) Value of minimal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param max_state (numeric) Value of maximal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param inputs (numeric vector) Vector of input values for which derivatives should be calculated (optional if min_state and max_state is given)
#' @param est_parms (named vector; semi-optional) A data frame with estimated individual parameters from the NN 
#' extracted through the \emph{indparm_extractor_mlx} function. For optionality, see \strong{Details}.
#' @param mlx_file (string; semi-optional) (path)/name of the Monolix run. Must include ".mlxtran" and estimation bust have been run previously. For optionality, see \strong{Details}.
#' @param length_out (numeric) Number of states between min_state and max_state for derivative calculations.
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' @param act (string) Activation function used in the NN. Currently "ReLU" and "Softplus" available.
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @param transform (string) Mathematical exression as string to transform the NN output. Independent variable must be called NN, e.g.,
#' "1/(1+exp(-NN))" for sigmoidal transformation.
#' @return Dataframe with columns for the state and the corresponding individual derivatives
#' @examples 
#' \dontrun{
#' ind_parms <- inparm_extractor_mlx("mlx_example1_ind.mlxtran")
#' derivative_data <- ind_der_vs_state_mlx(nn="c",min_state=0,max_state=10,est_parms=ind_parms)
#' }
#' @author Dominic Bräm
ind_der_vs_state_mlx <- function(nn_name,min_state=NULL,max_state=NULL,inputs=NULL,est_parms=NULL,mlx_file=NULL,time_nn=FALSE,
                                 length_out=100,act="ReLU",beta=20,transform=NULL){
  if(is.null(inputs) & (is.null(min_state) | is.null(max_state))){
    error_msg <- "Either inputs or both, min_state and max_state, must be given"
    stop(error_msg)
  }
  if(is.null(est_parms) & is.null(mlx_file)){
    error_msg <- "Either estimated parameters or monolix file must be given"
    stop(error_msg)
  }
  if(is.null(est_parms)){
    if(!is.character(mlx_file)){
      error_msg <- "mlx_file must be path and name of mlxtran file"
      stop(error_msg)
    }
    est_parms <- indparm_extractor_mlx(mlx_file)
  }
  names(est_parms) <- gsub("_pop","",names(est_parms))
  if(is.null(inputs)){
    inputs <- seq(min_state,max_state,length.out=length_out)
  }
  outputs <- apply(est_parms,1,function(x){
    names(x) <- gsub("_mode","",names(x))
    out <- derivative_calc_mlx(nn_name,x,inputs,time_nn=time_nn,act=act,beta=beta)
  })
  if(!is.null(transform)){
    if(!is.character(transform)){
      error_msg <- "transform must be a mathematical expression as string including NN as independent variable"
      stop(error_msg)
    }
    if(!grepl("NN",transform)){
      error_msg <- "transform must be a mathematical expression as string including NN as independent variable"
    }
    transform <- gsub("NN","outputs",transform)
    t <- try(eval(parse(text=transform)))
    if("try-error" %in% class(t)){
      error_msg <- "Invalid mathematical expression in transform"
      stop(error_msg)
    }
    outputs <- t
  }
  colnames(outputs) <- paste0("id_",est_parms[,"id"])
  out <- data.frame(state=inputs)
  out <- cbind(out,outputs)
  return(out)
}


#' Generate Derivative versus State Plot (Monolix)
#' 
#' This functions allows to generate a derivative versus state plot for a neural network from a NODE in Monolix.
#' 
#' Either \emph{est_parms} or \emph{mlx_file} must be given. If both arguments are given, \emph{est_parms} is prioritized.
#' 
#' @param nn_name (string) Name of the NN, e.g., \dQuote{c} for NNc(...)
#' @param min_state (numeric) Value of minimal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param max_state (numeric) Value of maximal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param inputs (numeric vector) Vector of input values for which derivatives should be calculated (optional if min_state and max_state is given)
#' @param est_parms (named vector; semi-optional) Named vector of estimated parameters from the NN extracted through the \emph{pre_fixef_extractor_mlx} function. For optionality, see \strong{Details}.
#' @param mlx_file (string; semi-optional) (path)/name of the Monolix run. Must include ".mlxtran" and estimation bust have been run previously. For optionality, see \strong{Details}.
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' @param act (string) Activation function used in the NN. Currently "ReLU" and "Softplus" available.
#' @param length_out (numeric) Number of points between min_state and max_state
#' @param plot_type (string) What plot type should be used; "base" or "ggplot"
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @param transform (string) Mathematical exression as string to transform the NN output. Independent variable must be called NN, e.g.,
#' "1/(1+exp(-NN))" for sigmoidal transformation.
#' @return Displaying derivative versus state plot; returns ggplot-object if \emph{plot_type="ggplot"}
#' @examples 
#' \dontrun{
#' der_state_plot <- der_state_plot_mlx(nn="c",
#'                                      min_state=0,max_state=10,
#'                                      mlx_file="mlx_example1_model_mlx_file_pop.mlxtran",
#'                                      plot_type="ggplot")
#' }
#' @author Dominic Bräm
#' @import ggplot2
#' @export
der_state_plot_mlx <- function(nn_name,min_state=NULL,max_state=NULL,inputs=NULL,est_parms=NULL,mlx_file=NULL,time_nn=FALSE,act="ReLU",
                               length_out=100,plot_type=c("base","ggplot"),beta=20,transform=NULL){
  data <- der_vs_state_mlx(nn_name=nn_name,min_state=min_state,max_state=max_state,inputs=inputs,
                           est_parms=est_parms,mlx_file=mlx_file,time_nn=time_nn,length_out=length_out,
                           act=act,beta=beta,transform=transform)
  
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
    if(requireNamespace("ggplot2", quietly = TRUE)){
      p <- ggplot2::ggplot(data) + ggplot2::geom_line(aes(x=state,y=derivatives)) +
        ggplot2::xlab("State") +
        ggplot2::ylab("Derivatives")
      print(p)
      return(p)
    } else{
      error_msg <- "To return a ggplot, the package ggplot2 must be installed"
      stop(error_msg)
    }
  }
}

#' Generate Derivative versus State Plot for individual parameter estimates (Monolix)
#' 
#' This functions allows to generate a derivative versus state plot for a neural network from a NODE in Monolix
#' with individual parameter estimates (EBEs).
#' 
#' Either \emph{est_parms} or \emph{mlx_file} must be given. If both arguments are given, \emph{est_parms} is prioritized.
#' 
#' @param nn_name (string) Name of the NN, e.g., \dQuote{c} for NNc(...)
#' @param min_state (numeric) Value of minimal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param max_state (numeric) Value of maximal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param inputs (numeric vector) Vector of input values for which derivatives should be calculated (optional if min_state and max_state is given)
#' @param est_parms (named vector; semi-optional) A data frame with estimated individual parameters from the NN 
#' extracted through the \emph{indparm_extractor_mlx} function. For optionality, see \strong{Details}.
#' @param mlx_file (string; semi-optional) (path)/name of the Monolix run. Must include ".mlxtran" and estimation bust have been run previously. For optionality, see \strong{Details}.
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' @param act (string) Activation function used in the NN. Currently "ReLU" and "Softplus" available.
#' @param ribbon (boolean) Whether individual derivatives versus states should be summarise in a ribbon (TRUE) or
#' displayed as individual spaghetti plot (FALSE)
#' @param length_out (numeric) Number of points between min_state and max_state
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @param transform (string) Mathematical exression as string to transform the NN output. Independent variable must be called NN, e.g.,
#' "1/(1+exp(-NN))" for sigmoidal transformation.
#' @return Displaying derivative versus state plot
#' @examples 
#' \dontrun{
#' der_state_plot <- ind_der_state_plot_mlx(nn="c",
#'                                          min_state=0,max_state=10,
#'                                          mlx_file="mlx_example1_model_mlx_file_pop.mlxtran",)
#' }
#' @author Dominic Bräm
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr starts_with
#' @export
ind_der_state_plot_mlx <- function(nn_name,min_state=NULL,max_state=NULL,inputs=NULL,est_parms=NULL,
                                   mlx_file=NULL,time_nn=FALSE,act="ReLU",
                               ribbon=TRUE,length_out=100,beta=20,transform=NULL){
  data <- ind_der_vs_state_mlx(nn_name=nn_name,min_state=min_state,max_state=max_state,inputs=inputs,
                           est_parms=est_parms,mlx_file=mlx_file,time_nn=time_nn,length_out=length_out,
                           act=act,beta=beta,transform=transform)
  
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
    print(p)
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
    print(p)
    return(p)
  }
}


#' Internal: Calculate the derivatives from a NN in NONMEM
#' 
#' Calculate the derivatives from a NN for derivative versus state plots. Can also be used for nlmixr2.
#' 
#' @param nn_name (string) Name of the NN, e.g., \dQuote{c} for NNc(...)
#' @param parms (named vector) Named vector of estimated parameters from the NN; Namings are e.g. Wc_11 for the weight from input to the first hidden unit of NN called "c"
#' @param inputs (vector) Vector cointain the state values for which the derivatives should be calculated
#' @param n_hidden (numeric) Number of neurons in the hidden layer, default value is 5
#' @param time_nn (boolean) Whether the NN is a time-dependent NN and negative weights should be applied from input to hidden layer. Default values is FALSE.
#' @param act (string) Activation function used in the NN. Currently "ReLU" and "Softplus" available.
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @return A vector of derivatives of the NN for the state values
#' @examples 
#' \dontrun{
#' est_parms <- pre_fixef_extractor_nm("nm_example1_model_converted_ind.res")
#' inputs <- seq(0,10,length.out=1000)
#' outputs <- derivative_calc_nm(nn_name="c",parms=est_parms,inputs=inputs,n_hidden=5)
#' plot(inputs,outputs)
#' }
#' @author Dominic Bräm
derivative_calc_nm <- function(nn_name,parms,inputs,n_hidden=5,time_nn=FALSE,act="ReLU",beta=20){
  if(!(act %in% c("ReLU","Softplus"))){
    warning(paste0("Only ReLU and Softplus are implemented yet as activation functions\n
            Activation function of NN",nn_name," was set to ReLU"))
    act <- "ReLU"
  }
  out <- lapply(inputs,function(y){
    out <- with(as.list(c(parms)),{
      if(!time_nn){
        if(act=="ReLU"){
          hs <- lapply(1:n_hidden,function(x) get(paste0("lW",nn_name,"_2",x)) * pmax(0,get(paste0("lW",nn_name,"_1",x)) * y + get(paste0("lb",nn_name,"_1",x))))
        } else if(act=="Softplus"){
          hs <- lapply(1:n_hidden,function(x) get(paste0("lW",nn_name,"_2",x)) * 1/beta * log(1 + exp(beta * get(paste0("lW",nn_name,"_1",x)) * y + get(paste0("lb",nn_name,"_1",x)))))
        }
        out <- sum(as.vector(unlist(hs))) + get(paste0("lb",nn_name,"_21"))
      } else{
        if(act=="ReLU"){
          hs <- lapply(1:n_hidden,function(x) get(paste0("lW",nn_name,"_2",x)) * pmax(0,-get(paste0("lW",nn_name,"_1",x))^2 * y + get(paste0("lb",nn_name,"_1",x))))
        } else if(act=="Softplus"){
          hs <- lapply(1:n_hidden,function(x) get(paste0("lW",nn_name,"_2",x)) * 1/beta * log(1 + exp(beta * -get(paste0("lW",nn_name,"_1",x))^2 * y + get(paste0("lb",nn_name,"_1",x)))))
        }
        out <- sum(as.vector(unlist(hs)))
      }
      return(out)
    })
  })
  return(unlist(out))
}


#' Generate Derivative versus State (NONMEM)
#' 
#' This functions allows to generate derivative versus state data for a neural network from a NODE in NONMEM.
#' Can also be used for nlmixr2.
#' 
#' Either \emph{est_parms} or \emph{nm_res_file} must be given. If both arguments are given, \emph{est_parms} is prioritized.
#' 
#' @param nn_name (string) Name of the NN, e.g., \dQuote{c} for NNc(...)
#' @param min_state (numeric) Value of minimal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param max_state (numeric) Value of maximal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param inputs (numeric vector) Vector of input values for which derivatives should be calculated (optional if min_state and max_state is given)
#' @param est_parms (named vector; semi-optional) Named vector of estimated parameters from the NN extracted through the \emph{pre_fixef_extractor_mlx} function. For optionality, see \strong{Details}.
#' @param nm_res_file (string; semi-optional) (path)/name of the results file of a NONMEM run, must include file extension, e.g., “.res”. For optionality, see \strong{Details}.
#' @param length_out (numeric) Number of states between min_state and max_state for derivative calculations.
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' @param act (string) Activation function used in the NN. Currently "ReLU" and "Softplus" available.
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @param transform (string) Mathematical exression as string to transform the NN output. Independent variable must be called NN, e.g.,
#' "1/(1+exp(-NN))" for sigmoidal transformation.
#' @return Dataframe with columns for the state and the corresponding derivatives
#' @examples 
#' \dontrun{
#' est_parms <- pre_fixef_extractor_nm("nm_example1_model_converted_ind.res")
#' derivative_data <- der_vs_state_nm(nn="c",min_state=0,max_state=10,est_parms=est_parms)
#' ggplot(derivative_data) + geom_line(aes(x=state,y=derivatives))
#' }
#' @author Dominic Bräm
der_vs_state_nm <- function(nn_name,min_state=NULL,max_state=NULL,inputs=NULL,est_parms=NULL,nm_res_file=NULL,
                            length_out=100,time_nn=FALSE,act="ReLU",beta=20,transform=NULL){
  if(is.null(inputs) & (is.null(min_state) | is.null(max_state))){
    error_msg <- "Either inputs or both, min_state and max_state, must be given"
    stop(error_msg)
  }
  if(is.null(est_parms) & is.null(nm_res_file)){
    error_msg <- "Either estimated parameters or NONMEM results file must be given"
    stop(error_msg)
  }
  if(is.null(est_parms)){
    if(!is.character(nm_res_file)){
      error_msg <- "nm_res_file must be path and name of NONMEM results file"
      stop(error_msg)
    }
    file_path <- nm_res_file
    if(!file.exists(file_path)){
      error_msg <- paste("NONMEM results file not found")
      stop(error_msg)
    }
    est_parms <- pre_fixef_extractor_nm(nm_res_file)
  }
  suppressWarnings({
    num_est_parms <- ifelse(is.na(as.numeric(est_parms)),0,as.numeric(est_parms))
  })
  names(num_est_parms) <- names(est_parms)
  if(is.null(inputs)){
    inputs <- seq(min_state,max_state,length.out=length_out)
  }
  outputs <- derivative_calc_nm(nn_name,num_est_parms,inputs,time_nn=time_nn,act=act,beta=beta)
  if(!is.null(transform)){
    if(!is.character(transform)){
      error_msg <- "transform must be a mathematical expression as string including NN as independent variable"
      stop(error_msg)
    }
    if(!grepl("NN",transform)){
      error_msg <- "transform must be a mathematical expression as string including NN as independent variable"
    }
    transform <- gsub("NN","outputs",transform)
    t <- try(eval(parse(text=transform)))
    if("try-error" %in% class(t)){
      error_msg <- "Invalid mathematical expression in transform"
      stop(error_msg)
    }
    outputs <- t
  }
  out <- data.frame(state=inputs,
                    derivatives=outputs)
  return(out)
}

#' Generate Derivative versus State with individual parameters (NONMEM)
#' 
#' This functions allows to generate derivative versus state data for a neural network from a NODE in NONMEM with individual parameters.
#' 
#' Either \emph{est_parms} or \emph{nm_res_file} and \emph{nm_phi_file} must be given. If both arguments are given, \emph{est_parms} is prioritized.
#' 
#' @param nn_name (string) Name of the NN, e.g., \dQuote{c} for NNc(...)
#' @param min_state (numeric) Value of minimal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param max_state (numeric) Value of maximal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param inputs (numeric vector) Vector of input values for which derivatives should be calculated (optional if min_state and max_state is given)
#' @param est_parms (named vector; semi-optional) A data frame with estimated individual parameters from the NN 
#' extracted through the \emph{indparm_extractor_nm} function. For optionality, see \strong{Details}.
#' @param nm_res_file (string; semi-optional) (path)/name of the results file of a NONMEM run, must include file extension, e.g., “.res”. For optionality, see \strong{Details}.
#' @param nm_phi_file (string; semi-optional) (path)/name of the phi file of a NONMEM run, must include file extension “.phi”. For optionality, see \strong{Details}.
#' @param length_out (numeric) Number of states between min_state and max_state for derivative calculations.
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' @param act (string) Activation function used in the NN. Currently "ReLU" and "Softplus" available.
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @param transform (string) Mathematical exression as string to transform the NN output. Independent variable must be called NN, e.g.,
#' "1/(1+exp(-NN))" for sigmoidal transformation.
#' @return Dataframe with columns for the state and the corresponding individual derivatives
#' @examples 
#' \dontrun{
#' ind_parms <- inparm_extractor_nm("nm_example1_ind.res","nm_example1_ind.phi")
#' derivative_data <- ind_der_vs_state_nm(nn="c",min_state=0,max_state=10,est_parms=ind_parms)
#' }
#' @author Dominic Bräm
ind_der_vs_state_nm <- function(nn_name,min_state=NULL,max_state=NULL,inputs=NULL,est_parms=NULL,nm_res_file=NULL,
                                nm_phi_file=NULL,length_out=100,time_nn=FALSE,act="ReLU",beta=20,transform=NULL){
  if(is.null(inputs) & (is.null(min_state) | is.null(max_state))){
    error_msg <- "Either inputs or both, min_state and max_state, must be given"
    stop(error_msg)
  }
  if(is.null(est_parms) & (is.null(nm_res_file) | is.null(nm_phi_file))){
    error_msg <- "Either estimated parameters or NONMEM results and phi file must be given"
    stop(error_msg)
  }
  if(is.null(est_parms)){
    if(!is.character(nm_res_file) | !is.character(nm_phi_file)){
      error_msg <- "nm_res_file and nm_phi_file must be path and name of NONMEM results file"
      stop(error_msg)
    }
    file_path <- nm_res_file
    if(!file.exists(file_path) | !file.exists(nm_phi_file)){
      error_msg <- paste("NONMEM results or phi file not found")
      stop(error_msg)
    }
    est_parms <- indparm_extractor_nm(nm_res_file,nm_phi_file)
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
  if(!is.null(transform)){
    if(!is.character(transform)){
      error_msg <- "transform must be a mathematical expression as string including NN as independent variable"
      stop(error_msg)
    }
    if(!grepl("NN",transform)){
      error_msg <- "transform must be a mathematical expression as string including NN as independent variable"
    }
    transform <- gsub("NN","outputs",transform)
    t <- try(eval(parse(text=transform)))
    if("try-error" %in% class(t)){
      error_msg <- "Invalid mathematical expression in transform"
      stop(error_msg)
    }
    outputs <- t
  }
  colnames(outputs) <- paste0("id_",est_parms[,"id"])
  out <- data.frame(state=inputs)
  out <- cbind(out,outputs)
  
  return(out)
}


#' Generate Derivative versus State Plot (NONMEM)
#' 
#' This functions allows to generate a derivative versus state plot for a neural network from a NODE in NONMEM.
#' Can also be used for nlmixr2.
#' 
#' Either \emph{est_parms} or \emph{nm_res_file} must be given. If both arguments are given, \emph{est_parms} is prioritized.
#' 
#' @param nn_name (string) Name of the NN, e.g., \dQuote{c} for NNc(...)
#' @param min_state (numeric) Value of minimal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param max_state (numeric) Value of maximal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param inputs (numeric vector) Vector of input values for which derivatives should be calculated (optional if min_state and max_state is given)
#' @param est_parms (named vector; semi-optional) Named vector of estimated parameters from the NN extracted through the \emph{pre_fixef_extractor_nm} function. For optionality, see \strong{Details}.
#' @param nm_res_file (string; semi-optional) (path)/name of the results file of a NONMEM run, must include file extension, e.g., “.res”. For optionality, see \strong{Details}.
#' @param length_out (numeric) Number of states between min_state and max_state for derivative calculations.
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' @param act (string) Activation function used in the NN. Currently "ReLU" and "Softplus" available.
#' @param plot_type (string) What plot type should be used; "base" or "ggplot"
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @param transform (string) Mathematical exression as string to transform the NN output. Independent variable must be called NN, e.g.,
#' "1/(1+exp(-NN))" for sigmoidal transformation.
#' @return Displaying derivative versus state plot; returns ggplot-object if \emph{plot_type="ggplot"}
#' @examples 
#' \dontrun{
#' der_state_plot <- der_state_plot_nm(nn="c",
#'                                     min_state=0,max_state=10,
#'                                     nm_res_file="nm_example1_model_converted_ind.res",
#'                                     plot_type="ggplot")
#' }
#' @author Dominic Bräm
#' @export
der_state_plot_nm <- function(nn_name,min_state=NULL,max_state=NULL,inputs=NULL,est_parms=NULL,nm_res_file=NULL,
                              length_out=100,time_nn=FALSE,act="ReLU",plot_type=c("base","ggplot"),beta=20,transform=NULL){
  data <- der_vs_state_nm(nn_name=nn_name,min_state=min_state,max_state=max_state,
                           est_parms=est_parms,nm_res_file=nm_res_file,length_out=length_out,
                          time_nn=time_nn,act=act,beta=beta,transform=transform)
  
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
    if(requireNamespace("ggplot2", quietly = TRUE)){
      p <- ggplot2::ggplot(data) + ggplot2::geom_line(aes(x=state,y=derivatives)) +
        ggplot2::xlab("State") +
        ggplot2::ylab("Derivatives")
      print(p)
      return(p)
    } else{
      error_msg <- "To return a ggplot, the package ggplot2 must be installed"
      stop(error_msg)
    }
  }
}

#' Generate Derivative versus State Plot for individual parameter estimates (NONMEM)
#' 
#' This functions allows to generate a derivative versus state plot for a neural network from a NODE in NONMEM
#' with individual parameter estimates (EBEs).
#' 
#' Either \emph{est_parms} or \emph{nm_res_file} and \emph{nm_phi_file} must be given. If both arguments are given, \emph{est_parms} is prioritized.
#' 
#' @param nn_name (string) Name of the NN, e.g., \dQuote{c} for NNc(...)
#' @param min_state (numeric) Value of minimal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param max_state (numeric) Value of maximal state for which the derivative should be calculated (optional if inputs is given, ignored if inputs is defined)
#' @param inputs (numeric vector) Vector of input values for which derivatives should be calculated (optional if min_state and max_state is given)
#' @param est_parms (named vector; semi-optional) A data frame with estimated individual parameters from the NN 
#' extracted through the \emph{indparm_extractor_nm} function. For optionality, see \strong{Details}.
#' @param nm_res_file (string; semi-optional) (path)/name of the results file of a NONMEM run, must include file extension, e.g., “.res”. For optionality, see \strong{Details}.
#' @param nm_phi_file (string; semi-optional) (path)/name of the phi file of a NONMEM run, must include file extension “.phi”. For optionality, see \strong{Details}.
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' @param act (string) Activation function used in the NN. Currently "ReLU" and "Softplus" available.
#' @param ribbon (boolean) Whether individual derivatives versus states should be summarise in a ribbon (TRUE) or
#' displayed as individual spaghetti plot (FALSE)
#' @param length_out (numeric) Number of points between min_state and max_state
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @param transform (string) Mathematical exression as string to transform the NN output. Independent variable must be called NN, e.g.,
#' "1/(1+exp(-NN))" for sigmoidal transformation.
#' @return Displaying derivative versus state plot
#' @examples 
#' \dontrun{
#' der_state_plot <- ind_der_state_plot_nm(nn="c",min_state=0,max_state=10,
#'                                         nm_res_file="nm_example1_ind.res",
#'                                         nm_phi_file="nm_example1_ind.phi")
#' }
#' @author Dominic Bräm
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr starts_with
#' @export
ind_der_state_plot_nm <- function(nn_name,min_state=NULL,max_state=NULL,inputs=NULL,est_parms=NULL,nm_res_file=NULL,
                                  nm_phi_file=NULL,length_out=100,time_nn=FALSE,ribbon=TRUE,act="ReLU",beta=20,
                                  transform=NULL){
  data <- ind_der_vs_state_nm(nn_name=nn_name,min_state=min_state,max_state=max_state,
                              est_parms=est_parms,nm_res_file=nm_res_file,nm_phi_file=nm_phi_file,
                              length_out=length_out,time_nn=time_nn,act=act,beta=beta,
                              transform=transform)
  
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
    print(p)
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
    print(p)
    return(p)
  }
  
}