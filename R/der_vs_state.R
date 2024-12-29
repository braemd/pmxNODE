#' Internal: Calculate the derivatives from a NN in Monolix
#' 
#' Calculate the derivatives from a NN for derivative versus state plots
#' 
#' @param nn_name (string) Name of the NN, e.g., \dQuote{c} for NNc(...)
#' @param parms (named vector) Named vector of estimated parameters from the NN; Namings are e.g. Wc_11 for the weight from input to the first hidden unit of NN called "c"
#' @param inputs (vector) Vector cointain the state values for which the derivatives should be calculated
#' @param n_hidden (numeric) Number of neurons in the hidden layer, default value is 5
#' @param time_nn (boolean) Whether the NN is a time-dependent NN and negative weights should be applied from input to hidden layer. Default values is FALSE.
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
#' Either \emph{est_parms} or \emph{mlx_file} must be given. If both arguments are given, \emph{est_parms} is priotized.
#' 
#' @param nn_name (string) Name of the NN, e.g., \dQuote{c} for NNc(...)
#' @param min_state (numeric) Value of minimal state for which the derivative should be calculated
#' @param max_state (numeric) Value of maximal state for which the derivative should be calculated
#' @param est_parms (named vector; semi-optional) Named vector of estimated parameters from the NN extracted through the \emph{pre_fixef_extractor_mlx} function. For optionality, see \strong{Details}.
#' @param mlx_file (string; semi-optional) (path)/name of the Monolix run. Must include ".mlxtran" and estimation bust have been run previously. For optionality, see \strong{Details}.
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' #' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @return Dataframe with columns for the state and the corresponding derivatives
#' @examples 
#' \dontrun{
#' est_parms <- pre_fixef_extractor_mlx("mlx_example1_ind.mlxtran")
#' derivative_data <- der_vs_state_mlx(nn="c",min_state=0,max_state=10,est_parms=est_parms)
#' ggplot(derivative_data) + geom_line(aes(x=state,y=derivatives))
#' }
#' @author Dominic Bräm
der_vs_state_mlx <- function(nn_name,min_state,max_state,est_parms=NULL,mlx_file=NULL,time_nn=FALSE,act="ReLU",beta=20){
  if(is.null(est_parms) & is.null(mlx_file)){
    error_msg <- "Either estimated parameters or monolix file must be given"
    stop(error_msg)
  }
  if(is.null(est_parms)){
    if(!is.character(mlx_file)){
      error_msg <- "mlx_file must be path and name of mlxtran file"
      stop(error_msg)
    }
    file_path <- paste0(tools::file_path_sans_ext(mlx_file),"/populationParameters.txt")
    if(!file.exists(file_path)){
      error_msg <- paste("No population estimates available for",mlx_file)
      stop(error_msg)
    }
    est_parms <- pre_fixef_extractor_mlx(mlx_file)
  }
  names(est_parms) <- gsub("_pop","",names(est_parms))
  inputs <- seq(min_state,max_state,length.out=1000)
  outputs <- derivative_calc_mlx(nn_name,est_parms,inputs,time_nn=time_nn,act=act,beta=beta)
  out <- data.frame(state=inputs,
                    derivatives=outputs)
  return(out)
}


#' Generate Derivative versus State Plot (Monolix)
#' 
#' This functions allows to generate a derivative versus state plot for a neural network from a NODE in Monolix.
#' 
#' Either \emph{est_parms} or \emph{mlx_file} must be given. If both arguments are given, \emph{est_parms} is priotized.
#' 
#' @param nn_name (string) Name of the NN, e.g., \dQuote{c} for NNc(...)
#' @param min_state (numeric) Value of minimal state for which the derivative should be calculated
#' @param max_state (numeric) Value of maximal state for which the derivative should be calculated
#' @param est_parms (named vector; semi-optional) Named vector of estimated parameters from the NN extracted through the \emph{pre_fixef_extractor_mlx} function. For optionality, see \strong{Details}.
#' @param mlx_file (string; semi-optional) (path)/name of the Monolix run. Must include ".mlxtran" and estimation bust have been run previously. For optionality, see \strong{Details}.
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' @param plot_type (string) What plot type should be used; "base" or "ggplot"
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @return Displaying derivative versus state plot; returns ggplot-object if \emph{plot_type="ggplot"}
#' @examples 
#' \dontrun{
#' der_state_plot <- der_state_plot_mlx(nn="c",min_state=0,max_state=10,mlx_file="mlx_example1_model_mlx_file_pop.mlxtran",plot_type="ggplot")
#' }
#' @author Dominic Bräm
#' @export
der_state_plot_mlx <- function(nn_name,min_state,max_state,est_parms=NULL,mlx_file=NULL,time_nn=FALSE,act="ReLU",
                               plot_type=c("base","ggplot"),beta=20){
  data <- der_vs_state_mlx(nn_name=nn_name,min_state=min_state,max_state=max_state,
                           est_parms=est_parms,mlx_file=mlx_file,time_nn=time_nn,act=act,
                           beta=beta)
  
  if(length(plot_type)>1){
    plot_type <- "base"
  }
  if(!(plot_type %in% c("base","ggplot"))){
    error_msg <- "Please provide valid plot type, i.e., base or ggplot"
    stop(error_msg)
  }
  if(!("ggplot2" %in% installed.packages())){
    error_msg <- "To return a ggplot, the package ggplot2 must be installed"
    stop(error_msg)
  }
  
  if(plot_type == "base"){
    plot(data$state,data$derivatives,xlab="State",ylab="Derivatives")
  } else if(plot_type == "ggplot"){
    p <- ggplot2::ggplot(data) + ggplot2::geom_line(aes(x=state,y=derivatives)) +
      ggplot2::xlab("State") +
      ggplot2::ylab("Derivatives")
    print(p)
    return(p)
  }
}


#' Internal: Calculate the derivatives from a NN in NONMEM
#' 
#' Calculate the derivatives from a NN for derivative versus state plots
#' 
#' @param nn_name (string) Name of the NN, e.g., \dQuote{c} for NNc(...)
#' @param parms (named vector) Named vector of estimated parameters from the NN; Namings are e.g. Wc_11 for the weight from input to the first hidden unit of NN called "c"
#' @param inputs (vector) Vector cointain the state values for which the derivatives should be calculated
#' @param n_hidden (numeric) Number of neurons in the hidden layer, default value is 5
#' @param time_nn (boolean) Whether the NN is a time-dependent NN and negative weights should be applied from input to hidden layer. Default values is FALSE.
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
#' 
#' Either \emph{est_parms} or \emph{nm_res_file} must be given. If both arguments are given, \emph{est_parms} is priotized.
#' 
#' @param nn_name (string) Name of the NN, e.g., \dQuote{c} for NNc(...)
#' @param min_state (numeric) Value of minimal state for which the derivative should be calculated
#' @param max_state (numeric) Value of maximal state for which the derivative should be calculated
#' @param est_parms (named vector; semi-optional) Named vector of estimated parameters from the NN extracted through the \emph{pre_fixef_extractor_mlx} function. For optionality, see \strong{Details}.
#' @param nm_res_file (string; semi-optional) (path)/name of the results file of a NONMEM run, must include file extension, e.g., “.res”. For optionality, see \strong{Details}.
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @return Dataframe with columns for the state and the corresponding derivatives
#' @examples 
#' \dontrun{
#' est_parms <- pre_fixef_extractor_nm("nm_example1_model_converted_ind.res")
#' derivative_data <- der_vs_state_nm(nn="c",min_state=0,max_state=10,est_parms=est_parms)
#' ggplot(derivative_data) + geom_line(aes(x=state,y=derivatives))
#' }
#' @author Dominic Bräm
der_vs_state_nm <- function(nn_name,min_state,max_state,est_parms=NULL,nm_res_file=NULL,time_nn=FALSE,act="ReLU",beta=20){
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
      error_msg <- paste("NONMEM results fiel not found")
      stop(error_msg)
    }
    est_parms <- pre_fixef_extractor_nm(nm_res_file)
  }
  suppressWarnings({
    num_est_parms <- ifelse(is.na(as.numeric(est_parms)),0,as.numeric(est_parms))
  })
  names(num_est_parms) <- names(est_parms)
  inputs <- seq(min_state,max_state,length.out=1000)
  outputs <- derivative_calc_nm(nn_name,num_est_parms,inputs,time_nn=time_nn,act=act,beta=beta)
  out <- data.frame(state=inputs,
                    derivatives=outputs)
  return(out)
}

#' Generate Derivative versus State Plot (NONMEM)
#' 
#' This functions allows to generate a derivative versus state plot for a neural network from a NODE in NONMEM.
#' 
#' Either \emph{est_parms} or \emph{mlx_file} must be given. If both arguments are given, \emph{est_parms} is priotized.
#' 
#' @param nn_name (string) Name of the NN, e.g., \dQuote{c} for NNc(...)
#' @param min_state (numeric) Value of minimal state for which the derivative should be calculated
#' @param max_state (numeric) Value of maximal state for which the derivative should be calculated
#' @param est_parms (named vector; semi-optional) Named vector of estimated parameters from the NN extracted through the \emph{pre_fixef_extractor_mlx} function. For optionality, see \strong{Details}.
#' @param nm_res_file (string; semi-optional) (path)/name of the results file of a NONMEM run, must include file extension, e.g., “.res”. For optionality, see \strong{Details}.
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' @param plot_type (string) What plot type should be used; "base" or "ggplot"
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @return Displaying derivative versus state plot; returns ggplot-object if \emph{plot_type="ggplot"}
#' @examples 
#' \dontrun{
#' der_state_plot <- der_state_plot_nm(nn="c",min_state=0,max_state=10,nm_res_file="nm_example1_model_converted_ind.res",plot_type="ggplot")
#' }
#' @author Dominic Bräm
#' @export
der_state_plot_nm <- function(nn_name,min_state,max_state,est_parms=NULL,nm_res_file=NULL,time_nn=FALSE,act="ReLU",
                               plot_type=c("base","ggplot"),beta=20){
  data <- der_vs_state_nm(nn_name=nn_name,min_state=min_state,max_state=max_state,
                           est_parms=est_parms,nm_res_file=nm_res_file,time_nn=time_nn,act=act,
                          beta=beta)
  
  if(length(plot_type)>1){
    plot_type <- "base"
  }
  if(!(plot_type %in% c("base","ggplot"))){
    error_msg <- "Please provide valid plot type, i.e., base or ggplot"
    stop(error_msg)
  }
  if(!("ggplot2" %in% installed.packages())){
    error_msg <- "To return a ggplot, the package ggplot2 must be installed"
    stop(error_msg)
  }
  
  if(plot_type == "base"){
    plot(data$state,data$derivatives,xlab="State",ylab="Derivatives")
  } else if(plot_type == "ggplot"){
    p <- ggplot2::ggplot(data) + ggplot2::geom_line(aes(x=state,y=derivatives)) +
      ggplot2::xlab("State") +
      ggplot2::ylab("Derivatives")
    print(p)
    return(p)
  }
}
