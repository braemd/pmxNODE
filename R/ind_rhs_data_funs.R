#' Generate individual Right-hand side data (Monolix)
#' 
#' This functions allows to generate right-hand side data for multiple individuals with individual parameter sets, i.e.,
#' combined derivative data of multiple NNs and base-R operations.
#' 
#' Either \emph{est_parms} or \emph{mlx_file} must be given. If both arguments are given, \emph{est_parms} is prioritized.
#' 
#' @param rhs (string) String of right-hand side
#' @param inputs (dataframe) Dataframe of inputs, with corresponding columns (including matching column names 
#' for each variable in \emph{rhs}.
#' @param group (string) Name of column in \emph{inputs} dataframe defining groups/individuals.
#' @param est_parms (dataframe; semi-optional) A data frame with estimated individual parameters from the NN 
#' extracted through the \emph{indparm_extractor_mlx} function. For optionality, see \strong{Details}.
#' @param mlx_file (string; semi-optional) (path)/name of the Monolix run. Must include ".mlxtran" and estimation bust have been run previously. For optionality, see \strong{Details}.
#' @param time_nn (boolean vector) Vector for each NN in \emph{rhs} defining whether the neural network is a time-dependent neural network or not. Default value for all NN is FALSE.
#' @param act (character vector) Vector for each NN in \emph{rhs} defining the activation function used in the NN. Default value for all NN is "ReLU".
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if any \emph{act} is softplus; Default to 20.
#' @return Dataframe with columns for the inputs and the combined right-hand side data.
#' @author Dominic Bräm
#' @importFrom checkmate assert_data_frame
#' @importFrom checkmate assert_character
#' @keywords internal
ind_rhs_calc_mlx <- function(rhs,inputs,group,est_parms=NULL,mlx_file=NULL,time_nn=NULL,
                             act=NULL,beta=20){
  checkmate::assert_data_frame(inputs)
  checkmate::assert_character(rhs)
  checkmate::assert_character(group)
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
    est_parms <- indparm_extractor_mlx(mlx_file)
  }
  if(length(unique(inputs[,group])) != nrow(est_parms)){
    error_msg <- "For each group in inputs, corresponding parameters are required (and vice versa)"
    stop(error_msg)
  }
  
  colnames(est_parms) <- gsub("_mode","",colnames(est_parms))
  
  variables <- strsplit(rhs,"[^A-Za-z0-9]+")[[1]]
  no_num_vars <- is.na(suppressWarnings(as.numeric(variables)))
  no_base_funs <- unlist(lapply(variables,function(x) !(exists(x,envir=baseenv()))))
  variables <- variables[no_num_vars & no_base_funs]
  if(!all(variables %in% colnames(inputs[-which(names(inputs) == group)]))){
    error_msg <- "All variables in rhs need a corresponding input column in inputs"
    stop(error_msg)
  }
  
  nns <- grepl("NN",variables)
  if(is.null(time_nn)){
    time_nn <- rep(FALSE,sum(nns))
  } else{
    if(length(time_nn) != sum(nns)){
      error_msg <- "Either for none or all NNs in rhs time_nn must be defined"
      stop(error_msg)
    }
  }
  if(is.null(act)){
    act <- rep("ReLU",sum(nns))
  } else{
    if(length(act) != sum(nns)){
      error_msg <- "Either for none or all NNs in rhs act must be defined"
      stop(error_msg)
    }
  }
  
  time_nn_list <- `[<-`(list(rep(FALSE,length(variables))),nns,time_nn)
  act_list <- `[<-`(list(rep(FALSE,length(variables))),nns,act)
  
  inputs_split <- split(inputs,inputs[,group])
  parms_split <- split(est_parms,est_parms[,1])
  
  inter_out <- mapply(function(inps,parms){
    inputs_list <- as.list(`colnames<-`(as.data.frame(inps[,variables]),variables))
    parms <- unlist(parms)
    variables_out <- mapply(function(variable,nn,inputs,time_nn,act) {
      if(nn){
        nn_name <- gsub("NN","",variable)
        out <- der_vs_state_mlx(nn_name=nn_name,inputs=inputs,est_parms=parms,time_nn=time_nn,act=act)$derivatives
        return(out)
      } else{
        out <- inputs
      }
    },
    as.list(stats::setNames(variables,variables)),
    as.list(nns),
    inputs_list,
    time_nn_list,
    act_list,
    SIMPLIFY = FALSE
    )
    
    rhs_out <- with(variables_out,{
      out <- eval(parse(text = rhs))
      return(out)
    })
    
    out <- cbind(inps,data.frame(rhs = rhs_out))
    return(out)
  },
  inputs_split,
  parms_split,
  SIMPLIFY = FALSE)
  
  out <- do.call(rbind,inter_out)
  rownames(out) <- NULL
  return(out)
  
}

#' Generate individual Right-hand side data plot (Monolix)
#' 
#' This functions allows to generate a right-hand side plot with multiple subjects, i.e., combined derivative data of multiple NNs and base-R operations.
#' 
#' Either \emph{est_parms} or \emph{mlx_file} must be given. If both arguments are given, \emph{est_parms} is prioritized.
#' 
#' @param rhs (string) String of right-hand side
#' @param x_var (string) Name of the variable in inputs against which the right-hand data should be plotted.
#' @param inputs (dataframe) Dataframe of inputs, with corresponding columns (including matching column names 
#' for each variable in \emph{rhs}.
#' @param group (string) Name of column in \emph{inputs} dataframe defining groups/individuals.
#' @param est_parms (dataframe; semi-optional) A data frame with estimated individual parameters from the NN 
#' extracted through the \emph{indparm_extractor_mlx} function. For optionality, see \strong{Details}.
#' @param mlx_file (string; semi-optional) (path)/name of the Monolix run. Must include ".mlxtran" and estimation bust have been run previously. For optionality, see \strong{Details}.
#' @param time_nn (boolean vector) Vector for each NN in \emph{rhs} defining whether the neural network is a time-dependent neural network or not. Default value for all NN is FALSE.
#' @param act (character vector) Vector for each NN in \emph{rhs} defining the activation function used in the NN. Default value for all NN is "ReLU".
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if any \emph{act} is softplus; Default to 20.
#' @return ggplot of right-hand side plot for all individuals
#' @examples
#' # Generate individual rhs-plot for predicted observations
#' 
#' mlx_path <- system.file("extdata","mlx_example1_ind.mlxtran",package="pmxNODE")
#' data_path <- system.file("extdata","mlx_example1_ind","predictions.txt",package="pmxNODE")
#' 
#' est_parms <- indparm_extractor_mlx(mlx_path)
#' 
#' input_data <- read.table(data_path,sep=",",header=TRUE)[,c("id","indivPred_mode","time")]
#' colnames(input_data) <- c("id","NNc","NNct")
#'                   
#' rhs_plot <- ind_rhs_plot_mlx(rhs="NNc + NNct",
#'                              x_var = "NNc",
#'                              group = "id",
#'                              inputs = input_data,
#'                              est_parms = est_parms,
#'                              time_nn = c(FALSE, TRUE))
#' @author Dominic Bräm
#' @import ggplot2
#' @export
ind_rhs_plot_mlx <- function(rhs,x_var,inputs,group,est_parms=NULL,mlx_file=NULL,time_nn=NULL,
                         act=NULL,beta=20){
  if(!x_var %in% colnames(inputs)){
    error_msg <- "x_var must be the name of a column in the inputs dataframe."
    stop(error_msg)
  }
  
  rhs_data <- ind_rhs_calc_mlx(rhs,inputs,group,est_parms = est_parms, mlx_file = mlx_file,
                           time_nn = time_nn, act = act, beta = beta)
  
  p <- ggplot(rhs_data) + geom_line(aes(x=.data[[x_var]],y=.data[["rhs"]],group=.data[[group]]))
  return(p)
}

#' Generate individual Right-hand side data (NONMEM)
#' 
#' This functions allows to generate right-hand side data for multiple individuals with individual parameter sets, i.e.,
#' combined derivative data of multiple NNs and base-R operations.
#' 
#' Either \emph{est_parms} or \emph{nm_res_file} and \emph{nm_phi_file} must be given. If both arguments are given, \emph{est_parms} is prioritized.
#' 
#' @param rhs (string) String of right-hand side
#' @param inputs (dataframe) Dataframe of inputs, with corresponding columns (including matching column names 
#' for each variable in \emph{rhs}.
#' @param group (string) Name of column in \emph{inputs} dataframe defining groups/individuals.
#' @param est_parms (dataframe; semi-optional) A data frame with estimated individual parameters from the NN 
#' extracted through the \emph{indparm_extractor_nm} function. For optionality, see \strong{Details}.
#' @param nm_res_file (string; semi-optional) (path)/name of the results file of a NONMEM run, must include file extension, e.g., “.res”. For optionality, see \strong{Details}.
#' @param nm_phi_file (string; semi-optional) (path)/name of the phi file of a NONMEM run, must include file extension “.phi”. For optionality, see \strong{Details}.
#' @param time_nn (boolean vector) Vector for each NN in \emph{rhs} defining whether the neural network is a time-dependent neural network or not. Default value for all NN is FALSE.
#' @param act (character vector) Vector for each NN in \emph{rhs} defining the activation function used in the NN. Default value for all NN is "ReLU".
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if any \emph{act} is softplus; Default to 20.
#' @return Dataframe with columns for the inputs and the combined right-hand side data.
#' @author Dominic Bräm
#' @importFrom checkmate assert_data_frame
#' @importFrom checkmate assert_character
#' @keywords internal
ind_rhs_calc_nm <- function(rhs,inputs,group,est_parms=NULL,nm_res_file=NULL,nm_phi_file=NULL,time_nn=NULL,
                             act=NULL,beta=20){
  checkmate::assert_data_frame(inputs)
  checkmate::assert_character(rhs)
  checkmate::assert_character(group)
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
  if(length(unique(inputs[,group])) != nrow(est_parms)){
    error_msg <- "For each group in inputs, corresponding parameters are required (and vice versa)"
    stop(error_msg)
  }
  
  variables <- strsplit(rhs,"[^A-Za-z0-9]+")[[1]]
  no_num_vars <- is.na(suppressWarnings(as.numeric(variables)))
  no_base_funs <- unlist(lapply(variables,function(x) !(exists(x,envir=baseenv()))))
  variables <- variables[no_num_vars & no_base_funs]
  if(!all(variables %in% colnames(inputs[-which(names(inputs) == group)]))){
    error_msg <- "All variables in rhs need a corresponding input column in inputs"
    stop(error_msg)
  }
  
  nns <- grepl("NN",variables)
  if(is.null(time_nn)){
    time_nn <- rep(FALSE,sum(nns))
  } else{
    if(length(time_nn) != sum(nns)){
      error_msg <- "Either for none or all NNs in rhs time_nn must be defined"
      stop(error_msg)
    }
  }
  if(is.null(act)){
    act <- rep("ReLU",sum(nns))
  } else{
    if(length(act) != sum(nns)){
      error_msg <- "Either for none or all NNs in rhs act must be defined"
      stop(error_msg)
    }
  }
  
  time_nn_list <- `[<-`(list(rep(FALSE,length(variables))),nns,time_nn)
  act_list <- `[<-`(list(rep(FALSE,length(variables))),nns,act)
  
  inputs_split <- split(inputs,inputs[,group])
  parms_split <- split(est_parms,est_parms[,1])
  
  inter_out <- mapply(function(inps,parms){
    inputs_list <- as.list(`colnames<-`(as.data.frame(inps[,variables]),variables))
    parms <- unlist(parms)
    variables_out <- mapply(function(variable,nn,inputs,time_nn,act) {
      if(nn){
        nn_name <- gsub("NN","",variable)
        out <- der_vs_state_nm(nn_name=nn_name,inputs=inputs,est_parms=parms,time_nn=time_nn,act=act)$derivatives
        return(out)
      } else{
        out <- inputs
      }
    },
    as.list(stats::setNames(variables,variables)),
    as.list(nns),
    inputs_list,
    time_nn_list,
    act_list,
    SIMPLIFY = FALSE
    )
    
    rhs_out <- with(variables_out,{
      out <- eval(parse(text = rhs))
      return(out)
    })
    
    out <- cbind(inps,data.frame(rhs = rhs_out))
    return(out)
  },
  inputs_split,
  parms_split,
  SIMPLIFY = FALSE)
  
  out <- do.call(rbind,inter_out)
  rownames(out) <- NULL
  return(out)
  
}

#' Generate individual Right-hand side data plot (NONMEM)
#' 
#' This functions allows to generate a right-hand side plot with multiple subjects, i.e., combined derivative data of multiple NNs and base-R operations.
#' 
#' Either \emph{est_parms} or \emph{mlx_file} must be given. If both arguments are given, \emph{est_parms} is prioritized.
#' 
#' @param rhs (string) String of right-hand side
#' @param x_var (string) Name of the variable in inputs against which the right-hand data should be plotted.
#' @param inputs (dataframe) Dataframe of inputs, with corresponding columns (including matching column names 
#' for each variable in \emph{rhs}.
#' @param group (string) Name of column in \emph{inputs} dataframe defining groups/individuals.
#' @param est_parms (dataframe; semi-optional) A data frame with estimated individual parameters from the NN 
#' extracted through the \emph{indparm_extractor_nm} function. For optionality, see \strong{Details}.
#' @param nm_res_file (string; semi-optional) (path)/name of the results file of a NONMEM run, must include file extension, e.g., “.res”. For optionality, see \strong{Details}.
#' @param nm_phi_file (string; semi-optional) (path)/name of the phi file of a NONMEM run, must include file extension “.phi”. For optionality, see \strong{Details}.
#' @param time_nn (boolean vector) Vector for each NN in \emph{rhs} defining whether the neural network is a time-dependent neural network or not. Default value for all NN is FALSE.
#' @param act (character vector) Vector for each NN in \emph{rhs} defining the activation function used in the NN. Default value for all NN is "ReLU".
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if any \emph{act} is softplus; Default to 20.
#' @return ggplot of right-hand side plot for all individuals
#' @examples
#' # Generate individual rhs-plot for predicted observations
#' 
#' res_path <- system.file("extdata","nm_example1_model_converted_ind.res",package="pmxNODE")
#' phi_path <- system.file("extdata","nm_example1_model_converted_ind.phi",package="pmxNODE")
#' data_path <- system.file("extdata","nm_example1.tab",package="pmxNODE")
#' 
#' est_parms <- indparm_extractor_nm(res_path,phi_path)
#' 
#' input_data <- read.table(data_path,skip=1,header=TRUE)[,c("ID","IPRED","TIME")]
#' colnames(input_data) <- c("ID","NNc","NNt")
#'                   
#' rhs_plot <- ind_rhs_plot_nm(rhs="NNc + NNt",
#'                              x_var = "NNc",
#'                              inputs = input_data,
#'                              group = "ID",
#'                              est_parms=est_parms,
#'                              time_nn = c(FALSE, TRUE))
#' @author Dominic Bräm
#' @import ggplot2
#' @export
ind_rhs_plot_nm <- function(rhs,x_var,inputs,group,est_parms=NULL,nm_res_file=NULL,nm_phi_file=NULL,time_nn=NULL,
                             act=NULL,beta=20){
  if(!x_var %in% colnames(inputs)){
    error_msg <- "x_var must be the name of a column in the inputs dataframe."
    stop(error_msg)
  }
  
  rhs_data <- ind_rhs_calc_nm(rhs,inputs,group,est_parms = est_parms, nm_res_file = nm_res_file,
                              nm_phi_file = nm_phi_file, time_nn = time_nn, act = act, beta = beta)
  
  p <- ggplot(rhs_data) + geom_line(aes(x=.data[[x_var]],y=.data[["rhs"]],group=.data[[group]]))
  return(p)
}

#' Generate individual Right-hand side data (nlmixr2)
#' 
#' This functions allows to generate right-hand side data for multiple individuals with individual parameter sets, i.e.,
#' combined derivative data of multiple NNs and base-R operations.
#' 
#' Either \emph{est_parms} or \emph{fit_obj} must be given. If both arguments are given, \emph{est_parms} is prioritized.
#' 
#' @param rhs (string) String of right-hand side
#' @param inputs (dataframe) Dataframe of inputs, with corresponding columns (including matching column names 
#' for each variable in \emph{rhs}.
#' @param group (string) Name of column in \emph{inputs} dataframe defining groups/individuals.
#' @param est_parms (named vector; semi-optional) A data frame with estimated individual parameters from the NN 
#' extracted through the \emph{indparm_extractor_nlmixr} function. For optionality, see \strong{Details}.
#' @param fit_obj (nlmixr fit object; semi-optional) The fit-object from nlmixr2(...), fitted with IIV. For optionality, see \strong{Details}.
#' @param time_nn (boolean vector) Vector for each NN in \emph{rhs} defining whether the neural network is a time-dependent neural network or not. Default value for all NN is FALSE.
#' @param act (character vector) Vector for each NN in \emph{rhs} defining the activation function used in the NN. Default value for all NN is "ReLU".
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if any \emph{act} is softplus; Default to 20.
#' @return Dataframe with columns for the inputs and the combined right-hand side data.
#' @author Dominic Bräm
#' @importFrom checkmate assert_data_frame
#' @importFrom checkmate assert_character
#' @keywords internal
ind_rhs_calc_nlmixr <- function(rhs,inputs,group,est_parms=NULL,fit_obj=NULL,time_nn=NULL,
                             act=NULL,beta=20){
  checkmate::assert_data_frame(inputs)
  checkmate::assert_character(rhs)
  checkmate::assert_character(group)
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
  if(length(unique(inputs[,group])) != nrow(est_parms)){
    error_msg <- "For each group in inputs, corresponding parameters are required (and vice versa)"
    stop(error_msg)
  }
  
  variables <- strsplit(rhs,"[^A-Za-z0-9]+")[[1]]
  no_num_vars <- is.na(suppressWarnings(as.numeric(variables)))
  no_base_funs <- unlist(lapply(variables,function(x) !(exists(x,envir=baseenv()))))
  variables <- variables[no_num_vars & no_base_funs]
  if(!all(variables %in% colnames(inputs[-which(names(inputs) == group)]))){
    error_msg <- "All variables in rhs need a corresponding input column in inputs"
    stop(error_msg)
  }
  
  nns <- grepl("NN",variables)
  if(is.null(time_nn)){
    time_nn <- rep(FALSE,sum(nns))
  } else{
    if(length(time_nn) != sum(nns)){
      error_msg <- "Either for none or all NNs in rhs time_nn must be defined"
      stop(error_msg)
    }
  }
  if(is.null(act)){
    act <- rep("ReLU",sum(nns))
  } else{
    if(length(act) != sum(nns)){
      error_msg <- "Either for none or all NNs in rhs act must be defined"
      stop(error_msg)
    }
  }
  
  time_nn_list <- `[<-`(list(rep(FALSE,length(variables))),nns,time_nn)
  act_list <- `[<-`(list(rep(FALSE,length(variables))),nns,act)
  
  inputs_split <- split(inputs,inputs[,group])
  parms_split <- split(est_parms,est_parms[,1])
  
  inter_out <- mapply(function(inps,parms){
    inputs_list <- as.list(`colnames<-`(as.data.frame(inps[,variables]),variables))
    parms <- unlist(parms)
    variables_out <- mapply(function(variable,nn,inputs,time_nn,act) {
      if(nn){
        nn_name <- gsub("NN","",variable)
        out <- der_vs_state_nlmixr(nn_name=nn_name,inputs=inputs,est_parms=parms,time_nn=time_nn,act=act)$derivatives
        return(out)
      } else{
        out <- inputs
      }
    },
    as.list(stats::setNames(variables,variables)),
    as.list(nns),
    inputs_list,
    time_nn_list,
    act_list,
    SIMPLIFY = FALSE
    )
    
    rhs_out <- with(variables_out,{
      out <- eval(parse(text = rhs))
      return(out)
    })
    
    out <- cbind(inps,data.frame(rhs = rhs_out))
    return(out)
  },
  inputs_split,
  parms_split,
  SIMPLIFY = FALSE)
  
  out <- do.call(rbind,inter_out)
  rownames(out) <- NULL
  return(out)
  
}

#' Generate individual Right-hand side data plot (nlmixr2)
#' 
#' This functions allows to generate a right-hand side plot with multiple subjects, i.e., combined derivative data of multiple NNs and base-R operations.
#' 
#' Either \emph{est_parms} or \emph{fit_obj} must be given. If both arguments are given, \emph{est_parms} is prioritized.
#' 
#' @param rhs (string) String of right-hand side
#' @param x_var (string) Name of the variable in inputs against which the right-hand data should be plotted.
#' @param inputs (dataframe) Dataframe of inputs, with corresponding columns (including matching column names 
#' for each variable in \emph{rhs}.
#' @param group (string) Name of column in \emph{inputs} dataframe defining groups/individuals.
#' @param est_parms (named vector; semi-optional) A data frame with estimated individual parameters from the NN 
#' extracted through the \emph{indparm_extractor_nlmixr} function. For optionality, see \strong{Details}.
#' @param fit_obj (nlmixr fit object; semi-optional) The fit-object from nlmixr2(...), fitted with IIV. For optionality, see \strong{Details}.
#' @param time_nn (boolean vector) Vector for each NN in \emph{rhs} defining whether the neural network is a time-dependent neural network or not. Default value for all NN is FALSE.
#' @param act (character vector) Vector for each NN in \emph{rhs} defining the activation function used in the NN. Default value for all NN is "ReLU".
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if any \emph{act} is softplus; Default to 20.
#' @return ggplot of right-hand side for all individuals.
#' @examples 
#' \dontrun{
#' ind_fit <- nlmxir2(node_model_ind,data=data,est="saem")
#' rhs_plot <- ind_rhs_plot_mlx(rhs="NNc + NNct",
#'                              x_var = "NNc",
#'                              group = "id",
#'                              inputs = input_data,
#'                              fit_obj = ind_fit)
#' }
#' @author Dominic Bräm
#' @import ggplot2
#' @export
ind_rhs_plot_nlmixr <- function(rhs,x_var,inputs,group,est_parms=NULL,fit_obj=NULL,time_nn=NULL,
                             act=NULL,beta=20){
  if(!x_var %in% colnames(inputs)){
    error_msg <- "x_var must be the name of a column in the inputs dataframe."
    stop(error_msg)
  }
  
  rhs_data <- ind_rhs_calc_nlmixr(rhs,inputs,group,est_parms = est_parms, fit_obj = fit_obj,
                               time_nn = time_nn, act = act, beta = beta)
  
  p <- ggplot(rhs_data) + geom_line(aes(x=.data[[x_var]],y=.data[["rhs"]],group=.data[[group]]))
  return(p)
}