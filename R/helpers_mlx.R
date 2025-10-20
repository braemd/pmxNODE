#' Internal: Extract model parameters
#' 
#' Extract model parameters and corresponding initial values from the model input line
#' 
#' NULL
#' 
#' @param text (list of strings) The Monolix model witch each line as element of list
#' @return 
#' \itemize{
#'   \item [1] - list of parameter names
#'   \item [2] - list of initial values
#'}
#' @examples 
#' \dontrun{
#' model_parms <- model_parm_extractor_mlx(list("input = {V=2,kel=0.1}"))
#' }
model_parm_extractor_mlx <- function(text){
  input_line <- grep("input\\s*=\\s*\\{",text)
  input <- text[input_line]
  
  parm_list <- gsub("input\\s*=\\s*","",input)
  
  parms_match <- gregexpr("\\w+\\s*=\\s*[^,^}]+",parm_list)
  parms <- unlist(regmatches(parm_list,parms_match))
  
  parms_name <- gsub("\\s*=\\s*[^,^}]+","",parms)
  parms_values <- gsub("\\w+\\s*=\\s*","",parms)
  
  out <- list(parms_name,parms_values)
  return(out)
}

#' Internal: Update input line
#' 
#' Update a Monolix model input to define non-NN and NN parameters as model inputs
#' 
#' NULL
#' 
#' @param text (list of strings) The Monolix model with each line as element of a list
#' @param model_parm_names (list of strings) A list of all non-NN parameters
#' @param nn_thetas (list of strings) A list of all NN parameters
#' @return The Monolix model including all non-NN and NN parameters in \emph{input = ...}
#' @examples 
#' \dontrun{
#' new_model <- model_parm_updater_mlx(list("input = {V,kel}"),
#'                                     list("V","kel"),
#'                                     list("W1","b1","W2","b2"))
#' }
#' @author Dominic Bräm
model_parm_updater_mlx <- function(text,model_parm_names,nn_thetas){
  input_line <- grep("input\\s*=\\s*\\{",text)
  
  if(length(model_parm_names)!=0){
    model_parm_names <- paste(unlist(model_parm_names),collapse = ",")
    nn_thetas <- paste(unlist(nn_thetas),collapse = ",")
    parms <- paste0(model_parm_names,",",nn_thetas)
  } else{
    nn_thetas <- paste(unlist(nn_thetas),collapse = ",")
    parms <- nn_thetas
  }
  
  input <- paste0("input = {",parms,"}")
  
  text[input_line] <- input
  return(text)
}

#' Internal: Monolix file initializer
#' 
#' Initializes a \emph{.mlxtran} file based on a converted model file, data, and initial values
#' 
#' If no specific file name was given to \emph{nn_converter_mlx} through the \emph{mlx_name} argumen,
#' the file is standardized to \strong{name of model file}_mlx_file_\strong{pop/ind} where \strong{pop} if \emph{pop=TRUE}
#' or \strong{ind} if \emph{pop=FALSE} in \emph{nn_converter_mlx}.
#' 
#' 
#' @param model_name (string) (Path/)Name of generated \emph{.mlxtran} file
#' @param model_file (string) (Path/)Name of converted structural model
#' @param data_file (string) (Path/)Name of data file to be fitted
#' @param header_types (vector) Vector of strings describing column types of data
#' @param parm_names (list) List of names of non-NN parameters in the model
#' @param parm_inis (list) List of initial values of non-NN parameters
#' @param theta_names (list) List of names of NN parameters in the model, i.e., weights and biases
#' @param theta_inis (list) List of initial values of NN parameters
#' @param pop (boolean) If only population fit without inter-individual variability (TRUE) or individual
#' fits with inter-individual variability (FALSE) should be used
#' @param omega_inis (numeric) Initial standard deviation of random effects on NN parameters; standard 0.1 from nn_converter_mlx
#' @param pre_fixef (named vector) Named vector of all initial values to be used for NN and non-NN parameters
#' @param obs_types (list) List of types of observations, e.g., \dQuote{continuous}; only required if non-continuous observations
#' @param mapping (list) List of mapping between model outputs and observation IDs
#' @return NA
#' @examples 
#' \dontrun{
#' mlx_model_initializer(model_name=mlx_name,model_file=file_name_new,
#'                       data_file=data_file,header_types=header_types,
#'                       parm_names=model_parms[1],parm_inis=model_parms[2],
#'                       theta_names=theta_defs,theta_inis=theta_inis,pop=pop,
#'                       pre_fixef=pre_fixef,omega_inis=eta_scale,
#'                       obs_types=obs_types,mapping=mapping)
#' }
#' @author Dominic Bräm
mlx_model_initializer <- function(model_name,model_file,data_file,header_types,
                                  parm_names,parm_inis,theta_names,theta_inis,
                                  pop=FALSE,omega_inis=NULL,pre_fixef=NULL,obs_types=NULL,mapping=NULL){
  
  parm_names <- unlist(parm_names)
  parm_inis <- unlist(parm_inis)
  theta_names <- unlist(theta_names)
  theta_inis <- unlist(theta_inis)
  
  header_possibilites <- c("ignore", "id", "time", "observation", "amount", "contcov",
                           "catcov", "occ", "evid", "mdv", "obsid", "cens", "limit",
                           "regressor", "nominaltime", "admid", "rate", "tinf", "ss",
                           "ii", "addl", "date")
  
  if(!all(header_types %in% header_possibilites)){
    stop(paste0("Header types must be in: ",paste(header_possibilites,collapse = ",")))
  }
  
  if(!is.null(obs_types) & is.null(mapping)){
    lixoftConnectors::newProject(modelFile = model_file, data = list(dataFile=data_file,headerTypes=header_types,observationTypes=obs_types))
  } else if(!is.null(obs_types) & !is.null(mapping)){
    lixoftConnectors::newProject(modelFile = model_file, data = list(dataFile=data_file,headerTypes=header_types,
                                                   observationTypes=obs_types,mapping=mapping))
  } else{
    lixoftConnectors::newProject(modelFile = model_file, data = list(dataFile=data_file,headerTypes=header_types))
  }
  
  lixoftConnectors::setConditionalDistributionSamplingSettings(enableMaxIterations=TRUE,nbMaxIterations=500)
  
  if(pop){
    lixoftConnectors::setPopulationParameterEstimationSettings(variability = "decreasing")
  }
  
  if(pop){
    nn_var_set <- as.list(rep(FALSE,length(theta_names)))
    names(nn_var_set) <- theta_names
    lixoftConnectors::setIndividualParameterVariability(nn_var_set)
  }
  
  nn_dist_set <- as.list(rep("normal",length(theta_names)))
  names(nn_dist_set) <- theta_names
  lixoftConnectors::setIndividualParameterDistribution(nn_dist_set)
  
  if(is.null(pre_fixef)){
    if(length(parm_names) != 0){
      model_theta_df <- data.frame(name=paste0(parm_names,"_pop"),
                            initialValue=parm_inis,
                            method="MLE")
        
      model_omega_df <- data.frame(name=paste0("omega_",parm_names),
                                   initialValue=1,
                                   method="MLE")
    }
    
    nn_theta_df <- data.frame(name=paste0(theta_names,"_pop"),
                             initialValue=theta_inis,
                             method="MLE")
    
    if(!pop){
      nn_omega_df <- data.frame(name=paste0("omega_",theta_names),
                                initialValue=omega_inis,
                                method="MLE")
    }
    
    if(length(parm_names) != 0){
      mlx_inis <- rbind(model_theta_df,
                        nn_theta_df,
                        model_omega_df)
    } else{
      mlx_inis <- nn_theta_df
    }
    
    if(!pop){
      mlx_inis <- rbind(mlx_inis,
                        nn_omega_df)
    }
  } else{
    theta_df <- data.frame(name=names(pre_fixef),
                           initialValue=pre_fixef,
                           method="MLE")
    
    model_omega_df <- data.frame(name=paste0("omega_",parm_names),
                                 initialValue=1,
                                 method="MLE")
    
    if(!pop){
      nn_omega_df <- data.frame(name=paste0("omega_",theta_names),
                                initialValue=omega_inis,
                                method="MLE")
      
      mlx_inis <- rbind(theta_df,
                        model_omega_df,
                        nn_omega_df)
      
    } else{
      mlx_inis <- rbind(theta_df,
                        model_omega_df)
    }
    
    
  }
  
  obs_model <- lixoftConnectors::getContinuousObservationModel()
  n_obs <- length(obs_model$errorModel)
  if(n_obs>1){
    error_parms <- paste0(c("a","b","c"),rep(1:n_obs,each=3))
  } else{
    error_parms <- c("a","b","c")
  }
  error_inits <- rep(c(1,0.3,1),n_obs)
  error_methods <- rep(c("MLE","MLE","FIXED"),n_obs)
  error_df <- data.frame(name=error_parms,
                         initialValue=error_inits,
                         method=error_methods)
  
  #error_df not required!?
  mlx_inis <- rbind(mlx_inis)#,
                    #error_df)
  
  mlx_inis$initialValue <- as.numeric(mlx_inis$initialValue)
  
  lixoftConnectors::setPopulationParameterInformation(mlx_inis)
  
  lixoftConnectors::saveProject(paste0(model_name,".mlxtran"))
  print(paste0("Monolix file saved under: ",model_name))
  
}
#' Monolix estimations extractor
#' 
#' When the Monolix model has been run, e.g., with only population estimation, this function allows to extract the
#' estimated parameters from the Monolix run folder. This function is meant, e.g., to get initial values for a
#' Monolix run with inter-individual variability and to be then used as \emph{pre_fixef} argument in the
#' \emph{nn_converter_mlx} function
#' 
#' NULL
#' 
#' @param model_name (string) Name of the Monolix run. Must include \dQuote{.mlxtran}
#' @return Named vector of Monolix parameter estimations
#' @examples 
#' \dontrun{
#' est_parms <- pre_fixef_extractor_mlx("run_1_pop.mlxtran")
#' nn_converter_mlx(...,pre_fixef=est_parms)
#' }
#' @author Dominic Bräm
#' @export
pre_fixef_extractor_mlx <- function(model_name){
  if(!grepl("\\.mlxtran",model_name)){
    stop("Please provid a .mlxtran file that already run population estimation")
  }
  
  file_path <- gsub("\\.mlxtran","",model_name)
  pop_file_path <- paste0(file_path,"/populationParameters.txt")
  
  if(!file.exists(pop_file_path)){
    stop("No population estimates available for provided Monolix file")
  }
  
  parm_table <- utils::read.table(pop_file_path,header=T,sep=",")
  
  pop_parm_table <- parm_table[grepl("_pop",parm_table$parameter),]
  
  pre_fixef <- pop_parm_table$value
  names(pre_fixef) <- pop_parm_table$parameter
  
  return(pre_fixef)
}

#' Monolix individual estimations extractor
#' 
#' When the Monolix model has been run, this function allows to extract the
#' estimated individual parameters (EBEs) from the Monolix run folder.
#' 
#' NULL
#' 
#' @param model_name (string) Name of the Monolix run. Must include \dQuote{.mlxtran}
#' @return Data frame with individual parameter estimates (EBEs)
#' @examples 
#' \dontrun{
#' est_parms <- indparm_extractor_mlx("run_1_ind.mlxtran")
#' }
#' @author Dominic Bräm
#' @export
indparm_extractor_mlx <- function(model_name){
  if(!grepl("\\.mlxtran",model_name)){
    stop("Please provid a .mlxtran file that already run population estimation")
  }
  
  file_path <- gsub("\\.mlxtran","",model_name)
  ind_file_path <- paste0(file_path,"/IndividualParameters/estimatedIndividualParameters.txt")
  
  if(!file.exists(ind_file_path)){
    stop("No individual estimates available for provided Monolix file")
  }
  
  parm_table <- utils::read.table(ind_file_path,header=T,sep=",")
  
  ind_parm_table <- parm_table[,grepl("id|_mode",colnames(parm_table))]
  
  return(ind_parm_table)
}

#' Run Monolix from R
#'
#' Runs Monolix from R
#' 
#' All paths must be given in R-style, i.e., slashes instead of backslashes. Paths can be absolute or relative.
#' 
#' @param mlx_file (string) Absolute or relative Path/Name of Monolix file to run. Must be in R-style, i.e., path must be with
#' slashes. File must be given with file extension, e.g., monolix_file\strong{.mlxtran}
#' @return NULL
#' @examples 
#' \dontrun{
#' fun_mlx("mlx_file.mlxtran")
#' }
#' @author Dominic Bräm
#' @export
run_mlx <- function(mlx_file){
  if(!file.exists(mlx_file)){
    stop("Monolix file does not exist in given directory")
  }
  if(!("lixoftConnectors" %in% .packages())){
    stop("lixoftConnectors must first be initialized. Use software_initializer(...) prior to use run_mlx")
  }
  lixoftConnectors::loadProject(mlx_file)
  lixoftConnectors::runScenario()
  lixoftConnectors::saveProject()
  print("Monolix run and saved")
}