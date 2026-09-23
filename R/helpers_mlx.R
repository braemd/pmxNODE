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
#' @author Dominic Bräm
#' @keywords internal
model_parm_extractor_mlx <- function(text){
  input_line <- grep("input\\s*=\\s*\\{",text)
  input <- text[input_line]
  
  parm_list <- gsub("input\\s*=\\s*","",input)
  parm_list <- gsub("[{} ]","",parm_list)
  
  parm_split <- strsplit(parm_list,",")[[1]]
  
  est_parms <- grepl("=",parm_split)
  parms <- parm_split[est_parms]
  regs <- parm_split[!est_parms]
  
  n_regs_text <- sum(grepl("use\\s*=\\s*regressor",text))
  if(n_regs_text != length(regs)){
    error_msg <- "Number of parameters in input without initial estimate does not match number of regressors"
    stop(error_msg)
  }
  
  parms_names <- gsub("([^=]+)=.+","\\1",parms)
  parms_values <- gsub("[^=]+=(.+)","\\1",parms)
  
  out <- list(parms_names,parms_values,regs)
  
  #parms_match <- gregexpr("\\w+\\s*=\\s*[^,^}]+",parm_list)
  #parms <- unlist(regmatches(parm_list,parms_match))
  
  #parms_name <- gsub("\\s*=\\s*[^,^}]+","",parms)
  #parms_values <- gsub("\\w+\\s*=\\s*","",parms)
  
  #out <- list(parms_name,parms_values)
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
#' @param model_reg_names (list of strings) A list of regressors
#' @param nn_thetas (list of strings) A list of all NN parameters
#' @return The Monolix model including all non-NN and NN parameters and regressors in \emph{input = ...}
#' @author Dominic Bräm
#' @keywords internal
model_parm_updater_mlx <- function(text,model_parm_names,model_reg_names,nn_thetas){
  input_line <- grep("input\\s*=\\s*\\{",text)
  
  if(length(model_parm_names)!=0 | length(model_reg_names)!=0){
    model_parm_names <- paste(c(unlist(model_parm_names),unlist(model_reg_names)),collapse = ",")
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
#' @param data_args (named list) Optional list of additional arguments to data of \emph{newProject} from \emph{lixoftConnectors}, i.e.,
#' \itemize{
#'    \item \emph{sheet} (string) for the sheet name if data_file is an excel file
#'    \item \emph{observationTypes} (list) defining "continuous", "discrete", or "event", default is "continuous"
#'    \item \emph{nbSSDoses} (integer) number of steady-state doses if SS column is present
#'    \item \emph{regressorsSettings} (character) regressors "lastCarriedForward" or "linearInterpolation"
#' }
#' @param mapping (list) List of mapping between model outputs and observation IDs
#' @param pmx_parm_dist (named list) Optional; named list of individual parameter distributions for non-NN parameters. "logNormal" is set for
#' all parameters not specified otherwise. Default is NULL, i.e., all non-NN parameters are set to "logNormal".
#' @return No return value, saving a Monolix .mlxtran file.
#' @author Dominic Bräm
#' @keywords internal
mlx_model_initializer <- function(model_name,model_file,data_file,header_types,
                                  parm_names,parm_inis,theta_names,theta_inis,
                                  pop=FALSE,omega_inis=NULL,pre_fixef=NULL,data_args=list(),mapping=NULL,
                                  pmx_parm_dist=NULL){
  
  if(requireNamespace("lixoftConnectors", quietly = TRUE)){
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
    
    data_args <- append(list(dataFile=data_file,headerTypes=header_types),data_args)
    
    if(!is.null(mapping)){
      lixoftConnectors::newProject(modelFile = model_file, data = data_args, mapping = mapping)
    } else{
      lixoftConnectors::newProject(modelFile = model_file, data = data_args)
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
    if(!is.null(pmx_parm_dist)){
      lixoftConnectors::setIndividualParameterDistribution(pmx_parm_dist)
    }
    
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
    message(paste0("Monolix file saved under: ",model_name))
  } else{
    stop("lixoftConnectors must first be loaded and initialized.")
  }
  
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
#' mlx_path <- system.file("extdata","mlx_example1_ind.mlxtran",package="pmxNODE")
#' est_parms <- pre_fixef_extractor_mlx(mlx_path)
#' @author Dominic Bräm
#' @export
pre_fixef_extractor_mlx <- function(model_name){
  if(!grepl("\\.mlxtran",model_name)){
    stop("Please provid a .mlxtran file that already run population estimation")
  }
  
  file_path <- gsub("\\.mlxtran","",model_name)
  pop_file_path <- file.path(file_path,"populationParameters.txt")
  
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
#' mlx_path <- system.file("extdata","mlx_example1_ind.mlxtran",package="pmxNODE")
#' est_parms <- indparm_extractor_mlx(mlx_path)
#' @author Dominic Bräm
#' @export
indparm_extractor_mlx <- function(model_name){
  if(!grepl("\\.mlxtran",model_name)){
    stop("Please provid a .mlxtran file that already run population estimation")
  }
  
  file_path <- gsub("\\.mlxtran","",model_name)
  ind_file_path <- file.path(file_path,"IndividualParameters","estimatedIndividualParameters.txt")
  
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
#' @return No return value, running the specified model in Monolix via lixoftConnectors.
#' @examples 
#' \dontrun{
#' run_mlx("mlx_file.mlxtran")
#' }
#' @author Dominic Bräm
#' @export
run_mlx <- function(mlx_file){
  if(!file.exists(mlx_file)){
    stop("Monolix file does not exist in given directory")
  }
  if(requireNamespace("lixoftConnectors", quietly = TRUE)){
    lixoftConnectors::loadProject(mlx_file)
    lixoftConnectors::runScenario()
    lixoftConnectors::saveProject()
    message("Monolix run and saved")
  } else{
    stop("lixoftConnectors must first be loaded and initialized.")
  }
}