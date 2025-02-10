#' Internal: Correct NONMEM states
#' 
#' For NNs to be handled correctly, the state argument may not include brackets. \emph{state_correcter_nm} translates argument
#' \emph{state=A(1)} to \emph{state=A1} and \emph{A1=A(1)}
#' 
#' NULL
#' 
#' @param text (string) NN of form \dQuote{NN1(state=A(1),min_init=1,max_init=5)}
#' @return
#' \itemize{
#'   \item [1] NN with corrected state argument
#'   \item [2] State translation, e.g., \dQuote{A1=A(1)}
#' }
#' @examples
#' \dontrun{
#' nn_corrections <- state_correcter_nm("NN1(state=A(1),min_init=1,max_init=5)")
#' }
#' @author Dominic Bräm
state_correcter_nm <- function(text){
  a_state_match <- gregexpr("state\\s*=\\s*A\\(\\d+\\)",text)
  a_state <- unlist(regmatches(text,a_state_match))
  
  if(length(a_state)>0){
    a_state_nr <- gsub("state\\s*=\\s*A\\((\\d+)\\)","\\1",unlist(a_state))
    defs <- unique(paste0("A",a_state_nr," = A(",a_state_nr,")"))
    
    text <- gsub("state\\s*=\\s*A\\((\\d+)\\)","state=A\\1",text)
  } else{
    defs <- NULL
  }
  
  return(list(text,defs))
}

#' THETA extraction from results file
#' 
#' Function to extract THETA estimates from a results file of an already run NONMEM file.
#' 
#' Can be used, e.g., to initialize THETAs of a run with inter-individual variability with
#' estimated THETAs of a previous population run without inter-individual variability. Parameters, for which
#' final gradient is equal to 0 are fixed to 0, because a gradient of 0 indicates that corresponding neuron 
#' was inactivated during parameter estimation.
#' 
#' @param res_path (string) (Path/)Name of the results file of a NONMEM run, must include file extension, e.g., \dQuote{.res}
#' @return Named vector with parameter estimates from the previous run
#' @examples 
#' \dontrun{
#' pre_fixef <- prefix_extractor_nm("run_1.res")
#' }
#' @author Dominic Bräm
#' @export
pre_fixef_extractor_nm <- function(res_path){
  res_file <- readLines(res_path)
  
  theta_start <- grep("\\$THETA",res_file)
  omega_start <- grep("\\$OMEGA",res_file)
  
  theta_lines <- res_file[(theta_start+1):(omega_start-1)]
  thetas <- theta_lines[grep(".+",theta_lines)]
  theta_names <- unlist(lapply(thetas,function(x) {
    names_deffed <- grepl("[^\\[]*\\[(.*)\\]",x)
    if(names_deffed){
      name <- gsub("[^\\[]*\\[(.*)\\]","\\1",x)
    } else{
      name <- " "
    }
  }))
  
  
  theta_est_start <- grep("THETA - VECTOR OF FIXED EFFECTS PARAMETERS",res_file)
  omega_est_start <- grep("OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS",res_file)
  
  theta_est_lines <- res_file[(theta_est_start+1):(omega_est_start-1)]
  
  theta_est_names_match <- gregexpr("TH\\s*\\d+",theta_est_lines)
  theta_est_names <- unlist(regmatches(theta_est_lines,theta_est_names_match))
  theta_est_values_match <- gregexpr("-?\\d+\\.?\\d+E[+-]?\\d+",theta_est_lines)
  theta_est_values <- unlist(regmatches(theta_est_lines,theta_est_values_match))
  
  grad_start <- max(grep("GRADIENT\\:",res_file))
  term_start <- grep("\\#TERM\\:",res_file)
  grad_lines <- res_file[(grad_start):(term_start)]
  
  grad_match <- gregexpr("-?\\d+\\.?\\d+E[+-]?\\d+",grad_lines)
  grads <- unlist(regmatches(grad_lines,grad_match))
  num_grads <- as.numeric(gsub("[^0-9]","",grads))[1:length(theta_est_names)]
  zero_grads <- which(num_grads==0)
  
  theta_est_values[zero_grads] <- "0 FIX"
  
  names(theta_est_values) <- theta_names[1:length(theta_est_names)]
  return(theta_est_values)
  
}

#' NONMEM individual estimations extractor
#' 
#' When the NOMEM model has been run, this function allows to extract the
#' estimated individual parameters for NN parameters by combining information from the .res and
#' the .phi file
#' 
#' NULL
#' 
#' @param res_file (Path/)Name of the results file of a NONMEM run, must include file extension, e.g., \dQuote{.res}
#' @param phi_file (Path/)Name of the phi file of a NONMEM run, must include file extension, e.g., \dQuote{.phi}
#' @return Data frame with individual parameter estimates for NN parameters
#' @examples 
#' \dontrun{
#' est_parms <- indparm_extractor_nm("run_1_ind.res","run_1_ind.phi")
#' }
#' @author Dominic Bräm
#' @export
indparm_extractor_nm <- function(res_file,phi_file){
  if(!file.exists(phi_file)){
    stop("Provided .phi file from NONMEM run doesn't exist")
  }
  
  if(tools::file_ext(phi_file) != "phi"){
    stop("Please provid a .phi file from NONMEM run")
  }
  
  phi_text <- readLines(phi_file)[-1]
  phi_standardised <- gsub("\\s+"," ",phi_text)
  phi_split <- lapply(phi_standardised,function(x) unlist(strsplit(x,split=" ")))
  
  id_place <- grep("ID",phi_split[[1]])
  etas_place <- grep("ETA",phi_split[[1]])
  etas_names <- phi_split[[1]][etas_place]
  
  ids <- unlist(lapply(phi_split[-1],function(x) x[id_place]))
  etas <- lapply(phi_split[-1],function(x) as.numeric(x[etas_place]))
  
  res_text <- readLines(res_file)
  res_pk <- res_text[grep("\\$PK",res_text):grep("\\$DES",res_text)]
  res_des <- res_text[grep("\\$DES",res_text):grep("\\$ERROR",res_text)]
  
  nn_thetas_def <- res_pk[grep("lW[^_]+_[0-9]+ =|lb[^_]+_[0-9]+ =",res_pk)]
  nn_etas_def <- res_pk[grep("etaW[^_]+_[0-9]+ =|etab[^_]+_[0-9]+ =",res_pk)]
  
  nn_thetas_theta <- unlist(regmatches(nn_thetas_def,gregexpr("THETA(.+)",nn_thetas_def)))
  nn_etas_eta <- unlist(regmatches(nn_etas,gregexpr("ETA(.+)",nn_etas_def)))
  
  nn_thetas_names <- c(unlist(regmatches(nn_thetas_def,gregexpr("lW[^_]+_[0-9]+",nn_thetas_def))),unlist(regmatches(nn_thetas_def,gregexpr("lb[^_]+_[0-9]+",nn_thetas_def))))
  
  etas_df <- do.call(rbind,etas)
  colnames(etas_df) <- etas_names
  
  nn_etas_df <- etas_df[,etas_names %in% nn_etas_eta]
  
  thetas <- pre_fixef_extractor_nm(res_file)
  nn_thetas <- thetas[names(thetas) %in% nn_thetas_names]
  nn_thetas_names_order <- names(nn_thetas)
  suppressWarnings({
    nn_thetas_num <- ifelse(is.na(as.numeric(nn_thetas)),0,as.numeric(nn_thetas))
  })
  
  ind_parms <- t(apply(nn_etas_df,1,function(x) nn_thetas_num * exp(x)))
  colnames(ind_parms) <- nn_thetas_names_order
  
  id_df <- data.frame(id = ids)
  out <- cbind(id_df,ind_parms)
  
  return(out)
}

#' Run NONMEM from R
#'
#' Runs NONMEM from R
#' 
#' All paths must be given in R-style, i.e., slashes instead of backslashes. Paths can be absolute or relative.
#' 
#' @param ctl_file (string) Absolute or relative Path/Name of NONMEM file to run. Must be in R-style, i.e., path must be with
#' slashes. File must be given with file extension, e.g., nonmem_file\strong{.ctl}
#' @param nm_path (string) Absolute or relative Path/Name of NONMEM to be executed, e.g., "C:/nm75g64/run/nmfe75".
#' @param parralel_command (string) (Optional) Command for parralel NONMEM execution, e.g., "-parafile=C:/nm75g64/run/mpiwini8.pnm [nodes]=30"
#' @param create_dir (boolean) If NONMEM file should be run and saved in new directory. If TRUE, new directory of type 
#' \emph{path_to_ctl_file/ctl_name} will be created. Default is TRUE.
#' @param data_file (string) Absolute or relative Path/Name of data file to be used in the NONMEM run. Required if \emph{create_dir=}TRUE
#' as data file will be copied to new directory.
#' @return NULL
#' @examples 
#' \dontrun{
#' run_nm("./test/nm_test.ctl","c:/nm75g64/run/nmfe75",parralel_command = "-parafile=C:/nm75g64/run/mpiwini8.pnm [nodes]=30", data_file="~/Test/test/test_data.csv")
#' }
#' @author Dominic Bräm
#' @export
run_nm <- function(ctl_file,nm_path,parralel_command=NULL,create_dir=TRUE,data_file=NULL){
  nm_path <- gsub("/","\\\\",nm_path)
  parralel_command <- gsub("/","\\\\",parralel_command)
  if(!file.exists(ctl_file)){
    stop("NONMEM file does not exist in given directory")
  }
  if(create_dir){
    if(is.null(data_file)){
      stop("Data file must be provided if new_dir=TRUE")
    }
    if(!file.exists(data_file)){
      stop("Data file does not exist in given directory")
    }
  }
  
  if(!file.exists(paste0(nm_path,".bat"))){
    stop("NONMEM nmfe does not exist in given directory")
  }
  ctl_name <- basename(ctl_file)
  ctl_path <- dirname(ctl_file)
  ctl_name_no_ext <- gsub("(.*)\\..*$","\\1",ctl_name)
  if(create_dir){
    list_dirs <- list.dirs(path=ctl_path,full.names=F,recursive = F)
    dir_exists <- sum(list_dirs==ctl_name_no_ext) + sum(grepl(paste0(ctl_name_no_ext,"_\\d+"),list_dirs))
    if(dir_exists == 0){
      new_dir <- paste0(ctl_path,"/",ctl_name_no_ext)
      dir.create(new_dir)
    } else{
      new_dir <- paste0(ctl_path,"/",ctl_name_no_ext,"_",dir_exists+1)
      dir.create(new_dir)
    }
    file.copy(ctl_file,new_dir)
    file.copy(data_file,new_dir)
  } else{
    new_dir <- ctl_path
  }
  shell_command <- paste0("start cmd.exe /k \"cd ",getwd(), " & cd ",new_dir," &"," ",nm_path," ",ctl_name," ",ctl_name_no_ext,".res\"")
  if(!is.null(parralel_command)){
    shell_command <- paste0(shell_command," ",parralel_command)
  }
  shell(shell_command)
  if(create_dir & (dir_exists != 0)){
    warning(paste0("Given directory already exists, new directery was generated at: ",new_dir))
  } else{
    print(paste0("NONMEM run and saved in: ",new_dir))
  }
}

#' Finde path to NONMEM nmfe file
#' 
#' To run a NONMEM model, a NONMEM nmfeXX file is required, with XX the NONMEM version. When opening the NONMEM
#' command prompt, working directory is usually set to folder, where the nmfe file is located. When running NONMEM
#' from R (with the run_nm function), the path and the nmfe file must be provided (as the nm_path argument). To facilitate
#' the search for the nmfe file, this function can be used.
#' 
#' This function assumes that the path to the nmfe file is "\emph{root}/nmXXXX/run/nmfeXX, with XXXX as the NONMEM version. If
#' any special installation settings were applied, this function might not be working.
#' 
#' @param root (string) Path to the root where NONMEM was installed. Default is "C:/", working if NONMEM was installed directly
#' into the C drive.
#' @return Path and name of NONMEM nmfe file, that can directly be used as \emph{nm_path} argument in the \emph{run_nm} function.
#' @examples 
#' \dontrun{
#' nmfe_path <- find_nmfe()
#' run_nm(ctl_file="./test/nm_test.ctl",nm_path=nmfe_path,create_dir=FALSE)
#' }
#' @author Dominic Bräm
#' @export
find_nmfe <- function(root="C:/"){
  dir_list_root <- list.dirs(path = root,full.names = T, recursive = FALSE)
  nm_root <- dir_list_root[grep("nm\\d",dir_list_root)]
  nm_run <- paste0(nm_root,"/run")
  dir_list_run <- dir(path = nm_run,full.names = T, recursive = FALSE)
  nmfe_file <- dir_list_run[grep("nmfe\\d+\\.bat",dir_list_run)]
  nmfe_file_no_ext <- gsub("(.*)\\.bat","\\1",nmfe_file)
  
  return(nmfe_file_no_ext)
}
