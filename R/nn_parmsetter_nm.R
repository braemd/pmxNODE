#' Internal: Define NN THETAs in NONMEM
#' 
#' Define NN THETAS used in the $PK section in NONMEM
#' 
#' Parameter definition in form of lW = THETA(X) with X the next number of THETA \cr
#' e.g., if \cr
#' lV = THETA(1) \cr
#' lkel = THETA(2) \cr
#' X is equal to 3
#' 
#' @param number (string) Name of the NN, e.g., \dQuote{1} for NN1(...)
#' @param theta_start (numeric) Number with which to start the THETA count of NN parameters
#' @param n_hidden (numeric) Number of neurons in the hidden layer, default value is 5
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' @return Vector with all NN parameter THETA definitions for a NN
#' @examples 
#' \dontrun{
#' nn_parm_names <- nn_theta_def_nm("1",3)
#' }
#' @author Dominic Bräm
nn_theta_def_nm <- function(number,theta_start,n_hidden=5,time_nn=FALSE){
  if(!time_nn){
    w1s <- paste0("lW",number,"_1",1:n_hidden," = THETA(",theta_start:(theta_start+n_hidden-1),")")
    theta_start <- theta_start+n_hidden
    b1s <- paste0("lb",number,"_1",1:n_hidden," = THETA(",theta_start:(theta_start+n_hidden-1),")")
    theta_start <- theta_start+n_hidden
    w2s <- paste0("lW",number,"_2",1:n_hidden," = THETA(",theta_start:(theta_start+n_hidden-1),")")
    theta_start <- theta_start+n_hidden
    b2s <- paste0("lb",number,"_21 = THETA(",theta_start,")")
    
    parms <- unlist(list(w1s,b1s,w2s,b2s))
  } else{
    w1s <- paste0("lW",number,"_1",1:n_hidden," = THETA(",theta_start:(theta_start+n_hidden-1),")")
    theta_start <- theta_start+n_hidden
    b1s <- paste0("lb",number,"_1",1:n_hidden," = THETA(",theta_start:(theta_start+n_hidden-1),")")
    theta_start <- theta_start+n_hidden
    w2s <- paste0("lW",number,"_2",1:n_hidden," = THETA(",theta_start:(theta_start+n_hidden-1),")")
    
    parms <- unlist(list(w1s,b1s,w2s))
  }
  
  
  defs <- list(parms,length(parms))
  return(defs)
}

#' Internal: Define NN ETAs in NONMEM
#' 
#' Define random effects of NN parameters as ETAs used in the $PK section in NONMEM
#' 
#' Parameter definition in form of etaW = ETA(X) with X the next number of ETA \cr
#' e.g., if \cr
#' etaV = ETA(1) \cr
#' etakel = ETA(2) \cr
#' X is equal to 3
#' 
#' @param number (string) Name of the NN, e.g., \dQuote{1} for NN1(...)
#' @param eta_start (numeric) Number with which to start the ETA count of NN parameters
#' @param n_hidden (numeric) Number of neurons in the hidden layer, default value is 5
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' @return Vector with all NN parameter ETA definitions for a NN
#' @examples 
#' \dontrun{
#' nn_parm_names <- nn_eta_def_nm("1",3)
#' }
#' @author Dominic Bräm
nn_eta_def_nm <- function(number,eta_start,n_hidden=5,time_nn=FALSE){
  if(!time_nn){
    w1s <- paste0("etaW",number,"_1",1:n_hidden," = ETA(",eta_start:(eta_start+n_hidden-1),")")
    eta_start <- eta_start+n_hidden
    b1s <- paste0("etab",number,"_1",1:n_hidden," = ETA(",eta_start:(eta_start+n_hidden-1),")")
    eta_start <- eta_start+n_hidden
    w2s <- paste0("etaW",number,"_2",1:n_hidden," = ETA(",eta_start:(eta_start+n_hidden-1),")")
    eta_start <- eta_start+n_hidden
    b2s <- paste0("etab",number,"_21 = ETA(",eta_start,")")
    
    defs <- list(unlist(list(w1s,b1s,w2s,b2s)),n_hidden*3+1)
  } else{
    w1s <- paste0("etaW",number,"_1",1:n_hidden," = ETA(",eta_start:(eta_start+n_hidden-1),")")
    eta_start <- eta_start+n_hidden
    b1s <- paste0("etab",number,"_1",1:n_hidden," = ETA(",eta_start:(eta_start+n_hidden-1),")")
    eta_start <- eta_start+n_hidden
    w2s <- paste0("etaW",number,"_2",1:n_hidden," = ETA(",eta_start:(eta_start+n_hidden-1),")")
    
    defs <- list(unlist(list(w1s,b1s,w2s)),n_hidden*3)
  }
  
  return(defs)
}

#' Internal: Calculate initial NN parameter values in NONMEM
#' 
#' Calculate the initial NN parameter values, such that activation points are within the range
#' between \emph{min_init} and \emph{max_init} defined in the un-converter NONMEM model file
#' 
#' \itemize{
#'   \item \emph{theta_scale} is the scale in which the weights from input to hidden layer are initialized,
#'   i.e., 0.1 initializes weights between -0.3 and 0.3; 0.01 initializes weights between -0.03 and 0.03
#'   \item \emph{time_nn} defines whether the NN is a time-dependent NN with the restriction that all weights from
#'   input to hidden layer are negative
#' }
#' 
#' @param number (string) Name of the NN, e.g., \dQuote{1} for NN1(...)
#' @param xmini (numeric) minimal activation point
#' @param xmaxi (numeric) maximal activation point
#' @param n_hidden (numeric) Number of neurons in the hidden layer, default value is 5
#' @param theta_scale (numeric) Scale for input-hidden-weights initialization
#' @param pre_fixef (named vector) Vector of pre-defined initial values
#' @param time_nn (boolean) Definition whether NN is time-dependent (TRUE) or not (FALSE)
#' @param act (string) Activation function used in the NN. Currently "ReLU" and "Softplus" available.
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @return Vector of initial NN parameter THETA values for one specific NN
#' @examples 
#' \dontrun{
#' ini_values <- nn_theta_initializer_nm(number="1",xmini=1,xmaxi=5)
#' }
#' @author Dominic Bräm
nn_theta_initializer_nm <- function(number,xmini,xmaxi,n_hidden=5,theta_scale=0.1,pre_fixef=NULL,time_nn=FALSE,
                                    act="ReLU",beta=20){
  if(!is.null(pre_fixef)){
    if(time_nn){
      nn_parm_names <- unlist(list(paste0("lW",number,"_1",1:n_hidden),
                                   paste0("lb",number,"_1",1:n_hidden),
                                   paste0("lW",number,"_2",1:n_hidden)))
      
      nn_parm_pre <- pre_fixef[names(pre_fixef) %in% nn_parm_names]
      
      inis <- paste0(nn_parm_pre," ; [",names(nn_parm_pre),"]")
    } else{
      nn_parm_names <- unlist(list(paste0("lW",number,"_1",1:n_hidden),
                                   paste0("lb",number,"_1",1:n_hidden),
                                   paste0("lW",number,"_2",1:n_hidden),
                                   paste0("lb",number,"_21")))
      
      nn_parm_pre <- pre_fixef[names(pre_fixef) %in% nn_parm_names]
      
      inis <- paste0(nn_parm_pre," ; [",names(nn_parm_pre),"]")
    }
    
  } else{
    if(time_nn){
      w1s <- sample(c(-3,-2,-2,-1,-1,-1,1,1,1,2,2,3),n_hidden,replace = T) * theta_scale
      acts <- stats::runif(n_hidden, min = xmini, max = xmaxi)
      b1s <- round(acts * w1s^2,3)
      w2s <- sample(c(-3,-2,-2,-1,-1,-1,1,1,1,2,2,3),n_hidden,replace = F) * theta_scale
      
      if(act=="Softplus"){
        b1s <- b1s * beta
      }
      
      w1inis <- paste0(w1s," ; [lW",number,"_1",1:n_hidden,"]")
      b1inis <- paste0(b1s," ; [lb",number,"_1",1:n_hidden,"]")
      w2inis <- paste0(w2s," ; [lW",number,"_2",1:n_hidden,"]")
      
      inis <- unlist(list(w1inis,b1inis,w2inis))
    } else{
      w1s <- sample(c(-3,-2,-2,-1,-1,-1,1,1,1,2,2,3),n_hidden,replace = T) * theta_scale
      acts <- stats::runif(n_hidden, min = xmini, max = xmaxi)
      b1s <- round(-acts * w1s,3)
      w2s <- sample(c(-3,-2,-2,-1,-1,-1,1,1,1,2,2,3),n_hidden,replace = F) * theta_scale
      b2s <- sample(c(-3,-2,-2,-1,-1,-1,1,1,1,2,2,3),1) * theta_scale
      
      if(act=="Softplus"){
        b1s <- b1s * beta
      }
      
      w1inis <- paste0(w1s," ; [lW",number,"_1",1:n_hidden,"]")
      b1inis <- paste0(b1s," ; [lb",number,"_1",1:n_hidden,"]")
      w2inis <- paste0(w2s," ; [lW",number,"_2",1:n_hidden,"]")
      b2inis <- paste0(b2s," ; [lb",number,"_21]")
      
      inis <- unlist(list(w1inis,b1inis,w2inis,b2inis))
    }
  }
  
  return(inis)
}

#' Internal: Set initial ETA estimates in NONMEM
#' 
#' Set the initial ETA estimates for the $OMEGA block in the NONMEM model file
#' 
#' ETAs are fixed to 0 if \cr
#' \itemize{
#'   \item population fit is enabled through \emph{pop=TRUE}, all ETAs are fixed to 0
#'   \item the corresponding THETA is fixed to 0 due to non-activity of the neuron
#' }
#' 
#' @param number (string) Name of the NN, e.g., \dQuote{1} for NN1(...)
#' @param theta_inis (list of string) THETA initial values generated in \emph{nn_theta_initializer_nm}
#' @param pop (boolean) Whether population fit without inter-individual variability is performed (TRUE)
#' or whether model is fitted with inter-individual variability (FALSE)
#' @param n_hidden (numeric) Number of neurons in the hidden layer, default value is 5
#' @param eta_scale (numeric) Initial standard deviation of random effects, default value is 0.1
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' @return Vector of initial NN parameter ETA values for one specific NN
#' @examples 
#' \dontrun{
#' ini_etas <- nn_eta_initializer_nm("1",theta_inis)
#' }
#' @author Dominic Bräm
nn_eta_initializer_nm <- function(number,theta_inis,pop=FALSE,n_hidden=5,eta_scale=0.1,time_nn=FALSE){
  if(!pop){
    if(is.null(theta_inis)){
      w1s <- rep(1,n_hidden)*eta_scale
      b1s <- rep(1,n_hidden)*eta_scale
      w2s <- rep(1,n_hidden)*eta_scale
      b2s <- rep(1,1)*eta_scale
      
      w1inis <- paste0(w1s," ; [etaW",number,"_1",1:n_hidden,"]")
      b1inis <- paste0(b1s," ; [etab",number,"_1",1:n_hidden,"]")
      w2inis <- paste0(w2s," ; [etaW",number,"_2",1:n_hidden,"]")
      b2inis <- paste0(b2s," ; [etab",number,"_21]")
      
      if(!time_nn){
        inis <- unlist(list(w1inis,b1inis,w2inis,b2inis))
      } else{
        inis <- unlist(list(w1inis,b1inis,w2inis))
      }
      
    } else{
      w1s <- rep(1,n_hidden)*eta_scale
      b1s <- rep(1,n_hidden)*eta_scale
      w2s <- rep(1,n_hidden)*eta_scale
      b2s <- rep(1,1)*eta_scale
      
      w1inis <- paste0(w1s," ; [etaW",number,"_1",1:n_hidden,"]")
      b1inis <- paste0(b1s," ; [etab",number,"_1",1:n_hidden,"]")
      w2inis <- paste0(w2s," ; [etaW",number,"_2",1:n_hidden,"]")
      b2inis <- paste0(b2s," ; [etab",number,"_21]")
      
      if(!time_nn){
        inis <- unlist(list(w1inis,b1inis,w2inis,b2inis))
      } else{
        inis <- unlist(list(w1inis,b1inis,w2inis))
      }
      
      
      zero_grads <- grep("FIX",theta_inis)
      zero_grads_names <- gsub("[^\\[]*\\[(.*)\\]","\\1",inis[zero_grads])
      inis[zero_grads] <- paste0("0 FIX ; [",zero_grads_names,"]")
    }
  } else{
    w1inis <- paste0("0 FIX ; [etaW",number,"_1",1:n_hidden,"]")
    b1inis <- paste0("0 FIX ; [etab",number,"_1",1:n_hidden,"]")
    w2inis <- paste0("0 FIX ; [etaW",number,"_2",1:n_hidden,"]")
    b2inis <- paste0("0 FIX ; [etab",number,"_21]")
    
    if(!time_nn){
      inis <- unlist(list(w1inis,b1inis,w2inis,b2inis))
    } else{
      inis <- unlist(list(w1inis,b1inis,w2inis))
    }
    
  }
  
  return(inis)
}

#' Internal: Definition of NN parameters in NONMEM
#' 
#' Define NN parameters consisting of typical parameter and potentially random effects in the $PK section
#' 
#' \emph{eta_model} is currently set to proportional as previous investigations showed better stability of fit
#' with this setting in NONMEM
#' 
#' @param number (string) Name of the NN, e.g., \dQuote{1} for NN1(...)
#' @param pop (boolean) Whether population fit without inter-individual variability is performed (TRUE)
#' or whether model is fitted with inter-individual variability (FALSE)
#' @param n_hidden (numeric) Number of neurons in the hidden layer, default value is 5
#' @param eta_model (string)
#' \itemize{
#'   \item \dQuote{prop} is of form W = lW * EXP(etaW)
#'   \item \dQuote{add} is of form W = lW + etaW
#' }
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' @return List of parameter definition to be used in the $PK section of the NONMEM model
#' @examples 
#' \dontrun{
#' parm_definitions <- nn_parm_setter_nm("1")
#' }
nn_parm_setter_nm <- function(number,pop=FALSE,n_hidden=5,eta_model=c("prop","add"),time_nn=FALSE){
  eta_model = match.arg(eta_model)
  #if(!pop){
  if(TRUE){
    if(eta_model=="prop"){
      w1s <- paste0("W",number,"_1",1:n_hidden," = lW",number,"_1",1:n_hidden," * EXP(etaW",number,"_1",1:n_hidden,")")
      b1s <- paste0("b",number,"_1",1:n_hidden," = lb",number,"_1",1:n_hidden," * EXP(etab",number,"_1",1:n_hidden,")")
      w2s <- paste0("W",number,"_2",1:n_hidden," = lW",number,"_2",1:n_hidden," * EXP(etaW",number,"_2",1:n_hidden,")")
      b2s <- paste0("b",number,"_21 = lb",number,"_21"," * EXP(etab",number,"_21)")
      
      if(!time_nn){
        parms <- unlist(list(w1s,b1s,w2s,b2s))
      } else{
        parms <- unlist(list(w1s,b1s,w2s))
      }
      
    } else if(eta_model=="add"){
      w1s <- paste0("W",number,"_1",1:n_hidden," = lW",number,"_1",1:n_hidden," + etaW",number,"_1",1:n_hidden,"")
      b1s <- paste0("b",number,"_1",1:n_hidden," = lb",number,"_1",1:n_hidden," + etab",number,"_1",1:n_hidden,"")
      w2s <- paste0("W",number,"_2",1:n_hidden," = lW",number,"_2",1:n_hidden," + etaW",number,"_2",1:n_hidden,"")
      b2s <- paste0("b",number,"_21 = lb",number,"_21"," + etab",number,"_21")
      
      if(!time_nn){
        parms <- unlist(list(w1s,b1s,w2s,b2s))
      } else{
        parms <- unlist(list(w1s,b1s,w2s))
      }
    }
  } else{
    w1s <- paste0("W",number,"_1",1:5," = lW",number,"_1",1:5)
    b1s <- paste0("b",number,"_1",1:5," = lb",number,"_1",1:5)
    w2s <- paste0("W",number,"_2",1:5," = lW",number,"_2",1:5)
    b2s <- paste0("b",number,"_21 = lb",number,"_21")
    
    if(!time_nn){
      parms <- unlist(list(w1s,b1s,w2s,b2s))
    } else{
      parms <- unlist(list(w1s,b1s,w2s))
    }
  }
  
  return(parms)
}
