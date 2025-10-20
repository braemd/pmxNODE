#' Internal: Initialize typical NN parameter in nlmixr
#' 
#' Calculate the initial typical NN parameter values, such that activation points are within the range
#' between \emph{min_init} and \emph{max_init} defined in the un-converter NONMEM model file, and define the 
#' the typical NN parameters for the \emph{ini} section of the nlmixr model
#' 
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
#' @return Vector of initial typical NN parameters for one specific NN
#' @examples 
#' \dontrun{
#' ini_values <- nn_theta_initializer_nlmixr(number="1",xmini=1,xmaxi=5)
#' }
#' @author Dominic Bräm
nn_theta_initializer_nlmixr <- function(number,xmini,xmaxi,n_hidden=5,theta_scale=0.1,pre_fixef=NULL,time_nn=FALSE,
                                        act="ReLU",beta=20){
  if(!is.null(pre_fixef)){
    if(!time_nn){
      nn_parm_names <- unlist(list(paste0("lW",number,"_1",1:n_hidden),
                                   paste0("lb",number,"_1",1:n_hidden),
                                   paste0("lW",number,"_2",1:n_hidden),
                                   paste0("lb",number,"_21")))
    } else{
      nn_parm_names <- unlist(list(paste0("lW",number,"_1",1:n_hidden),
                                   paste0("lb",number,"_1",1:n_hidden),
                                   paste0("lW",number,"_2",1:n_hidden)))
    }
    
    
    nn_parm_pre <- pre_fixef[names(pre_fixef) %in% nn_parm_names]
    
    inis <- paste0(names(nn_parm_pre)," <- ",round(nn_parm_pre,3))
  } else{
    if(time_nn){
      w1s <- sample(c(-3,-2,-2,-1,-1,-1,1,1,1,2,2,3),n_hidden,replace = T) * theta_scale
      acts <- stats::runif(n_hidden, min = xmini, max = xmaxi)
      b1s <- round(acts * w1s^2,3)
      w2s <- sample(c(-3,-2,-2,-1,-1,-1,1,1,1,2,2,3),n_hidden,replace = F) * theta_scale
      
      if(act=="Softplus"){
        b1s <- b1s * beta
      }
      
      w1inis <- paste0("lW",number,"_1",1:n_hidden," <- ",w1s)
      b1inis <- paste0("lb",number,"_1",1:n_hidden," <- ",b1s)
      w2inis <- paste0("lW",number,"_2",1:n_hidden," <- ",w2s)
      
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
      
      w1inis <- paste0("lW",number,"_1",1:n_hidden," <- ",w1s)
      b1inis <- paste0("lb",number,"_1",1:n_hidden," <- ",b1s)
      w2inis <- paste0("lW",number,"_2",1:n_hidden," <- ",w2s)
      b2inis <- paste0("lb",number,"_21 <- ",b2s)
      
      inis <- unlist(list(w1inis,b1inis,w2inis,b2inis))
    }
  }
  
  return(inis)
}

#' Internal: Initialize random effects on NN parameters in nlmixr
#' 
#' Define the standard deviation of random effects on NN parameters for the \emph{ini} section of the nlmixr model
#' 
#' NULL
#' 
#' @param number (string) Name of the NN, e.g., \dQuote{1} for NN1(...)
#' @param n_hidden (numeric) Number of neurons in the hidden layer, default value is 5
#' @param eta_scale (numeric) Initial standard deviation of random effects on NN parameters
#' @param time_nn (boolean) Definition whether NN is time-dependent (TRUE) or not (FALSE)
#' @return Vector of initial random effects on NN parameters for one specific NN
#' @examples 
#' \dontrun{
#' ini_etas <- nn_eta_initializer_nlmixr("1")
#' }
nn_eta_initializer_nlmixr <- function(number,n_hidden=5,eta_scale=0.1,time_nn=FALSE){
  w1s <- rep(1,n_hidden)*eta_scale
  b1s <- rep(1,n_hidden)*eta_scale
  w2s <- rep(1,n_hidden)*eta_scale
  b2s <- rep(1,1)*eta_scale
  
  w1inis <- paste0("eta.W",number,"_1",1:n_hidden," ~ ",w1s)
  b1inis <- paste0("eta.b",number,"_1",1:n_hidden," ~ ",b1s)
  w2inis <- paste0("eta.W",number,"_2",1:n_hidden," ~ ",w2s)
  b2inis <- paste0("eta.b",number,"_21 ~ ",b2s)
  
  if(!time_nn){
    inis <- unlist(list(w1inis,b1inis,w2inis,b2inis))
  } else{
    inis <- unlist(list(w1inis,b1inis,w2inis))
  }
  
  
  return(inis)
}

#' Internal: Definition of NN parameters in nlmixr
#' 
#' Define NN parameters consisting of typical parameter and potentially random effects in the \emph{model} section
#' of a nlmixr model
#' 
#' \emph{eta_model} is currently set to proportional as previous investigations showed better stability of fit
#' with this setting
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
#' Defaul value is \dQuote{prop}
#' @param time_nn (boolean) Definition whether NN is time-dependent (TRUE) or not (FALSE)
#' @return List of parameter definition to be used in the \emph{model} section of the nlmixr model
#' @examples 
#' \dontrun{
#' parm_definitions <- nn_parm_setter_nlmixr("1")
#' }
nn_parm_setter_nlmixr <- function(number,pop=FALSE,n_hidden=5,eta_model=c("prop","add"),time_nn=FALSE){
  eta_model = match.arg(eta_model)
  if(!pop){
    if(eta_model=="prop"){
      w1s <- paste0("W",number,"_1",1:n_hidden," <- lW",number,"_1",1:n_hidden," * exp(eta.W",number,"_1",1:n_hidden,")")
      b1s <- paste0("b",number,"_1",1:n_hidden," <- lb",number,"_1",1:n_hidden," * exp(eta.b",number,"_1",1:n_hidden,")")
      w2s <- paste0("W",number,"_2",1:n_hidden," <- lW",number,"_2",1:n_hidden," * exp(eta.W",number,"_2",1:n_hidden,")")
      b2s <- paste0("b",number,"_21 <- lb",number,"_21"," * exp(eta.b",number,"_21)")
      
      if(!time_nn){
        parms <- unlist(list(w1s,b1s,w2s,b2s))
      } else{
        parms <- unlist(list(w1s,b1s,w2s))
      }
      
    } else if(eta_model=="add"){
      w1s <- paste0("W",number,"_1",1:n_hidden," <- lW",number,"_1",1:n_hidden," + eta.W",number,"_1",1:n_hidden,"")
      b1s <- paste0("b",number,"_1",1:n_hidden," <- lb",number,"_1",1:n_hidden," + eta.b",number,"_1",1:n_hidden,"")
      w2s <- paste0("W",number,"_2",1:n_hidden," <- lW",number,"_2",1:n_hidden," + eta.W",number,"_2",1:n_hidden,"")
      b2s <- paste0("b",number,"_21 <- lb",number,"_21"," + eta.b",number,"_21")
      
      if(!time_nn){
        parms <- unlist(list(w1s,b1s,w2s,b2s))
      } else{
        parms <- unlist(list(w1s,b1s,w2s))
      }
    }
  } else{
    w1s <- paste0("W",number,"_1",1:5," <- lW",number,"_1",1:5)
    b1s <- paste0("b",number,"_1",1:5," <- lb",number,"_1",1:5)
    w2s <- paste0("W",number,"_2",1:5," <- lW",number,"_2",1:5)
    b2s <- paste0("b",number,"_21 <- lb",number,"_21")
    
    if(!time_nn){
      parms <- unlist(list(w1s,b1s,w2s,b2s))
    } else{
      parms <- unlist(list(w1s,b1s,w2s))
    }
  }
  
  return(parms)
}
