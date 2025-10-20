#' Internal: Define NN parameters names
#' 
#' Get parameter names for one specific NN in the model, i.e., weight and bias names
#' 
#' NULL
#' 
#' @param number (string) Name of the NN, e.g., \dQuote{1} for NN1(...)
#' @param n_hidden (numeric) Number of neurons in the hidden layer, default value is 5
#' @param time_nn (boolean) Whether the neural network to analyze is a time-dependent neural network or not. Default values is FALSE.
#' @return Vector with all NN parameter names
#' @examples 
#' \dontrun{
#' nn_parm_names <- nn_theta_def_mlx("1")
#' }
#' @author Dominic Bräm
nn_theta_def_mlx <- function(number,n_hidden=5,time_nn=FALSE){
  w1s <- paste0("W",number,"_1",1:n_hidden)
  b1s <- paste0("b",number,"_1",1:n_hidden)
  w2s <- paste0("W",number,"_2",1:n_hidden)
  b2s <- paste0("b",number,"_21")
  
  if(!time_nn){
    defs <- unlist(list(w1s,b1s,w2s,b2s))
  } else{
    defs <- unlist(list(w1s,b1s,w2s))
  }
  
  return(defs)
}

#' Internal: Calculate initial NN parameter values
#' 
#' Calculate the initial NN parameter values, such that activation points are within the range
#' between \emph{min_init} and \emph{max_init} defined in the un-converter Monolix model file
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
#' @return Vector of initial NN parameter values for one specific NN
#' @examples 
#' \dontrun{
#' ini_values <- nn_theta_initializer_mlx(number="1",xmini=1,xmaxi=5)
#' }
nn_theta_initializer_mlx<- function(number,xmini,xmaxi,n_hidden=5,theta_scale=0.1,pre_fixef=NULL,time_nn=FALSE,
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
    
    inis <- paste0(nn_parm_pre," ; [",names(nn_parm_pre),"]")
  } else{
    if(time_nn){
      w1s <- sample(c(-3,-2,-2,-1,-1,-1,1,1,1,2,2,3),n_hidden,replace = T) * theta_scale
      acts <- stats::runif(n_hidden, min = xmini, max = xmaxi)
      b1s <- round(acts * w1s^2,3)
      w2s <- sample(c(-3,-2,-2,-1,-1,-1,1,1,1,2,2,3),n_hidden,replace = F) * theta_scale
      
      if(act=="Softplus"){
        b1s <- b1s * beta
      }
      
      inis <- unlist(list(w1s,b1s,w2s))
    } else{
      w1s <- sample(c(-3,-2,-2,-1,-1,-1,1,1,1,2,2,3),n_hidden,replace = T) * theta_scale
      acts <- stats::runif(n_hidden, min = xmini, max = xmaxi)
      b1s <- round(-acts * w1s,3)
      w2s <- sample(c(-3,-2,-2,-1,-1,-1,1,1,1,2,2,3),n_hidden,replace = F) * theta_scale
      b2s <- sample(c(-3,-2,-2,-1,-1,-1,1,1,1,2,2,3),1) * theta_scale
      
      if(act=="Softplus"){
        b1s <- b1s * beta
      }
      
      inis <- unlist(list(w1s,b1s,w2s,b2s))
    }
  }
  
  return(inis)
}
