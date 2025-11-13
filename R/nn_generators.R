#' Internal: Generate NN code for nlmixr
#' 
#' Generate the explicit code for a NN in nlmixr
#' 
#' Structure of one unit in the hidden layer: \cr
#' h1 = W1 * state + b1 \cr
#' if(h1 < 0) \{h1 <- 0\} \cr
#' h2 = W2 * state + b2 \cr
#' ... \cr
#' NN = h1 + h2 + ...
#' 
#' @param number (string) Name of the NN
#' @param state (string) State to be used as input of the NN
#' @param n_hidden (numeric) Number of units in the hidden layer, default is 5
#' @param act (string) Activation function to be used in the hidden layer of the NN (currently ReLU and Softplus implemented), default is ReLU
#' @param time_nn (boolean) If NN should be set up specifically as a time-dependent NN with strictly negative weights
#' from input to hidden layer through w'=-w^2
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @return Explicit NN code in nlmixr as list of lines in nlmixr model file
#' @examples 
#' \dontrun{
#' conc_nn <- nn_generator_nlmixr(1,"C")
#' time_nn <- nn_generator_nlmixr(2,"t",time_nn=TRUE)
#' }
#' @author Dominic Bräm
nn_generator_nlmixr <- function(number,state,n_hidden=5,act="ReLU",time_nn=FALSE,beta=20){
  if(!(act %in% c("ReLU","Softplus"))){
    warning(paste0("Only ReLU and Softplus are implemented yet as activation functions\n
            Activation function of NN",number,"was set to ReLU"))
    act <- "ReLU"
  }
  if(!time_nn){
    hs <- paste0("        h",number,"_",1:n_hidden," = W",number,"_1",1:n_hidden," * ",state," + b",number,"_1",1:n_hidden,"")
    if(act=="ReLU"){
      acts <- paste0("        if(h",number,"_",1:n_hidden,"<0) {h",number,"_",1:n_hidden," <- 0}")
    } else if(act=="Softplus"){
      acts <- paste0("        h",number,"_",1:n_hidden," <- 1/",beta," * log(1 + exp(",beta,"*h",number,"_",1:n_hidden,"))")
    }
    nn <- paste0("        NN",number," = ",paste("W",number,"_2",1:n_hidden," * h",number,"_",1:n_hidden,sep="",collapse=" + ")," + b",number,"_21")
    out <- unlist(list(hs,acts,nn))
    return(out)
  } else{
    hs <- paste0("        h",number,"_",1:n_hidden," = -W",number,"_1",1:n_hidden,"^2 * ",state," + b",number,"_1",1:n_hidden,"")
    if(act=="ReLU"){
      acts <- paste0("        if(h",number,"_",1:n_hidden,"<0) {h",number,"_",1:n_hidden," <- 0}")
    } else if(act=="Softplus"){
      acts <- paste0("        h",number,"_",1:n_hidden," <- 1/",beta," * log(1 + exp(",beta,"*h",number,"_",1:n_hidden,"))")
    }
    nn <- paste0("        NN",number," = ",paste("W",number,"_2",1:n_hidden," * h",number,"_",1:n_hidden,sep="",collapse=" + "))
    out <- unlist(list(hs,acts,nn))
    return(out)
  }
}

#' Internal: Generate NN code for Monolix
#' 
#' Generate the explicit code for a NN in Monolix
#' 
#' Structure of one unit in the hidden layer: \cr
#' h1 = max(0,W1 * state + b1) \cr
#' h2 = max(0,W2 * state + b2) \cr
#' ... \cr
#' NN = h1 + h2 + ...
#' 
#' @param number (string) Name of the NN
#' @param state (string) State to be used as input of the NN
#' @param n_hidden (numeric) Number of units in the hidden layer; currently not implemented for the NN function
#' @param act (string) Activation function to be used in the hidden layer of the NN (currently ReLU and Softplus implemented), default is ReLU
#' @param time_nn (boolean) If NN should be set up specifically as a time-dependent NN with strictly negative weights
#' from input to hidden layer through w'=-w^2
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @return Explicit NN code in Monolix as list of lines in Monolix model file
#' @examples 
#' \dontrun{
#' conc_nn <- nn_generator_mlx(1,"C")
#' time_nn <- nn_generator_mlx(2,"t",time_nn=TRUE)
#' }
#' @author Dominic Bräm
nn_generator_mlx <- function(number,state,n_hidden=5,act="ReLU",time_nn=FALSE,beta=20){
  if(!(act %in% c("ReLU","Softplus"))){
    warning(paste0("Only ReLU and Softplus are implemented yet as activation functions\n
            Activation function of NN",number,"was set to ReLU"))
    act <- "ReLU"
  }
  if(!time_nn){
    if(act=="ReLU"){
      hs <- paste0("h",number,"_",1:n_hidden," = max(0,W",number,"_1",1:n_hidden," * ",state," + b",number,"_1",1:n_hidden,")")
    } else if(act=="Softplus"){
      hs <- paste0("h",number,"_",1:n_hidden," = 1/",beta," * log(1 + exp(",beta," * W",number,"_1",1:n_hidden," * ",state," + b",number,"_1",1:n_hidden,"))")
    }
    nn <- paste0("NN",number," = ",paste("W",number,"_2",1:n_hidden," * h",number,"_",1:n_hidden,sep="",collapse=" + ")," + b",number,"_21")
    out <- unlist(list(hs,nn))
    return(out)
  } else{
    if(act=="ReLU"){
      hs <- paste0("h",number,"_",1:n_hidden," = max(0,-(W",number,"_1",1:n_hidden,"^2) * ",state," + b",number,"_1",1:n_hidden,")")
    } else if(act=="Softplus"){
      hs <- paste0("h",number,"_",1:n_hidden," = 1/",beta," * log(1 + exp(",beta," * -(W",number,"_1",1:n_hidden,"^2) * ",state," + b",number,"_1",1:n_hidden,"))")
    }
    nn <- paste0("NN",number," = ",paste("W",number,"_2",1:n_hidden," * h",number,"_",1:n_hidden,sep="",collapse=" + "))
    out <- unlist(list(hs,nn))
    return(out)
  }
}

#' Internal: Generate NN code for NONMEM
#' 
#' Generate the explicit code for a NN in NONMEM
#' 
#' Structure of one unit in the hidden layer: \cr
#' h1 = 0 \cr
#' h1_thres = W1 * state + b1 \cr
#' IF (h1_thres.GT.h1) h1 = h1_thres \cr
#' h2 = 0 \cr
#' ... \cr
#' NN = h1 + h2 + ... \cr
#' 
#' @param number (string) Name of the NN
#' @param state (string) State to be used as input of the NN
#' @param n_hidden (numeric) Number of units in the hidden layer; currently not implemented for the NN function
#' @param act (string) Activation function to be used in the hidden layer of the NN (currently ReLU and Softplus implemented), default is ReLU
#' @param time_nn (boolean) If NN should be set up specifically as a time-dependent NN with strictly negative weights
#' from input to hidden layer through w'=-w^2
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#' @return Explicit NN code in NONMEM as list of lines in NONMEM model file
#' @examples 
#' \dontrun{
#' conc_nn <- nn_generator_nm(1,"C")
#' time_nn <- nn_generator_nm(2,"t",time_nn=TRUE)
#' }
#' @author Dominic Bräm
nn_generator_nm <- function(number,state,n_hidden=5,act="ReLU",time_nn=FALSE,beta=20){
  if(!(act %in% c("ReLU","Softplus"))){
    warning(paste0("Only ReLU and Softplus are implemented yet as activation functions\n
            Activation function of NN",number,"was set to ReLU"))
    act <- "ReLU"
  }
  if(!time_nn){
    if(act=="ReLU"){
      h0 <- paste0("h",number,"_",1:n_hidden," = 0")
      hs <- paste0("h",number,"_",1:n_hidden,"_thres = W",number,"_1",1:n_hidden," * ",state," + b",number,"_1",1:n_hidden,"")
      acts <- paste0("IF (h",number,"_",1:n_hidden,"_thres.GT.h",number,"_",1:n_hidden,") h",number,"_",1:n_hidden," = h",number,"_",1:n_hidden,"_thres")
    } else if(act=="Softplus"){
      h0 <- NULL
      hs <- paste0("h",number,"_",1:n_hidden," = 1/",beta," * log(1 + exp(",beta," * W",number,"_1",1:n_hidden," * ",state," + b",number,"_1",1:n_hidden,"))")
      acts <- NULL
    }
    nn <- paste0("NN",number," = ",paste("W",number,"_2",1:n_hidden," * h",number,"_",1:n_hidden,sep="",collapse=" + ")," + b",number,"_21")
    out <- unlist(list(h0,hs,acts,nn))
    return(out)
  } else{
    if(act=="ReLU"){
      h0 <- paste0("h",number,"_",1:n_hidden," = 0")
      hs <- paste0("h",number,"_",1:n_hidden,"_thres = -(W",number,"_1",1:n_hidden,"**2) * ",state," + b",number,"_1",1:n_hidden,"")
      acts <- paste0("IF (h",number,"_",1:n_hidden,"_thres.GT.h",number,"_",1:n_hidden,") h",number,"_",1:n_hidden," = h",number,"_",1:n_hidden,"_thres")
    } else if(act=="Softplus"){
      h0 <- NULL
      hs <- paste0("h",number,"_",1:n_hidden," = 1/",beta," * log(1 + exp(",beta," * -(W",number,"_1",1:n_hidden,"**2) * ",state," + b",number,"_1",1:n_hidden,"))")
      acts <- NULL
    }
    nn <- paste0("NN",number," = ",paste("W",number,"_2",1:n_hidden," * h",number,"_",1:n_hidden,sep="",collapse=" + "))
    out <- unlist(list(h0,hs,acts,nn))
    return(out)
  }
}
