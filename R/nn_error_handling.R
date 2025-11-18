#' Internal: Test NNs for completeness
#' 
#' Tests whether all required arguments are given to each NN
#' 
#' Currently required arguments are: NN-name NNX(), state (state=), minimal (min_init=) and maximal (max_init=) activation points
#' 
#' @param text NN function from the un-converted model file, e.g., NN1(state=C,min_init=1,max_init=5)
#' @return Missing arguments of a NN, \dQuote{None} if all arguments are given
#' @author Dominic Bräm
#' @keywords internal
nn_tester <- function(text){
  number <- grepl("NN(\\d+)|NN(\\w+)", text)
  state <- grepl("state\\s*=\\s*([^,^\\)]+)", text)
  min_init <- grepl("min_init\\s*=\\s*([^,^\\)]+)", text)
  max_init <- grepl("max_init\\s*=\\s*([^,^\\)]+)", text)
  
  complets <- c(number,state,min_init,max_init)
  
  requirements <- c("NN number","State","Min init","Max init")
  
  if(all(complets)){
    miss <- "None"
  } else{
    miss <- paste(requirements[!complets],collapse = ",")
  }
  return(miss)
}

#' Internal: Error for incomplete NNs
#' 
#' Raises error if one or more NNs are not complete, i.e., not all required arguments are given
#' 
#' Currently required arguments are: NN-name NNX(), state (state=), minimal (min_init=) and maximal (max_init=) activation points
#' 
#' @param nn_tests (list of strings) List of missing arguments for each NN, \dQuote{None} if no arguments are missing for a NN
#' @return Error: Missing argument; NNX: missing arguments
#' @author Dominic Bräm
#' @keywords internal
nn_errors <- function(nn_tests){
  if(all(nn_tests == "None")){
    return()
  } else{
    no_none <- nn_tests != "None"
    nn_none_numbers <- which(no_none)
    nn_numbers <- 1:length(nn_tests)
    nn_errors <- paste("NN",nn_numbers[nn_none_numbers],":",nn_tests[nn_none_numbers],collapse = "\n")
    error_msg <- paste("Error: Missing argumens\n",nn_errors)
    stop(error_msg)
  }
}
