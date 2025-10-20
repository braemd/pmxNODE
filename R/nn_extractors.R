#' Internal: Extract NN function from model
#' 
#' Extract the entire NN functions from a model file
#' 
#' NULL
#' 
#' @param text (string) Line of model code as string
#' @return NN function as string in form of "NN1(state=t,min_init=1,max_init=5)"
#' @examples 
#' \dontrun{
#' nns <- nn_extractor("dxdt = NN1(state=C,min_init=1,max_init=5) +
#'                             NN2(state=A,min_init=1,max_init=5) - kel*x")
#' }
#' @author Dominic Bräm
nn_extractor <- function(text){
  matcher <- gregexpr("(NN\\d+\\(s+[^\\)]+)|(NN\\w+\\(s+[^\\)]+)|(NN\\w+\\d+\\(s+[^\\)]+)|(NN\\d+\\w+\\(s+[^\\)]+)|(NN\\(s+[^\\)]+)",text)
  nns <- regmatches(text,matcher)
  
  return(nns)
}

#' Internal: Extract name of a NN
#' 
#' Extract the name of a NN given as the NN\emph{X} argument
#' 
#' Potentially be moved to argument in NN function in form of NN(\emph{name=1},state=C,min_init=1,max_init=5) at a later stage
#' 
#' @param text (string) String of NN in form of \emph{NN1(state=C,min_init=1,max_init=5)}
#' @return Name of the NN
#' @examples 
#' \dontrun{
#' nn_name <- nn_number_extractor("NN1(state=C,min_init=1,max_init=5)")
#' }
#' @author Dominic Bräm
nn_number_extractor <- function(text){
  match_info <- gregexpr("NN(\\d+)|NN(\\w+)", text)
  matched_strings <- regmatches(text, match_info)[[1]]
  numbers <- sub("NN(\\d+)|NN(\\w+)|NN(\\w+\\d+)|NN(\\d+\\w+)", "\\1\\2",matched_strings)
  
  return(numbers)
}

#' Internal: Extract state of a NN
#' 
#' Extract the state to be used for a NN given as the \emph{state} argument in the NN function
#' 
#' NULL
#' 
#' @param text (string) String of NN in form of \emph{NN1(state=C,min_init=1,max_init=5)}
#' @return State to be used for a NN
#' @examples 
#' \dontrun{
#' nn_state <- nn_state_extractor("NN1(state=C,min_init=1,max_init=5)")
#' }
#' @author Dominic Bräm
nn_state_extractor <- function(text){
  match_info <- gregexpr("state\\s*=\\s*([^,^\\)]+)", text)
  matched_strings <- regmatches(text, match_info)[[1]]
  states <- gsub("state\\s*=\\s*", "", matched_strings)
  
  return(states)
}

#' Internal: Extract minimal activation point of NN
#' 
#' Extract the minimal activation point of a NN given as the \emph{min_init} argument in the NN function
#' 
#' NULL
#' 
#' @param text (string) String of NN in form of \emph{NN1(state=C,min_init=1,max_init=5)}
#' @return Maximal activation point for a NN
#' @examples 
#' \dontrun{
#' min_init <- nn_minini_extractor("NN1(state=C,min_init=1,max_init=5)")
#' }
#' @author Dominic Bräm
nn_minini_extractor <- function(text){
  match_info <- gregexpr("min_init\\s*=\\s*([^,^\\)]+)", text)
  matched_strings <- regmatches(text, match_info)[[1]]
  states <- gsub("min_init\\s*=\\s*", "", matched_strings)
  
  return(states)
}

#' Internal: Extract maximal activation point of NN
#' 
#' Extract the maximal activation point of a NN given as the \emph{max_init} argument in the NN function
#' 
#' NULL
#' 
#' @param text (string) String of NN in form of \emph{NN1(state=C,min_init=1,max_init=5)}
#' @return Maximal activation point for a NN
#' @examples 
#' \dontrun{
#' max_init <- nn_maxini_extractor("NN1(state=C,min_init=1,max_init=5)")
#' }
#' @author Dominic Bräm
nn_maxini_extractor <- function(text){
  match_info <- gregexpr("max_init\\s*=\\s*([^,^\\)]+)", text)
  matched_strings <- regmatches(text, match_info)[[1]]
  states <- gsub("max_init\\s*=\\s*", "", matched_strings)
  
  return(states)
}

#' Internal: Extracting number of units in hidden layer
#' 
#' Extracts the number of units in the hidden layer of a NN given as the n_hidden argument in the NN function
#' 
#' Not yet in use
#' 
#' @param text (string) String of NN in form of \emph{NN1(state=C,min_init=1,max_init=5,n_hidden=6)}
#' @return Number of units in the hidden layer for a specific NN
#' @examples 
#' \dontrun{
#' n_hiddens <- nn_nhidden_extractor("NN1(state=C,min_init=1,max_init=5,n_hidden=6)")
#' }
#' @author Dominic Bräm
nn_nhidden_extractor <- function(text){
  match_info <- gregexpr("n_hidden\\s*=\\s*([^,^\\)]+)", text)
  matched_strings <- regmatches(text, match_info)[[1]]
  states <- gsub("n_hidden\\s*=\\s*", "", matched_strings)
  if(length(states)==0){
    states <- 5
  }
  
  return(states)
}

#' Internal: Extracting time-NN argument
#' 
#' Extracts, whether a NN should be treated as time-dependent NN
#' 
#' Time-dependent NNs have different model structures, i.e., weights from input to hidden layer are set to
#' negative through w'=-w^2
#' 
#' @param text (list of strings) List of strings of NN in form of \emph{NN1(state=t,min_init=1,max_init=5,time_nn=TRUE)}
#' @return List of boolean expression whether NN should be treated as time-NN (TRUE) or not (FALSE)
#' @examples 
#' \dontrun{
#' time_nns <- nn_time_nn_extractor(list("NN1(state=C,min_init=1,max_init=5)",
#'                                       "NN2(state=t,min_init=1,max_init=5,time_nn=TRUE)"))
#' }
#' @author Dominic Bräm
nn_time_nn_extractor <- function(text){
  time_nn <- grepl("time_nn\\s*=\\s*TRUE|time_nn\\s*=\\s*T",text)
  
  return(time_nn)
}

#' Internal: Extract activation function of a NN
#' 
#' Extract the activation function to be used for a NN given as the \emph{act} argument in the NN function
#' 
#' NULL
#' 
#' @param text (string) String of NN in form of \emph{NN1(state=C,min_init=1,max_init=5,act=ReLU)}
#' @return Activation function to be used for a NN
#' @examples 
#' \dontrun{
#' nn_act <- nn_act_extractor("NN1(state=C,min_init=1,max_init=5,act=ReLU)")
#' }
#' @author Dominic Bräm
nn_act_extractor <- function(text){
  match_info <- gregexpr("act\\s*=\\s*([^,^\\)]+)", text)
  matched_strings <- regmatches(text, match_info)[[1]]
  acts <- gsub("act\\s*=\\s*", "", matched_strings)
  if(length(acts)==0){
    acts <- "ReLU"
  }
  return(acts)
}

#' Internal: Reduces NN function in model file
#' 
#' Converts NN functions of form \emph{NN1(state=C,min_init=1,max_init=5)} to model usable form of \emph{NN1}.
#' 
#' NULL
#' 
#' @param text (list of strings) Model file read by readLines, with each line of the model as element of the list
#' @return Converted model with \emph{NN1(state=C,min_init=1,max_init=5)} expressed as \emph{NN1}
#' @examples 
#' \dontrun{
#' new_model <- nn_reducer(list("dxdt = NN1(state=C,min_init=1,max_init=5) - kel*x"))
#' }
#' @author Dominic Bräm
nn_reducer <- function(text){
  nn_lines_nr <- grep("NN",text)
  nn_lines <- text[nn_lines_nr]
  
  nn_lines_new <- gsub("(NN\\d+)\\([^)]+\\)|(NN\\w+)\\([^)]+\\)|(NN\\w+\\d+)\\([^)]+\\)|(NN\\d+\\w+)\\([^)]+\\)", "\\1\\2\\3\\4",nn_lines)
  text[unlist(nn_lines_nr)] <- nn_lines_new
  return(text)
}