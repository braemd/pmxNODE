#' Open Monolix example model
#' 
#' Opens unconverted Monolix example model with NN functions
#' 
#' Example numbers 1-4 currently available.
#' 
#' @param model_nr (number) What example number should be read (1-4 available).
#' @return Returns example model text.
#' @author Dominic Bräm
#' @keywords internal
open_mlx_example <- function(model_nr=1){
  example_name <- paste0("mlx_example",model_nr,"_model.txt")
  file_path <- system.file(example_name,package="pmxNODE")
  txt <- readLines(file_path,warn=FALSE)
  return(txt)
}

#' Open NONMEM example model
#' 
#' Opens unconverted NONMEM example model with NN functions
#' 
#' Example numbers 1-4 currently available.
#' 
#' @param model_nr (number) What example number should be read (1-4 available).
#' @return Returns example model text.
#' @author Dominic Bräm
#' @keywords internal
open_nm_example <- function(model_nr=1){
  example_name <- paste0("nm_example",model_nr,"_model.ctl")
  file_path <- system.file(example_name,package="pmxNODE")
  txt <- readLines(file_path,warn=FALSE)
  return(txt)
}