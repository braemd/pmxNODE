#' Open Monolix example model
#' 
#' Opens unconverted Monolix example model with NN functions
#' 
#' Currently only \emph{mlx_theophylline_model.txt} available
#' 
#' @param model (string) (Path/)Name of Monolix model to open
#' @return NULL
#' @examples 
#' \dontrun{
#' open_mlx_example()
#' }
#' @author Dominic Bräm
open_mlx_example <- function(model="mlx_theophylline_model.txt"){
  pkg_name <- "pmxNODE"
  lib_path <- .libPaths()
  pkg_path <- lib_path[unlist(lapply(lib_path,function(x) pkg_name %in% dir(x)))]
  if(length(pkg_path) > 1){
    pkg_path <- pkg_path[1]
  }
  file_path <- paste0(pkg_path,"/",pkg_name,"/",model)
  shell(file_path,wait=F)
}

#' Open NONMEM example model
#' 
#' Opens unconverted NONMEM example model with NN functions
#' 
#' Currently only \emph{nm_theophylline_model.txt} available
#' 
#' @param model (string) (Path/)Name of NONMEM model to open
#' @return NULL
#' @examples 
#' \dontrun{
#' open_nm_example()
#' }
#' @author Dominic Bräm
open_nm_example <- function(model="nm_theophylline_model.ctl"){
  pkg_name <- "pmxNODE"
  lib_path <- .libPaths()
  pkg_path <- lib_path[unlist(lapply(lib_path,function(x) pkg_name %in% dir(x)))]
  if(length(pkg_path) > 1){
    pkg_path <- pkg_path[1]
  }
  file_path <- paste0(pkg_path,"/",pkg_name,"/",model)
  shell(file_path,wait=F)
}