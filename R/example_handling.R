#' List of examples available
#' 
#' Get a list of examples available in this package
#' 
#' \itemize{
#'   \item Example 1: PK with IV drug administration
#'   \item Example 2: PK with IV drug administration
#'   \item Example 3: PK with PO drug administration
#'   \item Example 4: PK with IV drug administration and PD
#' }
#' 
#' @param pkg_name (string) Only required in development phase
#' @return A list of all examples available
#' @examples 
#' example_list <- get_example_list()
#' @author Dominic Bräm
#' @export
get_example_list <- function(pkg_name = "pmxNODE"){
  lib_path <- .libPaths()
  pkg_path <- lib_path[unlist(lapply(lib_path,function(x) pkg_name %in% dir(x)))]
  if(length(pkg_path) > 1){
    pkg_path <- pkg_path[1]
  }
  ex_files <- list.files(file.path(pkg_path,pkg_name),"example")
  return(ex_files)
}

#' Copy examples to your folder
#' 
#' This function allows to copy one or multiple examples (data and model files) to a directory of your choice. \cr
#' Either \emph{examples}, \emph{example_nr}, \emph{example_software}, or \emph{example_nr} + \emph{example_software} must be given.
#' 
#' NULL
#' 
#' @param target_folder (string) Path to the folder the examples should be copied to
#' @param examples (vector of strings) Explicit names of example data and/or model files to be copied. Must be in
#' the example list obtained by \emph{get_example_list()}
#' @param example_nr (numeric) Number of example data and model to be copied. If \emph{example_software} is not specified,
#' examples with \emph{example_nr} for all software will be copied.
#' @param example_software (string) Software of example data and model to be copied. Either \dQuote{Monolix} or \dQuote{NONMEM}
#' available. If \emph{example_nr} is not specified, all examples for this software will be copied.
#' @param pkg_name (string) Only required in development phase
#' @return Copied examples in specified folder.
#' @examples 
#' \dontrun{
#' copy_examples("path/to/target/folder",
#'               examples = c("data_example1_mlx.csv","mlx_example1_model.txt"))
#' copy_examples("path/to/target/folder",example_nr = 1)
#' copy_examples("path/to/target/folder",example_software = "Monolix")
#' copy_examples("path/to/target/folder",example_nr = 1, example_software = "NONMEM")
#' }
#' @author Dominic Bräm
#' @export
copy_examples <- function(target_folder,examples=NULL,example_nr=NULL,example_software=NULL,pkg_name = "pmxNODE"){
  if(!file.exists(target_folder)){
    stop("Please provide existing path to copy examples to")
  }
  lib_path <- .libPaths()
  pkg_lib_path <- lib_path[unlist(lapply(lib_path,function(x) pkg_name %in% dir(x)))]
  if(length(pkg_lib_path) > 1){
    pkg_lib_path <- pkg_lib_path[1]
  }
  pkg_path <- file.path(pkg_lib_path,pkg_name)
  ex_files <- list.files(pkg_path,"example")
  if(!is.null(examples)){
    ex_exist_to_copy <- examples %in% ex_files
    ex_files_to_copy <- ex_files %in% examples
    if(!all(ex_exist_to_copy)){
      stop(paste0("Not all examples exist, only following examples exist: ",paste(ex_files,collapse = ", ")))
    }
    ex_files_to_copy_names <- ex_files[ex_files_to_copy]
    paths_to_copy <- file.path(pkg_path,ex_files_to_copy_names)
    file.copy(paths_to_copy,target_folder)
  } else if(!is.null(example_nr) & is.null(example_software)){
    if(!(example_nr %in% 1:4)){
      stop("Only example numbers between 1 and 4 are available")
    }
    ex_files_to_copy_names <- ex_files[grepl(as.character(example_nr),ex_files)]
    paths_to_copy <- file.path(pkg_path,ex_files_to_copy_names)
    file.copy(paths_to_copy,target_folder)
  } else if(is.null(example_nr) & !is.null(example_software)){
    if(example_software == "Monolix"){
      software_abbreviation <- "mlx"
    } else if(example_software == "NONMEM"){
      software_abbreviation <- "nm"
    } else{
      stop("example_software argument must be either Monolix or NONMEM")
    }
    ex_files_to_copy_names <- ex_files[grepl(software_abbreviation,ex_files)]
    paths_to_copy <- file.path(pkg_path,ex_files_to_copy_names)
    file.copy(paths_to_copy,target_folder)
  } else if(!is.null(example_nr) & !is.null(example_software)){
    if(!(example_nr %in% 1:4)){
      stop("Only example numbers between 1 and 4 are available")
    }
    if(example_software == "Monolix"){
      software_abbreviation <- "mlx"
    } else if(example_software == "NONMEM"){
      software_abbreviation <- "nm"
    } else{
      stop("example_software argument must be either Monolix or NONMEM")
    }
    ex_files_to_copy_names <- ex_files[grepl(software_abbreviation,ex_files) & grepl(as.character(example_nr),ex_files)]
    paths_to_copy <- file.path(pkg_path,ex_files_to_copy_names)
    file.copy(paths_to_copy,target_folder)
  } else if(is.null(example_nr) & is.null(example_software)){
    stop("No examples copied, please provide either examples, example_nr, or/and example_software")
  }
}
