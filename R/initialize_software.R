#' Initialize software (Suspended)
#' 
#' Initialize the pharmacometric software you want to use (Monolix, nlmixr or NONMEM). Must be used before nn_converter functions
#' can be used for Monolix and nlmixr.
#' 
#' For Monolix, the lixoftConnectors package is loaded. For loading, the path to the Monolix location (under Windows usually 
#' C:/ProgramData/Lixoft/MonolixSuiteXXXX with XXXX as the version) is required.
#' Note: nlmixr2 and lixoftConnectors share function \emph{getData}. If both, nlmixr and Monolix, get initialized, \emph{getData}
#' will be used from the package initialized second
#' 
#' @param software (string) The software to be used for NN convertion; "Monolix","nlmixr", or "NONMEM"
#' @param mlx_path (string) Required if \emph{software="Monolix"}; path to Monolix location (under Windows usually 
#' C:/ProgramData/Lixoft/MonolixSuiteXXXX with XXXX as the version)
#' @return Initialization of software
#' @examples 
#' \dontrun{
#' software_initializer(software="NONMEM")
#' software_initializer(software="nlmixr")
#' software_initializer(software="Monolix",mlx_path="C:/ProgramData/Lixoft/MonolixSuite2021R2")
#' }
#' @author Dominic Bräm
#' @export
software_initializer <- function(software=c("Monolix","nlmixr","NONMEM"),mlx_path=NULL){
  software <- match.arg(software)
  if(software == "Monolix"){
    if(requireNamespace("lixoftConnectors",quietly = T)){
      if("lixoftConnectors" %in% .packages()){
        lixoftConnectors::initializeLixoftConnectors(software = "monolix", path = mlx_path)
      } else{
        stop("Please load the lixoftConnectors package via library first")
      }
    } else{
      stop("lixoftConnectors is not installed. Please install lixoftConnectors before using pmxNODE with Monolix. (See Lixoft website for instructions)")
    }
  } else if(software == "nlmixr"){
    if(requireNamespace("nlmixr2", quietly = T)){
      if("nlmixr2" %in% .packages()){
        stop("nlmixr2 already loaded, nothing to be done")
      } else{
        stop("please load the nlmixr2 package via library")
      }
    } else{
      stop("nlmixr2 not installed. Please install nlmixr2 befor using pmxNODE with nlmixr2.")
    }
  } else if(software == "NONMEM"){
    message("No additional package to be loaded")
  }
}
