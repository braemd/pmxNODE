#' Initialize software
#' 
#' Initialize the pharmacometric software you want to use (Monolix, nlmixr or NONMEM). Must be used before nn_converter functions
#' can be used for Monolix and nlmixr.
#' 
#' For NONMEM, no additional steps need to be done.\cr
#' For nlmixr, the nlmixr2 package is loaded. If nlmixr2 is not yet installed, it will install previous to loading. Note that automatic installation is only available for R versions 4.2 and higher. \cr
#' For Monolix, the lixoftConnectors package is loaded. For loading, the path to the Monolix location (under Windows usually 
#' C:/ProgramData/Lixoft/MonolixSuiteXXXX with XXXX as the version) is required. If lixoftConnectors is not yet installed, it
#' will install previous to loading, including its dependencies 'RJSONIO', 'ggplot2', and 'gridExtra'. \cr
#' Note: nlmixr2 and lixoftConnectors share function \emph{getData}. If both, nlmixr and Monolix, get initialized, \emph{getData}
#' will be used from the package initialized second
#' 
#' @param software (string) The software to be used for NN convertion; "Monolix","nlmixr", or "NONMEM"
#' @param mlx_path (string) Required if \emph{software="Monolix"}; path to Monolix location (under Windows usually 
#' C:/ProgramData/Lixoft/MonolixSuiteXXXX with XXXX as the version)
#' @param mlx_instal_path (string) Required if \emph{software="Monolix"} and lixoftConnectors package not yet installed. Path to the 
#' "lixoftConnectors.tar.gz" file, under Windows usually located at C:/ProgramData/Lixoft/MonolixSuiteXXXX/connectors/lixoftConnectors.tar.gz
#' @return Initialization of software
#' @example 
#' \dontrun{
#' software_initializer(software="NONMEM")
#' software_initializer(software="nlmixr")
#' software_initializer(software="Monolix",mlx_path="C:/ProgramData/Lixoft/MonolixSuite2021R2")
#' software_initializer(software="Monolix",mlx_path="C:/ProgramData/Lixoft/MonolixSuite2021R2",mlx_instal_path="C:/ProgramData/Lixoft/MonolixSuite2021R2/connectors/lixoftConnectors.tar.gz")
#' }
#' @author Dominic Bräm
#' @export
software_initializer <- function(software=c("Monolix","nlmixr","NONMEM"),mlx_path=NULL,mlx_instal_path=NULL){
  software <- match.arg(software)
  if(software == "Monolix"){
    if(!("lixoftConnectors") %in% installed.packages()){
      install_package_now <- askYesNo("lixoftConnectors not installed \n Do you want to install it now?")
      if(install_package_now){
        if(is.null(mlx_instal_path)){
          stop("Please provide path to Monolix installation (under Windows usually C:/ProgramData/Lixoft/MonolixSuiteXXXX/connectors/lixoftConnectors.tar.gz with XXXX as the version)")
        }
        install.packages('RJSONIO')
        install.packages('ggplot2')
        install.packages('gridExtra')
        install.packages(mlx_instal_path, 
                         repos = NULL, type="source", INSTALL_opts ="--no-multiarch")
        
      } else{
        stop("lixoftConnectors required and not yet installed")
      }
    }
    if(is.null(mlx_path)){
      stop("Please provide path to Monolix (under Windows usually C:/ProgramData/Lixoft/MonolixSuiteXXXX with XXXX as the version")
    }
    print("Load and initialize lixoftConnectors package")
    library(lixoftConnectors)
    initializeLixoftConnectors(software = "monolix", path = mlx_path)
  } else if(software == "nlmixr"){
    if(!("nlmixr2") %in% installed.packages()){
      install_package_now <- askYesNo("nlmixr2 not installed \n Do you want to install it now?")
      if(install_package_now){
        r_version <- paste0(R.version$major,".",substr(R.version$minor,1,1))
        if(as.numeric(r_version) < 4.2){
          stop("Your R version is older than 4.2, please install nlmixr2 according to instructions on nlmixr2.org")
        } else{
          install.packages("nlmixr2",dependencies = TRUE)
        }
      } else{
        stop("nlmixr required and not yet installed")
      }
    }
    print("Load nlmixr2 package")
    library(nlmixr2)
  } else if(software == "NONMEM"){
    print("No additional package to be loaded")
  }
}
