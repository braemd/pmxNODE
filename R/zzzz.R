.onLoad <- function(libname, pkgname) {
  if (requireNamespace("rxode2", quietly = TRUE)) {
    # Add NN to rxode2 functions
    rxode2::.s3register("rxode2::rxUdfUi", "NN")
  }
}
.onAttach <- function(libname, pkgname) {
  if (requireNamespace("rxode2", quietly = TRUE)) {
    # Add NN to rxode2 functions
    rxode2::.s3register("rxode2::rxUdfUi", "NN")
  }
}
