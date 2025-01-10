#' Neural Network ODE language in nlmixr2 langauage
#'
#' This is a bit different than the standard NN1 and NN2 ODEs in that
#' it is in the converter.
#'
#' @param fun The function to convert
#'
#'
rxUdfUi.NN <- function(fun) {
  eval(fun)
}

nn_nlmixr_env <- new.env(parent = emptyenv())

nn_nlmixr_reset <- function() {
  rm(list=ls(nn_nlmixr_env), envir=nn_nlmixr_env)
}

#' Neural Network ODE language in nlmixr2 langauage
#'
#' @param number The neural network number
#'
#' @param state The state to be used in the neural network.  For time, use \emph{t}
#'
#' @param min_init The minimum value of activation point for the
#'   neural network, (i.e., minimal expected state value)
#'
#' @param max_init The maximum value of activation point for the NN
#'   (i.e. maximum expected state value)
#'
#' @param n_hidden The number of hidden layers in the neural network (default 5)
#'
#' @param act activation in the hidden layer, ReLU and Softplus
#'   implemented. Default is ReLU.
#'
#' @param time_nn defines if the neural network is time dependent and
#'   consequently all weights from inputs to hidden layer should be
#'   strictly negative (default is FALSE)
#'
#' @param pop (boolean) If the generated nlmixr model function should
#'   be a fit without (TRUE) or with (FALSE).  Default is FALSE.
#'
#' @inheritParams nn_converter_nlmixr
#'
#' @inheritParams rxode2::linMod
#'
#' @return A list with the before and replace elements and iniDf to
#'   allow integration in the rxode2/nlmixr2 language directly.
#'
#' @examples
#'
#' NN(1, state="t", min_init=0.1, max_init=24, pop=TRUE)
NN <- function(number=1,state="t",min_init,max_init, n_hidden=5,
               act=c("ReLU", "Softplus"),
               time_nn=FALSE,
               pop=FALSE,theta_scale=0.1,eta_scale=0.1, pre_fixef=NULL,
               iniDf=NULL) {
  if (identical(rxode2::rxUdfUiNum(), 1L)) {
    # If this is the first call of NN()
    nn_nlmixr_reset()
  }
  act <- match.arg(act)
  checkmate::assertIntegerish(number, lower=1, len=1, any.missing=FALSE)
  number <- as.character(number)
  rxode2::assertVariableName(state)
  checkmate::assertNumeric(min_init, len=1, any.missing=FALSE)
  checkmate::assertNumeric(max_init, len=1, any.missing=FALSE)
  checkmate::assertIntegerish(n_hidden, lower=1, len=1, any.missing=FALSE)
  checkmate::assertLogical(time_nn, len=1, any.missing=FALSE)
  checkmate::assertLogical(pop, len=1, any.missing=FALSE)
  checkmate::assertNumeric(theta_scale, len=1, lower=0, any.missing=FALSE)
  checkmate::assertNumeric(eta_scale, len=1, lower=0, any.missing=FALSE)

  replace <- paste0("NN", number)
  if (exists(replace, envir = nn_nlmixr_env)) {
    # When NN# already exists, simply replace NN# in model
    return(list(replace=replace))
  }
  before <- c(nn_parm_setter_nlmixr(number=number,pop=pop, n_hidden=n_hidden,
                                    time_nn=time_nn),
              nn_generator_nlmixr(number=number,state=state,time_nn=time_nn,
                                  n_hidden = n_hidden,act=act))
  if (is.null(iniDf)) {
    iniDf <- rxode2::rxUdfUiIniDf()
  }
  rxode2::assertIniDf(iniDf, null.ok=TRUE)

  theta <- nn_theta_initializer_nlmixr(number=number,xmini=min_init,
                                       xmaxi=max_init,
                                       theta_scale=theta_scale,
                                       time_nn=time_nn,pre_fixef=pre_fixef,
                                       n_hidden=n_hidden)
  if (pop) {
    eta <- nn_eta_initializer_nlmixr(number=number, n_hidden=n_hidden,
                                     eta_scale=eta_scale, time_nn=time_nn)
  }
  # Fill in iniDf with thetas and etas
  if (!is.null(iniDf)) {
    iniDf <- rxode2::rxUdfUiIniDf(iniDf)
    theta1 <- iniDf[!is.na(iniDf$theta),,drop=FALSE]
    theta0 <- theta1
    if (length(theta1$ntheta) > 0L) {
      max_theta <- max(theta1$ntheta)
      theta1 <- theta1[1,]
    } else {
      max_theta <- 0L
      theta1 <- rxode2::.rxBlankIni("theta")
      theta0 <- NULL
    }
    eta1 <- iniDf[is.na(iniDf$ntheta),,drop=FALSE]
    eta0 <- eta1
    if (length(eta1$ntheta) > 0L) {
      max_eta <- max(eta1$neta1)
      eta1 <- eta1[1,]
    } else {
      max_eta <- 0L
      eta1 <- rxode2::.rxBlankIni("eta")
      eta0 <- NULL
    }
  } else {
    theta1 <- rxode2::.rxBlankIni("theta")
    theta0 <- NULL
    max_theta <- 0L
    max_eta <- 0L
    eta1 <- rxode2::.rxBlankIni("eta")
    eta0 <- NULL
  }
  theta1$lower <- -Inf
  theta1$upper <- Inf
  theta1$fix <- FALSE
  theta1$label <- NA_character_
  theta1$backTransform <- NA_character_
  theta1$condition <- NA_character_
  theta1$err <- NA_character_

  eta1$lower <- -Inf
  eta1$upper <- Inf
  eta1$fix <- FALSE
  eta1$label <- NA_character_
  eta1$backTransform <- NA_character_
  eta1$condition <- NA_character_
  eta1$err <- NA_character_

  env <- new.env(parent=baseenv())
  env$.theta <- theta
  with(env,
       base::eval(str2lang(paste(c("{", .theta, "}"), collapse="\n"))))
  env$.max_theta <- max_theta
  theta0 <- c(list(theta0),
              lapply(ls(env), function(v) {
                cur <- theta1
                cur$name <- v
                cur$est <- get(v, envir=env)
                env$.max_theta <- env$.max_theta+1
                cur$ntheta <- env$.max_theta
                cur
              }))
  theta0 <- do.call(`rbind`, theta0)

  env <- new.env(parent=baseenv())
  env$.eta <- eta
  env$.max_eta <- max_eta
  if (pop) {
    with(env,
         base::eval(str2lang(paste(c("{", gsub("~", "<-", .eta, fixed=TRUE), "}"), collapse="\n"))))
    eta0 <- c(list(eta0),
              lapply(ls(env), function(v) {
                cur <- eta1
                cur$name <- v
                cur$est <- get(v, envir=env)
                env$.max_eta <- env$.max_eta + 1L
                cur$neta1 <- cur$neta2 <- env$.max_eta
                cur
              }))
    eta0 <- do.call(`rbind`, eta0)
  }
  oldVars <- rxode2::rxUdfUiMv()
  if (is.null(oldVars)) {
    newVars <- rxode2::rxModelVars(paste(c(theta, eta, before), collapse="\n"))
    newVars <- c(newVars$lhs, newVars$params, newVars$state)
    newVars <- newVars[newVars != state]
    oldVars <- c(oldVars$lhs, oldVars$params, oldVars$state)
    both <- intercet(oldVars, newVars)
    if (length(both) > 0L) {
      stop("The following variables are already defined in the model and are needed for NN(): ", paste(both, collapse=", "))
    }
  }

  assign(replace, TRUE, envir=nn_nlmixr_env)
  list(before=before, replace=replace, iniDf=rbind(theta0, eta0))
}
