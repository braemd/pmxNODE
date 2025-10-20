#' Neural Network ODE language in nlmixr2 langauage
#'
#' This is a bit different than the standard NN1 and NN2 ODEs in that
#' it is in the converter.
#'
#' @param fun The function to convert
#'
#' @author Matthew L Fidler
#'
#' @noRd
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
#' @param beta (numeric) Beta value for the Softplus activation function, only applicable if \emph{act="Softplus"}; Default to 20.
#'
#' @inheritParams nn_converter_nlmixr

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
#' @param eta_model (string)
#' \itemize{
#'   \item \dQuote{prop} is of form W = lW * EXP(etaW)
#'   \item \dQuote{add} is of form W = lW + etaW
#' }
#'
#' @param iniDf iniDf
#'
#' @inheritParams nn_converter_nlmixr
#'
#' @inheritParams rxode2::linMod
#'
#' @return A list with the before and replace elements and iniDf to
#'   allow integration in the rxode2/nlmixr2 language directly.
#'
#' @author Matthew L Fidler (uses the same functions
#'   `nn_generator_nlmixr`, written by Dominic Bräm)
#'
#' @export
#'
#' @examples
#'
#'  if (requireNamespace("rxode2", quietly = TRUE)) {
#'
#' # Called directly, this isn't that interesting, but can show what
#' # is produced for rxode2 integration
#'
#' library(rxode2)
#'
#' NN(1, state="t", min_init=0.1, max_init=24, pop=TRUE)
#'
#' # This can be used in the rxode2 language as follows:
#'
#' f_ode_pop <- function(){
#'   ini({
#'     lV <- 1
#'     prop.err <- 0.1
#'   })
#'   model({
#'     V <- lV
#'     d/dt(centr)  =  NN(1, state=centr,min_init=0,max_init=300)
#'     cp = centr / V
#'     cp ~ prop(prop.err)
#'   })
#' }
#'
#' # but it expands to the complete model:
#'
#' f_ode_pop()
#'
#' # This is because pmxNODE uses the extensible user model interface
#' # in rxode2.  This only works if you load rxode2/nlmixr2 and pmxNODE
#'
#' }
#'
NN <- function(number=1,state="t",min_init=0.5,max_init=10, n_hidden=5,
               act=c("ReLU", "Softplus"),
               time_nn=FALSE,
               beta=20,
               pop=getOption("pmxNODE.pop",TRUE),
               eta_model=c("prop", "add"),
               theta_scale=0.1,eta_scale=0.1,
               pre_fixef=getOption("pmxNODE.pre_fixef",NULL),
               iniDf=NULL) {
  if (identical(rxode2::rxUdfUiNum(), 1L) &&
        is.null(rxode2::rxUdfUiMv())) {
    # If this is the first call of NN()
    nn_nlmixr_reset()
  }

  number <- as.character(substitute(number))
  tmp <- suppressWarnings(try(force(number), silent=TRUE))
  if (!inherits(tmp, "try-error")) {
    if (is.character(tmp)) {
      number <- tmp
    }
  }

  replace <- paste0("NN", number)
  if (exists(replace, envir = nn_nlmixr_env)) {
    # When NN# already exists, simply replace NN# in model
    if (!missing(state) ||
          !missing(min_init) ||
           !missing(max_init) ||
           !missing(n_hidden) ||
           !missing(act) ||
           !missing(time_nn) ||
           !missing(beta) ||
           !missing(pop) ||
           !missing(eta_model) ||
           !missing(theta_scale) ||
           !missing(eta_scale) ||
           !missing(pre_fixef) ||
           !missing(iniDf)) {
      warning(paste0("NN(", number, ") already exists. Using old definition"),
              call. = FALSE)
    }
    return(list(replace=replace))
  }
  ## Restore variables that are not part of the function call in the
  ## rxode2 model variable parse step
  if (exists(paste0(replace, "pre_fixef"), envir = nn_nlmixr_env)) {
    pre_fixef <- get(paste0(replace, "pre_fixef"), envir = nn_nlmixr_env)
  }
  if (exists(paste0(replace, "iniDf"), envir = nn_nlmixr_env)) {
    iniDf <- get(paste0(replace, "iniDf"), envir = nn_nlmixr_env)
  }
  if (exists(paste0(replace, "time_nn"), envir = nn_nlmixr_env)) {
    time_nn <- get(paste0(replace, "time_nn"), envir = nn_nlmixr_env)
  }
  if (exists(paste0(replace, "pop"), envir = nn_nlmixr_env)) {
    pop <- get(paste0(replace, "pop"), envir = nn_nlmixr_env)
  }
  if (is.numeric(act)) {
    act <- c("ReLU", "Softplus")[act]
  }
  if (is.numeric(eta_model)) {
    eta_model <- c("prop", "add")[eta_model]
  }
  act <- match.arg(act)
  eta_model <- match.arg(eta_model)

  state <- as.character(substitute(state))
  tmp <- suppressWarnings(try(force(state), silent=TRUE))
  if (!inherits(tmp, "try-error")) {
    if (is.character(tmp)) {
      state <- tmp
    }
  }

  if (is.null(rxode2::rxUdfUiMv())) {
    # If we use names (like ReLU or prop) in the model they become
    # model variables. To avoid this convert to numeric and allow
    # numeric processing. In theory the state should already be in the
    # model so it won't affect model parameters
    expr <- str2lang(deparse1(list(number, state, min_init, max_init,
         n_hidden, stats::setNames(c(ReLU=1, Softplus=2)[act], NULL), 0, beta,
         0, stats::setNames(c(prop=1,add=2)[eta_model], NULL), theta_scale,
         eta_scale, 0, 0)))
    # Save pre_fixef and iniDf since they may be a bit more complex
    # and can't be parsed by rxode2 base parser
    assign(paste0(replace, "pre_fixef"), pre_fixef, envir=nn_nlmixr_env)
    assign(paste0(replace, "iniDf"), iniDf, envir=nn_nlmixr_env)
    assign(paste0(replace, "time_nn"), time_nn, envir=nn_nlmixr_env)
    assign(paste0(replace, "pop"), pop, envir=nn_nlmixr_env)

    expr[[1]] <- quote(`NN`)
    # Need to have model variables to check for name collision
    # reparse with all variables filled out requesting MV
    rep <- gsub("\"","", deparse1(expr))
    return(list(replace=rep, uiUseMv=TRUE))
  }
  rxode2::assertVariableName(state)
  checkmate::assertNumeric(min_init, len=1, any.missing=FALSE)
  checkmate::assertNumeric(max_init, len=1, any.missing=FALSE)
  checkmate::assertIntegerish(n_hidden, lower=1, len=1, any.missing=FALSE)
  checkmate::assertLogical(time_nn, len=1, any.missing=FALSE)
  checkmate::assertLogical(pop, len=1, any.missing=FALSE)
  checkmate::assertNumeric(theta_scale, len=1, lower=0, any.missing=FALSE)
  checkmate::assertNumeric(eta_scale, len=1, lower=0, any.missing=FALSE)
  checkmate::assertNumeric(beta, len=1, lower=0, any.missing=FALSE)

  before <- c(nn_parm_setter_nlmixr(number=number, pop=pop, n_hidden=n_hidden,
                                    eta_model=eta_model,
                                    time_nn=time_nn),
              nn_generator_nlmixr(number=number,state=state,time_nn=time_nn,
                                  n_hidden = n_hidden,act=act,
                                  beta=beta))
  if (is.null(iniDf)) {
    iniDf <- rxode2::rxUdfUiIniDf()
  }
  rxode2::assertIniDf(iniDf, null.ok=TRUE)

  theta <- nn_theta_initializer_nlmixr(number=number,xmini=min_init,
                                       xmaxi=max_init,
                                       theta_scale=theta_scale,
                                       time_nn=time_nn,pre_fixef=pre_fixef,
                                       n_hidden=n_hidden)
  eta <- NULL
  if (!pop) {
    eta <- nn_eta_initializer_nlmixr(number=number, n_hidden=n_hidden,
                                     eta_scale=eta_scale, time_nn=time_nn)
  }
  # Fill in iniDf with thetas and etas
  if (!is.null(iniDf)) {
    theta1 <- iniDf[!is.na(iniDf$ntheta),,drop=FALSE]
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
  eta1$condition <- "id"
  eta1$err <- NA_character_

  env <- new.env(parent=baseenv())
  env$.theta <- theta
  with(env,
       base::eval(str2lang(paste(c("{", .theta, "}"), collapse="\n"))))
  env$.max_theta <- max_theta
  # For some reason the order changes with a simple ls(env) call
  # depending on interactive and possibly regions. So we need to
  # store the names in a separate variable.
  n <- gsub(" *<-.*","", theta)
  theta0 <- c(list(theta0),
              lapply(n, function(v) {
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
  if (!pop) {
    with(env,
         base::eval(str2lang(paste(c("{", gsub("~", "<-", .eta, fixed=TRUE), "}"), collapse="\n"))))
    n <- gsub(" *~.*","", eta)
    eta0 <- c(list(eta0),
              lapply(n, function(v) {
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
  if (!is.null(oldVars)) {
    newVars <- rxode2::rxModelVars(paste(c(theta, eta, before), collapse="\n"))
    newVars <- c(newVars$lhs, newVars$params, newVars$state)
    newVars <- newVars[newVars != state]
    oldVars <- c(oldVars$lhs, oldVars$params, oldVars$state)
    both <- intersect(oldVars, newVars)
    if (length(both) > 0L) {
      stop("The following variables are already defined in the model and are needed for NN(): ", paste(both, collapse=", "))
    }
  }

  assign(replace, TRUE, envir=nn_nlmixr_env)
  list(before=before, replace=replace, iniDf=rbind(theta0, eta0))
}

#' Change a population Neural Network model to a model with between subject variability
#'
#' This only changes the Neural Network model to add between subject
#' variability.  It assumes the following parameter structure
#'
#' @param ui -- nlmixr2 fit or rxode2 model function to modify and add
#'   between subject variabilities to the neural network.
#'
#' @param val -- initial value for the added etas
#'
#' @param str -- String used to construct the eta expressions.  The
#'   default is "%s <- l%s*exp(eta.%s)". This translates the variable
#'   to the `eta` variable.  If desired you can try different forms
#'   for the between subject variables.
#'
#' @param warn -- boolean; Should you warn or error if the element is
#'   not a nlmixr2 fit
#'
#' @return modified model with between subject variabilities added for
#'   neural-network components.
#' @author Matthew L. Fidler
#' @examples
#'
#' f_ode_pop <- function(){
#'   ini({
#'     lV <- 1
#'     prop.err <- 0.1
#'   })
#'   model({
#'     V <- lV
#'     d/dt(centr)  =  NN(1, state=centr,min_init=0,max_init=300)
#'     cp = centr / V
#'     cp ~ prop(prop.err)
#'   })
#' }
#'
#' f_ode_pop() %>% NNbsv(.2, warn=TRUE)
#'
#' @export
NNbsv <- function(ui, val=0.1, str="%s <- l%s*exp(eta.%s)",
                  warn=FALSE) {
  if (!inherits(ui, "nlmixr2FitCore")) {
    .txt <- "'ui' input function is not a fit\n population values should be fit before adding between subject variability"
    if (warn) {
      warning(.txt, call.=FALSE)
    } else {
      stop(.txt, call.=FALSE)
    }
  }
  .ui <- rxode2::assertRxUi(ui)
  .n <- names(.ui$theta)
  .etaNames <- dimnames(.ui$omega)[[1]]
  .nn <- vapply(seq_along(.n), function(i){
    grepl("^[l][Wb].*_[1-9]?[0-9]*", .n[i]) &&
      !any(paste0("eta.", .n[i]) %in% .etaNames)
  }, logical(1))
  .n <- .n[which(.nn)]
  if (length(.n) == 0) return(ui)
  .v <- gsub("^[l]", "", .n)
  .s1 <- paste0(.v, " <- l", .v)
  .s2 <- sprintf(str, .v, .v, .v)
  # Change the model expression first.
  .model <- vapply(.ui$lstChr,
                   function(l) {
                     .w <- which(.s1 == l)
                     if (length(.w) != 1) {
                       return(l)
                     }
                     .s2[.w]
                   }, character(1),
                   USE.NAMES=FALSE)
  rxode2::model(.ui) <- .model
  # Now add eta estimates
  .iniDf <- .ui$iniDf
  .w <- which(!is.na(.iniDf$neta1))
  if (length(.w) == 0L) {
    .maxEta <- 0
  } else {
    .maxEta <- max(.iniDf$neta1[.w])
  }
  .i1 <- .iniDf[1,]
  .i1$ntheta <- NA_integer_
  .i1$lower <- -Inf
  .i1$upper <- Inf
  .i1$est <- val
  .i1$label <- NA_character_
  .i1$backTransform <- NA_character_
  .i1$condition <- "id"
  .i1$err <- NA_character_
  .etas <- do.call(`rbind`,
                   lapply(seq_along(.v), function(i) {
                     .cur <- .i1
                     .cur$neta1 <- .maxEta+i
                     .cur$neta2 <- .maxEta+i
                     .cur$name <- paste0("eta.", .v[i])
                     .cur
                   }))
  .iniDf <- rbind(.iniDf, .etas)

  rxode2::ini(.ui) <- .iniDf
  .ui
}
