if (requireNamespace("rxode2", quietly = TRUE)) {
  test_that("NN ODE works with no initial estimates", {

    library(rxode2)

    f_ode_pop <- function() {
      model({
        V <- lV
        d/dt(centr)  =  NN(1, state=centr,min_init=0,max_init=300, pop=TRUE)
        cp = centr / V
      })
    }

    expect_error(f_ode_pop(), NA)

    f <- f_ode_pop()

    expect_equal(names(f$theta),
                 c("lW1_11", "lW1_12", "lW1_13", "lW1_14", "lW1_15", "lb1_11",
                   "lb1_12", "lb1_13", "lb1_14", "lb1_15", "lW1_21", "lW1_22", "lW1_23",
                   "lW1_24", "lW1_25", "lb1_21"))

    expect_equal(f$omega, NULL)

    f_ode_pop <- function() {
      model({
        V <- lV
        d/dt(centr)  =  NN(1, state=centr,min_init=0,max_init=300, pop=FALSE)
        cp = centr / V
      })
    }

    suppressWarnings(expect_error(f_ode_pop(), NA))

    f <- suppressWarnings(f_ode_pop())

    expect_equal(names(f$theta),
                 c("lW1_11", "lW1_12", "lW1_13", "lW1_14", "lW1_15", "lb1_11",
                   "lb1_12", "lb1_13", "lb1_14", "lb1_15", "lW1_21", "lW1_22", "lW1_23",
                   "lW1_24", "lW1_25", "lb1_21"))

    expect_equal(dimnames(f$omega)[[1]],
                 c("eta.W1_11", "eta.W1_12", "eta.W1_13", "eta.W1_14", "eta.W1_15",
                   "eta.b1_11", "eta.b1_12", "eta.b1_13", "eta.b1_14", "eta.b1_15",
                   "eta.W1_21", "eta.W1_22", "eta.W1_23", "eta.W1_24", "eta.W1_25",
                   "eta.b1_21"))

  })

  test_that("NN errors when overwritting a parameter", {

    f_ode_pop <- function() {
      model({
        h1_5 <- lV
        d/dt(centr)  =  NN(1, state=centr,min_init=0,max_init=300, pop=TRUE)
        cp = centr / h1_5
      })
    }

    expect_error(f_ode_pop(), "h1_5")

  })

  test_that("NN with no etas", {

    f_ode_pop <- function() {
       ini({
         lV <- 1
         prop.err <- 0.1
       })
       model({
         V <- lV
         d/dt(centr)  =  NN(1, state=centr,min_init=0,max_init=300)
         cp = centr / V
         cp ~ prop(prop.err)
       })
    }

    expect_error(f_ode_pop(), NA)

    f <- f_ode_pop()

    expect_equal(names(f$theta),
                 c("lV", "prop.err", "lW1_11", "lW1_12", "lW1_13", "lW1_14", "lW1_15",
                   "lb1_11", "lb1_12", "lb1_13", "lb1_14", "lb1_15", "lW1_21", "lW1_22",
                   "lW1_23", "lW1_24", "lW1_25", "lb1_21"))

    expect_equal(f$omega, NULL)


    f_ode_pop <- function() {
      ini({
        lV <- 1
        prop.err <- 0.1
      })
      model({
        V <- lV
        d/dt(centr)  =  NN(1, state=centr,min_init=0,max_init=300, pop=FALSE)
        cp = centr / V
        cp ~ prop(prop.err)
      })
    }

    expect_error(f_ode_pop(), NA)

    f <- f_ode_pop()

    expect_equal(names(f$theta),
                 c("lV", "prop.err", "lW1_11", "lW1_12", "lW1_13", "lW1_14", "lW1_15",
                   "lb1_11", "lb1_12", "lb1_13", "lb1_14", "lb1_15", "lW1_21", "lW1_22",
                   "lW1_23", "lW1_24", "lW1_25", "lb1_21"))

    expect_equal(dimnames(f$omega)[[1]],
                 c("eta.W1_11", "eta.W1_12", "eta.W1_13", "eta.W1_14", "eta.W1_15",
                   "eta.b1_11", "eta.b1_12", "eta.b1_13", "eta.b1_14", "eta.b1_15",
                   "eta.W1_21", "eta.W1_22", "eta.W1_23", "eta.W1_24", "eta.W1_25",
                   "eta.b1_21"))

  })

  test_that("NN with no thetas", {

    f_ode_pop <- function() {
      ini({
        eta.V ~ 1.1
      })
      model({
        V <- eta.V
        d/dt(centr)  =  NN(1, state=centr,min_init=0,max_init=300, pop=TRUE)
        cp = centr / V
      })
    }

    expect_error(f_ode_pop(), NA)

    f <- f_ode_pop()

    expect_equal(names(f$theta),
                 c("lW1_11", "lW1_12", "lW1_13", "lW1_14", "lW1_15",
                   "lb1_11", "lb1_12", "lb1_13", "lb1_14", "lb1_15", "lW1_21", "lW1_22",
                   "lW1_23", "lW1_24", "lW1_25", "lb1_21"))

    expect_equal(dimnames(f$omega)[[1]], "eta.V")

    f_ode_pop <- function() {
      ini({
        eta.V ~ 1.1
      })
      model({
        V <- eta.V
        d/dt(centr)  =  NN(1, state=centr,min_init=0,max_init=300, pop=FALSE)
        cp = centr / V
      })
    }

    expect_error(f_ode_pop(), NA)

    f <- f_ode_pop()

    expect_equal(names(f$theta),
                 c("lW1_11", "lW1_12", "lW1_13", "lW1_14", "lW1_15",
                   "lb1_11", "lb1_12", "lb1_13", "lb1_14", "lb1_15", "lW1_21", "lW1_22",
                   "lW1_23", "lW1_24", "lW1_25", "lb1_21"))

    expect_equal(dimnames(f$omega)[[1]],
                 c("eta.V", "eta.W1_11", "eta.W1_12", "eta.W1_13", "eta.W1_14",
                   "eta.W1_15", "eta.b1_11", "eta.b1_12", "eta.b1_13", "eta.b1_14",
                   "eta.b1_15", "eta.W1_21", "eta.W1_22", "eta.W1_23", "eta.W1_24",
                   "eta.W1_25", "eta.b1_21"))

  })

  }
