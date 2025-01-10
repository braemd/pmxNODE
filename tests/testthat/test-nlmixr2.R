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

    suppressWarnings(expect_error(f_ode_pop(), NA))

    f <- suppressWarnings(f_ode_pop())

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

    suppressWarnings(expect_error(f_ode_pop(), NA))

    f <- suppressWarnings(f_ode_pop())

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

    suppressWarnings(expect_error(f_ode_pop(), NA))

    f <- suppressWarnings(f_ode_pop())

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

  test_that("NN with etas and thetas", {

    f_ode_pop <- function() {
      ini({
        tV <- 1
        eta.V ~ 1.1
      })
      model({
        V <- exp(tV+eta.V)
        d/dt(centr)  =  NN(1, state=centr,min_init=0,max_init=300, pop=TRUE)
        cp = centr / V
      })
    }

    expect_error(f_ode_pop(), NA)

    f <- f_ode_pop()

    expect_equal(names(f$theta),
                 c("tV", "lW1_11", "lW1_12", "lW1_13", "lW1_14", "lW1_15", "lb1_11",
                   "lb1_12", "lb1_13", "lb1_14", "lb1_15", "lW1_21", "lW1_22", "lW1_23",
                   "lW1_24", "lW1_25", "lb1_21"))

    expect_equal(dimnames(f$omega)[[1]],
                 "eta.V")

    f_ode_pop <- function() {
      ini({
        tV <- 1
        eta.V ~ 1.1
      })
      model({
        V <- exp(tV+eta.V)
        d/dt(centr)  =  NN(1, state=centr,min_init=0,max_init=300, pop=FALSE)
        cp = centr / V
      })
    }

    suppressWarnings(expect_error(f_ode_pop(), NA))

    f <- suppressWarnings(f_ode_pop())

    expect_equal(names(f$theta),
                 c("tV", "lW1_11", "lW1_12", "lW1_13", "lW1_14", "lW1_15", "lb1_11",
                   "lb1_12", "lb1_13", "lb1_14", "lb1_15", "lW1_21", "lW1_22", "lW1_23",
                   "lW1_24", "lW1_25", "lb1_21"))

    expect_equal(dimnames(f$omega)[[1]],
                 c("eta.V", "eta.W1_11", "eta.W1_12", "eta.W1_13", "eta.W1_14",
                   "eta.W1_15", "eta.b1_11", "eta.b1_12", "eta.b1_13", "eta.b1_14",
                   "eta.b1_15", "eta.W1_21", "eta.W1_22", "eta.W1_23", "eta.W1_24",
                   "eta.W1_25", "eta.b1_21"))

  })

  test_that("Multiple NN ode", {

    f_ode_pop <- function() {
      ini({
        tV <- 1
        eta.V ~ 1.1
      })
      model({
        V <- exp(tV+eta.V)
        d/dt(depot) <- NN(a, state=depot, min_init=0.5, max_init=10) +
          DOSE*NN(at, state=t, min_init=0.5, max_init=10, time_nn=TRUE)
        d/dt(centr)  =  -NN(a) - DOSE*NN(at) +
          NN(c, state=centr, min_init=0.5, max_init=3) +
          DOSE*NN(ct, state=t, min_init=0.5, max_init=10, time_nn=TRUE)
        cp = centr / V
      })
    }

    expect_error(f_ode_pop(), NA)

    f <- f_ode_pop()

    expect_equal(names(f$theta),
                 c("tV", "lWa_11", "lWa_12", "lWa_13", "lWa_14", "lWa_15", "lba_11",
                   "lba_12", "lba_13", "lba_14", "lba_15", "lWa_21", "lWa_22", "lWa_23",
                   "lWa_24", "lWa_25", "lba_21", "lWat_11", "lWat_12", "lWat_13",
                   "lWat_14", "lWat_15", "lbat_11", "lbat_12", "lbat_13", "lbat_14",
                   "lbat_15", "lWat_21", "lWat_22", "lWat_23", "lWat_24", "lWat_25",
                   "lWc_11", "lWc_12", "lWc_13", "lWc_14", "lWc_15", "lbc_11", "lbc_12",
                   "lbc_13", "lbc_14", "lbc_15", "lWc_21", "lWc_22", "lWc_23", "lWc_24",
                   "lWc_25", "lbc_21", "lWct_11", "lWct_12", "lWct_13", "lWct_14",
                   "lWct_15", "lbct_11", "lbct_12", "lbct_13", "lbct_14", "lbct_15",
                   "lWct_21", "lWct_22", "lWct_23", "lWct_24", "lWct_25"))

    expect_equal(dimnames(f$omega)[[1]],
                 "eta.V")

    f_ode_pop <- function() {
      ini({
        tV <- 1
        eta.V ~ 1.1
      })
      model({
        V <- exp(tV+eta.V)
        d/dt(depot) <- NN(a, state=depot, min_init=0.5, max_init=10, pop=FALSE) +
          DOSE*NN(at, state=t, min_init=0.5, max_init=10, time_nn=TRUE, pop=FALSE)
        d/dt(centr)  =  -NN(a) - DOSE*NN(at) +
          NN(c, state=centr, min_init=0.5, max_init=3, pop=FALSE) +
          DOSE*NN(ct, state=t, min_init=0.5, max_init=10, time_nn=TRUE, pop=FALSE)
        cp = centr / V
      })
    }

    f <- f_ode_pop()

    expect_equal(names(f$theta),
                 c("tV", "lWa_11", "lWa_12", "lWa_13", "lWa_14", "lWa_15", "lba_11",
                   "lba_12", "lba_13", "lba_14", "lba_15", "lWa_21", "lWa_22", "lWa_23",
                   "lWa_24", "lWa_25", "lba_21", "lWat_11", "lWat_12", "lWat_13",
                   "lWat_14", "lWat_15", "lbat_11", "lbat_12", "lbat_13", "lbat_14",
                   "lbat_15", "lWat_21", "lWat_22", "lWat_23", "lWat_24", "lWat_25",
                   "lWc_11", "lWc_12", "lWc_13", "lWc_14", "lWc_15", "lbc_11", "lbc_12",
                   "lbc_13", "lbc_14", "lbc_15", "lWc_21", "lWc_22", "lWc_23", "lWc_24",
                   "lWc_25", "lbc_21", "lWct_11", "lWct_12", "lWct_13", "lWct_14",
                   "lWct_15", "lbct_11", "lbct_12", "lbct_13", "lbct_14", "lbct_15",
                   "lWct_21", "lWct_22", "lWct_23", "lWct_24", "lWct_25"))

    expect_equal(dimnames(f$omega)[[1]],
                 c("eta.V", "eta.Wa_11", "eta.Wa_12", "eta.Wa_13", "eta.Wa_14",
                   "eta.Wa_15", "eta.ba_11", "eta.ba_12", "eta.ba_13", "eta.ba_14",
                   "eta.ba_15", "eta.Wa_21", "eta.Wa_22", "eta.Wa_23", "eta.Wa_24",
                   "eta.Wa_25", "eta.ba_21", "eta.Wat_11", "eta.Wat_12", "eta.Wat_13",
                   "eta.Wat_14", "eta.Wat_15", "eta.bat_11", "eta.bat_12", "eta.bat_13",
                   "eta.bat_14", "eta.bat_15", "eta.Wat_21", "eta.Wat_22", "eta.Wat_23",
                   "eta.Wat_24", "eta.Wat_25", "eta.Wc_11", "eta.Wc_12", "eta.Wc_13",
                   "eta.Wc_14", "eta.Wc_15", "eta.bc_11", "eta.bc_12", "eta.bc_13",
                   "eta.bc_14", "eta.bc_15", "eta.Wc_21", "eta.Wc_22", "eta.Wc_23",
                   "eta.Wc_24", "eta.Wc_25", "eta.bc_21", "eta.Wct_11", "eta.Wct_12",
                   "eta.Wct_13", "eta.Wct_14", "eta.Wct_15", "eta.bct_11", "eta.bct_12",
                   "eta.bct_13", "eta.bct_14", "eta.bct_15", "eta.Wct_21", "eta.Wct_22",
                   "eta.Wct_23", "eta.Wct_24", "eta.Wct_25"))


  })

}
