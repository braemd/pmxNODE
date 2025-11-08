test_that("Convert NNs in NONMEM",{
  nm_model <- test_path("testmodels/nm_example1_model.ctl")
  ref_model_pop <- test_path("testmodels/nm_example1_model_converted_pop_ref.ctl")
  ref_model_ind <- test_path("testmodels/nm_example1_model_converted_ind_ref.ctl")
  
  tmpdir <- withr::local_tempdir()
  
  file.copy(nm_model,tmpdir)
  tmp_model <- file.path(tmpdir, "nm_example1_model.ctl")
  
  nn_converter_nm(tmp_model, pop_only = T)
  nn_converter_nm(tmp_model, pop_only = F)
  
  tmp_conv_model_pop <- file.path(tmpdir, "nm_example1_model_converted_pop.ctl")
  tmp_conv_model_ind <- file.path(tmpdir, "nm_example1_model_converted_ind.ctl")
  
  testthat::expect_equal(readLines(tmp_conv_model_pop,warn = F),readLines(ref_model_pop,warn = F))
  testthat::expect_equal(readLines(tmp_conv_model_ind,warn = F),readLines(ref_model_ind,warn = F))
})

test_that("Derivative calculations in NONMEM",{
  der_data_nm <- pmxNODE:::der_vs_state_nm("c",min_state = 0, max_state = 10,
                                           nm_res_file = test_path("testresults/nm_example1_model_converted_ind.res"))
  
  ind_der_data_nm <- pmxNODE:::ind_der_vs_state_nm("c",min_stat = 0, max_state = 10,
                                                   nm_res_file = test_path("testresults/nm_example1_model_converted_ind.res"),
                                                   nm_phi_file = test_path("testresults/nm_example1_model_converted_ind.phi"))
  
  der_data_t_nm <- pmxNODE:::der_vs_state_nm("t",min_state = 0, max_state = 10, time_nn = T,
                                             nm_res_file = test_path("testresults/nm_example1_model_converted_ind.res"))
  
  ind_der_data_t_nm <- pmxNODE:::ind_der_vs_state_nm("t",min_stat = 0, max_state = 10, time_nn = T,
                                                     nm_res_file = test_path("testresults/nm_example1_model_converted_ind.res"),
                                                     nm_phi_file = test_path("testresults/nm_example1_model_converted_ind.phi"))
  
  der_data_ref <- read.table(test_path("testresults/nm_example1_ind_der-data_c_ref.txt"),
                             sep = ",", header = T)
  
  ind_der_data_ref <- read.table(test_path("testresults/nm_example1_ind_ind-der-data_c_ref.txt"),
                                 sep = ",", header = T)
  
  der_data_t_ref <- read.table(test_path("testresults/nm_example1_ind_der-data_t_ref.txt"),
                               sep = ",", header = T)
  
  ind_der_data_t_ref <- read.table(test_path("testresults/nm_example1_ind_ind-der-data_t_ref.txt"),
                                   sep = ",", header = T)
  
  testthat::expect_equal(der_data_nm,der_data_ref)
  testthat::expect_equal(ind_der_data_nm,ind_der_data_ref)
  testthat::expect_equal(der_data_t_nm,der_data_t_ref)
  testthat::expect_equal(ind_der_data_t_nm,ind_der_data_t_ref)
})