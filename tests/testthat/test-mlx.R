test_that("Convert NNs in Monolix",{
  mlx_model <- test_path("testmodels/mlx_example1_model.txt")
  ref_model <- test_path("testmodels/mlx_example1_model_converted_ref.txt")
  
  tmpdir <- withr::local_tempdir()
  
  file.copy(mlx_model,tmpdir)
  tmp_model <- file.path(tmpdir, "mlx_example1_model.txt")
  
  nn_converter_mlx(tmp_model, pop_only = T)
  
  tmp_conv_model <- file.path(tmpdir, "mlx_example1_model_converted.txt")
  
  testthat::expect_equal(readLines(tmp_conv_model,warn = F),readLines(ref_model,warn = F))
})

test_that("Derivative calculations in Monolix",{
  der_data_mlx <- pmxNODE:::der_vs_state_mlx("c",min_state = 0, max_state = 0,
                                             mlx_file = test_path("testr/mlx_example1_ind.mlxtran"))
  
  ind_der_data_mlx <- pmxNODE:::ind_der_vs_state_mlx("c",min_stat = 0, max_state = 10,
                                                     mlx_file = test_path("testr/mlx_example1_ind.mlxtran"))
  
  der_data_t_mlx <- pmxNODE:::der_vs_state_mlx("ct",min_state = 0, max_state = 10, time_nn = T,
                                               mlx_file = test_path("testr/mlx_example1_ind.mlxtran"))
  
  ind_der_data_t_mlx <- pmxNODE:::ind_der_vs_state_mlx("ct",min_stat = 0, max_state = 10, time_nn = T,
                                                       mlx_file = test_path("testr/mlx_example1_ind.mlxtran"))
  
  der_data_ref <- read.table(test_path("testr/mlx_example1_ind_der-data_c_ref.txt"),
                             sep = ",", header = T)
  
  ind_der_data_ref <- read.table(test_path("testr/mlx_example1_ind_ind-der-data_c_ref.txt"),
                             sep = ",", header = T)
  
  der_data_t_ref <- read.table(test_path("testr/mlx_example1_ind_der-data_t_ref.txt"),
                             sep = ",", header = T)
  
  ind_der_data_t_ref <- read.table(test_path("testr/mlx_example1_ind_ind-der-data_t_ref.txt"),
                             sep = ",", header = T)
  
  testthat::expect_equal(der_data_mlx,der_data_ref)
  testthat::expect_equal(ind_der_data_mlx,ind_der_data_ref)
  testthat::expect_equal(der_data_t_mlx,der_data_t_ref)
  testthat::expect_equal(ind_der_data_t_mlx,ind_der_data_t_ref)
})