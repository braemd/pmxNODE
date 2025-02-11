
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pmxNODE \<img src=“man/pmxNODE_sticker_4.png” align=“right”, height=“138”\>

<!-- badges: start -->
<!-- badges: end -->

The goal of pmxNODE is to facilitate the implementation of neural
ordinary differential equations (NODEs) in pharmacometric software,
i.e., Monolix, NONMEM, and nlmixr2.

## Installation

You can install the development version of pmxNODE like so:

``` r
devtools::install_github("braemd/pmxNODE")
```

## How to use

#### General workflow

The general workflow for Monolix with pmxNODE consists of few steps:

- Write a Monolix model file, as you would do normaly. However, you can
  utilize NN functions in your model code, e.g., for complex or unknown
  model parts.

- Convert the written model file with the `nn_converter_mlx` function
  from the pmxNODE package. This function can also directly generate a
  *.mlxtran* file with automatically initialized model parameters.

- We suggest to fit the model to the data first without inter-individual
  variability on neural networks parameters (argument *pop = True* in
  the `nn_converter_mlx` function) and add the random effects in a
  second run, where parameters were initialized with last estimates.

#### Package loading and initialization

We first need to load the pmxNODE package and to initialize the
lixoftConnectors package (can be done with the `software_initializer`
function from pmxNODE).

If the lixoftConnectors package is not yet installed, an installation
path to the package .tar.gz file from Monolix must be provided.

``` r
library(pmxNODE)
library(ggplot2)
#> Warning: Paket 'ggplot2' wurde unter R Version 4.2.3 erstellt
software_initializer(software = "Monolix",
                     mlx_path = "C:/ProgramData/Lixoft/MonolixSuite2021R2")
#> Lade nötiges Paket: RJSONIO
#> [INFO] The lixoftConnectors package has been successfully initialized:
#> lixoftConnectors package version -> 2021.2
#> Lixoft softwares suite version   -> 2021R2
```

#### Examples

Some examples are available in the pmxNODE package. To see all example
files, you can use the `get_example_list` function. To copy an example
to a folder of your choice, you can use the `copy_example` function.
After calling the `copy_examples` function, two files should be in the
target folder, a data file and a Monolix model file.

``` r
get_example_list()

copy_examples(
  target_folder = "~/pmxNODE",
  example_nr = 1,
  example_software = "Monolix"
)
```

#### Converting and population fit

Before fitting the model, it needs to be converted with the
`nn_converter_mlx` function. In addition to the path/file name of the
unconverted Monolix model and the argument on including inter-individual
variability for the neural network parameters, a Monolix *.mlxtran* file
can be automatically generated with the *gen_mlx_file = TRUE* argument.
In order to do so, a data file and the header types must be provided. If
no file name for the new Monolix file is given through the *mlx_name*
argument, the Monolix file name is automatically generated based on the
Monolix model file. Note that a suffix is added to the file name, either
*\_pop* if *pop = TRUE* or *\_ind* if *ind = TRUE*.

The model can be automatically run from R with the function `run_mlx`
from the pmxNODE package.

``` r
nn_converter_mlx(mlx_path = "~/pmxNODE/mlx_example1_model.txt",
                 pop = TRUE,
                 gen_mlx_file = TRUE,
                 mlx_name = "~/pmxNODE/mlx_example1",
                 data_file = "~/pmxNODE/data_example1_mlx.csv",
                 header_types = c("id","time","amount","observation"))

run_mlx("~/pmxNODE/mlx_example1_pop.mlxtran")
```

#### Converting and individual fit

In order to get the parameter estimations from the population fit
(without inter-individual variability), the `pre_fixef_extractor_mlx`
function can be utilized.

These parameter estimates can be given as additional argument
*pre_fixef* to the `nn_converter_mlx` function. To include
inter-individual variability, the population argument is set to false
(*pop = FALSE*) in the `nn_converter_mlx` function.

The final model with inter-individual variability can then be fitted
again with the `run_mlx` function.

``` r
est_parms <- pre_fixef_extractor_mlx("~/pmxNODE/mlx_example1_pop.mlxtran")

nn_converter_mlx(mlx_path = "~/pmxNODE/mlx_example1_model.txt",
                 pop = FALSE,
                 pre_fixef = est_parms,
                 gen_mlx_file = TRUE,
                 mlx_name = "~/pmxNODE/mlx_example1",
                 data_file = "~/pmxNODE/data_example1_mlx.csv",
                 header_types = c("id","time","amount","observation"))

run_mlx("~/pmxNODE/mlx_example1_ind.mlxtran")
```
