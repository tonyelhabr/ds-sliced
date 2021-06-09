
path_env <- 'C:/Users/aelhabr/anaconda3/envs/tf'
path_py <- file.path(path_env, 'python.exe')
Sys.setenv(RETICULATE_PYTHON = path_py)
reticulate::use_condaenv(condaenv = path_env)

# install.packages('tensorflow')
# install.packages('keras')
library(keras)
library(tensorflow)
# use_python(path_py)
use_condaenv('tf', required = TRUE)
tf_config()


pass_input <- keras::layer_input(shape = c(52, 34, 3), name = 'pass_input')
dest_input <- keras::layer_input(shape = c(52, 34, 1), name = 'pass_input')
