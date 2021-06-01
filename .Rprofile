
base::.First.sys()

path_r_profile <- '~/.Rprofile'
if(file.exists(path_r_profile)) {
  source(path_r_profile)
}
rm('path_r_profile')

if (FALSE) {
  path_env <- 'C:/Users/aelhabr/anaconda3/envs/tf'
  path_py <- file.path(path_env, 'python.exe')
  Sys.setenv(RETICULATE_PYTHON = path_py)
  reticulate::use_condaenv(condaenv = path_env)
  reticulate::use_condaenv('tf', required = TRUE)
  tensorflow::tf_config()
}


