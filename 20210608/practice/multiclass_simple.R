
library(tonyverse)
library(tonymodels)
path_env <- 'C:/Users/aelhabr/anaconda3/envs/tf'
path_py <- file.path(path_env, 'python.exe')
Sys.setenv(RETICULATE_PYTHON = path_py)
reticulate::use_condaenv(condaenv = path_env)
library(keras)
library(tensorflow)
# use_python(path_py)
use_condaenv('tf', required = TRUE)
tf_config()

theme_set(theme_minimal(base_size = 14))
dir_proj <- '20210608'
df <- 
  here::here(dir_proj, 'Pokemon.csv') %>%
  read_csv() %>% 
  janitor::clean_names()

df %>% skimr::skim()

df_filt <-
  df %>% 
  # filter(generation == 1) %>% 
  group_by(number) %>% 
  filter(row_number() == 1L) %>% 
  ungroup() %>% 
  filter(
    type_1 %in% c('Normal', 'Water', 'Fire', 'Grass', 'Bug')
  ) %>% 
  mutate(
    across(legendary, as.integer),
    across(generation, ordered)
  )
df_filt %>% count(type_2, sort = TRUE)

seed <- 969
col_y <- 'type_1'
col_y_sym <- col_y %>% sym()

set.seed(seed)
split <- df_filt %>% initial_split(strata = !!col_y_sym)
df_trn <- split %>% training()
df_tst  <- split %>% testing()

folds <- df_trn %>% vfold_cv(strata = !!col_y_sym)
folds

form <- paste0(col_y, '~ .') %>% as.formula()
rec_init <-
  df_trn %>% 
  # select(-c(name, number)) %>% 
  recipe(form, data = .) %>% 
  update_role(c(name, number), new_role = 'id') %>%
  # step_rm(any_of(c('type_2', 'total', 'name', 'number'))) %>% 
  step_rm(any_of(c('type_2', 'total'))) %>% 
  # step_mutate(legendary = as.integer(legendary)) %>% 
  # step_string2factor(legendary) %>% 
  step_dummy(generation) %>% 
  # # step_num2factor(generation, levels = as.character(1:6)) %>% 
  # step_unknown(type_2) %>% 
  # step_other(type_2) %>% 
  # step_dummy(generation, type_2) %>% 
  step_nzv(all_numeric_predictors())
rec_init

# checking ----
jui_trn <- rec_init %>% prep() %>% juice()
jui_trn
# jui_trn %>% skimr::skim()

# debugonce(glmnet:::lognet)
x_mat <- jui_trn %>% select(-any_of(c('name', 'number', col_y)))  %>% as.matrix()
y <- jui_trn[[col_y]]
fit_glmnet <-
  glmnet::glmnet(
    x = x_mat,
    y = y,
    family = 'multinomial',
    nlambda = 100
  )
fit_glmnet

fit_rf <- 
  ranger::ranger(
    formula = form, 
    data = jui_trn %>% select(-any_of(c('name', 'number'))),
    importance = 'impurity',
    mtry = 3,
    min.node.size = 3,
    num.trees = 50
  )
# fit_rf <- ranger::ranger(formula = form, data = jui_trn, num.trees = 50)
fit_rf

vi_rf <- fit_rf %>% vip::vi()
vi_rf

# fit_rf %>% 
#   vip::vi(
#     target = col_y,
#     method = 'permute',
#     metric = 'auc',
#     reference_class = as.character(y[1]),
#     pred_wrapper = function(fit, newdata) {
#       predict(fit, newdata)
#     }
#   )

rec_norm <-
  rec_init %>% 
  step_normalize(all_numeric_predictors())
rec_norm

f_set <- function(spec) {
  spec %>% 
    set_mode('classification')
}

spec_glmnet_simple <- 
  multinom_reg(mixture = 0.5, penalty = 0.1) %>% 
  f_set() %>% 
  set_engine('glmnet')
spec_glmnet_simple

wf_glmnet <-
  workflow() %>% 
  add_recipe(rec_init) %>% 
  add_model(spec_glmnet_simple)
wf_glmnet

# debugonce(ranger::ranger)
fit_glmnet <- wf_glmnet %>% fit(data = df_trn)
fit_glmnet

spec_rf_simple <- 
  rand_forest(mtry = 3, trees = 50) %>% 
  f_set() %>% 
  set_engine('ranger')
spec_rf_simple

wf_rf <-
  workflow() %>% 
  add_recipe(rec_init) %>% 
  add_model(spec_rf_simple)
wf_rf

# debugonce(ranger::ranger)
fit_rf <- wf_rf %>% fit(data = df_trn)
fit_rf

spec_keras_simple <- 
  multinom_reg(mixture = 0.5, penalty = 0.1) %>% 
  f_set() %>% 
  set_engine('keras', verobse = 0)
spec_keras_simple

wf_keras <-
  workflow() %>% 
  add_recipe(rec_init) %>% 
  add_model(spec_keras_simple)

fit_keras <- wf_keras %>% fit(data = df_trn)
fit_keras

spec_keras_tune <- 
  multinom_reg(penalty = tune()) %>% 
  f_set() %>% 
  set_engine('keras', verbose = 0)
spec_keras_tune

wf_keras_tune <-
  workflow() %>% 
  add_recipe(rec_init) %>% 
  add_model(spec_keras_tune)

library(finetune)
ctrl_grid <-
  control_race(
    save_pred = TRUE,
    verbose_elim = TRUE,
    parallel_over = 'everything',
    save_workflow = TRUE
  )
ctrl_grid
met_set <- yardstick::metric_set(yardstick::mn_log_loss)

grid <-
  grid_max_entropy(
    penalty(),
    size = 3
  )
grid

set.seed(seed)
res_tune <-
  wf_keras_tune %>% 
  tune_race_anova(
    # grid = 2,
    grid = grid,
    resamples = folds,
    metrics = met_set,
    control = ctrl_grid
  )
show_best(res_tune, metric = 'mn_log_loss')
plot_race(res_tune)

# mlp ----
spec_keras_mlp_simple <- 
  mlp() %>% 
  f_set() %>% 
  set_engine('keras', verbose = 0)
spec_keras_mlp_simple

wf_keras_mlp <-
  workflow() %>% 
  add_recipe(rec_init) %>% 
  add_model(spec_keras_mlp_simple)

fit_keras_mlp <- wf_keras_mlp %>% fit(data = df_trn)
fit_keras_mlp

probs_keras_mlp_tst <- fit_keras_mlp %>% predict(new_data = df_tst, type = 'prob')
probs_keras_mlp_tst

spec_keras_mlp_tune <- 
  mlp(hidden_units = tune(), penalty = tune()) %>% 
  f_set() %>% 
  set_engine('keras')
spec_keras_mlp_tune %>% translate()

wf_keras_mlp_tune <-
  workflow() %>% 
  add_recipe(rec_init) %>% 
  add_model(spec_keras_mlp_tune)

library(finetune)
ctrl_grid <-
  control_race(
    save_pred = TRUE,
    verbose_elim = TRUE,
    parallel_over = 'everything',
    save_workflow = TRUE
  )
ctrl_grid
met_set <- yardstick::metric_set(yardstick::mn_log_loss)

set.seed(42)
res_tune <-
  wf_keras_mlp_tune %>% 
  tune_race_anova(
    grid = 10,
    resamples = folds,
    metrics = met_set,
    control = ctrl_grid
  )
show_best(res_tune, metric = 'mn_log_loss')
plot_race(res_tune)
fit_keras_mlp <- wf_keras_mlp %>% fit(data = df_trn)
fit_keras_mlp

probs_keras_mlp_tst <- fit_keras_mlp %>% predict(new_data = df_tst, type = 'prob')
probs_keras_mlp_tst
