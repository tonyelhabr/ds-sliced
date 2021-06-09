
library(tidyverse)
library(tidymodels)

# Only for keras
path_env <- 'C:/Users/yourname/anaconda3/envs/tf'
path_py <- file.path(path_env, 'python.exe')
Sys.setenv(RETICULATE_PYTHON = path_py)
reticulate::use_condaenv(condaenv = path_env)
reticulate::use_condaenv('tf', required = TRUE)

theme_set(theme_minimal(base_size = 14))

# https://www.kaggle.com/abcsds/pokemon
dir_proj <- '/path/to/file'
df <- 
  here::here(dir_proj, 'Pokemon.csv') %>%
  read_csv() %>% 
  janitor::clean_names()
df
df %>% skimr::skim()

df_filt <-
  df %>% 
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
df_filt %>% count(type_1, sort = TRUE)
df_filt %>% count(type_2, sort = TRUE)

seed <- 42
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
  recipe(form, data = .) %>% 
  update_role(c(name, number), new_role = 'id') %>%
  step_rm(any_of(c('type_2', 'total'))) %>% 
  step_dummy(generation) %>% 
  step_nzv(all_numeric_predictors())
rec_init

# workflowsets ----
rec_norm <-
  rec_init %>% 
  step_normalize(all_numeric_predictors())
rec_norm

f_mode <- function(spec) {
  spec %>% 
    set_mode('classification')
}

library(workflowsets)
spec_glmnet <- 
  multinom_reg(mixture = tune(), penalty = tune()) %>% 
  f_mode() %>% 
  set_engine('glmnet')
spec_glmnet

spec_rf <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 50) %>% 
  f_mode() %>% 
  set_engine('ranger')
spec_rf

# No `mixture` term with `multinom_reg` and "keras" and "nnet" engines.
spec_nnet <- 
  multinom_reg(penalty = tune()) %>% 
  f_mode() %>% 
  set_engine('nnet')
spec_keras

spec_keras <- 
  multinom_reg(penalty = tune()) %>% 
  f_mode() %>% 
  set_engine('keras', verbose = 0)
spec_keras

spec_mlp_keras <- 
  mlp(penalty = tune(), hidden_units = tune()) %>% 
  f_mode() %>% 
  set_engine('keras', verbose = 0)
spec_mlp_keras

sets <- 
  workflow_set(
    preproc = list(base = rec_init, norm = rec_norm), 
    models = list(
      mlp_keras = spec_mlp_keras,
      keras = spec_keras,
      nnet = spec_nnet,
      rf = spec_rf,
      glmnet = spec_glmnet
    )
  )
sets

path_res_grid <- here::here(dir_proj, 'res_grid.rds')
do_run <- !file.exists(path_res_grid)

met_set <- metric_set(mn_log_loss)

f_tune <- function() {
  library(finetune)
  ctrl_grid <-
    control_race(
      save_pred = TRUE,
      parallel_over = 'everything',
      save_workflow = TRUE
    )
  ctrl_grid
  
  res_grid <-
    sets %>%
    workflow_map(
      'tune_race_anova',
      seed = seed,
      resamples = folds,
      grid = 10,
      metrics = met_set,
      control = ctrl_grid,
      verbose = TRUE
    )
  write_rds(res_grid, path_res_grid)
  beepr::beep(8)
  res_grid
}

# A little over 10 minutes. 8 minutes without the `multinom_reg` "keras" spec.
if(do_run) { f_tune() }

res_grid <- read_rds(path_res_grid)
autoplot(res_grid)

n_model <-
  res_grid %>% 
  collect_metrics(summarize = FALSE) %>% 
  nrow()
n_model

res_ranks <-
  res_grid %>% 
  # ranks based on mn_log_loss since that was the only metric i used.
  rank_results() %>% 
  select(wflow_id, model, .config, .metric, mean, rank) %>% 
  group_by(wflow_id, .metric) %>% 
  slice_min(rank, with_ties = FALSE) %>% 
  ungroup() %>% 
  arrange(rank)
res_ranks

path_fit_ens <- file.path(dir_proj, 'fit_ens.rds')
do_run_ens <- !file.exists(path_fit_ens)

library(stacks)
f_ens <- function() {
  stack <-
    stacks::stacks() %>% 
    stacks::add_candidates(res_grid)
  stack
  
  set.seed(seed)
  blend <- stack %>% stacks::blend_predictions(metric = met_set)
  fit_ens <- blend %>% stacks::fit_members()
  beepr::beep(8)
  write_rds(fit_ens, path_fit_ens)
}

# A little over 3 minutes.
if(do_run_ens) { f_ens() }


fit_ens <- read_rds(path_fit_ens)

# wtfhotdog does this mean?!?
fit_ens

# stacks::autoplot
autoplot(fit_ens) # 'performance'
autoplot(fit_ens, 'weights')
autoplot(fit_ens, 'members')

coefs_top <- stacks:::top_coefs(fit_ens)
coefs_top

col_y_sym <- sym(col_y)
do_prob_eval <- function(df) {
  df_join <- 
    df %>% 
    select(all_of(col_y)) %>% 
    mutate(across(all_of(col_y), factor))
  
  probs <- 
    fit_ens %>% 
    predict(df, type = 'prob') %>% 
    bind_cols(df_join)
  
  preds <- 
    fit_ens %>% 
    predict(df) %>% 
    bind_cols(df_join)
  
  metrics <-
    probs %>% 
    mutate(across(type_1, factor)) %>% 
    mn_log_loss(!!col_y_sym, estimate = matches('^[.]pred'))
  
  cm <-
    preds %>% 
    yardstick::conf_mat(.pred_class, !!col_y_sym)
  
  list(probs = probs, preds = preds, metrics = metrics, cm = cm)
}

library(zeallot)
c(probs_trn, preds_trn, metrics_trn, cm_trn) %<-% do_prob_eval(df_trn)
c(probs_tst, preds_tst, metrics_tst, cm_tst) %<-% do_prob_eval(df_tst)

metrics_trn
metrics_tst

probs_trn %>% 
  select(where(is.numeric)) %>% 
  corrr::correlate() %>% 
  corrr::rplot()

# yardstick:::conf_mat_plot_types
autoplot(cm_tst) # 'mosiac'
autoplot(cm_tst, 'heatmap')


