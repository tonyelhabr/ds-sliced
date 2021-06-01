
library(tidyverse)
library(tidymodels)
library(stacks)
dir_proj <- file.path('20210302', 'review')
path_tune <- file.path(dir_proj, 'res_tune.rds')
path_fit_ens <- file.path(dir_proj, 'fit_ens.rds')

f_import <- function(suffix) {
  suffix <- ifelse(suffix == 'holdout', 'holdout-w-actual', suffx)
  res <-
    file.path('20210302', sprintf('sliced-s00e01-%s.csv', suffix)) %>%
    read_csv(guess_max = 20000) %>%
    select(-1) %>%
    select(-undergra, -matches('^dec')) %>% 
    mutate(idx = row_number())
  if(any('match' %in% colnames(res))) {
    res <-
      res %>% 
      mutate(across(match, factor))
  }
  res
}
df_trn <- f_import('data')
df_trn

df_tst <- f_import('holdout')
df_tst$match

library(embed)
rec <-
  recipe(match ~ ., data = df_trn) %>% 
  update_role(c(iid, id, pid, idx), new_role = 'id') %>%
  step_impute_linear(
    all_numeric_predictors(),
      impute_with = imp_vars(
        gender,
        idg,
        condtn,
        wave,
        round,
        position,
        order,
        partner,
        samerace
      )
  ) %>% 
  step_normalize(all_numeric_predictors()) %>%
  # step_other(all_nominal_predictors(), threshold = 0.1) %>% 
  # step_dummy(all_nominal_predictors()) %>% 
  step_rm(all_nominal_predictors()) %>% 
  step_pca(all_numeric_predictors(), num_comp = 30) %>% 
  themis::step_downsample(match)
rec
jui_trn <- rec %>% prep() %>% bake(df_trn)
jui_trn

f_mode <- function(x) { x %>% set_mode('classification') }

# spec_rf <-
#   rand_forest() %>% 
#   f_mode() %>% 
#   # set_engine('classification') %>% 
#   set_engine('ranger')
# spec_rf
# 
# wf_rf <-
#   workflow() %>% 
#   add_recipe(rec) %>% 
#   add_model(spec_rf)
# fit_rf <- wf_rf %>% fit(df_trn)
# fit_rf
# library(yardstick)
# met_set <- metric_set(accuracy)
# 
# library(zeallot)
# do_eval <- function(df) {
#   df <- df_trn
#   df_join <- df %>% select(match)
#   probs <- fit_rf %>% predict(df, type = 'prob') %>% bind_cols(df_join)
#   preds <- fit_rf %>% predict(df) %>% bind_cols(df_join)
#   metrics <- preds %>% met_set(match, estimate = .pred_class)
#   cm <- preds %>% conf_mat(match, .pred_class)
#   
#   list(probs = probs, preds = preds, metrics = metrics, cm = cm)
# }
# c(probs_trn, preds_trn, metrics_trn, cm_trn) %<-% do_eval(df_trn)

# workflowset time ----
library(workflowsets)
spec_glmnet <- 
  multinom_reg(mixture = tune(), penalty = tune()) %>% 
  f_mode() %>% 
  set_engine('glmnet')
spec_glmnet

spec_rf <- 
  rand_forest(mtry = tune(), trees = tune(), min_n = 3) %>% 
  f_mode() %>% 
  set_engine('ranger')
spec_rf

# No `mixture` term with `multinom_reg` and "keras" engine.
spec_nnet <- 
  multinom_reg(penalty = tune()) %>% 
  f_mode() %>% 
  set_engine('nnet')
spec_nnet

# # No `mixture` term with `multinom_reg` and "keras" engine.
# spec_keras <- 
#   multinom_reg(penalty = tune()) %>% 
#   f_mode() %>% 
#   set_engine('keras', verbose = 0)
# spec_keras

sets <- 
  workflow_set(
    preproc = list(base = rec), 
    models = list(
      # keras = spec_keras,
      # nnet = spec_nnet,
      rf = spec_rf,
      glmnet = spec_glmnet
    )
  )
sets

path_res_grid <- here::here(dir_proj, 'res_grid.rds')
do_run <- !file.exists(path_res_grid)

met_set <- metric_set(roc_auc, accuracy)

seed <- 42
folds <- group_vfold_cv(df_trn, group = 'wave', v = 10)
# folds <- vfold_cv(df_trn, strata = wave, v = 10)
folds
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
  # beepr::beep(8)
  res_grid
}
f_tune_timely <- xgbh::time_it(f_tune)

# A little over 10 minutes. 8 minutes without the `multinom_reg` "keras" spec.
# About 24 minutes
if(do_run) { f_tune_timely() }

res_grid <- read_rds(path_res_grid)
autoplot(res_grid)

res_ranks <-
  res_grid %>% 
  workflowsets::rank_results('accuracy') %>% 
  filter(.metric == 'accuracy') %>%
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
  # beepr::beep(8)
  write_rds(fit_ens, path_fit_ens)
}
f_ens_timely <- xgbh::time_it(f_ens)

# About 2 minutes.
if(do_run_ens) { f_ens_timely() }

fit_ens <- read_rds(path_fit_ens)

fit_ens
autoplot(fit_ens) # 'performance'
autoplot(fit_ens, 'weights')
autoplot(fit_ens, 'members')

n_model <-
  res_grid %>% 
  collect_metrics() %>% 
  nrow()
n_model

# Note that xgboost sucks if you don't have good parameters
res_ranks <-
  res_grid %>% 
  workflowsets::rank_results('accuracy') %>% 
  filter(.metric == 'accuracy') %>%
  select(wflow_id, model, .config, .metric, mean, rank) %>% 
  # group_by(wflow_id, .metric) %>% 
  # slice_min(rank, with_ties = FALSE) %>% 
  # ungroup() %>% 
  filter(rank == 1L) %>% 
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
  # beepr::beep(8)
  write_rds(fit_ens, path_fit_ens)
}
f_ens_timely <- xgbh::time_it(f_ens)

# A little over 3 minutes.
if(do_run_ens) { f_ens_timely() }

fit_ens <- read_rds(path_fit_ens)

fit_ens
autoplot(fit_ens) # 'performance'
autoplot(fit_ens, 'weights')
autoplot(fit_ens, 'members')

col_y <- 'match'
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
  
  acc <-
    preds %>% 
    accuracy(truth = !!col_y_sym, .pred_class)

  roc <-
    probs %>% 
    roc_auc(truth = !!col_y_sym, .pred_0)
  
  cm <-
    preds %>% 
    conf_mat(.pred_class, !!col_y_sym)

  list(probs = probs, preds = preds, metrics = bind_rows(acc, roc), cm = cm)
}

library(zeallot)
c(probs_trn, preds_trn, metrics_trn, cm_trn) %<-% do_prob_eval(df_trn)
c(probs_tst, preds_tst, metrics_tst, cm_tst) %<-% do_prob_eval(df_tst)

metrics_trn
metrics_tst
