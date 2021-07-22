
# setup ----
# https://github.com/dgrtwo/data-screencasts/blob/master/ml-practice/ep7.Rmd
library(tidyverse)
library(tidymodels)
library(tonythemes)
# doParallel::registerDoParallel(cores = 4)
dir_proj <- '20210713'

factor_to_ordinal <- function(x) {
  ifelse(x == "Unknown", NA, as.integer(x))
}
prep_df <- function(df) {
  df %>% 
    mutate(avg_transaction_amt = total_trans_amt / total_trans_ct) %>%
    mutate(income_category = factor_to_ordinal(income_category),
           education_level = factor_to_ordinal(education_level)) 
}

f_read <- function(x) {
  res <-
    here::here(dir_proj, sprintf('%s.csv', x)) %>%
    read_csv()
  
  if(x == 'train') {
    res <-
      res %>% 
      mutate(churned = factor(ifelse(attrition_flag == 1, "yes", "no"))) %>% 
      select(-attrition_flag)
  }
  res
}
df <- f_read('train')
df_hold <- f_read('test')


set.seed(2021)
spl <- initial_split(df)
df_trn <- training(spl)
df_tst <- testing(spl)


df <- df %>% prep_df()
df_trn <- df_trn %>% prep_df()
df_hold <- df_hold %>% prep_df()

set.seed(2021-07-13)
# folds <- df_trn %>% vfold_cv(10)
folds <- df %>% vfold_cv(10)

# model ----
rec <- 
  df %>% 
  recipe(formula(churned ~ .), data = .) %>% 
  step_impute_mean(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_rm(id) %>%
  # For now, remove the formerly categorical ones (low importance)
  step_rm(income_category, education_level)
rec

spec_xg <- 
  boost_tree(
    trees = tune(),
    mtry = tune(),
    tree_depth = 5,
    learn_rate = tune()
  ) %>% 
  set_engine('xgboost') %>% 
  set_mode('classification')

spec_lin <- 
  logistic_reg(
    mixture = tune(),
    penalty = tune()
  ) %>% 
  set_engine('glmnet') %>% 
  set_mode('classification')

library(workflowsets)
sets <- 
  workflow_set(
    preproc = list(base = rec), 
    models = list(
      xg = spec_xg,
      lin = spec_lin
    )
  )
sets

grid_xg <- crossing(
  trees = seq(400, 1300, 20),
  mtry = c(2, 3),
  learn_rate = c(.018, .02)
)
grid_xg

grid_lin <- crossing(
  penalty = 10 ^ seq(-7, 0, 1),
  mixture = c(0, 0.5, 1)
)
grid_lin

path_tune <- here::here(dir_proj, 'tune_drob.rds')
do_run <- !file.exists(path_tune)

sets_updated <-
  sets %>% 
  option_add(grid = grid_xg, id = 'base_xg') %>% 
  option_add(grid = grid_lin, id = 'base_lin')
sets_updated$option[[1]]$grid

metset <- metric_set(mn_log_loss)
ctrl <- control_grid(
  extract = extract_model,
  verbose = TRUE,
  save_pred = TRUE,
  save_workflow = TRUE
)

tune <-
  sets_updated %>%
  workflow_map(
    seed = 42,
    resamples = folds,
    metrics = metset,
    control = ctrl,
    verbose = TRUE
  )
write_rds(tune, path_tune)
tune

tune <- read_rds(path_tune)
autoplot(tune)
autoplot(tune, id = 'base_xg')
autoplot(tune, id = 'base_lin')

path_fit_ens <- file.path(dir_proj, 'fit_ens_drob.rds')

stack <-
  stacks::stacks() %>% 
  stacks::add_candidates(tune)
stack

set.seed(42)
blend <- stack %>% stacks::blend_predictions(metric = metset)
fit_ens <- blend %>% stacks::fit_members(df)
# beepr::beep(8)
write_rds(fit_ens, path_fit_ens)

fit_ens <- read_rds(path_fit_ens)
fit_ens

autoplot(fit_ens) # 'performance'
autoplot(fit_ens, 'weights')
autoplot(fit_ens, 'members')

probs <- 
  fit_ens %>% 
  predict(df_hold, type = 'prob') %>% 
  bind_cols(df_hold %>% select(id)) %>% 
  select(id, attrition_flag = .pred_yes)
probs

path_export <- here::here(dir_proj, 'probs_stack_drob.csv')
write_csv(probs, path_export)

shell(
  glue::glue('kaggle competitions submit -f {path_export} -m "m" sliced-s01e07-HmPsw2 -m "probs_stack_drob"')
)

