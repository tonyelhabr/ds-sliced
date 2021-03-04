
library(tidyverse)
library(tidymodels)
library(stacks)
df <- AmesHousing::make_ames()

set.seed(42)
split <- df %>% rsample::initial_split(prop = 0.8)
df_trn <- split %>% training()
df_tst <- split %>% testing()
folds <- df_trn %>% vfold_cv()

fmla <- formula(Sale_Price ~ .)

rec_init <-
  recipe(fmla, data = df_trn) %>% 
  step_nzv(all_predictors()) %>% 
  step_corr(all_numeric_predictors()) %>% 
  step_lincomb(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) 

rec_lm <-
  rec_init %>% 
  step_rm(all_nominal()) %>% 
  step_bs(all_predictors()) %>% 
  step_YeoJohnson(all_predictors())

rec_tree <-
  rec_init %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal()) 
  
  
fit_lm <- 
  linear_reg() %>% 
  set_mode('regression') %>% 
  set_engine('lm')

fit_rf <- 
  rand_forest(min_n = tune(), trees = tune()) %>% 
  set_mode('regression') %>% 
  set_engine('randomForest')

fit_gb <-
  boost_tree(
    learn_rate = tune(),
    trees = tune(),
    tree_depth = tune()
  ) %>%
  set_mode('regression') %>%
  set_engine('xgboost')

ctrl <- control_grid(save_pred = TRUE, save_workflow = TRUE)
mets <- metric_set(rmse, mae, rsq)

grid_rf <-
  grid_latin_hypercube(
    fit_rf %>% parameters(),
    size = 10
  )

grid_gb <-
  grid_latin_hypercube(
    fit_gb %>% parameters(),
    size = 10
  )

wf_lm <-
  workflow() %>% 
  add_recipe(rec_lm) %>% 
  add_model(fit_lm)

wf_tree_init <-
  workflow() %>% 
  add_recipe(rec_tree)

wf_rf <- wf_tree_init %>% add_model(fit_rf)
wf_gb <- wf_tree_init %>% add_model(fit_gb)

res_lm <-
  fit_resamples(
    wf_lm,
    resamples = folds,
    metrics = mets,
    control = ctrl
  )

tune_tree_grid_partial <-
  partial(
    tune_grid,
    resamples = folds,
    metrics = mets,
    control = ctrl,
    ... =
  )

res_rf <- wf_rf %>% tune_tree_grid_partial()
res_gb <- wf_gb %>% tune_tree_grid_partial()

save.image(file = 'env.RData')





