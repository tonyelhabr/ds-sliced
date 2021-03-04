
library(tidyverse)
library(tidymodels)
library(stacks)

df <- AmesHousing::make_ames() %>% janitor::clean_names()

summ <- df %>% skimr::skim()
summ

col_y <- 'sale_price'
col_y_sym <- col_y %>% sym()
fmla <- formula(sprintf('%s ~ .', col_y))

set.seed(42)
split <- df %>% rsample::initial_split(stata = !!col_y_sym, prop = 0.8)
df_trn <- split %>% training()
df_tst <- split %>% testing()
folds <- df_trn %>% vfold_cv(v = 10)

f_ctrl <- function(suffix) {
  f <- sprintf('control_%s', suffix)
  exec(f, save_pred = TRUE, save_workflow = TRUE, verbose = TRUE)
}
ctrl_grid <- f_ctrl('grid')
ctrl_resamples <- f_ctrl('resamples')
met_set <- metric_set(rmse, mae, rsq)

rec_init <-
  recipe(fmla, data = df_trn) %>% 
  step_nzv(all_predictors()) %>% 
  step_corr(all_numeric_predictors()) %>% 
  step_lincomb(all_numeric_predictors()) %>% 
  step_other(all_nominal_predictors()) %>% 
  step_impute_mean(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_YeoJohnson(all_outcomes())

rec_lm <-
  rec_init %>% 
  step_bs(all_numeric_predictors())

fit_lm <- 
  linear_reg() %>% 
  set_engine('lm')

wf_lm <-
  workflow() %>% 
  add_recipe(rec_lm) %>% 
  add_model(fit_lm)

rec_lm %>% prep()
rec_lm %>% prep() %>% juice()
# Check it works just once
wf_lm %>% fit(df_trn)
# Check it works with lm
res_lm <-
  fit_resamples(
    wf_lm,
    resamples = folds,
    metrics = met_set,
    control = ctrl_resamples
  )

rec_glmnet <- rec_init
rec_knn <- rec_init
rec_svm <- rec_init

fit_glmnet <- 
  linear_reg(
    mixture = tune(),
    penalty = tune()
  ) %>% 
  set_mode('regression') %>% 
  set_engine('glmnet')

fit_knn <- 
  nearest_neighbor(
    neighbors = tune('k')
  ) %>%
  set_mode('regression') %>% 
  set_engine('kknn')

fit_svm <-
  svm_rbf(
    cost = tune('cost'), 
    rbf_sigma = tune('sigma')
  ) %>%
  set_mode('regression') %>% 
  set_engine('kernlab')

n_grid <- 5L
f_grid <- function(fit) {
  grid_latin_hypercube(
    fit %>% parameters(),
    size = n_grid
  )
}

grid_glmnet <- fit_glmnet %>% f_grid()
grid_knn <- fit_knn  %>% f_grid()
grid_svm <- fit_svm %>% f_grid()

wf_glmnet <-
  workflow() %>% 
  add_recipe(rec_glmnet) %>% 
  add_model(fit_glmnet)

wf_knn <-
  workflow() %>% 
  add_recipe(rec_knn) %>% 
  add_model(fit_knn)

wf_svm <-
  workflow() %>% 
  add_recipe(rec_svm) %>% 
  add_model(fit_svm)


tune_grid_partial <-
  partial(
    tune_grid,
    resamples = folds,
    metrics = met_set,
    control = ctrl_grid,
    ... =
  )

res_glmnet <- wf_glmnet %>% tune_grid_partial()
res_knn <- wf_knn %>% tune_grid_partial()
res_svm <- wf_svm %>% tune_grid_partial()

res_glmnet %>% autoplot(metric = 'rmse')

# save.image(file = 'env.RData')

f_stack <- function(x, res, nm = str_remove(deparse(substitute(res)), 'res_')) {
  x %>% 
    add_candidates(res, name = nm)
}

ens <-
  stacks() %>% 
  f_stack(res_lm) %>% 
  f_stack(res_glmnet) %>% 
  f_stack(res_knn) %>% 
  f_stack(res_svm)
ens

fit_ens <-
  ens %>% 
  blend_predictions() %>% 
  fit_members()
fit_ens

res_glmnet %>% 
  show_best('rmse') %>% 
  slice(1)


fit_ens %>% 
  predict(df_tst) %>% 
  bind_cols(df_tst %>% select(!!col_y_sym)) %>% 
  met_set(truth = !!col_y_sym, estimate = .pred) %>% 
  pviot_wider(names_from = .metric, values_from = .estimate)
