
# https://juliasilge.com/blog/nyc-airbnb/
library(tidyverse)
library(tidymodels)
library(tonythemes)
dir_proj <- '20210629'
# options(xgbh.dir_data = dir_proj)

tonythemes::theme_set_tony()
col_y <- 'price'
f_read <- function(x) {
  res <-
    here::here(dir_proj, sprintf('%s.csv', x)) %>%
    read_csv()
  
  if(any(colnames(res) == 'price')) {
    res <-
      res %>% 
      mutate(across(price, ~log(.x + 1)))
  }
  res
}
df <- f_read('train')
df_hold <- f_read('test')
df %>% skimr::skim()

set.seed(42)
split <- df %>% initial_split(strata = price)
df_trn <- split %>% training()
df_tst <- split %>% testing()
folds <- df_trn %>% vfold_cv(v = 5)

library(textrecipes)
rec <- 
  recipe(price ~ latitude + longitude + neighbourhood + room_type +
           minimum_nights + number_of_reviews + availability_365 + name, data = df_trn) %>% 
  step_novel(neighbourhood) %>% 
  step_other(neighbourhood, threshold = 0.01) %>% 
  step_tokenize(name) %>% 
  step_stopwords(name) %>% 
  step_tokenfilter(name, max_tokens = 30) %>% 
  step_tf(name)
rec
# jui_trn <- rec %>% prep() %>% juice()
jui_trn

library(baguette)
spec <-
  bag_tree(min_n = 10) %>% 
  set_engine('rpart', times = 25) %>% 
  set_mode('regression')

# library(treesnip)
# spec <-
#   boost_tree() %>% 
#   set_engine('catboost') %>% 
#   set_mode('regression')

wf <-
  workflow() %>% 
  add_recipe(rec) %>% 
  add_model(spec)


set.seed(42)
fit <- wf %>% fit(df_trn)
fit

library(rlang)

rmsle_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  rmsle_impl <- function(truth, estimate) {
    sqrt(mean((log(truth + 1) - log(estimate + 1))^2))
  }
  
  metric_vec_template(
    metric_impl = rmsle_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

rmsle <- function(data, ...) {
  UseMethod("rmsle")
}
rmsle <- new_numeric_metric(rmsle, direction = "minimize")

rmsle.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
  metric_summarizer(
    metric_nm = "rmsle",
    metric_fn = rmsle_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ...
  )
}


met_set <- metric_set(rmsle)
ctrl_grid <- control_grid(
  verbose = TRUE,
  # save_pred = TRUE,
  save_workflow = TRUE
)

set.seed(42)
res_tune <- fit_resamples(wf, metrics = met_set, control = ctrl_grid, resamples = folds)
mets <- res_tune %>% collect_metrics()
res_tune %>% autoplot()

fit_last <- res_tune %>% last_fit(split)

params_best <- res_tune %>% select_best()
wf_final <- res_tune %>% pull_workflow(params_best$.config)
fit_final <- wf_final %>% finalize(df)

# ----
sets <-
  workflow_set(
    preproc = list(simple = rec),
    models = list(bag = spec)
  )
sets

ctrl_set <- 
  control_grid(
    save_pred = TRUE,
    parallel_over = 'everything',
    save_workflow = TRUE
  )

seed <- 42
res_grid <-
  sets %>% 
  workflow_map(
    resamples = folds,
    metrics = met_set,
    control = ctrl_set,
    seed = 42,
    verbose = TRUE
  )
res_grid
res_grid %>% autoplot()

wflow_id_best <- 
  res_grid %>% 
  workflowsets::rank_results() %>% 
  slice_min(rank) %>% 
  pull(wflow_id)
wflow_id_best

wf_best <-
  res_grid %>% 
  pull_workflow_set_result(wflow_id_best) %>% 
  select_best(metric = 'rmsle')
wf_best

fit_best <-
  res_grid %>% 
  pull_workflow(wflow_id_best) %>% 
  finalize_workflow(wf_best) %>% 
  # last_fit(split = split)
  fit(df)
fit_best
fit_best$.workflow %>% pull_workflow_fit()

metrics_best <-
  fit_best %>% 
  collect_metrics()
metrics_best

preds_tst <- fit_best %>% augment(df_tst)
preds_tst %>% 
  rmsle(price, .pred)

preds_hold <-
  fit_best %>% 
  augment(df_hold) %>% 
  select(id, price = .pred)
preds_hold <- preds_hold %>% mutate(across(price, ~exp(.x) - 1))
preds_hold
path_export <- file.path(dir_proj, 'preds1.csv')
preds_hold %>% write_csv(path_export)

shell(
  glue::glue('kaggle competitions submit -f {path_export} -m "m" sliced-s01e05-WXx7h8')
)

shell('kaggle competitions leaderboard sliced-s01e05-WXx7h8 -s')
