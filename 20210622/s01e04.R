
# https://github.com/dgrtwo/data-screencasts/blob/master/ml-practice/ep4.Rmd
library(tidyverse)
library(tidymodels)
library(tonythemes)
library(finetune)
library(xgboost)
dir_proj <- '20210622'

tonythemes::theme_set_tony()
col_y <- 'rain_tomorrow'
f_read <- function(x) {
  res <-
    here::here(dir_proj, sprintf('%s.csv', x)) %>%
    read_csv() # %>% 
    # mutate(
    #   across(
    #     rain_today,
    #     ~case_when(.x == 0 ~ FALSE, .x == 1 ~ TRUE, TRUE ~ NA)
    #   )
    # )
  if(any(colnames(res) == col_y)) {
    res <-
      res %>% 
      mutate(
        across(
          all_of(col_y),
          ~case_when(.x == 0 ~ 'no', .x == 1 ~ 'yes', TRUE ~ 'na') %>% 
            factor()
        )
      )
  }
}
df <- f_read('train')
df_hold <- f_read('test')
df %>% skimr::skim()

seed <- 42
set.seed(seed)
split <- df %>% initial_split(strata = all_of(col_y))
df_trn <- split %>% training()
df_tst <- split %>% testing()
folds <- df_trn %>% vfold_cv(v = 5)

ctrl_grid <- control_grid(
  save_pred = TRUE,
  save_workflow = TRUE,
  verbose = TRUE,
  extract = extract_model
)
ctrl_race <- control_bayes(
  save_pred = TRUE,
  save_workflow = TRUE
)
met_set <- metric_set(mn_log_loss)

predict_hold <- function(wf) {
  wf %>% 
    fit(df) %>% 
    augment(df_hold, type = 'prob') %>% 
    select(id, rain_tomorrow = .pred_yes)
}

compass_directions <- c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')
as_angle <- function(direction) {
  (match(direction, compass_directions) - 1) * 360 / length(compass_directions)
}

set.seed(seed)
df_trn_small <- df_trn %>% sample_n(4000) 
folds_small <- 
  df_trn_small %>%
  vfold_cv(v = 2)
df_trn_small

df %>% 
  count(location, sort = TRUE) %>% 
  mutate(frac = n / sum(n)) %>% 
  head(20)

rec <-
  df_trn %>% 
  recipe(formula(rain_tomorrow ~ .), data = .) %>% 
  step_rm(id, date, evaporation, sunshine) %>% 
  step_mutate_at(
    one_of(c('wind_gust_dir', 'wind_dir9am', 'wind_dir3pm')), fn = as_angle
  ) %>% 
  # step_other(location, threshold = 0.023) %>% 
  # step_mutate(
  #   location = ifelse(location %in% c('Canberra', 'Sydney', 'Ballarat'), location, 'other')
  # ) %>% 
  step_rm(location) %>% 
  step_impute_mean(all_numeric_predictors()) %>% 
  # step_novel(all_nominal_pred# ictors()) %>% 
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())
jui_trn <- rec %>% prep() %>% juice()
jui_trn

spec_xgb <- 
  boost_tree(
    mode = 'classification',
    learn_rate = 0.01,
    # mtry = tune(),
    trees = tune()
  ) %>% 
  set_engine('xgboost')
spec_xgb

set <-
  workflow_set(
    preproc = list(base = rec),
    models = list(xgb = spec_xgb)
  )
set

fit_xgb <-
  workflow() %>% 
  add_recipe(rec) %>% 
  add_model(spec_xgb) %>% 
  fit(df_trn)
fit_xgb
imp <- fit_xgb$fit$fit$fit %>% xgboost::xgb.importance(model = .)
imp
imp %>% xgboost::xgb.ggplot.importance()

path_res_grid <- here::here(dir_proj, 'res_grid.rds')
set
# doParallel::registerDoParallel(cores = 4)
res_grid <-
  set %>% 
  workflow_map(
    'tune_grid',
    seed = seed,
    # grid = 20,
    verbose = TRUE,
    resamples = folds_small,
    metrics = met_set,
    control = ctrl_grid
  )
res_grid$result[[1]]
res_grid
res_grid %>% autoplot()

write_rds(res_tune, path_res_grid)
res_grid

res_set <-
  set %>% 
  workflow_map(
    'tune_sim_anneal',
    # seed = seed,
    # grid = 10,
    verbose = TRUE,
    resamples = folds_small,
    metrics = met_set
  )
res_set$result[[1]]
res_set
res_set %>% autoplot()

res_grid <-
  res_set %>% 
  pull_workflow_set_result('base_xgb') 
res_grid

params_best <-
  res_grid %>% 
  select_best('mn_log_loss')
wf_best <-
  res_set %>%
  pull_workflow('base_xgb') %>% 
  finalize_workflow(params_best)
wf_best

fit_last <-
  wf_best %>% 
  last_fit(split, metrics = met_set)
fit_last %>% collect_metrics()

fit_best <- wf_best %>% fit(df_trn)
imp <- 
  fit_best$fit$fit$fit %>% 
  xgb.importance(model = .) %>% 
  as_tibble() %>% 
  janitor::clean_names()
imp
imp %>% 
  mutate(across(feature, ~fct_reorder(.x, gain))) %>% 
  ggplot() +
  aes(y = feature, x = gain) +
  geom_col()

augment.workflow <- function(x, newdata, ...) {
  predict(x, newdata, ...) %>%
    bind_cols(newdata)
}

probs_tst <-
  fit_best %>% 
  augment(df_tst, type = 'prob')
probs_tst
probs_tst %>% 
  mn_log_loss(rain_tomorrow, .pred_no)


