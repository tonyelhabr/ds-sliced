

library(tidyverse)
library(tidymodels)
source('c:/users/aelhabr/desktop/r/xgb_helpers.R')
source('c:/users/aelhabr/desktop/r/utils.R')

col_y <- 'qb_dropback'
col_strata <- col_y
col_id <- 'idx'
col_wt <- 'wt'
cols_x <- c(sprintf('down%d', 1:4), 'ydstogo', 'yardline_100', 'score_differential', sprintf('qtr%d', 1:4), 'half_seconds_remaining', 'off_to', 'def_to')

nrounds <- 1000
booster <- 'gbtree'
objective <- 'binary:logistic'
eval_metrics <- 'logloss'
early_stopping_rounds <- 10
print_every_n <- 10
n_fold <- 10
seed <- 42

set.seed(seed)

seasons <- 2016:2019
pbp_init <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds')
    )
  )
})

data <- 
  pbp_init %>% 
  filter(
    season_type == 'REG' &
      down %in% c(1, 2, 3) &
      !is.na(qb_dropback) &
      !is.na(score_differential)
  ) %>%
  mutate(
    # qb_dropback = factor(qb_dropback),
    across(qb_dropback, as.integer),
    off_to = if_else(
      posteam_type == 'away',
      away_timeouts_remaining,
      home_timeouts_remaining
    ),
    def_to = if_else(
      posteam_type == 'away',
      home_timeouts_remaining,
      away_timeouts_remaining
    ),
    across(
      down,
      list(
        `1` = ~if_else(.x == 1L, 1L, 0L),
        `2` = ~if_else(.x == 2L, 1L, 0L),
        `3` = ~if_else(.x == 3L, 1L, 0L),
        `4` = ~if_else(.x == 4L, 1L, 0L)
      ),
      .names = '{col}{fn}'
    ),
    across(
      qtr,
      list(
        `1` = ~if_else(.x == 1L, 1L, 0L),
        `2` = ~if_else(.x == 2L, 1L, 0L),
        `3` = ~if_else(.x == 3L, 1L, 0L),
        `4` = ~if_else(.x >= 4L, 1L, 0L)
      ),
      .names = '{col}{fn}'
    ),
    idx = row_number(),
    wt = 1
  ) %>% 
  select(all_of(c(col_y, col_id, col_strata, col_wt, cols_x)))
data

x_mat <- 
  data %>% 
  select(one_of(cols_x)) %>% 
  df2mat()
y <- data[[col_y]]
wt <- data[[col_wt]]

x_dmat <-
  xgboost::xgb.DMatrix(
    x_mat,
    weight = wt,
    label = y
  )
x_dmat

set.seed(seed)
folds_ids <-
  caret::createFolds(
    data[[col_strata]],
    k = n_fold,
    list = FALSE,
    returnTrain = FALSE
  )
folds_ids

col_strata_sym <- col_strata %>% sym()
folds <-
  data %>%
  bind_cols(tibble(fold = folds_ids)) %>%
  left_join(
    data %>% select(!!col_strata_sym, idx),
    by = c('idx', col_strata)
  ) %>%
  select(fold, idx) %>%
  split(.$fold) %>%
  map(~select(.x, -fold) %>% pull(idx))

n_obs <- folds %>% flatten_int() %>% length()
max_idx <- folds %>% flatten_int() %>% max()
assertthat::assert_that(n_obs == max_idx)

n_row <- 10
grid_params <-
  grid_latin_hypercube(
    finalize(mtry(), data),
    min_n(),
    tree_depth(),
    learn_rate(),
    loss_reduction(),
    sample_size = sample_prop(),
    size = n_row
  ) %>%
  mutate(
    learn_rate = 0.1 * ((1:n_row) / n_row),
    mtry = mtry / ncol(data),
    idx = row_number()
  ) %>%
  relocate(idx)
grid_params

tune_xgb_cv_timed <- .time_it(tune_xgb_cv)
res_tune_cv <- 
  tune_xgb_cv_timed(
    nrounds = nrounds,
    grid_params = grid_params,
    folds = folds,
    x_dmat = x_dmat,
    booster = booster,
    objective = objective,
    eval_metrics = eval_metrics,
    # sample_weight = wt,
    early_stopping_rounds = early_stopping_rounds,
    print_every_n = print_every_n
  )
res_tune_cv

eval_metric <- eval_metrics[1]
eval_metric_tst <- sprintf('%s_tst', eval_metric)
eval_metric_tst_sym <- eval_metric_tst %>% sym()
res_cv_best <- res_tune_cv %>% dplyr::slice_min(!!eval_metric_tst_sym)
res_cv_best

.pluck_param <- function(x) {
  res_cv_best %>% purrr::pluck(x)
}

params_best <-
  list(
    booster = booster,
    objective = objective,
    eval_metric = eval_metrics,
    eta = .pluck_param('eta'),
    gamma = .pluck_param('gamma'),
    subsample = .pluck_param('subsample'),
    colsample_bytree = .pluck_param('colsample_bytree'),
    max_depth = .pluck_param('max_depth'),
    min_child_weight = .pluck_param('min_child_weight')
  )
params_best

nrounds_best <- round((.pluck_param('iter') / ((n_fold - 1) / (n_fold))), 0) + early_stopping_rounds

fit <-
  xgboost::xgboost(
    params = params_best,
    data = x_dmat,
    # data = x_mat,
    # label = y,
    # sample_weight = wt,
    nrounds = nrounds_best,
    early_stopping_rounds = early_stopping_rounds,
    print_every_n = print_every_n,
    verbose = 1
  )

preds <-
  fit %>%
  predict(x_mat) %>%
  augment_preds(
    data = data,
    cols_id = col_id,
    col_y = col_y
  )
preds

shap <-
  shap_xgb(
    x_mat = x_mat,
    fit = fit,
    preds = preds,
    cols_id = col_id,
    col_y = col_y
  )
shap


shap %>% 
tidyr::pivot_wider(
  names_from = stem,
  values_from = c(pred, count, sign, shap_value),
  names_glue = '{stem}_{.value}',
  values_fill = list(pred = 0, count = 0, sign = 'neutral', shap_value = 0)
) %>% 
  dplyr::filter(feature != 'baseline') %>% 
  dplyr::arrange(dplyr::all_of(cols_lst$cols_id), feature)

col_id_sym <- col_id %>% sym()
shap_long <-
  shap %>% 
  select(-.actual) %>% 
  pivot_longer(
    -c(!!col_id_sym, .pred),
    names_to = 'col',
    values_to = 'shap_value'
  )
shap_long

shap_agg <-
  shap_long %>% 
  group_by(col) %>% 
  summarize(across(shap_value, ~mean(abs(.x)))) %>% 
  mutate(
    across(shap_value, list(rnk = ~row_number(desc(.x))))
  ) %>% 
  arrange(shap_value_rnk)
shap_agg

viz_shap_agg <-
  shap_agg %>% 
  ggplot() +
  aes(y = col, x = shap_value) +
  geom_col() +
  theme(
    panel.grid.major.y = element_blank()
  ) +
  labs(
    y = NULL,
    x = 'mean(|SHAP value|)'
  )
viz_shap_agg


shap_vip <- 
  fit %>% 
  vip::vi(
    train = x_mat, 
    pred_wrapper = function(fit, newdata) {
      x_dmat <- xgboost::xgb.DMatrix(newdata)
      predict(fit, x_dmat)
    },
    # num_features = ncol(x_mat),
    method = 'shap'
  )
shap_vip

gb <- shap_vip %>% ggplot_build()
gb$data %>% 
  pluck(1)
