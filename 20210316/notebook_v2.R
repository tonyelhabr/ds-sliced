
library(tidyverse)
library(tidymodels)
theme_set(theme_minimal(base_size = 14))
dir <- '20210316'
import_data <- function(file) {
  here::here(dir, sprintf('%s.csv', file)) %>% 
    read_csv() %>% 
    mutate(
      avg_peak_frac = str_remove(.data$avg_peak_perc, '[%]') %>% as.numeric(),
      avg_peak_frac = avg_peak_frac * 0.01
    ) %>% 
    select(-avg_peak_perc) %>% 
    group_by(gamename) %>% 
    arrange(yearmonth, .by_group = TRUE) %>%
    mutate(across(
      c(avg, peak, avg_peak_frac),
      list(
        lag1 = dplyr::lag,
        lag2 = ~ dplyr::lag(.x, 2),
        lag12 = ~ dplyr::lag(.x, 12)
      )
    )) %>%
    ungroup()
}

df_trn <- 'sliced_data' %>% import_data()
# df_trn <- df_trn %>% mutate(across(volatile, factor))
df_tst <- 'sliced_holdout_data' %>% import_data()
y_datadude <- 
  file.path(dir, 'predictions.csv') %>% 
  read_csv() %>% 
  select(-1) %>% 
  rename(pred = preds) %>% 
  mutate(across(pred, factor, levels = as.character(seq(-1, 1))))
y_datadude
df_actual <- tidytuesdayR::tt_load('2021-03-16') %>% pluck('games')
df_actual %>% nrow()
df_trn %>% nrow()
df_tst %>% nrow()

df_trn_fct <- df_trn %>% mutate(across(volatile, factor))
rec <-
  recipe(volatile ~ year + month + gain + peak + avg, data = df_trn_fct) %>% 
  step_impute_mean(all_numeric_predictors())
rec

jui <- rec %>% prep() %>% juice()

spec_rf <-
  # multinom_reg(mixture = 0) %>% 
  rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>% 
  set_mode('classification') %>% 
  set_engine('ranger')
spec_rf

spec_xgb <-
  # multinom_reg(mixture = 0) %>% 
  boost_tree() %>%  
  set_mode('classification') %>% 
  set_engine('xgboost')
spec_xgb

wf_rf <- 
  workflow() %>% 
  add_recipe(rec) %>% 
  add_model(spec_rf)
wf_rf

# fit_rf <- wf_rf %>% fit(df_trn_fct)
# fit_rf
# preds_rf <- fit_rf %>% predict(new_data = df_trn) %>% bind_cols(df_trn_fct %>% select(volatile))
# preds_rf %>% 
#   yardstick::accuracy(volatile, .pred_class)

set.seed(42)
folds <- df_trn_fct %>% group_vfold_cv('gamename', v = 10)
folds
n_col <- jui %>% ncol()
grid_params_rf <-
  grid_max_entropy(
    mtry(range = c(3L, n_col - 1L)),
    min_n(range = c(2L, 40L)),
    size = 3
  )
grid_params_rf

ctrl <- control_grid(verbose = TRUE)
mets <- metric_set(accuracy, roc_auc)

res_tune_rf <-
  tune_grid(
    wf_rf,
    grid = grid_params_rf,
    metrics = mets,
    resamples = folds,
    control = ctrl
  )
res_tune_rf
beepr::beep(3)

res_tune_rf %>% 
  autoplot()

mets_rf <-
  res_tune_rf %>% 
  collect_metrics()
mets_rf

params_best <- res_tune_rf %>% select_best('accuracy')
params_best

wf_rf_best <- wf_rf %>% finalize_workflow(params_best)
fit_rf_best <- wf_rf_best %>% fit(df_trn_fct)
fit_rf_best

preds_rf_best <-
  fit_rf_best %>% 
  predict(df_tst) %>% 
  bind_cols(y_datadude)
preds_rf_best

mets_rf_best <-
  preds_rf_best %>% 
  mutate(across(.pred_class, ~ifelse(.x == '-1', '1', '-1') %>% factor(levels = c('-1', '0', '1')))) %>% 
  accuracy(truth = pred, estimate = .pred_class)
mets_rf_best

# 82373+103+1258
# df_actual %>% 
#   semi_join(df_trn %>% filter(is.na(avg_peak_frac)) %>% select(gamename, year, month))
# df_trn %>% 
#   # select(volatile, gain) %>% 
#   select(where(is.numeric)) %>% 
#   corrr::correlate() %>% 
#   rename(col1 = rowname) %>% 
#   pivot_longer(
#     -col1
#   ) %>% 
#   filter(col1 == 'volatile')
# df_trn %>% 
#   filter(gamename %>% str_detect('Elder Scrolls')) %>% 
#   count(gamename)
#   
# df_actual$games %>% 
#   filter(gamename %>% str_detect('Skyrim')) %>% 
#   count(gamename)
