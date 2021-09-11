

library(tidyverse)
library(tidymodels)
library(tonythemes)
theme_set_tony()

dir_proj <- '20210622'

compass_directions <- c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')
as_angle <- function(direction) {
  (match(direction, compass_directions) - 1) * 360 / length(compass_directions)
}

f_read <- function(x) {
  file.path(dir_proj, sprintf('%s.csv', x)) %>%
    read_csv() %>% 
    mutate(
      week = date %>% lubridate::week(),
      across(c(wind_gust_dir, wind_dir9am, wind_dir3pm), as_angle)
    )
}

predict_on <- function(wf, df_trn, df_tst = df_trn) {
  wf %>% 
    fit(df_trn) %>% 
    augment(df_tst) %>% 
    select(id, rain_tomorrow = .pred_yes)
}

ctrl <- control_grid(verbose = TRUE)
metset <- metric_set(mn_log_loss)

df <- 
  f_read('train') %>% 
  mutate(across(rain_tomorrow, ~factor(ifelse(., 'yes', 'no'))))
df
df_hold <- f_read('test')
df_hold %>% skimr::skim()

set.seed(2021)
split <- df %>% initial_split(prop = .75)
df_trn <- split %>% training()
df_val <- split %>% testing()
folds <- df_trn %>% vfold_cv(v = 5)

rec <- 
  df_trn %>% 
  recipe(formula(rain_tomorrow ~ .), data = .) %>%
  step_rm(id, date, location, evaporation, sunshine) %>% 
  step_impute_mean(all_numeric_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())
jui_trn <- rec %>% prep() %>% juice()
jui_trn

wf_xg <- 
  rec %>% 
  workflow(
    boost_tree(
      trees = tune(),
      mtry = tune(),
      min_n = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      loss_reduction = tune(),
      sample_size = tune(),
      mode = 'classification'
    )
  )
wf_xg

# grid_xg <- crossing(
#   trees = seq(200, 2000, by = 200),
#   mtry = c(3, 5, 7, 9)
# )

set.seed(1337)
grid_xg_latin <-
  grid_latin_hypercube(
    trees(),
    finalize(mtry(), df_trn), 
    min_n(), 
    tree_depth(), 
    learn_rate(), 
    loss_reduction(), 
    sample_size = sample_prop(), 
    size = 50
  )
grid_xg_latin

doParallel::registerDoParallel(cores = 4)
path_tune_xg <- file.path(dir_proj, 'tune_xg.rds')
tune_xg <-
  wf_xg %>% 
  tune_grid(
    grid = grid_xg_latin,
    resamples = folds,
    metrics = metset,
    control = ctrl
  )
write_rds(tune_xg, path_tune_xg)

params_best_xg <- tune_xg %>% select_best('mn_log_loss')
wf_best_xg <- wf_xg %>% finalize_workflow(params_best_xg)
fit_val_xg <- wf_best_xg %>% fit(df_trn)
fit_best_xg <- wf_best_xg %>% fit(df)
probs_val_xg <- fit_val_xg %>% augment(df_val) 

# validation ----
df_full <- 
  bind_rows(
    df %>% mutate(is_hold = 'no'),
    df_hold %>% mutate(is_hold = 'yes')
  ) %>% 
  mutate(across(is_hold, factor))

rec_cls <- 
  df_full %>% 
  recipe(formula(is_hold ~ .), data = .) %>%
  step_rm(id, date, location, evaporation, sunshine) %>% 
  step_impute_mean(all_numeric_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

jui_cls <- rec_cls %>% prep() %>% juice()
jui_cls

wf_cls <-
  rec_cls %>% 
  workflow(
    rand_forest(mode = 'classification', engine = 'ranger', importance = 'impurity')
  )
wf_cls

set.seed(969)
split_full <- df_full %>% initial_split(strata = is_hold) 
df_trn_full <- split_full %>% training()
df_tst_full <- split_full %>% testing()
fit_cls <- wf_cls %>% fit(df_trn_full)
fit_cls
probs_tst_full <- fit_cls %>% augment(df_tst_full, type = 'prob')
probs_cls <- fit_cls %>% augment(df, type = 'prob')
probs_cls
vi_cls <-
  wf_cls %>% 
  fit(df_full) %>% 
  extract_model() %>% 
  vip::vip()
vi_cls

probs_tst_full %>% 
  select(matches('pred'), is_hold) %>% 
  roc_curve(is_hold, .pred_no) %>% 
  autoplot()

probs_tst_full %>% 
  mutate(rnk = row_number(desc(.pred_yes))) %>% 
  ggplot() +
  aes(x = rnk, y = .pred_yes, color = is_hold) +
  geom_point()


n_row <- probs_cls %>% nrow()

ids_adv_trn <-
  probs_cls %>% 
  relocate(id, matches('.pred')) %>% 
  arrange(desc(.pred_yes)) %>% 
  dplyr::slice(c(1:floor(0.8 * n_row))) %>% 
  pull(id)

df_adv_trn <- df %>% filter(id %in% ids_adv_trn)
df_adv_val <- df %>% filter(!(id %in% ids_adv_trn))

# fit_best_adv_xg <- wf_best_xg %>% fit(df_adv_trn)

# tune ----
set.seed(242)
folds <- df_trn %>% vfold_cv(v = 5) # %>% filter(id == 'Fold1')
folds

# fold <- folds %>% filter(id == 'Fold1')
# class(fold) <- c('vfold_cv', 'rset', class(fold))
# attr(fold, 'v') <- 1
# attr(fold, 'repeats') <- 1
# attr(fold, 'strata') <- FALSE
# attr(fold, 'row.names') <- 1L
# attr(fold, 'fingerprint') <- attr(folds, 'fingerprint')

# tune ----
path_tune_adv_xg <- file.path(dir_proj, 'tune_adv_xg.rds')
tune_adv_xg <-
  wf_xg %>% 
  tune_grid(
    grid = grid_xg_latin,
    resamples = folds,
    metrics = metset,
    control = ctrl
  )
write_rds(tune_adv_xg, path_tune_adv_xg)
tune_adv_xg %>% autoplot()

params_best_adv_xg <- tune_adv_xg %>% select_best('mn_log_loss')
wf_best_adv_xg <- wf_xg %>% finalize_workflow(params_best_adv_xg)
fit_val_adv_xg <- wf_best_adv_xg %>% fit(df_adv_trn)
probs_cls

fit_best_adv_xg <- wf_best_adv_xg %>% fit(df)
probs_val_adv_xg <- fit_val_adv_xg %>% augment(df_adv_val)
probs_val_adv_xg %>% metset(rain_tomorrow, .pred_no)
probs_val_xg %>% metset(rain_tomorrow, .pred_no)

probs_best_xg <- fit_best_xg %>% augment(df_hold) %>% select(id, .pred_no)
probs_best_adv_xg <- fit_best_adv_xg %>% augment(df_hold) %>% select(id, .pred_no)
path_export <- file.path(dir_proj, 'probs_.csv')
write_csv(preds, path_export)

shell(
  glue::glue('kaggle competitions submit -f {path_export} -m "m" sliced-s01e08')
)
