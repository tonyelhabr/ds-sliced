
library(tidyverse)
library(tidymodels)
library(stacks)
df <- 
  'c:/users/aelhabr/documents/projects/xengagement/inst/extdata/tweets_transformed.rds' %>% 
  read_rds() %>% 
  select(-text)

df_slim <-
  df %>%
  select(
    -c(idx, retweet_count, status_id, created_at, is_fresh),
    -matches('^wt|^team_|_[ah]$|_prnk$|_trans$')
  ) %>% 
  select(where( ~ !(assertthat::is.time(.x) | assertthat::is.date(.x)))) %>% 
  mutate(
    across(is_weekend, as.logical),
    across(season, factor)
  )
df_slim

if(FALSE) {
  cols <- df_slim %>% names()
  col_y <- 'favorite_count'
  cols_x <- cols %>% setdiff(col_y)
  cols_x
  cols_x_filt <- c('xg_w', 'xg_l', 'estimated_followers_count_w', 'estimated_followers_count_l', 'estimated_followers_count', 'xgd_w2l')
  all(cols_x_filt %in% cols_x)
  
  require(ggforce)
  df_slim %>% 
    mutate(across(favorite_count, list(grp = ~ggplot2::cut_number(.x, 3)))) %>% 
    ggplot() +
    aes(
      x = .panel_x,
      y = .panel_y,
      fill = favorite_count_grp,
      colour = favorite_count_grp
    ) +
    geom_point(
      size = 0.5,
      position = 'auto'
    ) +
    geom_autodensity(
      alpha = 0.3,
      colour = NA,
      position = 'identity'
    ) +
    geom_smooth(aes(colour = NULL, fill = NULL)) +
    guides(color = FALSE, fill = FALSE) +
    facet_matrix(
      vars(all_of(cols_x_filt)),
      layer.diag = 2,
      layer.continuous = TRUE,
      layer.mixed = -3
    )
  
  df_slim_nona <- df_slim %>% drop_na() %>% data.matrix() %>% apply(FUN = scale, MARGIN = 2) %>% as_tibble()
  fit_basic <- ranger::ranger(favorite_count ~ ., data = df_slim_nona)
  x <- df_slim_nona %>% select(-favorite_count) %>% data.matrix()
  f_predict <- function(object, newdata) {
    predict(object, newdata)$predictions
  }
  
  .inform <- function(x, ..., .envir = parent.frame()) {
    x <- glue::glue_collapse(x, '\n')
    x <- glue::glue(x, .envir = .envir)
    cli::cat_line(x)
  }
  
  timely <- function(f, ...) {
    
    function(...) {
      time_1 <- Sys.time()
      res <- f(...)
      time_2 <- Sys.time()
      dur <- (time_2 - time_1) %>% lubridate::as.duration()
      cat(dur, sep = '\n')
      invisible(res)
    }
  }
  
  f_explain <- timely(fastshap::explain)
  
  shap <- 
    f_explain(
      fit_basic,
      X = x,
      pred_wrapper = f_predict,
      nsim = 100
    )
  shap
  autoplot(shap)
  
  shap_agg <-
    shap %>% 
    as_tibble() %>% 
    mutate(idx = row_number()) %>% 
    pivot_longer(-idx) %>% 
    group_by(name) %>% 
    summarize(
      across(value, ~mean(abs(.x)))
    ) %>% 
    ungroup() %>% 
    arrange(desc(value))
  shap_agg
  
  autoplot(
    shap,
    type = 'dependence',
    feature = 'favorite_count_lag1_l',
    X = df_slim_nona
  ) +
    geom_smooth(se = FALSE)
}
# model ----
set.seed(42)

rec_init <-
  df_slim %>% 
  mutate(across(is_weekend, ~if_else(.x, 'yes', 'no') %>% factor())) %>% 
  recipe(formula(favorite_count ~ .), data = .) %>% 
  step_nzv(all_predictors()) %>% 
  step_corr(all_numeric_predictors()) %>% 
  step_YeoJohnson(matches('estimated_followers_count')) %>% 
  # step_lincomb(all_numeric_predictors()) %>% 
  step_impute_knn(all_predictors()) %>% 
  step_YeoJohnson(all_outcomes())
rec_init

# jui <-
#   rec_init %>% 
#   prep() %>% 
#   juice()
# jui

rec_rf <-
  rec_init %>% 
  step_dummy(all_nominal_predictors()) 

rec_knn <- 
  rec_init %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) 

jui_rf <-
  rec_rf %>% 
  prep() %>% 
  juice()
jui_rf

jui_knn <-
  rec_knn %>% 
  prep() %>% 
  juice()
jui_knn

spec_mars <-
  mars(num_terms = tune(), prune_method = tune()) %>% 
  set_mode('regression') %>% 
  set_engine('earth')

spec_knn <-
  nearest_neighbor(neighbors = tune(), weight_func = tune()) %>% 
  set_mode('regression') %>% 
  set_engine('kknn')

spec_rf <-
  rand_forest(mtry = tune(), min_n = tune()) %>% 
  set_mode('regression') %>% 
  set_engine('ranger')

wf_mars <-
  workflow() %>% 
  add_recipe(rec_knn) %>% 
  add_model(spec_mars)

wf_knn <-
  workflow() %>% 
  add_recipe(rec_knn) %>% 
  add_model(spec_knn)

wf_rf <-
  workflow() %>% 
  add_recipe(rec_rf) %>% 
  add_model(spec_rf)

ctrl <- control_grid(save_pred = TRUE, save_workflow = TRUE, verbose = TRUE)
folds <- df_slim %>% vfold_cv(v = 5)
mets <- metric_set(rmse, rsq, mape)

grid_params_mars <-
  grid_max_entropy(
    parameters(wf_mars),
    size = 10
  )
grid_params_mars

grid_params_knn <-
  grid_max_entropy(
    parameters(wf_knn),
    # finalize(neighbors(), wf_knn),
    # weight_func(),
    size = 10
  )
grid_params_knn

n_col <- jui_rf %>% ncol()
n_col

grid_params_rf <-
  grid_latin_hypercube(
    finalize(mtry(), jui_rf),
    min_n(),
    size = 10
  )
grid_params_rf

res_tune_mars <-
  tune_grid(
    wf_mars,
    grid = grid_params_mars,
    resamples = folds,
    control = ctrl,
    metrics = mets
  )
res_tune_knn

res_tune_knn <-
  tune_grid(
    wf_knn,
    grid = grid_params_knn,
    resamples = folds,
    control = ctrl,
    metrics = mets
  )
res_tune_knn

res_tune_rf <-
  tune_grid(
    wf_rf,
    grid = grid_params_rf,
    resamples = folds,
    control = ctrl,
    metrics = mets
  )
res_tune_rf

res_tune_knn %>% autoplot()
res_tune_rf %>% autoplot()

mets_rf <-
  res_tune_rf %>% 
  collect_metrics()
mets_rf

ens <-
  stacks() %>% 
  add_candidates(res_tune_knn) %>% 
  add_candidates(res_tune_rf)
ens

fit_ens <-
  ens %>% 
  blend_predictions() %>% 
  fit_members()
fit_ens

preds_ens <-
  fit_ens %>% 
  predict(new_data = df_slim)
preds_ens

preds_ens %>% 
  bind_cols(df_slim %>% select(favorite_count)) %>% 
  rmse(truth = favorite_count, estimate = .pred)
