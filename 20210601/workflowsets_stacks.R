
library(tidyverse)
library(tidymodels)
library(tonythemes)
theme_set_tony()

dir_proj <- '20210601'
f_read <- function(x) {
  file.path(dir_proj, sprintf('%s.csv', x)) %>%
    read_csv(
      col_types = cols(
        .default = col_character(),
        game_id = col_double(),
        min_players = col_double(),
        max_players = col_double(),
        avg_time = col_double(),
        min_time = col_double(),
        max_time = col_double(),
        year = col_double(),
        # geek_rating = col_double(),
        num_votes = col_double(),
        age = col_double(),
        owned = col_double()
      )
    )
}

df_init <- f_read('train') %>% mutate(across(geek_rating, as.double))
df_hold_init <- f_read('test')
df_init %>% skimr::skim()
df_init %>%
  count(category1, sort = TRUE) %>%
  mutate(frac = n / sum(n)) %>%
  mutate(rnk = row_number(desc(frac))) %>%
  ggplot() +
  aes(x = rnk, y = frac) +
  geom_col()
nms_numeric <- df_init %>% select(where(is.numeric)) %>% names()
f_select <- function(data) { data %>% select(one_of(c(nms_numeric, 'category1', 'mechanic'))) }
df <- df_init %>% f_select()
df_hold <- df_hold_init %>% f_select()

col_y <- 'geek_rating'
col_y_sym <- col_y %>% sym()
col_id <- 'game_id'

library(textrecipes)
form <- formula(sprintf('%s ~ .', col_y))
seed <- 42
set.seed(seed)
split <- df %>% initial_split(strata = all_of(col_y))
df_trn <- split %>% training()
df_tst <- split %>% testing()

folds <- df_trn %>% vfold_cv(strata = all_of(col_y))
folds

rec <-
  df_trn %>%
  recipe(form, .) %>%
  step_other(category1, threshold = 0.04) %>%
  step_dummy(category1) %>%
  step_tokenize(mechanic, custom_token = function(x) strsplit(x, split = ', ')) %>%
  step_tokenfilter(mechanic, min_times = 10) %>%
  step_tf(mechanic) %>%
  step_pca(matches('^(category|tf_mechanic)'), num_comp = 10)
rec
jui_trn <- rec %>% prep() %>% juice()
jui_tst <- rec %>% prep() %>% bake(df_tst)
jui_hold <- rec %>% prep() %>% bake(df_hold)

# workflowsets ----
f_mode <- function(spec) {
  spec %>% 
    set_mode('regression')
}

library(workflowsets)
spec_xgb <-
  boost_tree(
    tree_depth = tune(),
    trees = 500,
    learn_rate = tune(),
    min_n = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    stop_iter = tune()
  ) %>%
  f_mode() %>% 
  set_engine('xgboost')
spec_xgb

spec_rf <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>% 
  f_mode() %>% 
  set_engine('ranger')
spec_rf

sets <- 
  workflow_set(
    preproc = list(base = rec), 
    models = list(
      rf = spec_rf,
      xgb = spec_xgb
    )
  )
sets

path_res_grid <- here::here(dir_proj, 'res_grid.rds')
do_run <- !file.exists(path_res_grid)

met_set <- metric_set(rmse)

f_tune <- function() {
  doParallel::registerDoParallel(cores = 4)
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
      grid = 50,
      metrics = met_set,
      control = ctrl_grid,
      verbose = TRUE
    )
  write_rds(res_grid, path_res_grid)
  res_grid
  # doParallel::stopImplicitCluster()
}

if(do_run) { f_tune() }

res_grid <- read_rds(path_res_grid)
autoplot(res_grid)

n_model <-
  res_grid %>% 
  collect_metrics(summarize = FALSE) %>% 
  nrow()
n_model

res_ranks <-
  res_grid %>% 
  # ranks based on mn_log_loss since that was the only metric i used.
  rank_results() %>% 
  select(wflow_id, model, .config, .metric, mean, rank) %>% 
  group_by(wflow_id, .metric) %>% 
  slice_min(rank, with_ties = FALSE) %>% 
  ungroup() %>% 
  arrange(rank)
res_ranks

wflow_id_best <- 
  res_ranks %>% 
  slice_min(rank, with_ties = FALSE) %>% 
  pull(wflow_id)

wf_best <-
  res_grid %>% 
  pull_workflow_set_result(wflow_id_best) %>% 
  select_best(metric = 'rmse')

fit_best <-
  res_grid %>% 
  pull_workflow(wflow_id_best) %>% 
  finalize_workflow(wf_best) %>% 
  last_fit(split)
fit_best

metrics_best <-
  fit_best %>% 
  collect_metrics()
metrics_best

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
  beepr::beep(8)
  write_rds(fit_ens, path_fit_ens)
}

if(do_run_ens) { f_ens() }

fit_ens <- read_rds(path_fit_ens)
fit_ens

# stacks::autoplot
autoplot(fit_ens) # 'performance'
autoplot(fit_ens, 'weights')
autoplot(fit_ens, 'members')

do_prob_eval <- function(df) {
  df_join <- df %>% select(all_of(col_y))

  preds <- 
    fit_ens %>% 
    predict(df) %>% 
    bind_cols(df_join)
  
  metrics <-
    preds %>% 
    rmse(!!col_y_sym, estimate = .pred)
  
  list(preds = preds, metrics = metrics)
}

library(zeallot)
c(preds_trn, metrics_trn) %<-% do_prob_eval(df_trn)
c(preds_tst, metrics_tst) %<-% do_prob_eval(df_tst)

metrics_trn
metrics_tst

preds_hold <- 
  fit_ens %>% 
  predict(df_hold) %>% 
  bind_cols(df_hold) %>% 
  select(game_id, geek_rating = .pred)
preds_hold

path_export <- here::here(dir_proj, 'preds_s01e01_stacks.csv')
preds_hold %>% write_csv(path_export)

shell(
  glue::glue('kaggle competitions submit -f {path_export} -m "workflowsets + stacks" sliced-s01e01')
)
