
# setup ----
# https://github.com/dgrtwo/data-screencasts/blob/master/ml-practice/ep7.Rmd
library(tidyverse)
library(tidymodels)
library(tonythemes)
# doParallel::registerDoParallel(cores = 4)
doParallel::stopImplicitCluster()
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
  res %>% prep_df()
}
df <- f_read('train')
df_hold <- f_read('test')

set.seed(2021)
spl <- initial_split(df)
df_trn <- training(spl)
df_tst <- testing(spl)

predict_on <- function(wf, df_trn, df_tst, ...) {
  wf %>% 
    fit(df_trn) %>% 
    augment(df_tst, type = 'prob') %>% 
    select(id, attrition_flag = .pred_yes)
}

set.seed(2021-07-13)
folds <- df2 %>% rsample::bootstraps(times = 10, strata = .pred_yes)
folds

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

jp <- function(x, ...) { rec %>% prep() %>% bake(x, ...) }
df %>% jp()
j_sup <- df %>% jp() %>% mutate(is_hold = 'no', idx = row_number())
j <- bind_rows(
  j_sup %>% select(-c(idx, churned)),
  df_hold %>% jp() %>% mutate(is_hold = 'yes')
) %>% 
  mutate(across(is_hold, factor))
j

rec_cls <- recipe(formula(is_hold ~ .), j)

wf_cls <-
  rec_cls %>% 
  workflow(
    rand_forest() %>% 
      set_engine('ranger', importance = 'permutation') %>% 
      set_mode('classification')
  )
wf_cls

fit_cls <- wf_cls %>% fit(j)
probs_cls <- fit_cls %>% augment(j, type = 'prob')
df2 <- probs_cls %>% filter(is_hold == 'no') %>% select(.pred_yes) %>% bind_cols(df)
df2

wf_cls %>% 
  fit(j) %>% 
  extract_model() %>% 
  vip::vip()

probs_cls %>% 
  select(matches('pred'), is_hold) %>% 
  # mutate(across(is_hold, ~factor(.x, levels = c('no', 'yes')))) %>% 
  roc_curve(is_hold, .pred_no) %>% 
  autoplot()

n_row <- probs_cls %>% nrow()
floor(0.8 * n_row)

idx_trn <-
  probs_cls %>% 
  filter(is_hold == 'no') %>% 
  relocate(idx, matches('.pred')) %>% 
  arrange(.pred_yes) %>% 
  slice(1:floor(0.8 * n_row)) %>% 
  pull(idx)
idx_trn

idx_tst <- j_sup %>% filter(!(idx %in% idx_trn)) %>% pull(idx)
idx_tst
idx_trn %>% sort()
df_trn <- df %>% mutate(idx = row_number()) %>% filter(idx %in% idx_trn) %>% select(-idx)
idx_tst %>% sort()

intersect(idx_trn, idx_tst)
split <-
  make_splits(
    list(analysis = idx_trn, assessment = idx_tst),
    df
  )
split
class(split)

metset <- metric_set(mn_log_loss)
ctrl <- control_grid(
  extract = extract_model,
  verbose = TRUE,
  save_pred = TRUE,
  save_workflow = TRUE
)

library(treesnip)
library(lightgbm)
wf_xg <- 
  rec %>% 
  workflow(
    boost_tree(
      trees = tune(),
      mtry = tune(),
      tree_depth = 5,
      learn_rate = tune()
    ) %>% 
      # set_engine('lightgbm') %>% 
      set_mode('classification')
  )

grid_xg <- crossing(
  trees = seq(400, 1300, 20),
  mtry = c(2, 3),
  learn_rate = c(.018, .02)
)
grid_xg
class(spl)
fold <- tibble(splits = list(split), id = 'Fold01')
class(fold)
class(fold) <- c('vfold_cv', 'rset', class(fold))
attr(fold, 'v') <- 1
attr(fold, 'repeats') <- 1
attr(fold, 'strata') <- FALSE
attr(fold, 'fingerprint') <- 'a1'
fold
class(fold)
attributes(folds)
attributes(fold)

# debugonce(data.frame)
tune_xg <-
  wf_xg %>% 
  tune_grid(
    # grid = grid_xg,
    control = ctrl,
    metrics = metset,
    resamples = fold
  )
tune_xg
tune_xg$.notes[[1]]
rlang::trace_back()
last_error()
rlang::last_trace()

tune_xg %>% autoplot()
tune_xg %>% collect_metrics() %>% arrange(desc(mean))
params_best_xg <- select_best(tune_xg)
params_best_xg
wf_best_xg <-
  wf_xg %>% 
  finalize_workflow(params_best_xg)
wf_best_xg

mets_best_xg <-
  wf_best_xg %>% 
  last_fit(spl, metrics = metset) %>% 
  collect_metrics()
mets_best_xg

probs_xg <- wf_best_xg %>% predict_on(df, df_hold)
probs_xg

probs_tst_xg <- wf_best_xg %>% predict_on(df_trn, df_tst)
probs_tst_xg

get_shap <- function(wf, df_trn, df_tst, col_y = 'churned') {
  probs <- wf %>% predict_on(df_trn, df_tst)

  # df_new <- features_init %>% select(-all_of(col_y))
  df_new <- rec %>% prep() %>% bake(df_tst) %>% select(-all_of(col_y))
  
  shap_init <-
    wf %>% 
    fit(df_trn) %>% 
    extract_model() %>% 
    predict(
      as.matrix(df_new),
      predcontrib = TRUE
    )
  
  shap <-
    shap_init %>% 
    dplyr::as_tibble() %>% 
    set_names(c(colnames(df_new), 'baseline'))
  shap
  
  shap <- shap %>% bind_cols(probs)
  shap
}
shap_tst <- wf_best_xg %>% get_shap(df_trn, df_tst)

# set.seed(42)
shap_long <-
  shap_tst %>% 
  # slice_sample(n = 1000) %>% 
  pivot_longer(-c(id, attrition_flag))
shap_long

shap_agg <-
  shap_long %>%
  group_by(name) %>%
  summarize(
    across(value, ~mean(abs(.x))),
  ) %>%
  ungroup() %>%
  mutate(
    rnk = row_number(desc(value)),
    across(name, ~fct_reorder(.x, -rnk))
  ) %>%
  arrange(rnk)
shap_agg

shap_agg %>% 
  filter(name != 'baseline') %>% 
  ggplot() +
  aes(x = value, y = name) +
  geom_col()

shap_long %>%
  filter(name != 'baseline') %>%
  left_join(shap_agg) %>% 
  mutate(across(name, ~fct_reorder(.x, -rnk))) %>% 
  ggplot() +
  aes(x = value, y = name) +
  ggbeeswarm::geom_quasirandom(
    shape = 20,
    aes(color = attrition_flag, size = 0.5 - abs(attrition_flag)),
    alpha = 0.1,
    # show.legend = FALSE,
    groupOnX = FALSE,
    varwidth = TRUE
  ) +
  ggplot2::scale_color_distiller(
    palette = 'RdBu'
  ) +
  theme_tony() +
  ggplot2::scale_size_area(max_size = 3) +
  theme(legend.position = 'top')
  

fit_xg %>% 
  extract_model() %>% 
  xgboost::xgb.importance(model = .)

grid_lin <- crossing(
  penalty = 10 ^ seq(-7, 0, 1),
  mixture = c(0, 0.5, 1)
)
grid_lin

wf_lin <- 
  rec %>% 
  workflow(
    logistic_reg(
      mixture = tune(),
      penalty = tune()
    ) %>% 
      set_engine('glmnet') %>% 
      set_mode('classification')
  )

tune_lin <-
  wf_lin %>% 
  tune_grid(
    control = ctrl,
    metrics = metset,
    resamples = folds
  )

fit_lin <-
  wf_lin %>% 
  finalize_workflow(select_best(tune_lin)) %>% 
  fit(df)
fit_lin

library(stacks)
stack <-
  stacks() %>% 
  add_candidates(tune_xg) %>% 
  add_candidates(tune_lin)
stack

set.seed(42)
blend <- stack %>% stacks::blend_predictions(metric = metset)
fit_ens <- blend %>% stacks::fit_members(df)
# beepr::beep(8)
path_fit_ens <- file.path(dir_proj, 'fit_ens_drob_v2.rds')
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

path_export <- here::here(dir_proj, 'probs_stack_drob_v2.csv')
path_export <- file.path(dir_proj, 'probs_lg')
write_csv(probs_xg, path_export)

shell(
  glue::glue('kaggle competitions submit -f {path_export} -m "m" sliced-s01e07-HmPsw2 -m "probs_lg"')
)

