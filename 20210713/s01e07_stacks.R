
# setup ----
# https://github.com/dgrtwo/data-screencasts/blob/master/ml-practice/ep7.Rmd
library(tidyverse)
library(tidymodels)
library(tonythemes)
doParallel::registerDoParallel(cores = 4)
dir_proj <- '20210713'

tonythemes::theme_set_tony()
col_y <- 'attrition_flag'

f_read <- function(x) {
  res <-
    here::here(dir_proj, sprintf('%s.csv', x)) %>%
    read_csv()
  
  if(any(colnames(res) == col_y)) {
    res <-
      res %>% 
      mutate(across(all_of(col_y), factor))
  }
  res
}
df <- f_read('train')
df_hold <- f_read('test')

# eda ----
df %>% filter(!complete.cases(.))
df %>% skimr::skim()

# model ----
inc <-
  df %>% 
  count(income_category, sort = TRUE) %>% 
  mutate(rnk_ed = row_number(n))

ed <-
  df %>% 
  count(education_level, sort = TRUE) %>% 
  mutate(rnk_ed = row_number(n))
inc
ed

prep_df <- function(df) {
  df %>% 
    left_join(inc %>% select(-n)) %>% 
    left_join(ed %>% select(-n)) %>% 
    mutate(total_trans_avg = total_trans_amt / total_trans_ct) %>% 
    select(-c(income_category, education_level))
}


df_redux <- df %>% prep_df()
df_hold_redux <- df_hold %>% prep_df()
df %>% ggplot() + aes(x = credit_limit) + geom_density() + scale_x_log10()
df_redux %>% ggplot() + aes(x = total_trans_ct, y = total_trans_avg, color = attrition_flag) + geom_point(alpha = 0.5)

df %>% distinct(id)
df
set.seed(42)
split <- df_redux %>% initial_split(strata = attrition_flag)
df_trn <- split %>% training()
# set.seed(42)
# df_trn_small <- df_trn %>% sample_frac(0.1, weight = attrition_flag)
df_tst <- split %>% testing()
folds <- df %>% vfold_cv()

df %>% count(attrition_flag)

rec <- 
  df_trn %>% 
  # select(where(is.numeric) | all_of(col_y)) %>% 
  recipe(formula(attrition_flag ~ .), data = .) %>% 
  # step_novel(all_nominal_predictors()) %>% 
  # update_role('id', new_role = 'id') %>% 
  step_rm(id) %>% 
  step_rm(total_trans_amt) %>% 
  step_dummy(gender) %>% 
  # embed::step_umap(total_trans_ct, total_trans_amt, outcome = all_outcomes(), num_comp = 3) %>% 
  # step_pca(total_trans_ct, total_trans_amt, outcome = all_outcomes(), num_comp = 3) %>% 
  themis::step_upsample(attrition_flag)
rec

# rec <- rec %>% step_dummy(all_nominal_predictors())

jp <- function(x, ...) x %>% prep() %>% juice(...)

# j <- rec %>% jp()
j_trn <- rec %>% jp()
j_trn
# j_trn %>% relocate(last_col())
n_col <- j_trn %>% ncol()


spec_xg <- 
  boost_tree(
    trees = tune(),
    mtry = tune(),
    min_n = tune(),
    learn_rate = 0.02
  ) %>% 
  set_engine('xgboost') %>% 
  set_mode('classification')

spec_lin <- 
  logistic_reg(
    mixture = tune(),
    penalty = tune()
  ) %>% 
  set_engine('glmnet') %>% 
  set_mode('classification')

library(workflowsets)
sets <- 
  workflow_set(
    preproc = list(base = rec), 
    models = list(
      xg = spec_xg,
      lin = spec_lin
    )
  )
sets

grid_xg <- crossing(
  mtry = c(4, 5, 6),
  min_n = c(2, 3, 4),
  # trees = c(500, 1000)
  trees = c(500, 1000, 1500)
)
grid_xg

grid_lin <- crossing(
  penalty = 10 ^ seq(-7, 0, 1),
  mixture = c(0, 0.5, 1)
)
grid_lin

path_tune <- here::here(dir_proj, 'tune.rds')
do_run <- !file.exists(path_tune)

sets_updated <-
  sets %>% 
  option_add(grid = grid_xg, id = 'base_xg') %>% 
  option_add(grid = grid_lin, id = 'base_lin')
sets_updated$option[[1]]$grid

metset <- metric_set(mn_log_loss)
ctrl <- control_grid(
  extract = extract_model,
  verbose = TRUE,
  save_pred = TRUE,
  save_workflow = TRUE
)

tune <-
  sets_updated %>%
  workflow_map(
    seed = 42,
    resamples = folds,
    metrics = metset,
    control = ctrl,
    verbose = TRUE
  )
write_rds(tune, path_tune)
tune

tune <- read_rds(path_tune)
autoplot(tune)
autoplot(tune, id = 'base_xg')
autoplot(tune, id = 'base_lin')

path_fit_ens <- file.path(dir_proj, 'fit_ens.rds')

stack <-
  stacks::stacks() %>% 
  stacks::add_candidates(tune)
stack

set.seed(42)
blend <- stack %>% stacks::blend_predictions(metric = metset)
fit_ens <- blend %>% stacks::fit_members(df)
# beepr::beep(8)
write_rds(fit_ens, path_fit_ens)

fit_ens <- read_rds(path_fit_ens)
fit_ens

autoplot(fit_ens) # 'performance'
autoplot(fit_ens, 'weights')
autoplot(fit_ens, 'members')

df_join <- df %>% select(all_of(col_y))
probs <- 
  fit_ens %>% 
  predict(df_hold_redux, type = 'prob') %>% 
  bind_cols(df_hold_redux %>% select(id)) %>% 
  select(id, attrition_flag = .pred_1)
probs

path_export <- here::here(dir_proj, 'probs_stack1.csv')
write_csv(probs, path_export)

shell(
  glue::glue('kaggle competitions submit -f {path_export} -m "m" sliced-s01e07-HmPsw2 -m "probs_stack1"')
)

