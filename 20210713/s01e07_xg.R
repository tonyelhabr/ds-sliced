
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

df_redux %>% count(attrition_flag)
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
folds <- df_trn %>% vfold_cv()

df %>% count(attrition_flag)

rec <- 
  df_trn %>% 
  # select(where(is.numeric) | all_of(col_y)) %>% 
  recipe(formula(attrition_flag ~ .), data = .) %>% 
  # step_novel(all_nominal_predictors()) %>% 
  # update_role('id', new_role = 'id') %>% 
  step_rm(id) %>% 
  step_rm(total_trans_amt) %>% 
  step_dummy(gender) # %>% 
  # embed::step_umap(total_trans_ct, total_trans_amt, outcome = all_outcomes(), num_comp = 3) %>% 
  # step_pca(total_trans_ct, total_trans_amt, outcome = all_outcomes(), num_comp = 3) %>% 
  # themis::step_upsample(attrition_flag)
rec

# rec <- rec %>% step_dummy(all_nominal_predictors())
jp <- function(x, ...) x %>% prep() %>% juice(...)
j <- rec %>% jp()
j

mset <- metric_set(mn_log_loss)
ctrl <- control_grid(
  extract = extract_model,
  verbose = TRUE,
  save_pred = TRUE,
  save_workflow = TRUE
)

# library(treesnip)
# library(lightgbm)
wf <-
  rec %>%
  workflow(
    boost_tree(
      'classification',
      # engine = 'catboost',
      trees = tune(),
      mtry = tune(),
      min_n = tune(),
      learn_rate = 0.02
    ) # %>%
      # set_engine('lightgbm')
  )

n_col <- j %>% ncol()
grid <- crossing(
  mtry = seq(round(n_col / 3), n_col, by = 2),
  min_n = seq(2, 3, 4), # 11, by = 3),
  # trees = c(500, 1000)
  trees = c(1000, 1500, 200)
)

tune <-
  wf %>% 
  tune_grid(
    folds,
    metrics = mset,
    control = ctrl,
    grid = grid
  )
autoplot(tune)

fit_full <-
  wf %>% 
  finalize_workflow(select_best(tune)) %>% 
  fit(df_redux)
fit_full

fit_full %>% 
  extract_fit_engine() %>% 
  vip::vip()

probs <-
  fit_full %>% 
  predict(df_hold_redux, type = 'prob') %>% 
  bind_cols(df_hold_redux %>% select(id), .) %>% 
  rename(attrition_flag = .pred_1)
probs
probs %>% count(.pred_1 < 0.5)

fit_full %>% 
  predict(df_hold_redux) %>% 
  count(.pred_class)

probs %>% ggplot() + aes(x = .pred_0) + geom_density()
probs %>% count(attrition_flag)

path_export <- here::here(dir_proj, 'probs_xg_3.csv')
write_csv(probs %>% select(id, attrition_flag), path_export)
shell(
  glue::glue('kaggle competitions submit -f {path_export} -m "m" sliced-s01e07-HmPsw2 -m "probs_xg_3"')
)

