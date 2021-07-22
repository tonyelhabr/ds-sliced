
# setup ----
# https://github.com/dgrtwo/data-screencasts/blob/master/ml-practice/ep7.Rmd
library(tidyverse)
library(tidymodels)
library(tonythemes)
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
df %>% skimr::skim()

set.seed(42)
split <- df %>% initial_split(strata = attrition_flag)
df_trn <- split %>% training()
df_tst <- split %>% testing()
folds <- df_trn %>% vfold_cv()

# eda ----
df %>% filter(!complete.cases(.))
df %>% skimr::skim()

df_num <- df %>% select(where(is.numeric) | all_of(col_y))
df_num_long <-
  df_num %>% 
  pivot_longer(
    -attrition_flag
  )

# DataExplorer::create_report(df)
# require(ggforce)
# p <-
#   df_num %>% 
#   ggplot() +
#   aes(x = .panel_x, y = .panel_y, fill = attrition_flag, colour = attrition_flag) +
#   geom_point(shape = 16, size = 0.5, position = 'auto') + 
#   geom_autodensity(alpha = 0.3, colour = NA, position = 'identity') + 
#   geom_smooth(aes(colour = NULL, fill = NULL)) + 
#   facet_matrix(
#     vars(2:ncol(df_num)),
#     layer.diag = 2, layer.continuous = TRUE,
#     layer.mixed = -3, layer.discrete = -3
#   )
# p

# model ----
df %>% count(attrition_flag)
rec_cb <- 
  df_trn %>% 
  # select(where(is.numeric) | all_of(col_y)) %>% 
  recipe(formula(attrition_flag ~ .), data = .) %>% 
  # step_novel(all_nominal_predictors()) %>% 
  # update_role('id', new_role = 'id') %>% 
  themis::step_upsample(attrition_flag)
rec_cb

# rec <- rec_cb %>% step_dummy(all_nominal_predictors())

jp <- function(x, ...) x %>% prep() %>% juice(...)

# j <- rec %>% jp()
j_cb <- rec_cb %>% jp()
j_cb

mset <- metric_set(mn_log_loss)
ctrl <- control_grid(
  extract = extract_model,
  verbose = TRUE,
  save_pred = TRUE,
  save_workflow = TRUE
)

library(treesnip)
wf_cb <-
  rec_cb %>%
  workflow(
    boost_tree(
      'classification',
      # engine = 'catboost',
      trees = tune(),
      mtry = tune() # ,
      # learn_rate = 0.02
    ) %>%
      set_engine('catboost', nthread = 4)
  )

grid_cb <- crossing(
  trees = c(500, 750, 1000),
  mtry = seq.int(2, ncol(j_cb), by = 2)
)
grid_cb

# grid_cb <-
#   parameters(finalize(mtry(), df_trn), trees()) %>% 
#   grid_regular()
# grid_cb

tune_cb <-
  wf_cb %>% 
  tune_grid(
    folds,
    metrics = mset,
    control = ctrl,
    grid = grid_cb
  )
autoplot(tune_cb)

fit_full <-
  wf_cb %>% 
  finalize_workflow(select_best(tune_cb)) %>% 
  fit(df)
fit_full

imp <-
  extract_fit_engine(fit_full) %>% 
  catboost::catboost.get_feature_importance() %>% 
  as_tibble(rownames = 'feature') %>% 
  rename(imp = 2) %>% 
  arrange(desc(imp))
imp


probs <-
  fit_full %>% 
  predict(df_hold, type = 'prob') %>% 
  bind_cols(df_hold %>% select(id), .) %>% 
  rename(attrition_flag = .pred_0)
probs
probs %>% ggplot() + aes(x = .pred_1) + geom_density()
probs %>% count(attrition_flag)
path_export <- here::here(dir_proj, 'probs1.csv')
write_csv(probs, path_export)
shell(
  glue::glue('kaggle competitions submit -f {path_export} -m "m" sliced-s01e07-HmPsw2 -m "prob1"')
)

