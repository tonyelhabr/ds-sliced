
library(tidyverse)
library(tidymodels)

dir_proj <- '20210608'
options(xgbh.dir_data = dir_proj)

tonythemes::heme_set_tony()
f_read <- function(x) {
  res <-
    here::here(dir_proj, sprintf('%s.csv', x)) %>%
    read_csv(guess_max = 20000)
  if(x == 'train') {
    res <- res %>% mutate(across(damaged, as.factor))
  }
  res
}
df <- f_read('train')
df_hold <- f_read('test')
df %>% skimr::skim()

seed <- 42
set.seed(seed)
split <- df %>% initial_split(strata = damaged)
df_trn <- split %>% training()
df_tst <- split %>% testing()

# library(themis)
rec <-
  recipe(damaged ~ ., df_trn) %>%
  step_rm(incident_day, incident_month, incident_year, operator, species_name, matches('engine.*position'), aircraft_model) %>%
  update_role(id, new_role = 'id') %>% 
  step_other(
    aircraft,
    aircraft_type,
    operator_id,
    aircraft_make,
    engine_model,
    engine_type,
    airport_id,
    airport,
    state,
    faa_region,
    flight_phase,
    visibility,
    precipitation,
    species_id,
    species_quantity,
    flight_impact
  )
rec

jui_trn <- rec %>% prep() %>% juice()
jui_tst <- rec %>% prep() %>% bake(df_tst)
jui_hold <- rec %>% prep() %>% bake(df_hold)

to_gb <- function(x) { x %>% mutate(across(damaged, ~as.integer(.x) - 1L))}

jui_trn_gb <- jui_trn %>% to_gb()
jui_tst_gb <- jui_tst %>% to_gb()

col_id <- 'id'
col_y <- 'damaged'

library(catboost)
set.seed(seed)
feats_trn <- jui_trn_gb %>% select(-any_of(c(col_id, col_y)))
labs_trn <- jui_trn_gb$damaged
pool_trn <- catboost.load_pool(
  data = feats_trn,
  label = labs_trn
)
pool_trn
feats_tst <- jui_tst_gb %>% select(-any_of(c(col_id, col_y)))
labs_tst <- jui_tst_gb$damaged
pool_tst <- catboost.load_pool(
  data = feats_tst,
  label = labs_tst
)
pool_tst

fit_trn <- catboost.train(
  pool_trn,
  pool_tst,
  params = list(
    loss_function = 'Logloss',
    iterations = 100
  )
)

# n_col <- feats_trn %>% ncol()
grid_params <-
  jui_trn %>%
  select(-any_of(c(col_id, col_y))) %>%
  xgbh::generate_grid_params(20) %>% 
  rename(
    rsm = mtry,
    # iterations = trees,
    min_data_in_leaf = min_n,
    depth = tree_depth,
    learning_rate = learn_rate,
    subsample = sample_size
  ) %>% 
  mutate(loss_function = 'Logloss', iterations = 1000)
grid_params

fit_trn <- catboost.cv(
  pool_trn,
  # params = grid_params %>% slice(1) %>% enframe(),
  params = list(
    loss_function = rep('Logloss', 3),
    iterations = rep (1000, 3),
    rsm = c(0.5, 0.7, 0.9)
  ),
  early_stopping_rounds = 10,
  fold_count = 5
)



# Stopped this after 7 runs since each iteration was taking ~5mins and the 7th was pretty solid.
grid_params %>% slice(7)
c(tune, fit) %<-%
  do_fit_timely(
    data = jui_trn_xgb,
    overwrite = FALSE,
    suffix = suffix,
    objective = 'binary:logistic',
    eval_metrics = list('logloss'),
    col_y = col_y,
    col_id = col_id,
    grid_params = grid_params,
    nrounds = 1000
  )
fit
