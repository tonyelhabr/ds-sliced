
library(tonyverse) # aka tidyverse
library(tonymodels) # aka tidymodels
library(xgbh) # remotes::install_gtihub('tonyelhabr/xgbh')
library(tonythemes) # remotes::install_gtihub('tonyelhabr/xgbh')
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

df %>% 
  count(damaged) %>% 
  ggplot() + 
  aes(x = damaged, y = n) + 
  geom_col() + 
  labs(title = 'this is imbalanced af')

df %>%
  select(where(is.numeric)) %>%
  corrr::correlate() %>%
  corrr::rplot() +
  labs(
    title = 'engines are correlated, but who cares'
  )

df %>%
  select(where(is.numeric)) %>%
  corrr::correlate() %>%
  pivot_longer(
    -term
  ) %>%
  filter(term == 'damaged') %>%
  arrange(abs(desc(name)))

incidents <-
  df_trn %>%
  select(matches('^incident_'), damaged) %>%
  mutate(is_damaged = ifelse(damaged == '1', 1L, 0L))

incidents %>%
  group_by(incident_year) %>%
  summarize(n = sum(is_damaged == 1L), frac = n / n()) %>%
  ggplot() +
  aes(x = incident_year, y = frac) +
  geom_col() +
  labs(
    title = 'Pilots are getting better at their jobs?'
  )

incidents %>%
  filter(incident_year >= 2010) %>%
  mutate(date = sprintf('2031-%02d-01', incident_month) %>% lubridate::ymd()) %>%
  mutate(across(incident_year, factor)) %>%
  drop_na(date) %>%
  group_by(incident_year, date) %>%
  summarize(n = sum(is_damaged == 1L)) %>%
  ungroup() %>%
  ggplot() +
  aes(x = date, y = n) +
  geom_line(aes(color = incident_year), size = 3) +
  scale_x_date(date_labels = '%b %d') +
  labs(
    title = 'people be traveling around the holidays (nov and dec)'
  )

df_trn %>%
  count(precipitation, sort = TRUE) %>%
  drop_na(precipitation) %>%
  filter(precipitation != 'NONE') %>%
  mutate(
    across(precipitation, ~fct_reorder(.x, n))
  ) %>%
  ggplot() +
  aes(y = precipitation, x = n) +
  geom_col() +
  labs(
    title = 'basically every first drob plot',
    subtitle = 'dropping nones and missing'
  )

df_trn %>%
  count(precipitation, damaged, sort = TRUE) %>%
  group_by(precipitation) %>%
  mutate(frac = n / sum(n)) %>%
  ungroup() %>%
  filter(damaged == '1') %>%
  arrange(desc(frac))

seed <- 42
set.seed(seed)
split <- df %>% initial_split(strata = damaged)
df_trn <- split %>% training()
df_tst <- split %>% testing()

library(themis)
rec <-
  recipe(damaged ~ ., df_trn) %>%
  step_rm(incident_day, incident_month, incident_year, operator, species_name, matches('engine.*position'), aircraft_model) %>%
  update_role(id, new_role = 'id') %>%
  # step_other(aircraft_type) %>%
  # step_mutate(
  #   aircraft_type = ifelse(is.na(aircraft_type), 'C', aircraft_type)
  # ) %>%
  step_mutate(
    # aircraft_model = as.character(aircraft_model),
    aircraft = coalesce(aircraft, 'na'),
    aircraft_type = coalesce(aircraft_type, 'na'),
    operator_id = coalesce(operator_id, 'na'),
    aircraft_make = coalesce(aircraft_make, 'na'),
    engine_model = coalesce(engine_model, 'na'),
    engine_type = coalesce(engine_type, 'na'),
    airport_id = coalesce(airport_id, 'na'),
    airport = coalesce(airport, 'na'),
    state = coalesce(state, 'na'),
    faa_region = coalesce(faa_region, 'na'),
    flight_phase = coalesce(flight_phase, 'na'),
    visibility = coalesce(visibility, 'na'),
    precipitation = coalesce(precipitation, 'na'),
    species_id = coalesce(species_id, 'na'),
    species_quantity = coalesce(species_quantity, 'na'),
    flight_impact = coalesce(flight_impact, 'na')
  ) %>%
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
  ) %>%
  step_dummy(
    incident_year_bucket,
    incident_month_bucket,
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
  ) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_upsample(damaged)
jui_trn <- rec %>% prep() %>% juice()
jui_tst <- rec %>% prep() %>% bake(df_tst)
jui_hold <- rec %>% prep() %>% bake(df_hold)

to_xgb <- function(x) { x %>% mutate(across(damaged, ~as.integer(.x) - 1L))}

jui_trn_xgb <- jui_trn %>% to_xgb()
jui_tst_xgb <- jui_tst %>% to_xgb()
jui_hold_xgb <- jui_hold

col_id <- 'id'
col_y <- 'damaged'
do_fit_timely <- time_it(xgbh::do_fit)

set.seed(seed)
grid_params <-
  jui_trn %>%
  select(-any_of(c(col_id, col_y))) %>%
  xgbh::generate_grid_params(20)
grid_params
suffix <- 's01e02'

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

params <-
  list(
    booster = 'gbtree',
    objective = 'binary:logistic',
    eval_metrics = list('logloss'),
    # commented out values are the actuals from the hyperparam tuning
    eta = 0.02, # 0.035
    gamma = 0.201,
    subsample = 0.9, # 0.921
    colsample_bytree = 0.9, # 0.951
    max_depth = 10, # 13
    min_child_weight = 25 # 37
  )

fit <-
  xgboost::xgboost(
    params = params,
    data = x_dmat,
    label = y,
    nrounds = 5000,
    early_stopping_rounds = 10,
    print_every_n = 10
  )

x_mat <- jui_trn_xgb %>% select(-c(damaged, id)) %>% as.matrix()
x_mat
y <- jui_trn_xgb$damaged
x_dmat <-
  xgboost::xgb.DMatrix(
    x_mat,
    label = y
  )
probs_trn <- fit %>% predict(newdata = x_dmat)
MLmetrics::LogLoss(probs_trn, y_trn)

x_mat_tst <- jui_tst_xgb %>% select(-c(damaged, id)) %>% as.matrix()
y_tst <- jui_tst_xgb$damaged
x_dmat_tst <-
  xgboost::xgb.DMatrix(
    x_mat_tst,
    label = y_tst
  )
probs_tst <- fit %>% predict(newdata = x_dmat_tst)
MLmetrics::LogLoss(probs_tst, y_tst)


probs_tst %>%
  tibble(.prob = .) %>%
  bind_cols(jui_tst_xgb %>% select(id, damaged)) %>%
  select(.prob, damaged) %>%
  ggplot() +
  aes(y = damaged, x = .prob) +
  ggbeeswarm::geom_quasirandom(
    show.legend = FALSE,
    groupOnX = FALSE,
    alpha = 0.5
  ) +
  labs(
    title = 'Beeswarm >>>>>>>> violin plot',
    subtitle = 'test set'
  )


x_mat_hold <- jui_hold_xgb %>% select(-c(id)) %>% as.matrix()
x_dmat_hold <-
  xgboost::xgb.DMatrix(
    x_mat_hold
  )
probs_hold_before <- probs_hold # from a prior iteration
probs_hold <- fit %>% predict(newdata = x_dmat_hold)

bind_cols(
  tibble('after' = probs_hold),
  tibble('before' = probs_hold_before)
) %>%
  bind_cols(jui_hold_xgb %>% select(id)) %>%
  pivot_longer(-id) %>%
  ggplot() +
  geom_violin(aes(x = name, y = value)) +
  labs(title = 'before one last update')

probs_hold_export <-
  probs_hold %>%
  tibble(damaged = .) %>%
  bind_cols(jui_hold_xgb %>% select(id)) %>%
  select(id, damaged)
path_export <- here::here('s01e02_s2.csv')

probs_hold_export %>% write_csv(path_export)

shell(
  glue::glue('kaggle competitions submit -f {path_export} -m "sub2" sliced-s01e02-xunyc5')
)

shell('kaggle competitions leaderboard sliced-s01e02-xunyc5 -s')

# extra eda ----
# while waiting for the xgboost model to tune...
library(embed)
library(themis)
rec <-
  df %>%
  select(damaged, where(is.numeric)) %>%
  recipe(damaged ~ ., df) %>%
  update_role(id, new_role = 'id') %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_upsample(damaged)
rec

rec_u <-
  rec %>%
  step_umap(all_numeric_predictors(), num_comp = 2)

u <- rec_u %>% prep() %>% juice()
pal <- tonythemes::palette_sliced(5)[c(1, 5)]
u %>%
  ggplot() +
  aes(x = umap_1, y = umap_2) +
  geom_point(aes(color = damaged), size = 3) +
  scale_color_manual(values = pal) +
  labs(title = 'basically outer space')

