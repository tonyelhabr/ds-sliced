
# https://github.com/jthomasmock/espnscrapeR/blob/master/R/get_538_elo.R
# change oak to lv
all_teams <- c(
  'ari', 'atl', 'bal', 'buf', 'car', 'chi', 'cin', 'cle', 'dal', 'den', 'det',
  'gb', 'hou', 'ind', 'jax', 'kc', 'oak', 'lac', 'lar', 'mia', 'min', 'ne',
  'no', 'nyg', 'nyj', 'phi', 'pit', 'sf', 'sea', 'tb', 'ten', 'wsh'
)

f_safe <- safely(espnscrapeR::get_538_elo_historical)
elo_hist_init <-
  all_teams %>% 
  map(f_safe)

elo_hist <-
  elo_hist_init %>% 
  map_dfr(~pluck(.x, 'result'))
elo_hist

tms <- elo_hist %>% distinct(team) %>% pull(team)
tms %>% length()
opps <- elo_hist %>% filter(year == 2020) %>% distinct(opp) %>% pull(opp)
setdiff(all_teams, tms)
setdiff(tms, all_teams)
setdiff(all_teams, opps)
setdiff(opps, all_teams)

df <-
  elo_hist %>% 
  drop_na() %>% 
  mutate(across(year, as.integer)) %>%
  filter(year >= 2010, year < 2021) %>% 
  group_by(team, year) %>% 
  mutate(month_idx = row_number(week_num)) %>% 
  ungroup() %>% 
  filter(month_idx <= 12) %>% 
  # mutate(month = date %>% lubridate::month()) %>% 
  mutate(date = sprintf('%04d-%02d-01', year, month_idx) %>% lubridate::ymd()) %>% 
  select(team, date, year, month_idx, elo, pts_for, pts_against) %>%
  mutate(
    # across(year, factor),
    pts_diff = pts_for - pts_against,
    month = month.abb[month_idx] %>% factor(ordered = FALSE),
    across(elo, list(lag1 = dplyr::lag))
  )
df

df_trn <- df %>% filter(year <= 2019)
df_tst <- df %>% filter(year > 2019)

rec <-
  df_trn %>% 
  recipe(elo ~ date + pts_for + pts_diff + elo_lag1, data = df_trn) %>% 
  # step_rm(month) %>% 
  # step_string2factor(team) %>% 
  step_impute_mean(elo_lag1) %>% 
  step_date(date, features = 'month', ordinal = TRUE) %>% 
  step_rm(date)
rec

jui <- rec %>% prep() %>% juice()
jui

spec <-
  mars(num_terms = tune(), prod_degree = tune(), prune_method = tune()) %>% 
  set_mode('regression') %>% 
  set_engine('earth')

wf <-
  workflow() %>% 
  add_recipe(rec) %>% 
  add_model(spec)

folds <- df_trn %>% group_vfold_cv(group = 'team', strata = elo)
folds

grid_params <-
  grid_latin_hypercube(
    parameters(wf),
    size = 10
  )
grid_params
ctrl <- control_grid(save_pred = TRUE, verbose = TRUE)

res_tune <-
  tune_grid(
    wf,
    grid = grid_params,
    resamples = folds,
    control = ctrl
  )
res_tune
params_best <- res_tune %>% select_best('rmse')
wf_best <- wf %>% finalize_workflow(params_best)
wf_fit <- wf_best %>% fit(data = df_trn)
fit_best <- wf_fit %>% pull_workflow_fit()

fit_best %>% 
  predict(new_data = rec %>% prep() %>% bake(df_tst)) %>% 
  bind_cols(df_tst) %>% 
  rmse(elo, .pred)


trn <-
  df_trn %>% 
  filter(team == 'ari')
tst <-
  df_tst %>% 
  filter(team == 'ari')

fit_aa <- 
  arima_reg() %>% 
  set_engine(engine = 'auto_arima') %>% 
  fit(
    elo ~ date, data = trn
  )
fit_aa

fit_ets <- 
  exp_smoothing() %>% 
  set_engine(engine = 'ets') %>% 
  fit(
    elo ~ date, data = trn
  )
fit_ets

fit_proph <- 
  prophet_reg() %>% 
  set_engine(engine = 'prophet') %>% 
  fit(
    elo ~ date, data = trn
  )
fit_proph

fit_aa_boost <-
  arima_boost(
    min_n = 2,
    learn_rate = 0.015
  ) %>%
  set_engine(engine = 'auto_arima_xgboost') %>%
  fit(
    elo ~ as.numeric(date) + month + pts_for + pts_against, 
    data = trn
  )
fit_aa_boost

fit_lm <-
  linear_reg() %>% 
  set_engine('lm') %>% 
  fit(
    elo ~ as.numeric(date) + month,
    data = trn
  )

tbl <- 
  modeltime_table(
    fit_aa,
    fit_ets,
    fit_proph,
    fit_aa_boost
  )
tbl

calib <-
  tbl %>% 
  modeltime_calibrate(new_data = tst, quiet = FALSE)
calib

acc <-
  calib %>% 
  modeltime_accuracy()
acc

best_model <- acc %>% slice_min(rmse)
best_model
fit_best <- tbl %>% modeltime::pull_modeltime_model(best_model$.model_id)
fit_best

acc %>% table_modeltime_accuracy()

res <-
  tbl %>% 
  modeltime_forecast(new_data = tst, actual_data = tst)
res
res %>% plot_modeltime_forecast()

do_fit <- function(data) {
  
  fit_aa_boost <-
    arima_boost(
      min_n = 2,
      learn_rate = 0.015
    ) %>%
    set_engine(engine = 'aa_xgb') %>%
    fit(
      elo ~ date + factor(year) + factor(week), ordered = F),
      data = data
    )
}

df %>% 
  group_nest(team)
