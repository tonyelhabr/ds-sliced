
library(tidyverse)
library(tidymodels)
library(tonythemes)
theme_set_tony()
dir_proj <- '20210810/v2'
f_read <- function(x) {
  file.path(dir_proj, sprintf('%s.csv', x)) %>%
    read_csv()
}

predict_on <- function(wf, df_trn, df_tst = df_trn) {
  # wf <- wf_lin
  probs <-
    wf %>% 
    fit(df_trn) %>% 
    augment(df_tst, type = 'prob') %>% 
    # mutate(across(.pred, exp)) %>% 
    select(uid, matches('^[.]pred_[0-9]'))
  probs <- probs %>% rename_all(~str_remove(.x,  '^[.]pred_'))
  write_csv(probs, file.path(dir_proj, 'multi_s1.csv'), na = '')
}

df <- f_read('train')
df_hold <- f_read('test')
df %>% skimr::skim()
df %>% count(priceRange)


tx <- ggplot2::map_data('county', regions = 'texas') %>% filter(subregion == 'travis')
tx

df %>% 
  ggplot() +
  geom_polygon(data = tx, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
  # aes(x = latitude, y = longitude, z = log(priceRange)) +
  geom_point(aes(x = longitude, y = latitude, color = priceRange), alpha = 0.8) +
  theme(panel.grid.major = element_blank(), axis.text = element_blank()) +
  labs(x = NULL, y = NULL)
df


df %>% 
  filter(yearBuilt >= 2015) %>% 
  ggplot() +
  geom_polygon(data = tx, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
  # aes(x = latitude, y = longitude, z = log(priceRange)) +
  geom_point(aes(x = longitude, y = latitude, color = factor(yearBuilt), size = priceRange), alpha = 0.8) +
  theme(panel.grid.major = element_blank(), axis.text = element_blank()) +
  labs(x = NULL, y = NULL)
df

df_full <-
  bind_rows(
    df %>% mutate(is_hold = 'no'),
    df_hold %>% mutate(is_hold = 'yes')
  ) %>% 
  mutate(across(is_hold, factor))

df_full_num <- 
  df_full %>% 
  select(is_hold, where(is.numeric))
df_full_num

cors_full <-
  df_full_num %>%
  # mutate(across(is_hold, as.integer)) %>% 
  group_nest(is_hold) %>% 
  mutate(
    data = map(data, corrr::correlate, quiet = TRUE)
  ) %>% 
  unnest(data) %>% 
  pivot_longer(-c(is_hold, term)) %>% 
  rename(col1 = term, col2 = name, cor = value) %>% 
  filter(col1 != col2) %>% 
  arrange(desc(abs(cor)))
cors_full      

p_cors_full <-
  cors_full %>% 
  pivot_wider(
    names_from = is_hold,
    values_from = cor
  ) %>% 
  filter(col1 != 'priceRange') %>% 
  filter(col2 != 'priceRange') %>% 
  filter(col1 < col2) %>% 
  mutate(diff = yes - no, rnk = row_number(desc(diff))) %>%
  arrange(desc(abs(diff))) %>% 
  mutate(col12 = sprintf('%s + %s', col1, col2) %>% fct_reorder(rnk)) %>% 
  head(20) %>% 
  ggplot() +
  aes(x = diff, y = col12, fill = factor(sign(diff))) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(labels = scales::percent) +
  labs(
    title = 'Top 20 Differences in Pairwise Feature Correlations between train and test CSVs'
  )
ggsave(p_cors_full, filename = file.path(dir_proj, 'cor_diffs_full.png'), width = 12, height = 8, type = 'cairo')

df_num <- df %>% select(priceRange, where(is.numeric)) %>% select(-uid)
df_num

cors <-
  df_num %>%
  mutate(across(priceRange, ~factor(.x) %>% as.integer())) %>% 
  corrr::correlate(quiet = TRUE) %>% 
  pivot_longer(-term) %>% 
  rename(col1 = term, col2 = name, cor = value) %>% 
  filter(col1 != col2) %>% 
  arrange(desc(abs(cor)))
cors

cors_filt <-
  cors %>% 
  filter(col1 == 'priceRange') %>% 
  filter(col2 != 'priceRange')
cors_filt

p_num <-
  df_num %>% 
  pivot_longer(-priceRange) %>% 
  group_by(name) %>% 
  mutate(across(value, percent_rank)) %>% 
  ungroup() %>% 
  ggplot() +
  aes(x = value, fill = priceRange) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(labels = percent) +
  facet_wrap(~name, scales = 'free_y') +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = 'top'
  ) +
  labs(
    title = 'How are the distributions of numeric features different, by priceRange?',
    y = 'Density',
    x = 'Percentile'
  )
p_num
ggsave(p_num, filename = file.path(dir_proj, 'num_diffs.png'), width = 12, height = 8, type = 'cairo')

set.seed(42)
split <- df %>% initial_split(strata = priceRange)
df_trn <- split %>% training()
df_tst <- split %>% testing()
folds <- df_trn %>% vfold_cv(strata = priceRange, v = 10) #  group_vfold_cv(group = id_artists, v = 5)
# df %>% select(duration_ms) %>% arrange(desc(duration_ms)) %>% ggplot(aes(duration_ms)) + geom_histogram()
# df %>% select(duration_ms) %>% summarize(p95 = quantile(duration_ms, 0.8))
metset <- metric_set(mn_log_loss)
library(textrecipes)

rec <- 
  recipe(priceRange ~ latitude + longitude + yearBuilt + numOfBedrooms + avgSchoolRating, df_trn) %>% 
  step_interact(~ latitude:longitude) %>%
  # step_interact(~ numOfBathrooms:numOfBedrooms) %>%
  step_nzv(all_numeric_predictors()) %>% 
  # 20 is best according to tune
  step_ns(longitude, latitude, matches('latitude_x_longitude'), deg_free = 20)
rec

jp <- function(x, ...) { rec2 %>% prep() %>% bake(x, ...) }
# j_trn <- df_trn %>% jp()
# j_trn %>% skimr::skim()
# df_tst %>% jp()
# df_hold %>% jp()

ctrl <- control_grid(
  verbose = TRUE,
  # save_workflow = TRUE,
  # save_pred = TRUE,
  extract = extract_model
)

wf_lin <- rec %>% workflow(rand_forest(trees = 1000) %>% set_engine('ranger', importance = 'impurity') %>% set_mode('classification'))
wf_lin
fit_trn <- wf_lin %>% fit(df_trn)
fit_trn %>% predict(df_tst, type = 'prob')
probs <- wf_lin %>% predict_on(df, df_hold)

preds_tst <- fit_trn %>% augment(df_tst) # %>% mutate(across(.pred, exp))

preds_tst %>% 
  select(priceRange, .pred_class) %>% 
  mutate(across(priceRange, factor)) %>% 
  accuracy(priceRange, .pred_class)

preds_tst %>% 
  select(priceRange, .pred_class, matches('^[.]pred_[0-9]')) %>% 
   #select(.pred_)
  mutate(across(priceRange, factor)) %>% 
  metset(priceRange, matches('^[.]pred_[0-9]'))

df %>% 
  filter(yearBuilt == 2019) %>% 
  filter(latitude == max(lat))
select(description) %>% 
  filter(description %>% str_detect('flug'))

full_join(
  preds_tst %>% select(zpid, .pred1 = .pred),
  preds_tst_lg %>% select(zpid, .pred2 = priceRange)
) %>% 
  ggplot() +
  aes(x = .pred1, .pred2) +
  geom_point() +
  xlim(100000, 3000000) +
  ylim(100000, 3000000)

grid_lin <- tibble(deg_free = c(12, 16, 20, 24))
tune_lin <-
  wf_lin %>% 
  tune_grid(
    metrics = metset,
    resamples = folds,
    control = ctrl,
    grid = grid_lin
  )
tune_lin %>% autoplot()

# library(treesnip)
wf <- rec %>% workflow(boost_tree(learn_rate = 0.02) %>% set_engine('xgboost') %>% set_mode('regression'))
fit <- wf %>% fit(df_trn)
preds <- fit %>% augment(df_tst) # predict_on(df_tst)
preds %>% ggplot() + aes(x = priceRange, y = .pred) + geom_point() + ylim(0, 3*10^5)

wf_lg <-
  rec2 %>% 
  workflow(
    boost_tree(
      mtry = tune(),
      trees = tune(),
      # min_n = tune(),
      learn_rate = tune()
    ) %>% 
      set_engine('xgboost') %>% 
      set_mode('regression')
  )

n_col_trn <- df_trn %>% jp() %>% ncol()
n_col_trn

grid_lg <- crossing(
  # min_n = c(2, 4, 6, 8, 10),
  mtry = c(2, 4, 6, 8),
  trees = c(1000, 1250, 1500),
  learn_rate = c(0.015, 0.02)
)

# path_tune_lg <- file.path(dir_proj, 'tune_lg.rds')


doParallel::registerDoParallel(cores = 4)
tune_lg <-
  wf_lg %>% 
  tune_grid(
    metrics = metset,
    resamples = folds,
    control = ctrl,
    grid = grid_lg
  )
# write_rds(tune_lg, path_tune_lg)
autoplot(tune_lg)

params_best_lg <- tune_lg %>% select_best('rmse')
wf_best_lg <-
  wf_lg %>% 
  finalize_workflow(params_best_lg)
wf_best_lg

do_shap <- function(wf, df_trn, df_tst = df_trn) {
  # wf <- wf_best_lg
  preds <- wf %>% predict_on(df_trn, df_tst)
  
  df_new <- 
    rec %>% 
    prep() %>% 
    bake(df_tst) %>% 
    select(-priceRange)
  df_new
  # damn this gonna take a while
  fit_shap <-
    wf %>% 
    fit(df_trn) %>% 
    extract_model()
  
  shap_init <-
    fit_shap %>% 
    predict(
      as.matrix(df_new),
      predcontrib = TRUE
    )
  
  shap <-
    shap_init %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    rename(baseline = BIAS)
  
  res <- shap %>% bind_cols(preds)
  # shap <- res
  res
}

shap <- wf_best_lg %>% do_shap(df_trn, df_tst)
shap_long <- shap %>% pivot_longer(-c(zpid, priceRange))

shap_agg <-
  shap_long %>% 
  group_by(name) %>%
  summarize(across(value, ~mean(abs(.x), na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(rnk = row_number(desc(value))) %>% 
  mutate(across(name, ~fct_reorder(.x, -rnk))) %>% 
  ungroup()
shap_agg

p_shap_agg <-
  shap_agg %>% 
  filter(name != 'baseline') %>% 
  ggplot() +
  aes(x = value, y = name) +
  geom_col() +
  labs(
    title = 'da shap',
    x = 'mean(abs(SHAP))'
  )
p_shap_agg

set.seed(1337)
shap_long_filt <- shap %>% sample_n(1000) %>% pivot_longer(-c(zpid, priceRange))
p_swarm <-
  shap_long_filt %>% 
  filter(name != 'baseline') %>% 
  left_join(shap_agg %>% select(-value)) %>% 
  mutate(across(name, ~fct_reorder(.x, -rnk))) %>% 
  ggplot() +
  aes(x = value, y = name) +
  ggbeeswarm::geom_quasirandom(
    aes(colour = priceRange),
    groupOnX = FALSE,
    shape = 20
  ) +
  # scale_color_distiller(palette = 'RdBu') +
  scale_size_area(max_size = 3) +
  theme(legend.position = 'none') +
  labs(
    title = 'lat and lon is all you need'
  )
p_swarm

fit_best_lg <- wf_best_lg %>% fit(df)

preds_tst_lg <- wf_best_lg %>% predict_on(df_trn, df_tst)
preds_tst_lg %>% 
  left_join(df_tst %>% select(zpid, actual = priceRange)) %>% # mutate(across(actual, exp))) %>% 
  metset(actual, priceRange)

preds_tst_lg %>% 
  left_join(df_tst %>% select(zpid, actual = priceRange)) %>% 
  ggplot() +
  aes(x = priceRange, y = actual) +
  geom_point() +
  labs(
    title = 'actual vs preds'
  )

preds_lg <- wf_best_lg %>% predict_on(df, df_hold)
preds_lg %>% ggplot() + aes(x = priceRange) + geom_density()
path_export <- file.path(dir_proj, 'preds_xg2.csv')
write_csv(preds_lg, path_export)

