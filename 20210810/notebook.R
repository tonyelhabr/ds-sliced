
library(tidyverse)
library(tidymodels)
library(tonythemes)
theme_set_tony()
dir_proj <- '20210810'
f_read <- function(x) {
  file.path(dir_proj, sprintf('%s.csv', x)) %>%
    read_csv()
}

predict_on <- function(wf, df_trn, df_tst = df_trn) {
  wf %>% 
    fit(df_trn) %>% 
    augment(df_tst) %>% 
    # mutate(across(.pred, exp)) %>% 
    select(zpid, latestPrice = .pred)
}

df <- f_read('train')
df_hold <- f_read('test')
df %>% skimr::skim()
df %>% count(city)
df %>% mutate(across(latestPrice, log10)) %>% ggplot() + aes(latestPrice) + geom_histogram()
df %>% filter(latestPrice < 100000)
df %>% ggplot() + aes(x = lotSizeSqFt, y = latestPrice) + geom_point() + scale_y_log10() + scale_x_log10()

tx <- ggplot2::map_data('county', regions = 'texas') %>% filter(subregion == 'travis')# filter(subregion %in% c('williamson', 'travis', 'hays'))
tx

df %>% 
  ggplot() +
  geom_polygon(data = tx, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
  # aes(x = latitude, y = longitude, z = log(latestPrice)) +
  stat_summary_hex(aes(x = longitude, y = latitude, z = log(latestPrice)), alpha = 0.8) +
  scale_fill_viridis_c() +
  theme(panel.grid.major = element_blank(), axis.text = element_blank(), legend.position = 'none') +
  labs(x = NULL, y = NULL)

df %>% 
  filter(lotSizeSqFt < 20000) %>% 
  ggplot() +
  geom_polygon(data = tx, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
  # aes(x = latitude, y = longitude, z = log(latestPrice)) +
  stat_summary_hex(aes(x = longitude, y = latitude, z = lotSizeSqFt), alpha = 0.8) +
  scale_fill_viridis_c() +
  theme(panel.grid.major = element_blank(), axis.text = element_blank()) +
  labs(x = NULL, y = NULL)

df %>% 
  ggplot() +
  geom_polygon(data = tx, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
  # aes(x = latitude, y = longitude, z = log(latestPrice)) +
  geom_point(aes(x = longitude, y = latitude, color = hasSpa), alpha = 0.8) +
  theme(panel.grid.major = element_blank(), axis.text = element_blank()) +
  labs(x = NULL, y = NULL)
df


df %>% 
  filter(yearBuilt >= 2015) %>% 
  ggplot() +
  geom_polygon(data = tx, aes(x = long, y = lat, group = group), fill = NA, color = 'black') +
  # aes(x = latitude, y = longitude, z = log(latestPrice)) +
  geom_point(aes(x = longitude, y = latitude, color = factor(yearBuilt), size = latestPrice), alpha = 0.8) +
  theme(panel.grid.major = element_blank(), axis.text = element_blank()) +
  labs(x = NULL, y = NULL)
df

df_full <-
  bind_rows(
    df %>% mutate(is_hold = 'no'),
    df_hold %>% mutate(is_hold = 'yes')
  ) %>% 
  mutate(across(is_hold, factor))

df_full %>% ggplot() + aes(x = zpid, fill = is_hold) + geom_density(alpha = 0.7)

df_num <- 
  df_full %>% 
  select(is_hold, where(is.numeric))
df_num

cors <-
  df_num %>%
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
cors      

p_cors <-
  cors %>% 
  pivot_wider(
    names_from = is_hold,
    values_from = cor
  ) %>% 
  filter(col1 != 'latestPrice') %>% 
  filter(col2 != 'latestPrice') %>% 
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
ggsave(p_cors, filename = file.path(dir_proj, 'cor_diffs.png'), width = 12, height = 8, type = 'cairo')

cors_filt <-
  cors %>% 
  filter(is_hold == 'no') %>% 
  filter(col1 == 'latestPrice') %>% 
  filter(col2 != 'latestPrice')
cors_filt

df %>% 
  group_by(city) %>% 
  summarize(
    n = n(),
    across(latestPrice, median)
  ) %>% 
  arrange(desc(latestPrice))

df %>% 
  group_by(homeType) %>% 
  summarize(
    n = n(),
    across(latestPrice, median)
  ) %>% 
  ungroup() %>% 
  mutate(frac = n / sum(n)) %>% 
  arrange(desc(frac))

set.seed(42)
df2 <- df %>% mutate(across(latestPrice, log))
split <- df2 %>% initial_split(strata = latestPrice)
df_trn <- split %>% training()
df_tst <- split %>% testing()
folds <- df_trn %>% vfold_cv(strata = latestPrice, v = 10) #  group_vfold_cv(group = id_artists, v = 5)
# df %>% select(duration_ms) %>% arrange(desc(duration_ms)) %>% ggplot(aes(duration_ms)) + geom_histogram()
# df %>% select(duration_ms) %>% summarize(p95 = quantile(duration_ms, 0.8))
metset <- metric_set(rmse)
library(textrecipes)

rec <-
  recipe(latestPrice ~ ., df_trn) %>% 
  # step_impute_median(is_pop) %>% 
  # step_mutate(n_genres = coalesce(n_genres, 1)) %>% 
  step_mutate(hasSpa = ifelse(hasSpa, 1, 0)) %>% 
  step_tokenize(description) %>%
  step_stopwords(description) %>% 
  step_tokenfilter(description, max_tokens = 200) %>%
  step_tf(description) %>% 
  step_pca(matches('^tf_description'), num_comp = 5) %>% 
  step_rm(zpid, city, homeType)
rec

df_trn %>% 
  group_by(yearBuilt) %>% 
  summarize(
    n = n(),
    across(latestPrice, mean)
  ) %>% 
  filter(yearBuilt > 1950) %>% 
  ungroup() %>% 
  ggplot() +
  aes(x = yearBuilt, y = latestPrice) +
  geom_col()

rec2 <- 
  recipe(latestPrice ~ latitude + longitude + yearBuilt + numOfBedrooms + avgSchoolRating, df_trn) %>% 
  step_interact(~ latitude:longitude) %>%
  # step_interact(~ numOfBathrooms:numOfBedrooms) %>%
  step_nzv(all_numeric_predictors()) %>% 
  # 20 is best according to tune
  step_ns(longitude, latitude, matches('latitude_x_longitude'), deg_free = 20)
rec2

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

wf_lin <- rec2 %>% workflow(linear_reg())
wf_lin
fit_trn <- wf_lin %>% fit(df_trn)
preds_tst <- fit %>% augment(df_tst) # %>% mutate(across(.pred, exp))
preds_tst %>% select(.pred) %>%mutate(across(.pred, list(exp = exp)))
preds_tst %>% ggplot() + aes(x = exp(latestPrice), y = .pred) + geom_point() + ylim(0, 3*10^5)
preds_tst %>% 
  metset(latestPrice, .pred)

df %>% 
  filter(yearBuilt == 2019) %>% 
  filter(latitude == max(lat))
  select(description) %>% 
  filter(description %>% str_detect('flug'))

full_join(
  preds_tst %>% select(zpid, .pred1 = .pred),
  preds_tst_lg %>% select(zpid, .pred2 = latestPrice)
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
preds %>% ggplot() + aes(x = latestPrice, y = .pred) + geom_point() + ylim(0, 3*10^5)

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
    select(-latestPrice)
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
shap_long <- shap %>% pivot_longer(-c(zpid, latestPrice))

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
shap_long_filt <- shap %>% sample_n(1000) %>% pivot_longer(-c(zpid, latestPrice))
p_swarm <-
  shap_long_filt %>% 
  filter(name != 'baseline') %>% 
  left_join(shap_agg %>% select(-value)) %>% 
  mutate(across(name, ~fct_reorder(.x, -rnk))) %>% 
  ggplot() +
  aes(x = value, y = name) +
  ggbeeswarm::geom_quasirandom(
    aes(colour = latestPrice),
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
  left_join(df_tst %>% select(zpid, actual = latestPrice)) %>% # mutate(across(actual, exp))) %>% 
  metset(actual, latestPrice)

preds_tst_lg %>% 
  left_join(df_tst %>% select(zpid, actual = latestPrice)) %>% 
  ggplot() +
  aes(x = latestPrice, y = actual) +
  geom_point() +
  labs(
    title = 'actual vs preds'
  )

preds_lg <- wf_best_lg %>% predict_on(df, df_hold)
preds_lg %>% ggplot() + aes(x = latestPrice) + geom_density()
path_export <- file.path(dir_proj, 'preds_xg2.csv')
write_csv(preds_lg, path_export)

