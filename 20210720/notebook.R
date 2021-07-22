
# # artists.csv
# log transform followers
# step_other on genres
# drop name
# what to do about 0s in popularity
# 
# # train.csv
# 
# popularity isn't normal. log transform
# filter out outliers in duration_ms
# join on artist_id
# danceability is left skewed
# energy is sort of uniform
# key... ordinal encode by most popular?
# loudness is also left skewed
# speechiness is very skewed
# acousticness is bimodal
# maybe just drop instrumentalness (extreme bimodal)
# log transform liveness
# valence also left skewed
# tempo is sort of normal
# more releases more recently
# just drop release monmth and day

library(tidyverse)
library(tidymodels)
library(tonythemes)
theme_set_tony()
dir_proj <- '20210720'
import <- function(x) {
  res <-
    file.path(dir_proj, sprintf('%s.csv', x)) %>% 
    read_csv()
}

artists <- import('artists')
artists

artists_clean <-
  artists %>% 
  mutate(
    across(
      genres,
      ~str_remove_all(.x, "\\[\\'|\\'\\]") %>% 
        str_replace_all("\\',\\s\\'", ', ') %>% 
        str_remove_all('\\,.*$')
    )
  ) %>% 
  mutate(is_pop = ifelse(genres %>% str_detect('pop'), 1, 0))
artists_clean

add_artists <- function(df) {
  ids <-
    df %>% 
    select(id_artists) %>% 
    mutate(
      across(
        id_artists,
        ~str_remove_all(.x, "\\[\\'|\\'\\]") %>% 
          str_replace_all("\\',\\s\\'", ', ')
      )
    ) %>% 
    mutate(idx = row_number())
  
  rgx_split <- ', '
  n_cols_max <-
    ids %>%
    pull(id_artists) %>% 
    str_split(rgx_split) %>% 
    map_dbl(~length(.)) %>% 
    max()
  n_cols_max
  
  nms_sep <- sprintf("id_artist_%02d", 1:n_cols_max)
  df_sep <-
    ids %>% 
    separate(id_artists, into = nms_sep, sep = rgx_split, fill = "right") %>% 
    mutate(idx = row_number())
  df_sep
  
  df_redux <-
    df_sep %>% 
    pivot_longer(
      -idx,
      names_to = 'temp',
      values_to = 'id_artists'
    ) %>% 
    drop_na(id_artists) %>% 
    left_join(artists_clean %>% select(id_artists = id, genres, artist_popularity = popularity, is_pop)) %>% 
    mutate(across(artist_popularity, ~coalesce(.x, 27)))
  
  agg <-
    df_redux %>% 
    group_by(idx) %>% 
    summarize(
      n_artist = n(),
      across(artist_popularity, list(sum = sum, med = median, max = max))
    ) %>% 
    ungroup()
  
  df %>% 
    bind_cols(agg)
}

df <- import('train') %>% add_artists()
df_hold <- import('test') %>% add_artists()
n_artist <- df %>% count(id_artists, sort = TRUE)
n_artist_hold <- df_hold %>% count(id_artists, sort = TRUE)

top_artists <-
  df %>% 
  group_by(artists) %>% 
  summarize(
    n = n(),
    across(popularity, median, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  filter(n > 5) %>% 
  arrange(desc(popularity))
top_artists

top_artists %>% 
  head(20) %>% 
  mutate(across(artists, ~fct_reorder(.x, popularity))) %>% 
  ggplot() +
  aes(x = popularity, y = artists) +
  geom_col() +
  labs(
    title = 'tell me this is a golden feature (median pop)'
  )

n_artist %>% 
  rename(n1 = n) %>% 
  full_join(n_artist_hold %>% rename(n2 = n)) %>% 
  ggplot() +
  aes(x = n1, y = n2) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = 'Artists appearances in train and test set')

df %>% 
  group_by(artists) %>% 
  summarize(n = n(), across(popularity, ~max(.x))) %>% 
  ungroup() %>% 
  ggplot() +
  aes(x = n, y = popularity) +
  geom_point(size = 3) +
  scale_x_log10() +
  labs(
    title = 'more obscure artists are more likely to have higher popularity?',
    x = '# of occurrences of artist',
    y = 'median popularity'
  )

genres <-
  df %>% 
  count(genres, sort = TRUE) %>% 
  filter(genres != '[]') %>% 
  drop_na(genres)
genres

top_genres <-
  genres %>% 
  head(20) %>% 
  mutate(across(genres, ~fct_reorder(.x, n)))
top_genres

top_genres %>% 
  ggplot() +
  aes(x = n, y = genres) +
  geom_col() +
  labs(
    title = 'adult standards is the most common genre for artist?!?'
  )

df %>% 
  # semi_join(genres) %>% 
  group_by(genres) %>% 
  summarize(n = n(), across(popularity, ~median(.x))) %>% 
  ungroup() %>% 
  ggplot() +
  aes(x = n, y = popularity) +
  geom_point(size = 3) +
  scale_x_log10() +
  labs(
    title = 'more obscure genres are more likely to have higher popularity?',
    x = '# of occurrences of genre',
    y = 'median popularity'
  )

df %>% 
  select(where(is.numeric)) %>% 
  corrr::correlate() %>% 
  # corrr::rplot()
  pivot_longer(-term) %>%
  rename(col1 = term, col2 = name) %>% 
  filter(col1 == 'popularity') %>% 
  arrange(desc(abs(value))) %>% 
  mutate(across(col2, ~fct_reorder(.x, value))) %>% 
  drop_na() %>% 
  ggplot() +
  aes(x = value, y = col2, fill = factor(sign(value))) +
  geom_col() +
  labs(
    subtitle = 'wtf why so much correlation with release_year',
    x = 'Numeric features correlation with popularity'
  )

new_keys <- df %>% count(key, sort = TRUE) %>% mutate(new_key = row_number(desc(n))) %>% select(-n)
new_keys

prep_df <- function(df) {
  df %>% 
    left_join(new_keys) %>%
    select(-key) %>% 
    rename(key = new_key) %>% 
    left_join(n_artist %>% rename(n_arists = n)) # %>% 
    # left_join(genres %>% rename(n_genres = n))
}

df %>% 
  group_by(release_year) %>% 
  summarize(across(popularity, median)) %>% 
  ggplot() +
  aes(x = factor(release_year), y = popularity) +
  geom_col() +
  labs(
    title = 'newer songs are just more popular in general'
  )

df <- df %>% prep_df()
df_hold <- df_hold %>% prep_df()

set.seed(1337)
split <- df %>% initial_split(strata = popularity)
df_trn <- split %>% training()
df_tst <- split %>% testing()
folds <- df_trn %>% vfold_cv(strata = popularity, v = 5) #  group_vfold_cv(group = id_artists, v = 5)
# df %>% select(duration_ms) %>% arrange(desc(duration_ms)) %>% ggplot(aes(duration_ms)) + geom_histogram()
# df %>% select(duration_ms) %>% summarize(p95 = quantile(duration_ms, 0.8))

rec <-
  recipe(popularity ~ ., df_trn) %>% 
  # step_impute_median(is_pop) %>% 
  # step_mutate(n_genres = coalesce(n_genres, 1)) %>% 
  step_rm(id, artists, id_artists, name, release_month, release_day, instrumentalness, duration_ms)
rec
jp <- function(x, ...) { rec %>% prep() %>% bake(x, ...) }
j_trn <- df_trn %>% jp()
j_trn %>% skimr::skim()
df_tst %>% jp()
df_hold %>% jp()

ctrl <- control_grid(
  verbose = TRUE,
  # save_workflow = TRUE,
  # save_pred = TRUE,
  extract = extract_model
)

predict_on <- function(wf, df_trn, df_tst = df_trn) {
  wf %>% 
    fit(df_trn) %>% 
    augment(df_tst) %>% 
    select(id, popularity = .pred)
}

# library(treesnip)
wf_lg <-
  rec %>% 
  workflow(
    boost_tree(
      mtry = tune(),
      trees = tune(),
      # min_n = tune(),
      learn_rate = tune()
    ) %>% 
      # set_engine('lightgbm') %>% 
      set_mode('regression')
  )

n_col_trn <- df_trn %>% jp() %>% ncol()
n_col_trn

grid_lg <- crossing(
  # min_n = c(2, 4, 6, 8, 10),
  mtry = c(5, 6, 7, 8 , 9),
  trees = c(1000),
  learn_rate = c(0.015, 0.02)
)

# path_tune_lg <- file.path(dir_proj, 'tune_lg.rds')


metset <- metric_set(rmse)

# doParallel::registerDoParallel(cores = 4)
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
    select(-popularity)
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
shap_long <- shap %>% pivot_longer(-c(id, popularity))

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
    title = 'here\'s your shap plot nick',
    x = 'mean(abs(SHAP))'
  )
p_shap_agg

set.seed(1337)
shap_long_filt <- shap %>% sample_n(1000) %>% pivot_longer(-c(id, popularity))
p_swarm <-
  shap_long_filt %>% 
  filter(name != 'baseline') %>% 
  left_join(shap_agg %>% select(-value)) %>% 
  mutate(across(name, ~fct_reorder(.x, -rnk))) %>% 
  ggplot() +
  aes(x = value, y = name) +
  ggbeeswarm::geom_quasirandom(
    aes(colour = popularity),
    groupOnX = FALSE,
    shape = 20
  ) +
  scale_color_distiller(palette = 'RdBu') +
  scale_size_area(max_size = 3) +
  theme(legend.position = 'none') +
  labs(
    title = 'release year is all you need'
  )
p_swarm

fit_best_lg <- wf_best_lg %>% fit(df)

preds_tst_lg <- wf_best_lg %>% predict_on(df_trn, df_tst)

preds_tst_lg %>% 
  left_join(df_tst %>% select(id, actual = popularity)) %>% 
  ggplot() +
  aes(x = popularity, y = actual) +
  geom_point() +
  labs(
    title = 'xgboost v4 actual vs preds'
  )

preds_lg <- wf_best_lg %>% predict_on(df, df_hold)
path_export <- file.path(dir_proj, 'preds_xg_v4.csv')
write_csv(preds_lg, path_export)

shell(
  glue::glue('kaggle competitions submit -f {path_export} -m "4th xgboost, adding more artists pop stuff" sliced-s01e08-KJSEks')
)
shell('kaggle competitions leaderboard sliced-s01e08-KJSEks -s')

wf_knn <-
  rec %>% 
  workflow(
    nearest_neighbor(
      neighbors = tune()
    )
  )

grid_knn <- crossing(
  neighbors = c(2, 5, 8, 12, 20, 30, 40)
)

tune_knn <-
  wf_knn %>% 
  tune_grid(
    metrics = metset,
    resamples = folds,
    control = ctrl,
    grid = grid_knn
  )
tune_knn
autoplot(tune_knn)

wf_best_knn <-
  wf_knn %>% 
  finalize_workflow(params_best_knn)
fit_best_knn <-
  wf_best_knn %>% 
  fit(df)

preds_tst_knn <- wf_best_knn %>% predict_on(df_trn, df_tst)
preds_tst_knn %>% 
  left_join(df_tst %>% select(actual = popularity)) %>% 
  ggplot() +
  aes(x = popularity, y = actual) +
  geom_point()

preds_knn <- wf_best_knn %>% predict_on(df, df_hold)

wf_lin <-
  rec %>% 
  workflow(
    linear_reg(
      penalty = tune()
    ) %>% 
      set_engine('glmnet')
  )

grid_lin <- crossing(
  penalty = 10 ^ seq(-7, -5, by = 0.1)
)

tune_lin <-
  wf_lin %>% 
  tune_grid(
    metrics = metset,
    resamples = folds,
    control = ctrl,
    grid = grid_lin
  )
tune_lin
autoplot(tune_lin)

params_best_lin <- wf_lin %>% select_best()
wf_best_lin <-
  wf_lin %>% 
  finalize_workflow(params_best_lin)
fit_best_lin <-
  wf_best_lin %>% 
  fit(df)
preds_lin <- wf_best_lin %>% predict_on(df, df_hold)

path_export <- file.path(dir_proj, 'preds1.csv')
write_csv(path_export)



