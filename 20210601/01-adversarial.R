
library(tidyverse)
library(tidymodels)
library(tonythemes)
theme_set_tony()

dir_proj <- '20210601'
f_read <- function(x) {
  file.path(dir_proj, sprintf('%s.csv', x)) %>%
    read_csv() %>% 
    select(-matches('category[2-9]$|category1[0-2]$'))
}

predict_on <- function(wf, df_trn, df_tst = df_trn) {
  wf %>% 
    fit(df_trn) %>% 
    augment(df_tst) %>% 
    select(game_id, geek_rating = .pred)
}

df <- f_read('train')
df_hold <- f_read('test')
df_hold %>% skimr::skim()
df_hold %>% count(category1, sort = TRUE)
df_full <- bind_rows(
  df %>% mutate(is_hold = 'no'),
  df_hold %>% mutate(is_hold = 'yes')
) %>% 
  mutate(across(is_hold, factor))

df_full %>% ggplot() + aes(x = game_id, fill = is_hold) + geom_density(alpha = 0.7)

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

cors_filt <-
  cors %>% 
  filter(is_hold == 'no') %>% 
  filter(col1 == 'geek_rating') %>% 
  filter(col2 != 'geek_rating')
cors_filt

p_num <-
  df_num %>% 
  filter(year > 0) %>% 
  pivot_longer(-c(is_hold, geek_rating)) %>% 
  # group_by(name) %>%
  # mutate(across(value, percent_rank)) %>%
  # ungroup() %>%
  ggplot() +
  aes(x = value, fill = is_hold) +
  geom_density(alpha = 0.5) +
  # scale_x_continuous(labels = percent) +
  facet_wrap(~name, scales = 'free') +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = 'top'
  ) +
  labs(
    title = str_wrap('How are the distributions of numeric features different?', 60),
    y = 'Density',
    x = 'Percentile'
  )
p_num

rec_cls <-
  df_full %>%
  # recipe(formula(is_hold ~ .), .) %>%
  # step_rm(geek_rating) %>% 
  # step_rm(mechanic, designer, names, game_id) %>% 
  # step_other(category1, other = 0.05) %>% 
  # step_dummy(category1)
  recipe(
    formula(is_hold ~ owned + num_votes + avg_time + max_players + year + age), 
    data = .
  )
rec_cls

jui_cls <- rec_cls %>% prep() %>% juice()
jui_cls

# wf_cls <-
#   rec_cls %>% 
#   workflow(
#     rand_forest() %>% 
#       set_engine('ranger', importance = 'permutation') %>% 
#       set_mode('classification')
#   )
# wf_cls

wf_cls <-
  rec_cls %>% 
  workflow(
    nearest_neighbor(mode = 'classification')
  )
wf_cls

fit_cls <- wf_cls %>% fit(df_full)
fit_cls
probs_full <- fit_cls %>% augment(df_full, type = 'prob')
probs_cls <- fit_cls %>% augment(df, type = 'prob')
probs_cls

# wf_cls %>% 
#   fit(df_full) %>% 
#   extract_model() %>% 
#   vip::vip()

probs_full %>% 
  select(matches('pred'), is_hold) %>% 
  # mutate(across(is_hold, ~factor(.x, levels = c('no', 'yes')))) %>% 
  roc_curve(is_hold, .pred_no) %>% 
  autoplot()

n_row <- probs_cls %>% nrow()
floor(0.8 * n_row)

ids_trn <-
  probs_cls %>% 
  relocate(game_id, matches('.pred')) %>% 
  arrange(desc(.pred_yes)) %>% 
  dplyr::slice(c(1:floor(0.8 * n_row))) %>% 
  pull(game_id)
ids_trn

probs_full %>% 
  mutate(rnk = row_number(desc(.pred_yes))) %>% 
  ggplot() +
  aes(x = rnk, y = .pred_yes, color = is_hold) +
  geom_point()

df_trn <- df %>% filter(game_id %in% ids_trn) # %>% select(-game_id)
df_tst <- df %>% filter(!(game_id %in% ids_trn))

folds <- df_trn %>% vfold_cv(v = 5) # %>% filter(id == 'Fold1')
folds

fold <- folds %>% filter(id == 'Fold1')
class(fold) <- c('vfold_cv', 'rset', class(fold))
attr(fold, 'v') <- 1
attr(fold, 'repeats') <- 1
attr(fold, 'strata') <- FALSE
attr(fold, 'row.names') <- 1L
attr(fold, 'fingerprint') <- attr(folds, 'fingerprint')

metset <- metric_set(rmse)
ctrl <- control_grid(
  extract = extract_model,
  verbose = TRUE
)

# actual ----
rec <-
  df_trn %>%
  recipe(
    formula(geek_rating ~ owned + num_votes + avg_time + max_players + year + age), 
    data = .
  )
rec

jui <- rec %>% prep() %>% juice()
jui
n_col <- jui %>% ncol()
n_col

wf_xg <- 
  rec %>% 
  workflow(
    boost_tree(
      trees = tune(),
      mtry = tune(),
      learn_rate = tune(),
      engine = 'xgboost',
      mode = 'regression'
    )
  )
wf_xg

grid_xg <- crossing(
  trees = c(1000, 3000, 5000),
  mtry = 2:7,
  learn_rate = c(.005, .001)
)
grid_xg

set.seed(2021)
folds_drob <- df %>% vfold_cv(10)

# doParallel::registerDoParallel(cores = 4)
tune_xg <-
  wf_xg %>% 
  tune_grid(
    grid = grid_xg,
    control = ctrl,
    metrics = metset,
    resamples = folds_drob
  )
tune_xg
write_rds(tune_xg, file.path(dir_proj, 'tune_xg.rds'))
tune_xg %>% collect_metrics() %>% arrange(mean)
tune_xg %>% autoplot()
tune_xg %>% select_best('rmse')
params_best_xg <- tune_xg %>% select_best('rmse')
wf_best_xg <- wf_xg %>% finalize_workflow(params_best_xg)
wf_best_xg
