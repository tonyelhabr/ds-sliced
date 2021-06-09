
shell('kaggle competitions leaderboard sliced-s01e02-xunyc5 -s')

library(tonyverse)
library(tonymodels)
library(xgbh)
library(tonythemes)
dir_proj <- '20210608'
options(xgbh.dir_data = dir_proj)

theme_set_tony()
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
df
# df_hold <- f_read('test')
# df_hold

df %>%
  drop_na(distance) %>%
  ggplot() +
  aes(x = distance) +
  geom_histogram() +
  scale_x_log10() +
  labs(
    title = 'i\'m basically drob doing a log transform'
  )

df %>%
  count(flight_impact, damaged, sort = TRUE) %>%
  # mutate(across(precipitation, ~coalesce(.x, 'missing'))) %>%
  group_by(flight_impact) %>%
  mutate(frac = n / sum(n)) %>%
  ungroup() %>%
  filter(damaged == '1') %>%
  mutate(across(flight_impact, ~fct_reorder(.x, frac))) %>%
  arrange(desc(frac)) %>%
  ggplot() +
  aes(y = flight_impact, x = frac) +
  geom_col()

seed <- 42
set.seed(seed)
split <- df %>% initial_split(strata = damaged)
df_trn <- split %>% training()
df_tst <- split %>% testing()

library(embed)
library(themis)
rec <-
  df %>%
  select(damaged, where(is.numeric)) %>%
  recipe(damaged ~ ., df) %>%
  update_role(id, new_role = 'id') %>%
  # step_naomit(all_numeric_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_upsample(damaged)
rec

jui_trn <- rec %>% prep() %>% juice()
jui_tst <- rec %>% prep() %>% bake(df_tst)
jui_hold <- rec %>% prep() %>% bake(df_hold)

to_xgb <- function(x) { x %>% mutate(across(damaged, ~as.integer(.x) - 1L))}

jui_trn_xgb <- jui_trn %>% to_xgb()
jui_tst_xgb <- jui_tst %>% to_xgb()
jui_hold_xgb <- jui_hold #  %>% to_xgb()

col_id <- 'id'
col_y <- 'damaged'


rec_u <-
  rec %>%
  step_umap(all_numeric_predictors(), num_comp = 2)
# rec %>% prep()
u <- rec_u %>% prep() %>% juice()
pal <- palette_sliced(5)[c(1, 5)]
viz_umap <-
  u %>%
  ggplot() +
  aes(x = umap_1, y = umap_2) +
  geom_point(aes(color = damaged), size = 3) +
  scale_color_manual(values = pal) +
  labs(
    title = 'pretty umap, per usual'
  )
viz_umap +
  labs(title = 'basically outer space')

set.seed(seed)
grid_params <-
  jui_trn %>%
  select(-any_of(c(col_id, col_y))) %>%
  xgbh::generate_grid_params(30)
grid_params

to_xgb <- function(x) { x %>% mutate(across(damaged, ~as.integer(.x) - 1L))}

jui_trn_xgb <- jui_trn %>% to_xgb()
jui_tst_xgb <- jui_tst %>% to_xgb()
jui_hold_xgb <- jui_hold %>% to_xgb()

set.seed(seed)
grid_params <-
  jui_trn %>%
  select(-any_of(c(col_id, col_y))) %>%
  xgbh::generate_grid_params(10)
grid_params

suffix <- 's01e02_quick'

fit <-
  xgbh::fit(
    data = jui_trn_xgb,
    overwrite = TRUE,
    suffix = suffix,
    objective = 'binary:logistic',
    eval_metrics = list('logloss'),
    col_y = col_y,
    col_id = col_id,
    nrounds = 1000
  )
