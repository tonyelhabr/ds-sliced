
library(tidyverse)
library(tidymodels)
library(stacks)

data('attrition', package = 'modeldata')
col_y <- 'attrition'
df <-
  # data('tree_frogs', package = 'stacks') %>%
  attrition %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  mutate(across(job_level, ordered))
df

summ <- df %>% skimr::skim()
summ

f_glm <- function(col_x) {
  form <- sprintf('%s ~ %s', col_y, col_x) %>% as.formula()
  fit <- glm(form, data = df %>% select(!!sym(col_y), !!sym(col_x)), family = 'binomial')
  tidy(fit)
}
cols_x <- df %>% colnames() %>% setdiff(col_y)

summs <-
  cols_x %>%
  setNames(., .) %>% 
  map_dfr(f_glm, .id = 'col')
summs
summs %>% 
  filter(term != '(Intercept)') %>% 
  arrange(p.value)

cors_long <-
  df %>% 
  mutate(across(where(is.factor), as.integer)) %>% 
  select(where(is.numeric)) %>% 
  corrr::correlate() %>% 
  rename(col1 = rowname) %>% 
  pivot_longer(
    -col1,
    names_to = 'col2',
    values_to = 'cor'
  ) # %>% 
# filter(col1 < col2) 
cors_long

viz_cors <-
  cors_long %>%
  filter(col1 < col2) %>% 
  ggplot() +
  aes(x = col1, y = col2) +
  geom_tile(
    aes(fill = cor), 
    alpha = 0.7, 
    # hjust = 0, vjust = 0, 
    show.legend = FALSE
  ) +
  geom_text(
    aes(label = scales::number(cor, accuracy = 0.1)),
    # hjust = +1, vjust = +1,
    size = 4,
    family = 'bold'
  ) +
  theme_minimal(base_family = 'Lato', base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = 'bold'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 30)
  ) +
  coord_equal() +
  scale_fill_viridis_c(option = 'E') +
  labs(
    title = 'Numeric Feature Correlations',
    x = NULL,
    y = NULL
  )
viz_cors
require(ggforce)

cols_x_filt <- c('years_in_current_role', 'over_time', 'job_level', 'total_working_years', 'environment_satisfaction', 'monthly_income')

df %>%
  # sample_n(100) %>%
  ggplot(
    aes(
      x = .panel_x,
      y = .panel_y,
      fill = attrition,
      colour = attrition
    )
  ) +
  geom_point(
    shape = 16,
    size = 0.5,
    position = 'auto'
  ) +
  geom_autodensity(
    alpha = 0.3,
    colour = NA,
    position = 'identity'
  ) +
  geom_smooth(aes(colour = NULL, fill = NULL)) +
  facet_matrix(
    # vars(all_vars()),
    vars(all_of(cols_x_filt)),
    layer.diag = 2,
    layer.continuous = TRUE,
    layer.mixed = -3,
    layer.discrete = -3
  ) +
  theme_minimal() +
  theme(
    legend.position = 'top'
  )

df %>% 
  select(all_of(cols_x_filt)) %>% 
  select(where(is.numeric)) %>%
  bind_cols(df %>% select(all_of(col_y))) %>% 
  pivot_longer(
    -all_of(col_y)
  ) %>% 
  ggplot() +
  aes(x = value, group = attrition, fill = attrition) +
  geom_density(alpha = 0.3) +
  facet_wrap(~name, scales = 'free') +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = 'top'
  )


# cors_long_fct <-
#   cors_long %>% 
#   mutate(
#     across(c(col1, col2), list(id = ~factor(.x) %>% as.integer()))
#   )
# cors_long_fct
# lvls <- cors_long_fct %>% pull(col1) %>% unique()
# lvls
# 
# cors_long_fct %>% 
#   ggplot() +
#   aes(x = col1_id, y = col2_id) +
#   # geom_rect(
#   #   data = cors_long_fct %>% filter(col1 <= col2),
#   #   aes(xmin = col1_id, xmax = col1_id + 1, ymin = col2_id, ymax = col2_id + 1),
#   #   fill = 'white',
#   #   color = 'black',
#   #   alpha = 0.7,
#   #   show.legend = FALSE
#   # ) +
#   geom_rect(
#     data = . %>% filter(col1 > col2),
#     aes(xmin = col1_id, xmax = col1_id + 1, ymin = col2_id, ymax = col2_id + 1, fill = cor),
#     color = 'black',
#     alpha = 0.7,
#     show.legend = FALSE
#   ) +
#   geom_text(
#     data = . %>% filter(col1 > col2),
#     aes(label = scales::number(cor, accuracy = 0.01)),
#     hjust = -0.1,
#     vjust = -1,
#     size = 5,
#     family = 'bold'
#   ) +
#   scale_x_continuous(
#     breaks = seq(1.5, length(lvls) + 0.5, 1), labels = lvls # ,
#     # limits = c(1.5, length(lvls) + 0.5 + 1)
#   ) +
#   scale_y_continuous(
#     breaks = seq(1.5, length(lvls) + 0.5, 1), labels = lvls # ,
#     # limits = c(1.5, length(lvls) + 0.5 + 1)
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor.y = element_line(size = 1),
#     panel.grid.minor.x = element_blank(),
#     # panel.background = element_rect(fill = 'transparent'),
#     axis.text.x = element_text(angle = 45, hjust = 0.95)
#   ) +
#   # coord_equal() +
#   scale_fill_viridis_c(option = 'E') +
#   labs(
#     title = 'Numeric Feature Correlations',
#     x = NULL,
#     y = NULL
#   )

col_y <- 'sale_price'
col_y_sym <- col_y %>% sym()
fmla <- formula(sprintf('%s ~ .', col_y))

set.seed(42)
split <- df %>% rsample::initial_split(stata = !!col_y_sym, prop = 0.8)
df_trn <- split %>% training()
df_tst <- split %>% testing()
folds <- df_trn %>% vfold_cv(v = 10)

f_ctrl <- function(suffix) {
  f <- sprintf('control_%s', suffix)
  exec(f, save_pred = TRUE, save_workflow = TRUE, verbose = TRUE)
}
ctrl_grid <- f_ctrl('grid')
ctrl_resamples <- f_ctrl('resamples')
met_set <- metric_set(rmse, mae, rsq)

rec_init <-
  recipe(fmla, data = df_trn) %>% 
  step_nzv(all_predictors()) %>% 
  step_corr(all_numeric_predictors()) %>% 
  step_lincomb(all_numeric_predictors()) %>% 
  step_other(all_nominal_predictors()) %>% 
  step_impute_mean(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_YeoJohnson(all_outcomes())

rec_lm <-
  rec_init %>% 
  step_bs(all_numeric_predictors())

fit_lm <- 
  linear_reg() %>% 
  set_engine('lm')

wf_lm <-
  workflow() %>% 
  add_recipe(rec_lm) %>% 
  add_model(fit_lm)

rec_lm %>% prep()
rec_lm %>% prep() %>% juice()
# Check it works just once
wf_lm %>% fit(df_trn)
# Check it works with lm
res_lm <-
  fit_resamples(
    wf_lm,
    resamples = folds,
    metrics = met_set,
    control = ctrl_resamples
  )

rec_glmnet <- rec_init
rec_knn <- rec_init
rec_svm <- rec_init

fit_glmnet <- 
  linear_reg(
    mixture = tune(),
    penalty = tune()
  ) %>% 
  set_mode('regression') %>% 
  set_engine('glmnet')

fit_knn <- 
  nearest_neighbor(
    neighbors = tune('k')
  ) %>%
  set_mode('regression') %>% 
  set_engine('kknn')

fit_svm <-
  svm_rbf(
    cost = tune('cost'), 
    rbf_sigma = tune('sigma')
  ) %>%
  set_mode('regression') %>% 
  set_engine('kernlab')

n_grid <- 5L
f_grid <- function(fit) {
  grid_latin_hypercube(
    fit %>% parameters(),
    size = n_grid
  )
}

grid_glmnet <- fit_glmnet %>% f_grid()
grid_knn <- fit_knn  %>% f_grid()
grid_svm <- fit_svm %>% f_grid()

wf_glmnet <-
  workflow() %>% 
  add_recipe(rec_glmnet) %>% 
  add_model(fit_glmnet)

wf_knn <-
  workflow() %>% 
  add_recipe(rec_knn) %>% 
  add_model(fit_knn)

wf_svm <-
  workflow() %>% 
  add_recipe(rec_svm) %>% 
  add_model(fit_svm)


tune_grid_partial <-
  partial(
    tune_grid,
    resamples = folds,
    metrics = met_set,
    control = ctrl_grid,
    ... =
  )

res_glmnet <- wf_glmnet %>% tune_grid_partial()
res_knn <- wf_knn %>% tune_grid_partial()
res_svm <- wf_svm %>% tune_grid_partial()

res_glmnet %>% autoplot(metric = 'rmse')

# save.image(file = 'env.RData')

f_stack <- function(x, res, nm = str_remove(deparse(substitute(res)), 'res_')) {
  x %>% 
    add_candidates(res, name = nm)
}

ens <-
  stacks() %>% 
  f_stack(res_lm) %>% 
  f_stack(res_glmnet) %>% 
  f_stack(res_knn) %>% 
  f_stack(res_svm)
ens

fit_ens <-
  ens %>% 
  blend_predictions() %>% 
  fit_members()
fit_ens

res_glmnet %>% 
  show_best('rmse') %>% 
  slice(1)


fit_ens %>% 
  predict(df_tst) %>% 
  bind_cols(df_tst %>% select(!!col_y_sym)) %>% 
  met_set(truth = !!col_y_sym, estimate = .pred) %>% 
  pviot_wider(names_from = .metric, values_from = .estimate)
