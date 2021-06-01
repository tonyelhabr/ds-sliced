
library(tidyverse)
library(tidymodels)
data(mtcars)
df <- mtcars %>% as_tibble()

rec <-
  df %>% 
  recipe(mpg ~ ., data = .) %>% 
  step_dummy(all_nominal_predictors()) # %>% 
  # step_normalize(all_numeric_predictors())

spec <-
  rand_forest(mtry = tune(), min_n = tune()) %>% 
  set_mode('regression') %>% 
  set_engine('ranger')

wf <-
  workflow() %>% 
  add_recipe(rec) %>% 
  add_model(spec)

set.seed(6*6*6)
params_grid <-
  dials::grid_latin_hypercube(
    min_n(range = c(1, 10)),
    # min_n(),
    finalize(mtry(), mtcars),
    size = 5
  )
params_grid

folds <- df %>% vfold_cv(v = 5)

res_tune <-
  tune_grid(
    wf,
    grid = params_grid,
    resamples = folds,
    control = control_grid(save_pred = TRUE, save_workflow = TRUE, verbose = TRUE)
  )
res_tune

params_best <- res_tune %>% select_best('rmse')
wf_final <- wf %>% finalize_workflow(params_best)
fit_final <- wf_final %>% fit(df)
fit_final2 <- fit_final %>% pull_workflow_fit()

vi <-
  vip::vi(fit_final2, train = df %>% select(-mpg), method = 'shap', pred_wrapper = predict)
vi

fit_ranger <- ranger::ranger(mpg ~ ., data = mtcars)


f_predict <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}
x <- mtcars %>% select(-mpg) %>% data.matrix()

vi_ranger <-
  fastshap::explain(
    fit_ranger,
    X = x,
    pred_wrapper = f_predict,
    nsim = 10
  )

shap <-
  fastshap::explain(
    fit_final2$fit,
    X = x,
    pred_wrapper = pfun,
    nsim = 10
  )
shap
shap %>% 
  as_tibble() %>% 
  mutate(idx = row_number()) %>% 
  pivot_longer(-idx) %>% 
  group_by(name) %>% 
  summarize(
    across(value, ~mean(abs(.x)))
  ) %>% 
  ungroup() %>% 
  arrange(desc(value))
autoplot(shap)
autoplot(shap, type = 'dependence', feature = 'wt', X = mtcars)
autoplot(shap, type = 'contribution', row_num = 1)

wf_final <- wf_best %>% pull_workflow_fit()
fit <- parnsip::fit(mpg ~ ., data = mtcars)




vi <-
  vip::vi(fit, method = 'shap', pred_wrapper = predict)
vi

# Compute approximate Shapley values using 10 Monte Carlo simulations
set.seed(101)  # for reproducibility
shap <- fastshap::explain(fit, X = subset(mtcars, select = -mpg), nsim = 10, 
                pred_wrapper = predict)
shap

# Compute exact Shapley (i.e., LinearSHAP) values
shap <- fastshap::explain(fit, exact = TRUE)
shap
library(tidyverse)
shap %>% 
  as_tibble() %>% 
  mutate(idx = row_number()) %>% 
  pivot_longer(-idx) %>% 
  group_by(name) %>% 
  summarize(
    across(value, ~mean(abs(.x)))
  ) %>% 
  ungroup() %>% 
  arrange(desc(value))

autoplot(shap)
autoplot(shap, type = "dependence", feature = "wt", X = mtcars)
autoplot(shap, type = "contribution", row_num = 1)
