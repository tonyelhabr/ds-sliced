
# https://github.com/dgrtwo/data-screencasts/blob/master/ml-practice/ep4.Rmd
library(tidyverse)
library(tidymodels)
library(scales)
library(lubridate)
library(textrecipes)
library(stacks)
dir_proj <- '20210622'
# options(xgbh.dir_data = dir_proj)

dataset <- read_csv(file.path(dir_proj, "train.csv")) %>%
  mutate(rain_tomorrow = factor(ifelse(rain_tomorrow, "Rained", "Didn't Rain")))
holdout <- read_csv(file.path(dir_proj, "test.csv"))
holdout

set.seed(2021)
spl <- initial_split(dataset, prop = .75)
train <- training(spl)
test <- testing(spl)
train_fold <- train %>%
  vfold_cv(v = 5)

mset <- metric_set(mn_log_loss)
grid_control <- control_grid(save_pred = TRUE,
                             save_workflow = TRUE,
                             extract = extract_model)
augment.workflow <- function(x, newdata, ...) {
  predict(x, newdata, ...) %>%
    bind_cols(newdata)
}

predict_on_holdout <- function(workflow) {
  workflow %>%
    fit(dataset) %>%
    augment(holdout, type = "prob") %>%
    select(id, rain_tomorrow = .pred_Rained)
}

set.seed(2021)
train_fold_small <- train %>%
  sample_n(4000) %>%
  vfold_cv(v = 2)
compass_directions <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S",
                        "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
as_angle <- function(direction) {
  (match(direction, compass_directions) - 1) * 360 / length(compass_directions)
}

xg_rec <- recipe(rain_tomorrow ~ date + rain_today +
                   min_temp + max_temp + rainfall +
                   wind_gust_speed + wind_speed9am +
                   wind_speed3pm + humidity9am + humidity3pm + pressure9am +
                   pressure3pm + cloud9am + cloud3pm + temp9am + temp3pm +
                   wind_gust_dir + wind_dir9am + wind_dir3pm +
                   location +
                   rain_today, data = train) %>%
  step_mutate(week = week(date)) %>%
  step_mutate(wind_gust_dir = as_angle(wind_gust_dir)) %>%
  step_mutate(wind_dir9am = as_angle(wind_dir9am)) %>%
  step_mutate(wind_dir3pm = as_angle(wind_dir3pm)) %>%
  # step_other(location, threshold = 0.01) %>%
  step_rm(location) %>% 
  step_rm(date) %>% 
  step_impute_mean(all_numeric_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())
jui_trn <- xg_rec %>% prep() %>% juice()
jui_trn
xg_wf <- workflow() %>%
  add_recipe(xg_rec) %>%
  add_model(boost_tree("classification",
                       mtry = tune(),
                       trees = tune(),
                       learn_rate = .01) %>% set_engine("xgboost"))
xg_tune <- xg_wf %>%
  tune_grid(train_fold,
            grid = crossing(mtry = c(9, 12),
                            trees = seq(100, 1600, 100),
                            threshold = c(.01)),
            metrics = mset,
            control = grid_control)
autoplot(xg_tune)
xg_tune$.notes[[1]]
