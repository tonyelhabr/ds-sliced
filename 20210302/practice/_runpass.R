
# library(rstan)
# library(lme4)
library(tidyverse)
library(vip)
library(tidymodels)
# library(DT)
# library(arm)
# library(tidybayes)
# library(ggrepel)

set.seed(1234)

seasons <- 2016:2019
dat <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

post16 <- filter(
  dat,
  season_type == "REG" &
    down %in% c(1, 2, 3) &
    !is.na(qb_dropback) &
    !is.na(score_differential)
) %>%
  mutate(
    qb_dropback = factor(qb_dropback),
    off_to = if_else(posteam_type == "away", away_timeouts_remaining, home_timeouts_remaining),
    def_to = if_else(posteam_type == "away", home_timeouts_remaining, away_timeouts_remaining)
  ) %>%
  dplyr::select(qb_dropback, down, ydstogo, yardline_100, score_differential, qtr, half_seconds_remaining, off_to, def_to)

# xgb_res <- readRDS('objects/xgb-grid-search.RDS') ## Loading hyperparameter grid results
# final_mod <- readRDS('objects/final-mod-test-dat.RDS') ##Loading xgboost model
# final_qb_mod <- readRDS('objects/final-full-xgb.RDS') ##loading xgboost model
# fit_no_epa <- readRDS('objects/no_epa_model.RDS') ##loading stan model

# samps_no_epa <- rstan::extract(fit_no_epa, pars = "mu")$mu ## Extract mu estimates
# quantile_025_no_epa <- apply(samps_no_epa, 2, quantile, .025) ## Calculate 2.5th percentile of mu estimates
# quantile_975_no_epa <- apply(samps_no_epa, 2, quantile, .975) ## Extract 97.5th percentile of mu estimates
# mean_no_epa <- apply(samps_no_epa, 2, mean) ## extract mean estimates

teams <- dat %>%
  filter(!is.na(posteam)) %>%
  dplyr::select(posteam, season, qb_dropback) %>%
  mutate(
    team_string = str_c(posteam, "-", season),
    team_idx = as.numeric(factor(team_string))
  ) %>%
  group_by(posteam, season) %>%
  summarise(
    team_idx = max(team_idx),
    dropback_pct = mean(qb_dropback)
  ) %>%
  ungroup()

# teams$q_025_no_epa <- quantile_025_no_epa
# teams$q_975_no_epa <- quantile_975_no_epa
# teams$mean_no_epa <- mean_no_epa
# teams$display_name <- factor(str_c(teams$posteam, " - ", teams$season))
# teams$display_name <- fct_reorder(teams$display_name, teams$mean_no_epa)
# teams <- teams %>%
#   group_by(season) %>%
#   mutate(
#     qb_dropback_rank = rank(desc(dropback_pct)),
#     qb_dropback_est_rank = rank(desc(mean_no_epa))
#   )

dat_split <- initial_split(post16)
dat_train <- training(dat_split)
dat_test <- testing(dat_split)

qb_folds <- vfold_cv(dat_train)

qb_recipe <- recipe(qb_dropback ~ down + 
                      ydstogo + 
                      yardline_100 + 
                      score_differential + 
                      qtr + 
                      half_seconds_remaining +
                      off_to +
                      def_to,
                    data = dat_train)

qb_model <- 
  boost_tree(
    mtry = tune(),
    trees = 2000, 
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),                    
    sample_size = tune(),         
    stop_iter = 100
  ) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

qb_workflow <- workflow() %>%
  add_recipe(qb_recipe) %>%
  add_model(qb_model)

set.seed(42)
xgb_grid <- grid_latin_hypercube(
  finalize(mtry(), dat_train),
  min_n(),
  tree_depth(),
  learn_rate(),
  loss_reduction(),
  sample_size = sample_prop(),
  size = 5
)

# started at 5:53. thru 12 models in 9 minutes.
xgb_res <- tune_grid(
  qb_workflow,
  resamples = qb_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE, verbose = TRUE)
)

xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  dplyr::select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC") +
  theme_minimal()

best_auc <- select_best(xgb_res, "roc_auc")

qb_xgb <- finalize_workflow(
  qb_workflow,
  parameters = best_auc
)

final_mod <- last_fit(qb_xgb, dat_split)

collect_metrics(final_mod)
final_mod %>%
  collect_predictions() %>%
  roc_curve(qb_dropback, .pred_0) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 1.5, color = "midnightblue") +
  xlab('1 - Specificity') +
  ylab('Sensitivity') +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  ) +
  ggtitle('ROC Curve') +
  theme_minimal()

