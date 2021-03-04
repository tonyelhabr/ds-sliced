
df2mat <- function(data) {
  model.matrix(
    ~.+0,
    data =
      model.frame(
        ~.+0,
        data,
        na.action = na.pass
      )
  )
}

.postprocess_xgb_cv_res <- function(params_lst, fit_cv, eval_metrics) {
  
  .eval_metric <- eval_metrics[1]
  col_trn <- sprintf('train_%s_mean', .eval_metric)
  col_trn_params_lst <- sprintf('%s_trn', .eval_metric)
  col_trn_sym <- col_trn %>% sym()
  col_trn_params_lst_sym <- col_trn_params_lst %>% sym()
  col_tst <- sprintf('test_%s_mean', .eval_metric)
  col_tst_params_lst <- sprintf('%s_tst', .eval_metric)
  col_tst_sym <- col_tst %>% sym()
  col_tst_params_lst_sym <- col_tst_params_lst %>% sym()
  
  params_lst$iter <- fit_cv$best_iteration
  params_lst[[col_trn_params_lst]] = fit_cv$evaluation_log[params_lst$iter, ][[col_trn]]
  params_lst[[col_tst_params_lst]] = fit_cv$evaluation_log[params_lst$iter, ][[col_tst]]
  params_lst[['eval_metric']] <- NULL
  # The `bind_rows()` here is just to reduce from a list to a df
  res <- bind_rows(params_lst)
  res
}

tune_xgb_cv <-
  function(grid_params,
           x_dmat,
           booster,
           objective,
           eval_metrics,
           ...) {
    
    .get_metrics <- function(params, idx = 1) {
      
      .display_info('Row {cli::bg_black(idx)}')
      params_lst <-
        list(
          booster = booster,
          objective = objective,
          eval_metric = eval_metrics,
          eta = params$learn_rate,
          gamma = params$loss_reduction,
          subsample = params$sample_size,
          colsample_bytree = params$mtry,
          max_depth = params$tree_depth,
          min_child_weight = params$min_n
        )
      
      # browser()
      fit_cv <-
        xgboost::xgb.cv(
          data = x_dmat,
          params = params_lst,
          metrics = eval_metrics,
          ...
        )
      res <- .postprocess_xgb_cv_res(params_lst, fit_cv, eval_metrics)
      res
    }
    
    res <-
      grid_params %>%
      nest(params = -c(idx)) %>%
      mutate(metrics = purrr::map2(params, idx, ~.get_metrics(params = ..1, idx = ..2))) %>%
      select(-params) %>%
      unnest(metrics)
    res
  }

augment_preds <-
  function(v,
           data,
           cols_id = 'idx',
           cols_extra = NULL,
           col_y,
           f_trans = NULL) {
    col_y_sym <- col_y %>% sym()
    preds <-
      v %>%
      tibble(.pred = .) %>%
      bind_cols(
        data %>%
          select(
            all_of(cols_id),
            one_of(cols_extra),
            all_of(col_y)
          )
      )
    
    if (!is.null(f_trans) & is.function(f_trans)) {
      preds <-
        preds %>%
        mutate(across(.pred, f_trans))
    }
    preds
  }

shap_xgb <- function(fit, x_mat, preds, col_y, cols_id) {
  
  suppressWarnings(
    feature_values_init <-
      x_mat %>%
      as.data.frame() %>% 
      mutate_all(scale) %>%
      gather('feature', 'feature_value') %>%
      as_tibble()
  )
  feature_values_init
  
  feature_values <-
    feature_values_init %>%
    pull(feature_value)
  feature_values
  
  shap_init <-
    fit %>%
    predict(newdata = x_mat, predcontrib = TRUE) %>%
    as.data.frame() %>%
    as_tibble() %>%
    rename(baseline = BIAS)
  shap_init
  
  shap <-
    shap_init %>%
    bind_cols(
      preds %>%
        select(!!!syms(cols_id), .pred, .actual = !!sym(col_y))
    )
  shap
}
