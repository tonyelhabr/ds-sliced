

.display_info <- function(x, ..., .envir = parent.frame(), .verbose = TRUE, .f_glue = glue::glue_collapse) {
  if (!.verbose) {
    return(invisible(x))
  }
  x <- .f_glue(x, '\n')
  x <- glue::glue(x, .envir = .envir)
  cli::cat_line(x)
}


.display_warning <- function(x, ..., .envir = parent.frame()) {
  x <- glue::glue_collapse(x, '\n')
  x <- glue::glue(x, .envir = .envir)
  warning(x, call. = FALSE, immediate. = TRUE)
}


.display_error <- function(x, ..., .envir = parent.frame()) {
  x <- glue::glue_collapse(x, '\n')
  x <- glue::glue(x, .envir = .envir)
  cnd <- structure(class = c('usethis_error', 'error', 'condition'), list(message = x))
  stop(cnd)
}

.time_it <- function(f, ..., .name = NULL, .verbose = .get_verbose()) {
  
  if(is.null(.name)) {
    .name <- rev(as.character(sys.call()))[1]
  }
  
  function(...) {
    time_1 <- Sys.time()
    .display_info('Starting {cli::bg_black(.name)} at {cli::bg_black(time_1)}.', .verbose = .verbose)
    res <- f(...)
    time_2 <- Sys.time()
    dur <- (time_2 - time_1) %>% lubridate::as.duration()
    dur_s <- dur %>% as.numeric('seconds') %>% scales::comma(accuracy = 0.1)
    dur_m <- dur %>% as.numeric('minutes') %>% scales::comma(accuracy = 0.1)
    parenth <-
      ifelse(
        as.numeric(dur, 'seconds') >= 31L,
        glue::glue(' (~{cli::bg_black(dur_m)} minutes)') %>% as.character(),
        ''
      )
    .display_info('Finished {cli::bg_black(.name)} at {cli::bg_black(time_2)}. It took {cli::bg_black(dur_s)} seconds{parenth} to complete.', .verbose = .verbose)
    invisible(res)
  }
}

do_get <-
  function(...,
           f = NULL,
           file = tempfile(),
           ext = 'rds',
           dir = getwd(),
           path = NULL,
           f_import = rio::import,
           f_export = rio::export,
           append = FALSE,
           export = TRUE,
           overwrite = FALSE) {
    path <- .generate_path(path = path, dir = dir, file = file, ext = ext)
    path_exists <- path %>% file.exists()
    
    if(!path_exists & append) {
      .display_warning('Can\'t append to `path = "{path}` since it doesn\t exist!')
    }
    
    # if(!path_exists & overwrite) {
    #   .display_warning('Can\'t overwrite `path = "{path}` since it doesn\t exist!')
    # }
    
    if(path_exists & !overwrite & !append) {
      .display_info('Importing from `path = "{path}"`.')
      return(f_import(path))
    }
    
    if(path_exists & append) {
      if(!export) {
        .display_warning('Setting `export = TRUE` since `append = TRUE` take higher priority.')
        export <- TRUE
      }
      .display_info('Importing from `path = "{path}"` for appending.')
      res_init <- f_import(path)
    }
    
    f_safe <- purrr::safely(f, otherwise = NULL)
    res <- f_safe(...)
    if (is.null(res)) {
      .display_error('Something went wrong with function call `f`!')
    } else {
      res <- res$result
    }
    
    if(append) {
      res <- list(res_init, res) %>% purrr::map(rbind)
    }
    
    if(!export) {
      return(res)
    }
    
    dir <- dirname(path)
    if(!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
    f_export(res, path)
    .display_info('Exported to `path = "{path}"`.')
    res
    
  }


