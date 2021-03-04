
library(magrittr)
.inform <- function(x, ..., .envir = parent.frame()) {
  x <- glue::glue_collapse(x, '\n')
  x <- glue::glue(x, .envir = .envir)
  cli::cat_line(x)
}

timely <- function(f, ..., .name = NULL) {
  
  if(is.null(.name)) {
    .name <- rev(as.character(sys.call()))[1]
  }
  function(...) {
    time_1 <- Sys.time()
    .inform('Starting {.name} at {cli::bg_blue(time_1)}.')
    res <- f(...)
    time_2 <- Sys.time()
    dur <- (time_2 - time_1) %>% lubridate::as.duration()
    dur_s <- dur %>% as.numeric('seconds') %>% scales::comma(accuracy = 0.1)
    dur_m <- dur %>% as.numeric('minutes') %>% scales::comma(accuracy = 0.1)
    parenth <- 
      ifelse(
        as.numeric(dur, 'seconds') >= 31L, 
        glue::glue(' (~{cli::bg_blue(dur_m)} minutes)') %>% as.character(), 
        ''
      )
    .inform('Finished {.name} at {cli::bg_blue(time_2)}. It took {cli::bg_blue(dur_s)} seconds{parenth} to complete.')
    invisible(res)
  }
}

hello <- function(...) {
  'Hello world'
}

hello_timely <- timely(hello)
hello_timely()

dummy_function <- function(.data) {
  # dat <- sys.frames()[[1]]$lhs
  dat <- rev(as.character(sys.call()))[1]
  print(dat)
}
mtcars %>% dummy_function()
mtcars %>% group_by(gear) %>% summarize(n = n()) %>% dummy_function()
