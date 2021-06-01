
library(tidyverse)

dir_data <- '20210302'
df_tst <-
  file.path(dir_data, 'sliced-s00e01-holdout.csv') %>%
  read_csv() %>%
  select(-1)
df_tst

df_actual <-
  file.path(dir_data, 'Speed Dating Data.csv') %>%
  read_csv(guess_max = 20000) %>% 
  filter(wave == 11)
df_actual

nms1 <- df_tst %>% names()
nms2 <- df_actual %>% names()
setdiff(nms1, nms2)
setdiff(nms2, nms1)

df_actual %>% 
  # select(-matches('^dec')) %>% 
  select(all_of(nms1), match) %>% 
  # emulate row index
  mutate(X1 = row_number()) %>% 
  relocate(X1) %>% 
  write_csv(file.path(dir_data, 'sliced-s00e01-holdout-w-actual.csv'), na = '')
