
load(url("https://github.com/topepo/FES/blob/master/Data_Sets/Chicago_trains/chicago.RData?raw=true"))

pca_on_stations <- 
  recipe(~ ., data = training %>% select(starts_with("l14_"))) %>% 
  step_center(all_numeric_predictors()) %>% 
  step_scale(all_numeric_predictors()) %>%
  # step_pca(all_predictors(), num_comp = 5) %>% 
  embed::step_umap(all_numeric_predictors(), num_comp = 2) %>% 
  prep() %>% 
  juice()
# training
pca_on_stations

pca_on_stations %>% 
  mutate(idx = row_number()) %>% 
  # pivot_longer(
  #   -idx
  # ) %>% 
  ggplot() +
  aes(x = umap_1, y = umap_2) +
  geom_point(alpha = 0.7)

biopsy <- read_csv("https://wilkelab.org/classes/SDS348/data_sets/biopsy.csv")

pca_fit <- biopsy %>% 
  select(where(is.numeric)) %>% # retain only numeric columns
  prcomp(scale = TRUE, center = TRUE, rank = 5)
biopsy
pca_fit
pca_fit$rotation %>%
  as.data.frame() %>%
  mutate(column = row.names(.)) %>%
  pivot_longer(matches('^PC')) %>%
  arrange(name, column) %>%
  pivot_wider(names_from = "name", values_from = "value") 
