

library(tidyverse)
dir_proj <- '20210817'

mic <- file.path(dir_proj, 'michelin.csv') %>% read_csv()
mic
mic %>% count(type, sort = TRUE)
mic %>% filter(type %>% str_detect(''))

world <- map_data('world')
dfr <- map_data('world') %>% select(long, lat, region)
sfr <- sf::st_as_sf(dfr, coords=c('long', 'lat'))
sfc <- rnaturalearth::ne_countries(returnclass='sf')
sf_usa <- sfc %>% filter(adm0_a3 == 'USA')
# sfc$geometry[[1]]
# sfj <- sf::st_join(sfc, sfr)
library(sf)
sf_usa %>% sf::st_intersects(pts)

pts = st_sfc(st_point(c(.5,.5)), st_point(c(1.5, 1.5)), st_point(c(2.5, 2.5)))
pol = st_polygon(list(rbind(c(0,0), c(2,0), c(2,2), c(0,2), c(0,0))))
(lst = st_intersects(pts, pol))
(mat = st_intersects(pts, pol, sparse = FALSE))
# which points fall inside a polygon?
apply(mat, 1, any)
lengths(lst) > 0
# which points fall inside the first polygon?
st_intersects(pol, pts)

ggplot() +
  geom_polygon(
    data = world,
    fill = NA,
    color = 'black',
    aes(x = long, y = lat, group = group)
  ) + 
  geom_point(
    data = mic_usa,
    color = 'red',
    aes(x = lng, y = lat)
  )

mic_usa <- mic %>% filter(lng >= -150, lng <= -50, lat >= 25)
mic_usa
# other stuff ----
tidycensus::census_api_key('2d9e26c5a328e0abf0daeddaa97623285d82d567', overwrite = TRUE)

tidycensus::get_estimates(geography = 'state', product = 'characteristics') -> x

county <- tigris::counties(cb = TRUE, resolution = '20m')
population <- 
  tidycensus::get_estimates(geography = 'county', product = 'population') %>%
  filter(variable == 'POP') %>%
  rename(full_name = NAME) %>%
  arrange(-value)
population

counties <-
  county %>%
  tigris::geo_join(population, by_counties = 'GEOID', by_sp = 'GEOID') %>%
  arrange(-value) %>% 
  slice(1:250) %>% 
  as_tibble()
counties

centers <- sf::st_centroid(counties$geometry) %>% unlist() %>% matrix(ncol = 2, byrow = TRUE)
centers
info <- 
  counties %>% 
  select(county = NAME) %>% 
  bind_cols(centers %>% as_tibble() %>% set_names(c('lon', 'lat'))) %>% 
  distinct()
info

f_safe <- possibly(business_search, otherwise = tibble(), quiet = FALSE)
res_nest <- 
  info %>% 
  mutate(
    data = map2(lat, lon, ~f_safe('restaurant', location = NA, latitude = ..1, longitude = ..2, limit = 50))
  )
res_nest

res <- res_nest %>% unnest(data)
res

res_export <-
  res %>% 
  mutate(
    category_aliases = map_chr(category_aliases, ~glue::glue_collapse(.x, sep = ',') %>% as.character()),
    n_transactions = map_int(transactions, length),
    transactions = map2_chr(n_transactions, transactions, ~ifelse(..1 == 0L, 'none', glue::glue_collapse(..2, sep = ',') %>% as.character())),
    # across(c(category_aliases, transactions), map_chr, glue::glue_collapse, sep = ','),
    across(where(is.character), ~na_if(.x, ''))
  ) %>% 
  select(-matches('phone$'), -matches('address[123]'), -c(category_titles, n_transactions, county, lon, lat))
res_export %>% write_csv(file.path(dir_proj, 'yelp_ratings.csv'), na = '')

