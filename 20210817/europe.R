
library(tidyverse)
library(rvest)
dir_proj <- '20210817'

# wiki countries ----
url <- 'https://en.wikipedia.org/wiki/List_of_European_countries_by_area'
page <- url %>% xml2::read_html()
countries <-
  page %>% 
  html_nodes('.wikitable') %>% 
  html_table() %>% 
  pluck(1) %>% 
  select(country = State) %>% 
  mutate(across(country, ~str_remove(.x, '\\*$'))) %>% 
  add_row(country = 'UK')
countries

# cities ----
cities_db <- 
  maps::world.cities %>%
  as_tibble() %>% 
  set_names(c('city', 'country', 'pop', 'lat', 'lon', 'is_capital')) %>% 
  mutate(across(city, ~str_remove(.x, "^'"))) %>% 
  arrange(desc(pop))
cities_db

# cities_euro <- 
#   cities_db %>% 
#   semi_join(countries)
# countries %>% anti_join(cities_euro %>% distinct(country))
# cities_euro %>% count(country, sort = TRUE)
# 
# top_cities_euro <- cities_euro %>% head(250)
# top_cities_euro %>% count(country, sort = TRUE)

# michelin ----
mic <- file.path(dir_proj, 'michelin.csv') %>% read_csv() %>% mutate(region = year %>% str_remove('MICHELIN Guide ')) %>% select(-c(img, link, year))
mic
#A mic %>% count(year, sort = TRUE)
mic %>% count(type, sort = TRUE)
mic$country <- maps::map.where(
  'world',
  mic$lng,
  mic$lat
)

# mic %>% count(country, sort = TRUE)
mic <- 
  mic %>%
  mutate(
    across(country, ~ifelse(is.na(.x) & between(lng, -40, 50) & lat >= 35, 'Europe', .x))
  )
mic
# mic %>% filter(region == country)
# mic %>% distinct(country)
# mic %>% filter(country %>% str_detect('USA')) %>% filter(lng %>% between(-10, 30))
mic_euro <- mic %>% semi_join(countries %>% add_row(country = 'Europe')) %>% mutate(idx = row_number()) %>% relocate(idx)
mic_euro
mic_countries <- mic_euro %>% distinct(country)
# mic %>% filter(country %>% str_detect('Turk'))
mic_countries

# yelp ----
f_safe <- possibly(yelp::business_search, otherwise = tibble(), quiet = FALSE)

dir_data <- file.path(dir_proj, 'data')
fs::dir_create(dir_data)
do_scrape <- function(city, country, lat, lon) {
  path <- file.path(dir_data, sprintf('%s-%s.rds', str_replace_all(city, '\\/', '_'), str_replace_all(country, '\\/', '_')))
  if(file.exists(path)) {
    cat(glue::glue('{Sys.time()}: Returning early'), sep = '\n')
    return(read_rds(path))
  }
  res <- f_safe('restaurant', location = NA, latitude = lat, longitude = lon, limit = 50)
  cat(glue::glue('{Sys.time()} Retrieved for {city}, {country}.'), sep = '\n')
  write_rds(res, path)
  res
}
# APG_9lB0XUPfnFtW8c2euA
yelp::store_access_token("DW4rR-HA2A4cRL20zxefH7lADdRb7tQmimexUyW4nB65lPtYpsOcWtB7XqhxPMi3h8yzTAq-uTbRML1KosVqnPqEaHmjloZeG-O2KAvynhqe7mvzESvZP4-ge_gOYXYx")
# mic_countries %>% anti_join(cities_db)
cities_euro_mic <-
  cities_db %>% 
  semi_join(mic_countries) %>% 
  # count(country, sort = TRUE)
  head(1000)
cities_euro_mic %>% tail()

yelp_nest <- 
  cities_euro_mic %>% 
  mutate(
    data = pmap(list(city, country, lat, lon), do_scrape)
  )
yelp_nest

yelp <- yelp_nest %>% select(-c(city, country)) %>% unnest(data)
yelp

yelp_export <-
  yelp %>% 
  mutate(
    n_category_titles = map_int(category_titles, length),
    category_titles = map2_chr(n_category_titles, category_titles, ~ifelse(..1 == 0L, 'none', glue::glue_collapse(..2, sep = ',') %>% as.character())),
    n_transactions = map_int(transactions, length),
    transactions = map2_chr(n_transactions, transactions, ~ifelse(..1 == 0L, 'none', glue::glue_collapse(..2, sep = ',') %>% as.character()))
  ) %>% 
  select(city, country, population = pop, is_capital, latitude, longitude, alias, name, rating, review_count, category_titles, transactions)
yelp_export
# yelp_export$country_map <- maps::map.where(
#   'world',
#   yelp_export$longitude,
#   yelp_export$latitude
# )
yelp_export %>% write_csv(file.path(dir_proj, 'yelp_ratings_europe.csv'), na = '')

# eda ----
mic_both <- bind_rows(mic %>% mutate(src = 'orig'), mic_euro %>% mutate(src = 'tony'))
world <- map_data('world')
ratings <- bind_rows(mic_euro %>% select(country, lat, lng) %>% mutate(is_mic = TRUE), yelp_export %>% select(country, lat = latitude, lng = longitude) %>% mutate(is_mic = FALSE))
ratings

yelp_export %>% 
  filter(longitude %>% between(-10, 30)) %>% 
  filter(latitude %>% between(35, 70))

ggplot() +
  geom_polygon(
    data = world,
    fill = NA,
    color = 'black',
    aes(x = long, y = lat, group = group)
  ) + 
  geom_point(
    data = yelp_export,
    # show.legend = FALSE,
    aes(x = longitude, y = latitude, color = country)
  )+
  coord_cartesian(xlim = c(-10, 30), ylim = c(35, 65))

ggplot() +
  geom_polygon(
    data = world,
    fill = NA,
    color = 'black',
    aes(x = long, y = lat, group = group)
  ) + 
  geom_point(
    data = mic_euro,
    # show.legend = FALSE,
    aes(x = lng, y = lat, color = country)
  ) +
  coord_cartesian(xlim = c(-10, 30), ylim = c(35, 65))

ggplot() +
  geom_polygon(
    data = world,
    fill = NA,
    color = 'black',
    aes(x = long, y = lat, group = group)
  ) + 
  geom_point(
    data = ratings,
    # show.legend = FALSE,
    aes(x = lng, y = lat, color = country)
  ) +
  facet_wrap(~is_mic) +
  coord_cartesian(xlim = c(-10, 30), ylim = c(35, 65))

p <-
  ggplot() +
  geom_polygon(
    data = world,
    fill = NA,
    color = 'black',
    aes(x = long, y = lat, group = group)
  ) + 
  geom_point(
    data = mic_both,
    show.legend = FALSE,
    aes(x = lng, y = lat, color = country)
  ) +
  facet_wrap(~src) +
  coord_cartesian(xlim = c(-10, 30), ylim = c(35, 65))
p


# match mic ----
mic_euro %>% filter(country == 'France') %>% arrange(desc(rating))
yelp_export %>% filter(name %>% str_detect('Les Pres'))
yelp::business_match("Les Prés d'Eugénie - Michel Guérard", city = 'Eugénie les Bains', country = 'France', state = NA_character_)
mic_euro %>% filter(location %>% str_detect('Naurath'))
mic_yelp_nest <- 
  mic_euro %>% 
  select(idx, location, country, lat, lng) %>% 
  mutate(
    data = pmap(list(location, country, lat, lng), do_scrape)
  )

mic_yelp_nest

mic_yelp <- mic_yelp_nest %>% select(-c(location, country)) %>% unnest(data)
mic_yelp

mic_yelp_export <-
  mic_yelp %>% 
  mutate(
    n_category_titles = map_int(category_titles, length),
    category_titles = map2_chr(n_category_titles, category_titles, ~ifelse(..1 == 0L, 'none', glue::glue_collapse(..2, sep = ',') %>% as.character())),
    n_transactions = map_int(transactions, length),
    transactions = map2_chr(n_transactions, transactions, ~ifelse(..1 == 0L, 'none', glue::glue_collapse(..2, sep = ',') %>% as.character()))
  ) %>% 
  select(idx, country, latitude, longitude, alias, name, rating, review_count, category_titles, transactions) %>% 
  left_join(mic_euro %>% select(idx, name_mic = name, region, lat, lng))
mic_yelp_export %>% distinct(idx)

z <-
  mic_yelp_export %>% 
  mutate(
    dlat = latitude - lat,
    dlng = longitude - lng,
    d = sqrt(dlat^2 + dlng^2)
  ) %>% 
  # group_by(idx) %>% 
  # slice_min(d, with_ties = FALSE) %>% 
  # ungroup() %>% 
  select(-c(dlat, dlng)) %>% 
  mutate(across(d, ~.x*100)) %>% 
  select(idx, name, name_mic, d) %>% 
  # arrange(d)
  filter(d < 1)
z

res_fuz <-
  join_fuzzily(
    mic_euro %>% select(name),
    mic_yelp_export %>% select(name),
    col = 'name',
    suffix = c('orig', 'mic')
  )
res_fuz


z %>% ggplot() + aes(x = d) + geom_histogram()
filter(round(latitude, 3) == round(lat, 3), round(longitude, 3) == round(lng, 3))
mic_yelp_export %>% filter(idx == 7) %>% filter(name %>% str_detect('nder'))
mic_yelp_export %>% 
  filter(name_mic == name)

mic_yelp_export %>% 
  group_by(location, country, lat, lng)
mic_yelp_nest %>% unnest(data)
mic %>% filter(country %>% str_detect('Roman'))
countries %>% anti_join(mic_euro %>% distinct(country))
mic_euro %>% filter(country %>% str_detect('King'))
mic_euro %>% count(country, sort = TRUE)
