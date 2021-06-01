
library(memer)

meme_list()
m <-
  meme_get('what-is-grief') %>% 
  meme_text_top('What is sliced') %>% 
  meme_text_bottom('if not contestants persevering')
m
magick::image_write(m, 'whatisgrief.png')
