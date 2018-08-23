# bachelor-ratings

echo "# bachelor-ratings" >> README.md
git init
git add README.md
git commit -m "first commit"
git remote add origin https://github.com/leilatwl/bachelor-ratings.git
git push -u origin master

install.packages("rvest")
install.packages("tidyverse")
install.packages("magrittr")
install.packages("scales")
install.packages("knitr")
install.packages("lubridate")

library(rvest)
library(tidyverse)
library(magrittr)
library(scales)
library(knitr)
library(lubridate)


url <- "http://www.imdb.com/title/tt0313038/episodes?season="
timevalues <- c(1:22)

unitedata <- function(x){
  full_url <- paste0(url, x)
  full_url
}

finalurl <- unitedata(timevalues)
finalurl

imdbScrape <- function(x){
  page <- x
  name <- page %>% read_html() %>% html_nodes('#episodes_content strong a') %>% html_text() %>% as.data.frame()
  rating <- page %>% read_html() %>% html_nodes('.ipl-rating-widget > .ipl-rating-star .ipl-rating-star__rating') %>% html_text() %>% as.data.frame()
  details <- page %>% read_html() %>% html_nodes('.zero-z-index div') %>% html_text() %>% as.data.frame()
  
  # combining, naming, classifying our variables
  chart <- cbind(name, rating, details)
  names(chart) <- c("Name", "Rating", "Details")
  chart <- as.tibble(chart)
  return(chart)
  Sys.sleep(5)
}


bachelor <- map_df(finalurl, imdbScrape) #####this is where my code breaks!

bachelor$Season <- str_extract(bachelor$Details, "S[0-9]+")
bachelor$Season <- as.numeric(str_extract(bachelor$Season, "[0-9]+"))

bachelor$Episode <- str_extract(bachelor$Details, "Ep[0-9]+")
bachelor$Episode <- as.numeric(str_extract(bachelor$Episode, "[0-9]+"))

bachelor$Rating <- as.numeric(bachelor$Rating)

bachelor$Details <- NULL


bachelor %>%
  group_by(Season) %>%
  summarise(Rating = mean(Rating)) %>%
  
  ggplot() +

  geom_line(aes(x=Season, y=Rating), color = "Red", size = 1) +
  
  scale_x_continuous(breaks=c(1:22), labels=c(1:22), limits=c(1,22)) +
  
  scale_y_continuous(breaks=c(1:10), labels=c(1:10), limits=c(1,10)) +
  
  geom_line(aes(x=Season, y=mean(Rating)), linetype=2, color = "Black") + 
  annotate("text", x=27, y= 7.45, label = "avg", color = "black", size = 3) +
  
  theme_bw() + 
  labs(title = "Was Arie the Worst Bachelor in Show History?",
       subtitle = "Average Episode Ratings by Season",
       caption = "Source: IMDB, August 2018",
       x = "Season",
       y = "Rating") +
  theme(plot.title = element_text(family='', face = 'bold', colour = 'black', size = 20),
        plot.subtitle = element_text(family='', face = 'italic', colour = 'black', size = 10),
        plot.caption = element_text(family='', colour = 'black', size = 10),
        axis.title.x = element_text(family='', face = 'bold', colour = 'black', size = 12),
        axis.title.y = element_text(family='', colour = 'black', size = 12),
        plot.background = element_rect(fill = "white"))




