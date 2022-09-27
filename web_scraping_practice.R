library(tidyverse)
library(rvest)
library(stringr)
library(stringi)

# books website
books <- read_html("http://books.toscrape.com")

books_all <- books %>% 
  html_nodes('body') %>% 
  html_text()

main <- books %>% 
  html_nodes('.default .page .page_inner .row')

sidebar <- main %>% 
  html_nodes('.sidebar .side_categories .nav li ul') %>% 
  html_text()

temp <- str_split(sidebar, '\\n')
temp2 <- sapply(temp, trimws)
sidebar2 <- temp2[temp2 != '']

all_products <- main %>% 
  html_nodes('.col-sm-8 .row li') %>% 
  html_text()

temp3 <- str_split(all_products , '\\n')
temp4 <- sapply(temp3, trimws)
titles <- temp4[30,]
prices <- temp4[43,]
stock <- temp4[49,]

books_table <- data.frame(titles, prices, stock)
# pretty cool

quotes <- read_html('http://quotes.toscrape.com')

quotes_main <- quotes %>% 
  html_nodes('body .container .row .col-md-8 .quote') %>% 
  html_text()

quotes_main2 <- str_split(quotes_main, '\\n')
quotes_main3 <- sapply(quotes_main2, trimws)
quotes_text <- sapply(quotes_main3, '[[', 2)
quotes_author <- str_replace_all(sapply(quotes_main3, '[[', 3), 'by ', '')

my_paste <- function(vec) {
  trimws(paste(vec[8:length(vec)], collapse = ' '))
}

quotes_tags <- lapply(quotes_main3, my_paste)
quotes_tags2 <- sapply(quotes_tags, '[[', 1)

quotes_table <- tibble(quotes_text, quotes_author, quotes_tags2)

# movies
base_url <- "https://www.the-numbers.com/movie/budgets/all"
base_webpage <- read_html(base_url)

new_urls<- "https://www.the-numbers.com/movie/budgets/all/%s" # add the %s for all pages;

# this only works with a <table> node
table_base <- rvest::html_table(base_webpage)[[1]] %>% 
  tibble::as_tibble(.name_repair = "unique") # repair the repeated columns

#creating two empty dataframes
table_new <-data.frame()
df <- data.frame()

#iterator
i<-101

#it loops through 5501 times so as to extract and then store and then combine about 5000 movies so far extracted.
while (i<5502) {
  new_webpage<- read_html(sprintf(new_urls,i))
  table_new <- rvest::html_table(new_webpage)[[1]] %>% 
    tibble::as_tibble(.name_repair = "unique") # repair the repeated columns
  df<- rbind(df,table_new)
  i=i+100
}

df_movies <- merge(table_base,df, all = T)
