# read the world record data from Wikipedia
library(rvest)
library(dplyr)
library(lubridate)
library(readr)
page <- "https://en.wikipedia.org/wiki/World_record_progression_100_metres_butterfly"
page <- "https://en.wikipedia.org/wiki/World_record_progression_200_metres_butterfly"
table_nodes <- page %>%
  read_html() %>%
  html_nodes("table")
table_list <-
  html_table(table_nodes, fill = TRUE)

parse_time <- function(str) {
  str <- gsub("\\[.+\\]", "", str)
  str <- gsub("^([0-9]{2})", "0:\\1", str)
  lapply(strsplit(str, ":", fixed = TRUE),
         FUN = function(x) parse_number(x[1]) * 60 + parse_number(x[2])) %>%
    unlist()
}

sex <- c("M", "M", "F", "F")
lengths <- c(2, 4, 2, 4) * 2
dist <- rep(50,4)  # Change to length being read in
res <- NULL
for (k in 1:4) {
  tmp <- table_list[[k]] %>%
    rename(time = Time, swimmer = Swimmer, date = Date, place = Place) %>%
    mutate(sex = sex[k], lengths = lengths[k], dist = dist[k]) %>%
    mutate(time = parse_time(time)) %>%
    mutate(date = lubridate::mdy(date))
  res <- bind_rows(res, tmp)
}


# save(Butterfly, file = "../data/Butterfly.Rda")
