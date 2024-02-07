Boston_marathon <- readr::read_csv(
  "/Users/kaplan/Packages/LST/inst/BostonMarathon/Boston-marathon.csv")
foo <- Boston_marathon %>% tidyr::separate(time, c("hour", "min", "sec" ), sep=":",
                                           remove=FALSE, convert=TRUE) %>%
  mutate(minutes = 60*hour + min + sec/60) %>%
  tidyr::separate(name, c("name", "country"), sep="\t") %>%
  select(-hour, -min, -sec)
Boston_marathon <- foo
save(Boston_marathon, file="/Users/kaplan/Packages/LST/data/Boston_marathon.rda")
