# Construct a data frame for the Kristen Gilbert proportions.

set.seed(903)
Gilbert <- tibble(gilbert = c(rep("on_duty", 257), rep("not", 1384)),
                  death = c(rep("yes", 100), rep("no", 157),
                            rep("yes", 357), rep("no", 1027))) %>%
  take_sample() %>%
  select(death, gilbert) # scramble the order
save(Gilbert, file="data/Gilbert.rda")
