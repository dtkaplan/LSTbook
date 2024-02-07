set.seed(103)
library(dplyr)
Big <- palmerpenguins::penguins |>
  select(species, mass=body_mass_g, flipper=flipper_length_mm, sex) |>
  data.frame(stringsAsFactors = FALSE)
Tiny <- Big |>
  sample_n(size=8) |> as.data.frame(stringsAsFactors = FALSE)
row.names(Tiny) <- NULL

Nats <- tibble::tribble(
  ~country, ~year, ~GDP, ~pop,
  "Korea",  2020, 874, 32,
  "Cuba" ,  2020, 80, 7,
  "France", 2020, 1203, 55,
  "India",  2020, 1100, 1300,
  "Korea",  1950, 100, 32,
  "Cuba" ,  1950, 60, 8,
  "France", 1950, 250, 40,
  "India",  1950, 300, 700,
)

save(Big, Tiny, Nats, file="data/Book-examples.rda")
