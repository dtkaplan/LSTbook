# fix the data column in the wildfire dataset

Raw <- readr::read_csv("inst/Wildfires/wildfires.csv")
US_wildfires <- Raw |> mutate(when = gsub("(..)$", "-\\1", date),
                       date = lubridate::ym(when),
                       month = lubridate::month(date, label=TRUE)) |>
  select(-when)


save(US_wildfires, file="data/US_wildfires.rda")
