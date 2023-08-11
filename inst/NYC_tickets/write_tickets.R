# Read and nicely format the NYC Tickets data files

load("inst/NYC_tickets/NYC_tickets_001.rda")

working_file <- Tickets_001

new_names <- c("plate", "state", "licence_type", "skip0",
               "date", "time", "violation", "skip1", "fine",
               "penalty", "skip2", "reduction", "skip3", "payment",
               "precinct", "county", "agency", "status", "summons")

# translate the HH:MMA format to a number
time_to_decimal_hour <- function(t) {
  hour <- as.numeric(substr(t, 1, 2))
  minute <- as.numeric(substr(t, 4, 5))
  PM <- as.numeric(substr(t, 6, 6) == "P")

  12*PM + hour + minute/60
}

names(working_file) <- new_names
working_file2 <- working_file |>
  select(-skip0, -skip1, -skip2, -skip3) |>
  mutate(summons = gsub("^.*\\((http.*)\\)$", "\\1", summons)) |>
  mutate(time = time_to_decimal_hour(time))

NYC_parking <- working_file2
save(NYC_parking, file="data/NYC_parking.rda")
