# Bus delays in NYC

# Idea and code from <https://datascienceplus.com/nyc-bus-delays/>
# Data file on NYC Open Data site: <https://data.cityofnewyork.us/api/views/ez4e-fazm/rows.csv?accessType=DOWNLOAD>

# Downloaded the CSV file and translated to .rda

Raw <- readr::read_csv("~/Downloads/Bus_Breakdown_and_Delays.csv")
save(Raw, file = "Raw.rda")

library(stringr)
#--sequential clean
ii_times <- Raw %>%
  mutate(units_delayed = str_extract(How_Long_Delayed, "[:alpha:]+"),
         time_delayed = as.numeric(str_extract(How_Long_Delayed, "[:digit:]+")),
         units_delayed = ifelse(str_detect(units_delayed, "^[mM]"), "minutes", "other"),
         units_delayed = ifelse(is.na(units_delayed), "other", units_delayed)
  ) %>%
  #remove "in" case, exclude any flagged with "[hH}"
  mutate(
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "in")  & !str_detect(How_Long_Delayed, "[hH]"),"minutes", units_delayed)
  ) %>%
  #deal with "to" case with minutes by taking only the first number. There are apparently no 'to' strings with hours in them.
  mutate(
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "to") & str_detect(How_Long_Delayed, "[mM]"),"minutes", units_delayed)
  ) %>%
  #deal with the - cases which don't involve hours at all
  mutate(
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "-") & !str_detect(How_Long_Delayed, "[hH]"),"minutes", units_delayed)
  ) %>%
  #deal with the double digit minute-1hr:
  mutate(
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "-") & str_detect(How_Long_Delayed, "^[1-5][0-9]"),"minutes", units_delayed)
  ) %>%
  #everything left with a hyphen is a 1 hour or more delay
  mutate(
    time_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "-"), 60, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "-"), "minutes", units_delayed)
  ) %>%
  #-all hyphens dealt with at this point
  #Now work on the "/": first take the minute timers: all those with double digits first
  mutate(
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "/") & str_detect(How_Long_Delayed, "^[1-9][0-9]"), "minutes", units_delayed)
  ) %>%
  #All those starting with 1/2 go to 30 minutes
  mutate(
    time_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1/2"), 30, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1/2"), "minutes", units_delayed)
  ) %>%
  #All those starting with 1 1/2 go to 90 minutes
  mutate(
    time_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1 1/2"), 90, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1 1/2"), "minutes", units_delayed)
  ) %>%
  #---specific cases
  #"1 HOUR1/2"
  mutate(
    time_delayed = ifelse(units_delayed=="other" & How_Long_Delayed == "1 HOUR1/2", 90, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & How_Long_Delayed == "1 HOUR1/2", "minutes", units_delayed)
  ) %>%
  #"@least 1/2"
  mutate(
    time_delayed = ifelse(units_delayed=="other" & How_Long_Delayed == "@least 1/2", 30, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & How_Long_Delayed == "@least 1/2", "minutes", units_delayed)
  ) %>%
  #"1HR/20MIN"
  mutate(
    time_delayed = ifelse(units_delayed=="other" & How_Long_Delayed == "1HR/20MIN", 80, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & How_Long_Delayed == "1HR/20MIN", "minutes", units_delayed)
  )%>%
  #--end specific cases
  #1hr/x case
  #This is modifying things it shouldn't. Why is this even here?
  mutate(
    time_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "1hr"), 60, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "1hr"), "minutes", units_delayed)
  )%>%
  #---cases with ":"
  #remove cases which erroneously report a time instead of a duration, since these are not reliable
  mutate(
    time_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, ":") & str_detect(How_Long_Delayed, "[aA][mM]"), NA, time_delayed)
  )%>%
  #2:hr cases
  mutate(
    time_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^2:[hH][rR]"), 120, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^2:[hH][rR]"), "minutes", units_delayed)
  )%>%
  #1:45 cases
  mutate(
    time_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1:45"), 105, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1:45"), "minutes", units_delayed)
  )%>%
  #1:30 cases
  mutate(
    time_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1:30"), 90, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1:30"), "minutes", units_delayed)
  )%>%
  #1: cases
  mutate(
    time_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1:"), 60, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1:"), "minutes", units_delayed)
  )%>%
  #--end cases with ":"
  #--cases with "^2"
  #2 hr cases
  mutate(
    time_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^2") & str_detect(How_Long_Delayed, "[hH]"), 120, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^2") & str_detect(How_Long_Delayed, "[hH]"), "minutes", units_delayed)
  )%>%
  #double digit minute cases:
  mutate(
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^2[0-9]"), "minutes", units_delayed) #times are already correct
  )%>%
  #---"^1"
  #1 hour cases
  mutate(
    time_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1.*[rR]$") & !(str_detect(How_Long_Delayed, "^1.5.*[rR]$")), 60, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1.*[rR]$") & !(str_detect(How_Long_Delayed, "^1.5.*[rR]$")), "minutes", units_delayed)
  )%>%
  #1.5 hour cases
  mutate(
    time_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1.5"), 90, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1.5"), "minutes", units_delayed)
  )%>%
  #NR: some of the ^1 x min cases going in here are already tagged as minutes.
  #1 hr 30 cases
  mutate(
    time_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1") & str_detect(How_Long_Delayed, "30"), 90, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1") & str_detect(How_Long_Delayed, "30"), "minutes", units_delayed)
  )%>%
  #1 hr 15 cases
  mutate(
    time_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1") & str_detect(How_Long_Delayed, "15"), 75, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1") & str_detect(How_Long_Delayed, "15"), "minutes", units_delayed)
  )%>%
  #1 hr 20 cases
  mutate(
    time_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1") & str_detect(How_Long_Delayed, "20"), 80, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1") & str_detect(How_Long_Delayed, "20"), "minutes", units_delayed)
  )%>%
  #1 hr 45 cases
  mutate(
    time_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1") & str_detect(How_Long_Delayed, "45"), 105, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1") & str_detect(How_Long_Delayed, "45"), "minutes", units_delayed)
  )%>%
  #1 hr half
  mutate(
    time_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1") & str_detect(How_Long_Delayed, "half"), 90, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1") & str_detect(How_Long_Delayed, "half"), "minutes", units_delayed)
  )%>%
  #1 h cases
  mutate(
    time_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1") & str_detect(How_Long_Delayed, "[hH]"), 60, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^1") & str_detect(How_Long_Delayed, "[hH]"), "minutes", units_delayed)
  )%>%
  #---has pattern "^[1-9][oO]", that is, o replaces 0
  mutate(
    time_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^[1-9][oO]"), time_delayed*10, time_delayed),
    units_delayed = ifelse(units_delayed=="other" & str_detect(How_Long_Delayed, "^[1-9][oO]") , "minutes", units_delayed)
  ) %>%
  #---final outliers and rejects
  #just switch to minutes
  mutate(
    units_delayed = ifelse(How_Long_Delayed == "35 SM", "minutes", units_delayed),
    units_delayed = ifelse(How_Long_Delayed == "30am", "minutes", units_delayed),
    units_delayed = ifelse(How_Long_Delayed == "30INS", "minutes", units_delayed),
    units_delayed = ifelse(How_Long_Delayed == "45 late", "minutes", units_delayed),
    units_delayed = ifelse(How_Long_Delayed == "40n", "minutes", units_delayed),
    units_delayed = ifelse(How_Long_Delayed == "45 am", "minutes", units_delayed),
    units_delayed = ifelse(How_Long_Delayed == "45NIN", "minutes", units_delayed),
    units_delayed = ifelse(How_Long_Delayed == "45 INUTES", "minutes", units_delayed),
    units_delayed = ifelse(How_Long_Delayed == "45IN", "minutes", units_delayed)
  ) %>%
  #needs time modifications
  mutate(
    time_delayed = ifelse(How_Long_Delayed == "!0 mins", 1, time_delayed),
    time_delayed = ifelse(How_Long_Delayed == "0-15 Min", 5, time_delayed),
    time_delayed = ifelse(How_Long_Delayed == "0-15 Min", 5, time_delayed),
    time_delayed = ifelse(How_Long_Delayed == "MAYBE 1/2", 30, time_delayed),
    units_delayed = ifelse(How_Long_Delayed == "half hour", "minutes", units_delayed),
    time_delayed = ifelse(How_Long_Delayed == "Half hour", 30, time_delayed),
    units_delayed = ifelse(How_Long_Delayed == "Half hour", "minutes", units_delayed),
    time_delayed = ifelse(How_Long_Delayed == "one hour", 60, time_delayed),
    units_delayed = ifelse(How_Long_Delayed == "one hour", "minutes", units_delayed),
    time_delayed = ifelse(How_Long_Delayed == "ONE HOUR", 60, time_delayed),
    units_delayed = ifelse(How_Long_Delayed == "ONE HOUR", "minutes", units_delayed),
    time_delayed = ifelse(How_Long_Delayed == "HOUR", 60, time_delayed),
    units_delayed = ifelse(How_Long_Delayed == "HOUR", "minutes", units_delayed),
    time_delayed = ifelse(How_Long_Delayed == "hour", 60, time_delayed),
    units_delayed = ifelse(How_Long_Delayed == "hour", "minutes", units_delayed),
    time_delayed = ifelse(How_Long_Delayed == "HR1", 60, time_delayed),
    units_delayed = ifelse(How_Long_Delayed == "HR1", "minutes", units_delayed),
    time_delayed = ifelse(How_Long_Delayed == "1/ 15 min", 75, time_delayed),
    time_delayed = ifelse(How_Long_Delayed == "1:20MIN", 80, time_delayed),
    time_delayed = ifelse(How_Long_Delayed == "1 20 min", 80, time_delayed),
    time_delayed = ifelse(How_Long_Delayed == "1:30 min", 90, time_delayed),
    time_delayed = ifelse(How_Long_Delayed == "1:30?mins", 90, time_delayed),
    time_delayed = ifelse(How_Long_Delayed == "1 30mnts", 90, time_delayed),
    time_delayed = ifelse(How_Long_Delayed == "1 30 min", 90, time_delayed),
    time_delayed = ifelse(How_Long_Delayed == "1:40 MINS", 90, time_delayed),
    time_delayed = ifelse(How_Long_Delayed == "IHR40MIN", 100, time_delayed),
    units_delayed = ifelse(How_Long_Delayed == "IHR40MIN", "minutes", units_delayed),
    time_delayed = ifelse(How_Long_Delayed == "3 HRS", 180, time_delayed),
    units_delayed = ifelse(How_Long_Delayed == "3 HRS", "minutes", units_delayed),
    time_delayed = ifelse(How_Long_Delayed == "3 hr", 180, time_delayed),
    units_delayed = ifelse(How_Long_Delayed == "3 hr", "minutes", units_delayed),
    time_delayed = ifelse(How_Long_Delayed == "3 HOUR", 180, time_delayed),
    units_delayed = ifelse(How_Long_Delayed == "3 HOUR", "minutes", units_delayed),
    time_delayed = ifelse(How_Long_Delayed == "4 hours", 240, time_delayed),
    units_delayed = ifelse(How_Long_Delayed == "4 hours", "minutes", units_delayed),
    time_delayed = ifelse(How_Long_Delayed == "4 hrs", 240, time_delayed),
    units_delayed = ifelse(How_Long_Delayed == "4 hrs", "minutes", units_delayed)
  ) %>%
  #sometimes the record just reads 'mins' or equivalent. My 'units_delayed' picks this up as 'minutes'. We take a guess and set this to 5 minutes.
  mutate(
    time_delayed = ifelse(units_delayed == "minutes" & str_detect(How_Long_Delayed, "[mM][iI][nN]") & is.na(time_delayed), 5, time_delayed)
  ) %>%
  filter(!(is.na(time_delayed))) %>%
  filter(time_delayed <= 300) %>% #Any delays beyond 5 hours are probably typos, but also problematic if real.
  filter(units_delayed == "minutes") %>% #222 820 / 259 637. The rest are garbage or NA.
  select(
    Busbreakdown_ID, School_Year, Route_Number, Schools_Serviced, Bus_Company_Name,
    Has_Contractor_Notified_Schools, Has_Contractor_Notified_Parents, Have_You_Alerted_OPT, Reason,
    Boro, Number_Of_Students_On_The_Bus, School_Age_or_PreK,
    Created_On, Occurred_On,
    time_delayed
  )


Bus_delays <-
  ii_times %>%
  select(
    breakdown_id = "Busbreakdown_ID",
    year = "School_Year",
    route_name = "Route_Number",
    delay_duration = "time_delayed",
    pre_k = "School_Age_or_PreK",
    reason = Reason,
    boro = Boro,
    n_students = "Number_Of_Students_On_The_Bus",
    company = "Bus_Company_Name",
    date = Occurred_On) %>%
  filter(n_students <= 48) %>%
  mutate(date = lubridate::dmy_hms(date))

save(Bus_delays, file = "data/Bus_delays.rda")
