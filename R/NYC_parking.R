#' Parking summonses in New York City in 2022
#'
#' A 1-in-1000 subset of the parking summonses issued in New York City in 2022.
#'
#' @format
#' A data frame with 16,427 rows and 16 columns:
#' - plate License-plate of the offending vehicle
#' - state Two letter code for US state or Canadian province of the vehicle
#' - license_type Three letter code for the "type" of license plate. Many of the types
#' are in honor of groups, e.g. Purple-Heart veterans. Vanity plates are SRF.
#' - date Date on which the summons was issued.
#' - time Time of day, in decimal hours. 0.5 means 30 minutes past midnight, 15 means 3pm.
#' - violation Type of parking violation, e.g. fire hydrant
#' - fine Dollar amount of fine
#' - penalty Additional dollar amount (for late payment?)
#' - reduction Amount (in dollars) by which fine was reduced. (Mitigating circumstances: see status.)
#' - payment Amount actually paid (as of Aug 1, 2023).
#' - precinct Location in NYC by police precinct
#' - county Somewhat mysterious
#' - agency Which government issued the summons.
#' - status Indicates whether the summons was appealled and, if so, what the outcome was.
#' - summons URL with a facsimile of the actual summons, that is, what gets placed on the windshield.
#'
#'
#' @details Each summons is given a unique ID in the original NYC data. We have omitted this along with "amount due" and "interest amount" variables, as well as the "judgement entry date."
#'
#' R provides a convenient way to open the `summons` URL from the command line: Use
#' `browseURL()` giving the URL (in quotes) as the argument, e.g. `browseURL("http://nycserv.nyc.gov/NYCServWeb/ShowImage?searchID=VDBSck1FMXFZelJOUkVsM1QxRTlQUT09&locationName=_____________________")`
#'
#' The `State_pop_dist` data frame provides data on the distance of states and provinces from NYC and the population
#' of each state or province.
#'
#' @references C Andrade et al. (2023) "Does it pay to park in front of a fire hydrant?" *Significance* **20**(1):28-30
#'
#' @source <https://data.cityofnewyork.us/City-Government/Open-Parking-and-Camera-Violations/nc67-uf89>
"NYC_parking"
