#' Fuel economy measurements on US car models
#'
#' @docType data
#'
#' @usage data(MPG)
#'
#' @format A data.frame object with one row year for each model or configuration
#' of automobile or light truck sold in the US.
#'
#' - manufacturer: name of company making the vehicle
#' - division: name of the company division making the vehicle
#' - model: vehicle model name
#' - fuel_year: fuel consumed in 10,000 miles (roughly 1 year.)
#' - CO2_year: Carbon dioxide produced per year, in kilograms. 10,000 miles of driving
#' is taken to represent a year. Note, Carbon-per-year (without the oxygen) is roughly one-quarter the mass of CO2-per-year.
#' - hybrid: whether the car is a hybrid
#' - class: the type of vehicle, e.g. midsize, compact, large, SUV
#' - vol_passenger: volume for passengers (cubic feet)
#' - vol_luggage: volume for luggage (cubic feet)
#' - doors: number of passenger doors

#' - mpg_city: Estimated fuel consumption in city driving (miles per gallon)
#' - mpg_hwy: like `mpg_city` but for highway driving
#' - mpg_comb: like `mpg_city` but for a standard combination of city and highway driving
#' - EPA_fuel_cost: Annual fuel cost using a standard price for gas and a standard miles per year of driving.

#' - valves_exhaust: how many exhaust valves per cylinder
#' - valves_intake: how many air intake valves per cylinder
#' - CO2city: estimate of carbon-dioxide (grams/mile) production per mile in city driving.
#' - CO2hwy: like `CO2city` but for highway driving
#' - CO2combined: like `CO2city` but for a standard mixture of city and highway driving
#' - hatchback: is there a hatchback rear door
# - power: rated engine power in kW (1 kW = 1.341 hp)
#' - start_stop: does the vehicle have a system to stop the engine when idling
#' - cyl_deact: are cylinders in the engine deactivated when power demand warrants
#' - fuel: the kind of fuel used.
#'     - G = regular unleaded gasoline,
#'     - GM = mid-grade recommended,
#'     - GP = premium unleaded recommended,
#'     - GPR = premium unleaded required,
#'     - DU = diesel (ultra low sulfur)
#' - drive: type of drive, e.g. front-wheel, 4-wheel, ...

#' - regen: wheels with regenerative breaking (for hybrids)
#' - n_gears: number of transmission gears
#' - n_cyl: number of engine cylinders
#' - displacement: engine displacement (liters)
#' - transmission: transmission type
#'     - A = automatic,
#'     - M = manual,
#'     - AM  = automated manual,
#'     - AM = automated manual (paddles),
#'     - CVT = continuously variable,
#'     - SCV = continuously variable with selection paddles,
#'     - SA = semi-automatic
#' - lockup_torque_converter:
#' - air_aspiration:
#' - model_year:

#'
#'
#' @keywords datasets
#'
#' @source  Data from the US Environmental Protection Agency (EPA)
#' available at <https://www.fueleconomy.gov/feg/download.shtml>. The file for 2019 model-year
#' vehicles is <https://www.fueleconomy.gov/feg/epadata/19data.zip>
"MPG"
