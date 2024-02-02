#' A library of simulations. These are available in data/.
#'
#'
#'
library(LSTbook)


sim_00 <- datasim_make(
  x <- rnorm(n, sd=2) + 5,
  y <- rnorm(n, sd=1) - 7
)

sim_01 <- datasim_make(
  x <- rnorm(n),
  y <- 1.5*x + 4.0 + rnorm(n)
)


sim_02 <- datasim_make(
  x <- rnorm(n),
  a <- rnorm(n),
  y <- 3*x - 1.5*a + 5 +  rnorm(n)
)

sim_03 <- datasim_make(
  g <- rnorm(n),
  x <- 1.0*g + rnorm(n),
  y <- 1.0*g + rnorm(n)
)

sim_04 <- datasim_make(
  a <- rnorm(n),
  b <- rnorm(n),
  c <- rnorm(n),
  d <- a + b + c + rnorm(n)
)

sim_05 <- datasim_make(
  a <- rnorm(n),
  b <- a + rnorm(n),
  c <- b + rnorm(n),
  d <- c + rnorm(n)
)

sim_06 <- datasim_make(
  a <- rnorm(n),
  b <- a + rnorm(n),
  c <- b + rnorm(n),
  d <- c + a + rnorm(n)
)

sim_07 <- datasim_make(
  a <- rnorm(n),
  b <- rnorm(n) - a,
  c <- a - b + rnorm(n),
  d <- rnorm(n)
)

sim_08 <- datasim_make(
  c <- rnorm(n),
  x <- c + rnorm(n),
  y <- x + c + 3 + rnorm(n)
)

sim_09 <- datasim_make(
  a <- rnorm(n),
  b <- rnorm(n),
  c <- bernoulli(logodds = 2*a+ 3*b)
)


# a case-control style data source. There are roughly
# even numbers of 0s and 1s. Only a, b, c have an impact on y
sim_10 <- datasim_make(
  a <- rnorm(n),
  b <- rnorm(n),
  c <- rnorm(n),
  d <- rnorm(n),
  e <- rnorm(n),
  f <- 2*bernoulli(logodds = rnorm(n)) - 1,
  y <- bernoulli(logodds = 2*a - 3*b + c - 1.5*d + 1*e + 0.5*f)
)

sim_11 <- datasim_make(
  x <- rnorm(n),
  y <- rnorm(n),
  g <- x + y + rnorm(n)
)

sim_12 <- datasim_make(
  x <- rnorm(n),
  y <- rnorm(n),
  h <- x + y,
  g ~- h + rnorm(n)
)

sim_prob_21.1 <- datasim_make(
  x <- rnorm(n, sd=1),
  y <- x + rnorm(n, sd=2)
)

sim_school1 <- datasim_make(
  expenditure <-runif(n,7000, 18000),
  participation <-runif(n, 1,100),
  outcome <- 1100 + 0.01*expenditure - 4*participation + rnorm(n, sd=50)
)

sim_school2 <- datasim_make(
  culture <-runif(n, -1, 1),
  expenditure <- 12000 + 4000 * culture + rnorm(n, sd=1000),
  participation <- (50 + 30 * culture + rnorm(n, sd=15)) %>%
    pmax(0) %>% pmin(100),
  outcome <- 1100 + 0.01*expenditure - 4*participation + rnorm(n, sd=50)
)

sim_vaccine <- datasim_make(
  .h <- rnorm(n, sd=1),
  .v <- 0.2 + 2* .h + rnorm(n, sd=.25),
  .f <- -0.5 - 0.5 * bernoulli(logodds = .v) - 1*.h,
  .s <- 2 - 0.2*bernoulli(logodds = .f) + 0.4*(.h + 0.5),
  died <- bernoulli(logodds = .s, labels=c("yes", "no")),
  vaccinated <- bernoulli(logodds = .v, labels=c("none", "yes")),
  health <- bernoulli(logodds = .h, labels=c("poor", "good")),
  flu <- bernoulli(logodds = .f)
)

sim_satgpa <- datasim_make(
  sat <-runif(n, min=400, max=1600),
  gpa <- 4*(pnorm(((sat-1000)/300 + rnorm(n, sd=2.0))/4))^0.6
)

sim_flights <- datasim_make(
  ready <- 10,
  abortAM <- rbinom(n, ready, .06),
  AM <- ready - abortAM,
  brokeAM  <- rbinom(n, AM, 0.12),
  PM <- AM - brokeAM + rbinom(n, abortAM, 0.67) + rbinom(n, brokeAM, .4),
  abortPM <- rbinom(n, PM, .06),
  brokePM <- rbinom(n, PM - abortPM, .12)
)


sim_medical_observations <- datasim_make(
  .sex <- bernoulli(n=n, labels=c("F", "M")),
  .cond <- rnorm(n),
  treatment <- bernoulli(logodds=1*(.sex=="F")- .cond, labels=c("none", "treat")),
  outcome <- -0.5*(treatment=="treat") - .cond + 1.5*(.sex=="F") + rnorm(n)
)

list_of_sims <- c(
  lapply(as.list(ls(pattern="sim[_0-9]")), as.name),
  file = "data/simlib.rda")

do.call(save, list_of_sims)

