#' A library of DAGS. These are available in data/.
#'
#'
#'
library(LST)


dag00 <- datasim_make(
  x <- rnorm(n, sd=2) + 5,
  y <- rnorm(n, sd=1) - 7
)

dag01 <- datasim_make(
  x <- rnorm(n),
  y <- 1.5*x + 4.0 + rnorm(n)
)


dag02 <- datasim_make(
  x <- rnorm(n),
  a <- rnorm(n),
  y <- 3*x - 1.5*a + 5 +  rnorm(n)
)

dag03 <- datasim_make(
  g <- rnorm(n),
  x <- 1.0*g + rnorm(n),
  y <- 1.0*g + rnorm(n)
)

dag04 <- datasim_make(
  a <- rnorm(n),
  b <- rnorm(n),
  c <- rnorm(n),
  d <- a + b + c + rnorm(n)
)

dag05 <- datasim_make(
  a <- rnorm(n),
  b <- a + rnorm(n),
  c <- b + rnorm(n),
  d <- c + rnorm(n)
)

dag06 <- datasim_make(
  a <- rnorm(n),
  b <- a + rnorm(n),
  c <- b + rnorm(n),
  d <- c + a + rnorm(n)
)

dag07 <- datasim_make(
  a <- rnorm(n),
  b <- rnorm(n) - a,
  c <- a - b + rnorm(n),
  d <- rnorm(n)
)

dag08 <- datasim_make(
  c <- rnorm(n),
  x <- c + rnorm(n),
  y <- x + c + 3 + rnorm(n)
)

dag09 <- datasim_make(
  a <- rnorm(n),
  b <- rnorm(n),
  c <- bernoulli(2*a+ 3*b)
)


# a case-control style data source. There are roughly
# even numbers of 0s and 1s. Only a, b, c have an impact on y
dag10 <- datasim_make(
  a <- rnorm(n),
  b <- rnorm(n),
  c <- rnorm(n),
  d <- rnorm(n),
  e <- rnorm(n),
  f <- 2*bernoulli(rnorm(n)) - 1,
  y <- bernoulli(2*a - 3*b + c - 1.5*d + 1*e + 0.5*f)
)

dag11 <- datasim_make(
  x <- rnorm(n),
  y <- rnorm(n),
  g <- x + y + rnorm(n)
)

dag12 <- datasim_make(
  x <- rnorm(n),
  y <- rnorm(n),
  h <- x + y,
  g ~- h + rnorm(n)
)

dag_prob_21.1 <- datasim_make(
  x <- rnorm(n, sd=1),
  y <- x + rnorm(n, sd=2)
)

dag_school1 <- datasim_make(
  expenditure <-runif(n,7000, 18000),
  participation <-runif(n, 1,100),
  outcome <- 1100 + 0.01*expenditure - 4*participation + rnorm(n, sd=50)
)

dag_school2 <- datasim_make(
  culture <-runif(n, -1, 1),
  expenditure <- 12000 + 4000 * culture + rnorm(n, sd=1000),
  participation <- (50 + 30 * culture + rnorm(n, sd=15)) %>%
    pmax(0) %>% pmin(100),
  outcome <- 1100 + 0.01*expenditure - 4*participation + rnorm(n, sd=50)
)

dag_vaccine <- datasim_make(
  .h <- rnorm(n, sd=1),
  .v <- 0.2 + 2* .h + rnorm(n, sd=.25),
  .f <- -0.5 - 0.5 * bernoulli(.v) - 1*.h,
  .s <- 2 - 0.2*bernoulli(.f) + 0.4*(.h + 0.5),
  died <- bernoulli(.s, labels=c("yes", "no")),
  vaccinated <- bernoulli(.v, labels=c("none", "yes")),
  health <- bernoulli(.h, labels=c("poor", "good")),
  flu <- bernoulli(.f)
)

dag_satgpa <- datasim_make(
  sat <-runif(n, min=400, max=1600),
  gpa <- 4*(pnorm(((sat-1000)/300 + rnorm(n, sd=2.0))/4))^0.6
)

dag_flights <- datasim_make(
  ready <- rep(10, n),
  abortAM <- rbinom(n, ready, .06),
  AM <- ready - abortAM,
  brokeAM  <- rbinom(n, AM, 0.12),
  PM <- AM - brokeAM + rbinom(n, abortAM, 0.67) + rbinom(n, brokeAM, .4),
  abortPM <- rbinom(n, PM, .06),
  brokePM <- rbinom(n, PM - abortPM, .12)
)

dag_medical_observations <- datasim_make(
  .sex <- bernoulli(x=rnorm(n), labels=c("F", "M")),
  .cond <- rnorm(n),
  treatment <- bernoulli(1*(.sex=="F")- .cond, labels=c("none", "treat")),
  outcome <- -0.5*(treatment=="treat") - .cond + 1.5*(.sex=="F") + rnorm(n)
)

list_of_dags <- c(
  lapply(as.list(ls(pattern="dag[_0-9]")), as.name),
  file = "data/daglib.rda")

do.call(save, list_of_dags)

