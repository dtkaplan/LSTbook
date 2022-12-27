#' A library of DAGS. These are available in data/.
#'
#'
#'
library(math300)


dag00 <- dag_make(
  x ~ exo(2) + 5,
  y ~ exo(1) - 7
)

dag01 <- dag_make(
  x ~ exo(),
  y ~ 1.5*x + 4.0 + exo()
)


dag02 <- dag_make(
  x ~ exo(),
  a ~ exo(),
  y ~ 3*x - 1.5*a + 5 +  exo()
)

dag03 <- dag_make(
  g ~ exo(),
  x ~ 1.0*g + exo(),
  y ~ 1.0*g + exo()
)

dag04 <- dag_make(
  a ~ exo(),
  b ~ exo(),
  c ~ exo(),
  d ~ a + b + c + exo()
)

dag05 <- dag_make(
  a ~ exo(),
  b ~ a + exo(),
  c ~ b + exo(),
  d ~ c + exo()
)

dag06 <- dag_make(
  a ~ exo(),
  b ~ a + exo(),
  c ~ b + exo(),
  d ~ c + a + exo()
)

dag07 <- dag_make(
  a ~ exo(),
  b ~ exo() - a,
  c ~ a - b + exo(),
  d ~ exo()
)

dag08 <- dag_make(
  c ~ exo(),
  x ~ c + exo(),
  y ~ x + c + 3 + exo()
)

dag09 <- dag_make(
  a ~ exo(),
  b ~ exo(),
  c ~ binom(2*a+ 3*b)
)


# a case-control style data source. There are roughly
# even numbers of 0s and 1s. Only a, b, c have an impact on y
dag10 <- dag_make(
  a ~ exo(),
  b ~ exo(),
  c ~ exo(),
  d ~ exo(),
  e ~ exo(),
  f ~ 2*binom() - 1,
  y ~ binom(2*a - 3*b + c + 0*d + 0*e + 0*f)
)

dag11 <- dag_make(
  x ~ exo(),
  y ~ exo(),
  g ~ x + y + exo()
)

dag12 <- dag_make(
  x ~ exo(),
  y ~ exo(),
  h ~ x + y,
  g ~- h + exo()
)

dag_school1 <- dag_make(
  expenditure ~ unif(7000, 18000),
  participation ~ unif(1,100),
  outcome ~ 1100 + 0.01*expenditure - 4*participation + exo(50)
)

dag_school2 <- dag_make(
  culture ~ unif(-1, 1),
  expenditure ~ 12000 + 4000 * culture + exo(1000),
  participation ~ (50 + 30 * culture + exo(15)) %>%
    pmax(0) %>% pmin(100),
  outcome ~ 1100 + 0.01*expenditure - 4*participation + exo(50)
)

dag_vaccine <- dag_make(
  .h ~ exo(1),
  .v ~ 0.2 + 2* .h + exo(.25),
  .f ~ -0.5 - 0.5 * binom(.v) - 1*.h,
  .s ~ 2 - 0.2*binom(.f) + 0.4*(.h + 0.5),
  died ~ binom(.s, labels=c("yes", "no")),
  vaccinated ~ binom(.v, labels=c("none", "yes")),
  health ~ binom(.h, labels=c("poor", "good")),
  flu ~ binom(.f)
)

save(dag00, dag01, dag02, dag03, dag04, dag05,
     dag06, dag07, dag08, dag09, dag10, dag11, dag12,
     dag_vaccine,
     dag_school1, dag_school2,
     file = "data/daglib.rda")

