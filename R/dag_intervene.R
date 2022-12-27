#' Convert a DAG to an experiment
#'
#' By an "experiment," we mean a DAG with one of the variables set exogenously. That is, an intervention.
#'
#' Typically, an experiment is done by taking a variable that depends on other variables in the DAG and
#' replacing it with an exogenous expression, that is, a formula where there are no other variables on the right-hand
#' side. But strictly speaking
#'
#' @param DAG An existing DAG
#' @param tilde Tilde expression such as `y ~ binom()`. The left-hand side is the name of the variable
#' whose mechanism is to be changed. The right-hand side is the new mechanism for that variable.
#' @param mix Number between 0 and 1. Fraction of the original mechanism to mix with the experiment. This is
#' just for convenience, since you can specify the mixing "by hand" in `tilde`.
#'
#' @examples
#' dag03
#' dag_intervene(dag03, x ~ binom())
#' dag_intervene(dag03, x ~ roll(c(-1, 0, 2)))
#'
#' @importFrom glue glue
#' @export
dag_intervene <- function(DAG, tilde, mix=0) {
  var_names <- unlist(lapply(DAG, function(x) all.vars(rlang::f_lhs(x))))
  new_name <- all.vars(rlang::f_lhs(tilde))

  if (!new_name %in% var_names) stop("Experimental variable must already exist in DAG.")

  replacement <- which(new_name == var_names)
  if (mix == 0) {
    # Just replace with the new tilde expression
    DAG[[replacement]] <- tilde
  } else {
    if (mix < 0 || mix > 1) stop("Mixing fraction must be between 0.0 and 1.0.")
    exp_part <- deparse(tilde[[3]])
    old_part <- deparse(DAG[[replacement]][[3]])
    new_tilde <- glue::glue("{new_name} ~ {mix}*({old_part}) + ({1-mix})*({exp_part})")
    DAG[[replacement]] <- new_tilde
  }

  DAG
}

