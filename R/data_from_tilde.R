#' Evaluate a tilde expression on a data frame
#'

split_tilde <- function(.tilde) {
  if (is.null(.tilde)) return(c())
  if (is.name(.tilde)) return(.tilde)
  if (length(.tilde) < 3) {
    return(.tilde)
  } else if (inherits(.tilde, "formula")) {
    return(list(left = split_tilde(.tilde[[2]]),
                right = split_tilde(.tilde[[3]])))
  } else if (! deparse(.tilde[[1]]) %in% c("+", "-", "*")) .tilde
    else c(split_tilde(.tilde[[2]]), split_tilde(.tilde[[3]]))
}


#'
eval_exp_list <- function(EL, .data) {
  if (is.name(EL) || is.call(EL)) {
    EL <- list(EL)
  }
  res <- list()
  for (k in 1:length(EL)) {
    tmp <- try(eval(EL[[k]], envir = .data), silent = TRUE)
    if (inherits(tmp, "try-error"))
      stop(best_var_name_match(names(.data), tmp),
                               #gsub(".*object '(.*)' not found.*", "\\1", tmp)),
           call. = FALSE)
    res[[k]] <- as_tibble(tmp, .name_repair = "minimal")
    if (ncol(res[[k]]) == 1) {
      names(res[[k]]) <- deparse(EL[[k]])
    } else {
      # just pull out the variable as one column, leaving the multi-column
      # stuff for the annotation based on the model
      the_var <- all.vars(EL[[k]])[1]
      res[[k]] <- .data[the_var]
      names(res[[k]]) <- the_var
    }
  }

  # We allow duplicate names, but start them with dots
  # Turn into a data frame and restore the names if there were any duplicates.
  Res <- bind_cols(res, .name_repair="minimal")
  fixed_names <- unlist(lapply(res, names))
  # just in case there is a duplicate, or more than one
  fixed_names <- ifelse(duplicated(fixed_names), paste0(".", fixed_names), fixed_names)
  fixed_names <- ifelse(duplicated(fixed_names), paste0(".", fixed_names), fixed_names)
  fixed_names <- ifelse(duplicated(fixed_names), paste0(".", fixed_names), fixed_names)

  names(Res) <- fixed_names

  Res
}

# create a data frame from a tilde expression with one column
# for each high-level modeling term +, -, or *. The columns will be named
# for the modeling term, e.g. "log(mt)" or "ntiles(wt, 3)".
data_from_tilde <- function(.data, .tilde) {
  tmp <- split_tilde(.tilde)
  if ("right" %in% names(tmp)) {
    # Turn each into a data frame
    Left <- eval_exp_list(tmp$left, .data)
    Right <- eval_exp_list(tmp$right, .data)
    cbind(Left, Right) # using cbind() to avoid the name repair in bind_cols()
  } else {
    eval_exp_list(tmp, .data)
  }
}

get_error_object_name <- function(msg) {
  gsub(".* : object '(.*)' not found\n", "\\1", msg)
}

best_var_name_match <- function(nms, msg) {
  obj_name <- get_error_object_name(msg)
  starter <- glue::glue("`{obj_name}` not found among variable names.")
  if (requireNamespace("stringdist", quietly = TRUE)) {
    best <- nms[stringdist::amatch(obj_name, nms, maxDist=5)][1]
    if (is.na(best)) return(starter)
    glue::glue("{starter}  Perhaps you meant `{best}`?")
  } else {
    starter
  }
}
