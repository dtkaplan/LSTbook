# Copied from mosaic do.R in order to separate it from other dependencies
# that prevent mosaic from working with WebR


utils::globalVariables(c('.row'))
require(parallel, quietly = TRUE)
require(compiler, quietly = TRUE)
parallel::detectCores()
NA


# Set seed in parallel compatible way
#
# When the parallel package is used, setting the RNG seed for reproducibility
# involves more than simply calling [set.seed()]. `set.rseed` takes
# care of the additional overhead.
#
# @param seed seed for the random number generator
# @details
# If the `parallel` package is not on the search path, then [set.seed()] is called.
# If `parallel` is on the search path, then the RNG kind is set to `"L'Ecuyer-CMRG"`,
# the seed is set and `mc.reset.stream` is called.
#
# @examples
# # These should give identical results, even if the `parallel' package is loaded.
# set.rseed(123); do(3) * resample(1:10, 2)
# set.rseed(123); do(3) * resample(1:10, 2)
# ## @export

mosaic_set.rseed <- function(seed) {
  if ("package:parallel" %in% search()) {
    set.seed(seed, kind = "L'Ecuyer-CMRG")
    parallel::mc.reset.stream()
  } else {
    set.seed(seed)
  }
}


#
# @keywords internal
# @details
# `.make.data.frame` converts things to a data frame
# @param x object to be converted
# @return a data frame

.make.data.frame <- function( x ) {
  if (is.data.frame(x)) return(x)
  if (is.vector(x)) {
    nn <- names(x)
    result <- as.data.frame( matrix(x, nrow=1) )
    if (! is.null(nn) ) names(result) <- nn
    return(result)
  }
  return(as.data.frame(x))
}



null2na <- function(x) if (is.null(x)) NA else x

# Repeater objects
#
# Repeater objects can be used with the `*` operator to repeat
# things multiple time using a different syntax and different output
# format from that used by, for example, [replicate()].
#
#
# @name repeater-class
# @seealso [do()]
# @section Slots:
# \describe{
#   \item{`n`:}{Object of class `"numeric"` indicating how many times to repeat something.}
#   \item{`cull`:}{Object of class `"function"` that culls the output from each repetition.}
#   \item{`mode`:}{Object of class `"character"` indicating the output mode
#   ('default', 'data.frame', 'matrix', 'vector', or 'list').  For most purposes 'default' (the default)
#   should suffice.}
#   \item{`algorithm`:}{an algorithm number.}
#   \item{`parallel`:}{a logical indicating whether to attempt parallel execution.}
# }



# old version
if(FALSE) {
  .merge_data_frames <- function(a, b) {
    a <- .make.data.frame(a)
    b <- .make.data.frame(b)
    if (nrow(b) < 1) return (a)
    if (nrow(a) < 1) return (b)

    a$mosaic_merge_id <- paste('A',1:nrow(a))
    b$mosaic_merge_id <- paste('B',1:nrow(b))
    result <- merge(a,b,all=TRUE)
    w <- which(names(result) == 'mosaic_merge_id')
    result <- result[, -w]
    return(result)
  }
}


#
# @keywords internal
# @details `.merge_data_frames` is a wrapper around merge
#
# @param a a data frame
# @param b a data frame
#
# @return a data frame

.merge_data_frames = function(a,b) {
  a <- .make.data.frame(a)
  b <- .make.data.frame(b)
  if (nrow(b) < 1) return (a)
  if (nrow(a) < 1) return (b)
  missing.from.b = setdiff(names(a),names(b))
  missing.from.a = setdiff(names(b),names(a))
  for (var in missing.from.b) b[[var]] = NA
  for (var in missing.from.a) a[[var]] = NA
  dplyr::bind_rows(a,b)
}


#
# @keywords internal
# @details
# `.squash_names` squashes names of a data frame into a single string
#
# @param object an object
# @param sep a character
#
# @return a character vector

.squash_names <- function(object,sep=":") {
  if ( ncol(object) < 1 ) {return(rep("",nrow(object)))}

  result <- object[,1]
  if ( ncol(object) < 2 ) {return(as.character(result))}

  for (c in 2:ncol(object)) {
    result <- paste(result, as.character(object[,c]), sep=sep)
  }
  return(result)

}


#
# @param x an object created by `do`.

.list2tidy.data.frame <- function (l) {

  # see if we really just have a vector
  ul <- unlist( l )
  if ( length(ul) == length(l) ) {
    result <- data.frame(..result.. = as.vector(ul))
    row.names(result) <- NULL
    if( !is.null(names(l[[1]])) ) names(result) <- names(l[[1]])
    return(result)
  }

  # if each element is a data frame, combine them with bind_rows
  if ( all( sapply( l, is.data.frame ) ) ) {
    return(
      lapply(l, function(x) {mutate(x, .row= 1:n())}) %>%
        dplyr::bind_rows() %>%
        mutate(.index = c(1, 1 + cumsum( diff(.row) != 1 )))
    )
  }

  # If rbind() works, do it
  tryCatch(
    return ( as.data.frame( do.call( rbind, l) ) ),
    error=function(e) {}
  )

  if (all (sapply(l, length) ) == length(l[[1]]) ) {
    result <-  as.data.frame( matrix( ul, nrow=length(l) ) )
    names(result) <- names(l[[1]])
    return(result)
  }

  # nothing worked.  Just return the list as is.
  return( l )
}

#' Cull objects used with do()
#'
#' The [do()] function facilitates easy replication for
#' randomization tests and bootstrapping (among other things).  Part of what
#' makes this particularly useful is the ability to cull from the objects
#' produced those elements that are useful for subsequent analysis.
#' `cull_for_do` does this culling.  It is generic, and users
#' can add new methods to either change behavior or to handle additional
#' classes of objects.
#'
#' @param object an object to be culled
#' @param ... additional arguments (currently ignored)
#'
#' @details When `do(n) * expression` is evaluated, `expression`
#' is evaluated `n` times to produce a list of `n` result objects.
#' `cull_for_do` is then applied to each element of this list to
#' extract from it the information that should be stored.  For example,
#' when applied to a object of class `"lm"`,
#' the default `cull_for_do` extracts the coefficients, coefficient
#' of determinism, an the estimate for the variance, etc.
#'
#'
#' @examples
#' KidsFeet |> resample() |> model_train(length ~ resample(width)) |>
#'   R2() |> trials(times=10)


mosaic_cull_for_do <- function(object, ...) {
  UseMethod("mosaic_cull_for_do")
}


#' @export
mosaic_cull_for_do.default <- function(object, ...) {
  object
}

#' @export
mosaic_cull_for_do.fitdistr <- function(object, ...) {
  est <- object$estimate
  names(est) <- paste0(names(est), ".est")
  se <- object$sd
  names(se) <- paste0(names(se), ".se")
  c(est, se)
}

#' @export
mosaic_cull_for_do.aov <- function(object, ...) {
  mosaic_cull_for_do(stats::anova(object))
}

#' @export
mosaic_cull_for_do.anova <- function(object, ...) {
  res <- as.data.frame(object)
  res <- cbind (data.frame(source=row.names(res)), res)
  names(res)[names(res) == "Df"] <- "df"
  names(res)[names(res) == "Sum Sq"] <- "SS"
  names(res)[names(res) == "Mean Sq"] <- "MS"
  names(res)[names(res) == "F value"] <- "F"
  names(res)[names(res) == "Pr(>F)"] <- "pval"
  names(res)[names(res) == "Sum of Sq"] <- "diff.SS"
  names(res)[names(res) == "Res.Df"] <- "res.df"
  return(res)
  return( data.frame(
    SSTotal= sum(object$`Sum Sq`),
    SSModel= object$`Sum Sq`[1],
    SSError= object$`Sum Sq`[2],
    MSTotal= sum(object$`Sum Sq`),
    MSModel= object$`Mean Sq`[1],
    MSError= object$`Mean Sq`[2],
    F=object$`F value`[1],
    dfModel=object$Df[1],
    dfError=object$Df[2],
    dfTotal=sum(object$Df)
  ) )
}

#' @export
mosaic_cull_for_do.table <- function(object, ...) {
  result <- data.frame(object)
  res <- result[[ncol(result)]]
  nms <- as.character(result[[1]])
  if (ncol(result) > 2) {
    for (k in 2:(ncol(result)-1)) {
      nms <- paste(nms, result[[k]],sep=".")
    }
  }
  names(res) <- nms
  return(res)
}

#' @export
mosaic_cull_for_do.aggregated.stat <- function(object, ...) {
  result <- object
  res <- as.vector(result[, "S"])  # ncol(result)]
  names(res) <-
    paste( attr(object, 'stat.name'),
           .squash_names(object[,1:(ncol(object)-3),drop=FALSE]), sep=".")
  return(res)
}

#' @export
mosaic_cull_for_do.lm <- function(object, ...) {
  regression_summary(object)
}


#' @export
mosaic_cull_for_do.htest <- function(object, ...) {
  if (is.null(object$conf.int)) {
    result <-  data.frame(
      statistic = null2na(object$statistic),
      parameter = null2na(object$parameter),
      p.value = null2na(object$p.value),
      method = null2na(object$method),
      alternative = null2na(object$alternative),
      data = null2na(object$data.name)
    )
  } else {
    result <-  data.frame(
      statistic = null2na(object$statistic),
      parameter = null2na(object$parameter),
      p.value = null2na(object$p.value),
      conf.level = attr(object$conf.int,"conf.level"),
      lower = object$conf.int[1],
      upper = object$conf.int[2],
      method = null2na(object$method),
      alternative = null2na(object$alternative),
      data = null2na(object$data.name)
    )
  }
  if ( !is.null(names(object$statistic)) )
    names(result)[1] <-  names(object$statistic)
  if ( !is.null(names(object$parameter)) )
    names(result)[2] <- names(object$parameter)
  return(result)
}

#   if (inherits(object, 'table') ) {
#     nm <- names(object)
#     result <-  as.vector(object)
#     names(result) <- nm
#     return(result)
#   }

#' @export
mosaic_cull_for_do.cointoss <- function(object, ...) {
  return( c(n=attr(object,'n'),
            heads=sum(attr(object,'sequence')=='H'),
            tails=sum(attr(object,'sequence')=='T'),
            prop=sum(attr(object,'sequence')=="H") / attr(object,'n')
  ) )
}

#' @export
mosaic_cull_for_do.matrix <- function(object, ...) {
  if (ncol(object) == 1) {
    nn <- rownames(object)
    object <- as.vector(object)
    if (is.null(nn)) {
      names(object) <- paste('v',1:length(object),sep="")
    } else {
      names(object) <- nn
    }
    return(object)
  }
  if (nrow(object) > 1) {
    res <- as.data.frame(object)
    res[[".row"]] <- row.names(object)
    return(res)
  }
  # if we get here, we have a 1-row or empty matrix
  row.names(object) <- NULL
  object
}

