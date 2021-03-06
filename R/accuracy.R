#' Calculate accuracy
#'
#' Calculate the accuracy (percent of cases where the favored team wins)
#' for a given model.
#' @param object An object
#' @param subset (optional) A vector of indices on which to calculate accuracy.
#' @param running logical, denoting whether to use the running predicted values.
#' @param ... Other arguments (not in use at this time).
#' @name elo.accuracy
NULL
#> NULL

#' @rdname elo.accuracy
#' @export
accuracy <- function(object, subset, ...)
{
  UseMethod("accuracy")
}

#' @rdname elo.accuracy
#' @export
accuracy.elo.run <- function(object, subset, ...)
{
  results <- (object$elos[,3] >0.5 & object$elos[,4] == 1) |
    (object$elos[,3] < 0.5 & object$elos[,4] == 0) |
    (object$elos[,3] == 0.5 & object$elos[,4] == 0.5)
  if(!missing(subset)) results <- results[subset]
  sum(results) / length(results)
}

#' @rdname elo.accuracy
#' @export
accuracy.elo.glm <- function(object, subset, ...)
{
  results <- (object$fitted.values >0.5 & object$y == 1) |
    (object$fitted.values < 0.5 & object$y == 0) |
    (object$fitted.values == 0.5 & object$y == 0.5)
  if(!missing(subset)) results <- results[subset]
  sum(results) / length(results)
}

#' @rdname elo.accuracy
#' @export
accuracy.elo.running <- function(object, subset, running = TRUE, ...)
{
  if(!running) return(NextMethod())
  results <- (object$running.values >0.5 & object$y == 1) |
    (object$running.values < 0.5 & object$y == 0) |
    (object$running.values == 0.5 & object$y == 0.5)
  if(!missing(subset)) results <- results[subset]
  sum(results) / length(results)
}

#' @rdname elo.accuracy
#' @export
accuracy.elo.markovchain <- accuracy.elo.glm

#' @rdname elo.accuracy
#' @export
accuracy.elo.winpct <- accuracy.elo.glm

#' @rdname elo.accuracy
#' @export
accuracy.elo.colley <- accuracy.elo.glm
