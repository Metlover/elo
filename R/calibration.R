#' Calculate calibration
#'
#' Calculate the calibration (sum of times teams are predicted to win over the
#' number of times they actually won) for a given model. Perfect calibration
#' would be a score of 1, over-prediction would be a score greater than 1, and
#' under-prediction would be a score less than 1.
#' @param object An object
#' @param subset (optional) A vector of indices on which to calculate calibration.
#' @param running logical, denoting whether to use the running predicted values.
#' @param ... Other arguments (not in use at this time).
#' @name elo.calibration
NULL
#> NULL

#' @rdname elo.calibration
#' @export
calibration <- function(object, subset, ...)
{
  UseMethod("calibration")
}

#' @rdname elo.calibration
#' @export
calibration.elo.run <- function(object, subset, ...)
{
  pred_wins <- object$elos[which(score(object$elos[,3],0.5) == 1),3]
  correct <- score(object$elos[,3],0.5) & object$elos[,4]
  if(!missing(subset)) {
    pred_wins <- pred_wins[subset]
    correct <- correct[subset]
  }
  sum(pred_wins) / sum(correct)
}

#' @rdname elo.calibration
#' @export
calibration.elo.glm <- function(object, subset, ...)
{
  pred_wins <- object$fitted.values[which(score(object$fitted.values,0.5) == 1)]
  correct <- score(object$fitted.values,0.5) & object$y
  if(!missing(subset)) {
    pred_wins <- pred_wins[subset]
    correct <- correct[subset]
  }
  sum(pred_wins) / sum(correct)
}

#' @rdname elo.calibration
#' @export
calibration.elo.running <- function(object, subset, running = TRUE, ...)
{
  if(!running) return(NextMethod())
  pred_wins <-
    object$running.values[which(score(object$running.values,0.5) == 1)]
  correct <- score(object$running.values,0.5) & object$y
  if(!missing(subset)) {
    pred_wins <- pred_wins[subset]
    correct <- correct[subset]
  }
  sum(pred_wins) / sum(correct)
}

#' @rdname elo.calibration
#' @export
calibration.elo.markovchain <- calibration.elo.glm

#' @rdname elo.calibration
#' @export
calibration.elo.winpct <- calibration.elo.glm

#' @rdname elo.calibration
#' @export
calibration.elo.colley <- calibration.elo.glm
