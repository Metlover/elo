#' Train elo.run models with a gridsearch
#'
#' Takes in the arguments of the given elo.run function along with an optimization
#' function and a grid of parameters to find the best combination of the
#' parameters.
#' @inheritParams elo.run
#' @param optimfun A string refering to an optimization function to train the
#' model on. See details.
#' @param gridsearch A dataframe of values to be used for optimization.
#' See details.
#' @param ... Other arguments (not used at this time).
#' @return An elo object optimized by the given optimization function
#' @details \code{optimfun} refers to one of the following optimization functions
#' built into the elo package. 'mse' or 'brier' will train on minimizing the
#' mean squared error of the model, 'accuracy' will train on maximizing the
#' accuracy of the mode, 'calibration' will train on getting the calibration score
#' of the model as close to 1 as possible, and 'auc' will train on maximizing the
#' auc of the model.
#'
#' \code{gridsearch} is a dataframe of parameter values that \code{fit.elo} will
#' search through to optimize. The dataframe's column names correspond to the
#' parameters to be optimized within the formula (i.e. if the term
#' \code{adjust(team.home,hfa)} is included, to optimize hfa you will need a
#' column named 'hfa' with plausible values in the grid search dataframe.
#'
#' @examples
#' data(tournament)
#' params = expand.grid(kval = seq(1,250,by=5),
#'                      hfa = seq(0,100,by=5))
#' fit.elo.run(score(points.Home, points.Visitor) ~ adjust(team.Home, hfa) + team.Visitor + k(kval),
#'             data = tournament,
#'             optimfun = 'mse',
#'             gridsearch = params)
#' @name fit.elo.run
NULL
#> NULL

#' @rdname fit.elo.run
#' @export

fit.elo.run <- function(formula, data, optimfun = 'mse', gridsearch, ...){

  if(!any(grepl("k\\((.*)\\)", as.character(formula)))){
    warning("k not passed in as a formula term, using constant K instead...")
  }

  gridsearch$mse = NA_real_
  gridsearch$accuracy = NA_real_
  gridsearch$calibration = NA_real_
  gridsearch$auc = NA_real_

  for(n in seq(nrow(gridsearch))){
    parameters = list()
    for(col in seq(ncol(gridsearch))){
      if(!(colnames(gridsearch)[col] %in%
                  c('mse','accuracy','calibration','auc'))) {
        parameters[colnames(gridsearch)[col]] = as.numeric(gridsearch[n,col])
      }
    }

    adj_formula = formula(do.call("substitute",list(formula, parameters)))

    run = suppressWarnings(elo.run(formula = adj_formula, data = data, ...))

    gridsearch$mse[n] = mse(run)
    gridsearch$accuracy[n] = accuracy(run)
    gridsearch$calibration[n] = calibration(run)
    gridsearch$auc[n] = calibration(run)
  }

  if(!(tolower(optimfun) %in% c('mse','brier','accuracy',
                                'calibration','auc'))){
    warning("No valid optimization metric was specified.
            Optimizing for Brier scores by default.")
  }

  if(tolower(optimfun) == 'accuracy'){
    optimized = gridsearch[which.max(gridsearch$accuracy),]
    cat("Optimized elo for accuracy at a value of ",
        gridsearch$accuracy[which.max(gridsearch$accuracy)],'\n')
  } else if(tolower(optimfun) == 'calibration'){
    optimized = gridsearch[which.min(abs(grifsearch$calibration - 1)),]
    cat("Optimized elo for calibration at a value of ",
        gridsearch$calibration[which.min(abs(grifsearch$calibration - 1)),],'\n')
  } else if(tolower(optimfun) == 'auc'){
    optimized = gridsearch[which.max(gridsearch$auc),]
    cat("Optimized elo for auc at a value of ",
        gridsearch$auc[which.max(gridsearch$auc)],'\n')
  } else {
    optimized = gridsearch[which.min(gridsearch$mse),]
    cat("Optimized elo for mse at a value of ",
        gridsearch$mse[which.min(gridsearch$mse)],'\n')
  }

  final_parameters = list()
  for(col in seq(ncol(gridsearch))){
    if(!(colnames(gridsearch)[col] %in%
                c('mse','accuracy','calibration','auc'))){
      final_parameters[colnames(gridsearch)[col]] = as.numeric(optimized[1,col])
    }
  }

  final_formula = formula(do.call("substitute",list(formula, final_parameters)))

  run = elo.run(formula = final_formula, data = data, ...)

  return(run)
}

#' Use rolling-origin cv to grid search for optimal elo.run parameters
#'
#' Takes in the arguments of the given elo.run function along with an optimization
#' function and a grid of parameters, then performs rolling origin cross validation
#' to determine the optimal parameters.
#' @inheritParams fit.elo.run
#' @param nfolds Indicates how many folds should be used in cross-validation.
#' Either this value or \code{groups} should be specified. See details for more
#' information.
#' @param groups Indicates what groups should be used in cross-validation.
#' Either this value or \code{nfolds} should be specified. See details for more
#' information.
#' @param new.team.elo An optional argument indicating how new teams (teams not
#' in the training set but in the test set) should be viewed from an elo
#' perspective. Defaults to 1500. See details for more information.
#' @param ... Other arguments (not used at this time).
#' @return An elo object optimized by the given optimization function and
#' cross-validated.
#' @details \code{optimfun} refers to one of the following optimization functions
#' built into the elo package. 'mse' or 'brier' will train on minimizing the
#' mean squared error of the model, 'accuracy' will train on maximizing the
#' accuracy of the mode, 'calibration' will train on getting the calibration score
#' of the model as close to 1 as possible, and 'auc' will train on maximizing the
#' auc of the model.
#'
#' \code{gridsearch} is a dataframe of parameter values that \code{fit.elo} will
#' search through to optimize. The dataframe's column names correspond to the
#' parameters to be optimized within the formula (i.e. if the term
#' \code{adjust(team.home,hfa)} is included, to optimize hfa you will need a
#' column named 'hfa' with plausible values in the grid search dataframe.
#' A column named "k" must be included. If you wish to optimize other parameters
#' while holding k constant, simply set k to the same value for all rows of the
#' grid search.
#'
#' \code{nfolds} indicates how many folds should be used in cross-validation.
#' This function divides \code{data} into \code{nfolds} for cv.
#'
#' \code{groups} indicates the groups to be used for folds for cross-validation.
#' These should be unique identifiers for each group, such as a string indicating
#' which tournament a game took place in or the season where a game took place.
#'
#' For those unfamiliar with how rolling-origin cv works, the function first
#' splits the dataset into folds using either \code{nfolds} or \code{groups}.
#' Using the parameters in the grid-search, this function generates elo values
#' using the first fold, then tests those elo values on the second fold.
#' It then repeats the process but combines the first and second folds as the
#' training set, and tests on the third fold. It repeats this process until all
#' the folds except the first have been tested, then repeats *this* process until
#' all parameters have been tested. It then evaluates each set of parameters on
#' the simple average of the optimization.
#'
#' To walk through an example, imagine that we were fitting elo parameters on a
#' dataframe over the past 5 season's worth of games of a professional sports
#' league, and I have specified the season as the grouping factor for
#' cross-validation. Using the first set of parameters in the grid-search,
#' \code{cv.elo.run} fits an elo model on the first season, then takes those
#' final elo values and uses them to predict the outcomes of the second season's
#' matches, then gathers summary metrics on those results. Then, using the same
#' parameters, \code{cv.elo.run} trains an elo.run on the first and second
#' season, then evaluates on the third season, and so on. Once the second
#' through fifth seasons have been evaluated, the process repeats with a new
#' set of parameters.
#'
#' \code{new.team.elo} is used for evaluating new teams. Sometimes new teams
#' will join tournaments or games, especially over large datasets or small datasets
#' with a lot of folds (the latter, by the way, is not recommended), to handle
#' those instances, those new teams are treated as having an elo of 1500 unless
#' specified otherwise by \code{new.team.elo}. For example, an expansion team
#' joining a professional sports league will tend to perform worse than the
#' inaugural teams initially, and hence, a \code{new.team.elo} value lower than
#' 1500 may be more predictive.
#'
#'
#' @examples
#' data(tournament)
#' params = expand.grid(kval = seq(1,250,by=5),
#'                      hfa = seq(0,100,by=5))
#' cv.elo.run(score(points.Home, points.Visitor) ~ adjust(team.Home, hfa) + team.Visitor + k(kval),
#' data = tournament,
#' optimfun = 'mse',
#' gridsearch = params,
#' nfolds = 3)
#' @name cv.elo.run
NULL
#> NULL

#' @rdname cv.elo.run
#' @export


cv.elo.run <- function(formula, data, optimfun = 'mse', gridsearch, nfolds = NULL,
                       groups = NULL, new.team.elo = 1500, ...){
  gridsearch$mse = 0
  gridsearch$accuracy = 0
  gridsearch$calibration = 0
  gridsearch$auc = 0

  for(n in seq(nrow(gridsearch))){
    parameters = list()
    for(col in seq(ncol(gridsearch))){
      if(!(colnames(gridsearch)[col] %in%
                  c('mse','accuracy','calibration','auc'))) {
        parameters[colnames(gridsearch)[col]] = as.numeric(gridsearch[n,col])
      }
    }

    adj_formula = formula(do.call("substitute",list(formula, parameters)))

    if(!is.null(nfolds)){
      rows = nrow(data)
      fold_size <- ceiling(rows / nfolds)
      temp = split(data, rep(1:ceiling(rows/fold_size), each=fold_size, length.out=rows))
      nfolds.temp = nfolds
    } else if(!is.null(groups)){
      data = as.data.frame(data)
      factors = factor(data[,groups], levels = unique(data[,groups]))
      temp = split(data, factors)
      nfolds.temp = length(unique(data[,groups]))
    } else {
      stop("No folds or groups provided. Please input a value for nfolds or groups and try again.")
    }

    for(m in c(2:nfolds.temp)){
      train = do.call("rbind", temp[c(1:m-1)])
      test = temp[m][[1]]

      train.run = suppressWarnings(elo.run(formula = adj_formula, data = train, ...))
      test.run = suppressWarnings(elo.run(formula = adj_formula, data = test, ...))

      elos = final.elos(train.run)
      for(team in test.run$teams){
        if(!(team %in% train.run$teams)){
          elos[team] = new.team.elo
        }
      }

      pred = elo.prob(adj_formula, data = test, ..., elos = elos)

      results <- (pred >0.5 & test.run$elos[,4] == 1) |
        (pred < 0.5 & test.run$elos[,4] == 0) |
        (pred == 0.5 & test.run$elos[,4] == 0.5)
      gridsearch$accuracy[n] = gridsearch$accuracy[n] + (sum(results) / length(results))

      x.won <- pred[test.run$elos[,4] == 1]
      x.lost <- pred[test.run$elos[,4] == 0]

      n.won <- 0 + length(x.won) # to coerce to double
      if(n.won == 0 || length(x.lost) == 0) stop("Unable to calculate AUC: need both 0 and 1 in the wins column.")
      gridsearch$auc[n] = gridsearch$auc[n] + ((sum(rank(c(x.won, x.lost))[1:n.won]) - n.won*(n.won + 1)/2)/(n.won * length(x.lost)))

      pred_wins <- pred[which(score(pred,0.5) == 1)]
      correct <- score(pred,0.5) & test.run$elos[,4]
      gridsearch$calibration[n] = gridsearch$calibration[n] + (sum(pred_wins) / sum(correct))

      r <- test.run$elos[,4] - pred
      gridsearch$mse[n] = gridsearch$mse[n] + mean(r^2)
    }
    gridsearch$mse[n] = gridsearch$mse[n] / (nfolds.temp - 1)
    gridsearch$accuracy[n] = gridsearch$accuracy[n] / (nfolds.temp - 1)
    gridsearch$calibration[n] = gridsearch$calibration[n] / (nfolds.temp - 1)
    gridsearch$auc[n] = gridsearch$auc[n] / (nfolds.temp - 1)
  }

  if(!(tolower(optimfun) %in% c('mse','brier','accuracy',
                                'calibration','auc'))){
    warning("No valid optimization metric was specified.
            Optimizing for Brier scores by default.")
  }

  if(tolower(optimfun) == 'accuracy'){
    optimized = gridsearch[which.max(gridsearch$accuracy),]
    cat("Optimized elo for accuracy at a value of ",
        gridsearch$accuracy[which.max(gridsearch$accuracy)],'\n')
  } else if(tolower(optimfun) == 'calibration'){
    optimized = gridsearch[which.min(abs(grifsearch$calibration - 1)),]
    cat("Optimized elo for calibration at a value of ",
        gridsearch$calibration[which.min(abs(grifsearch$calibration - 1)),],'\n')
  } else if(tolower(optimfun) == 'auc'){
    optimized = gridsearch[which.max(gridsearch$auc),]
    cat("Optimized elo for auc at a value of ",
        gridsearch$auc[which.max(gridsearch$auc)],'\n')
  } else {
    optimized = gridsearch[which.min(gridsearch$mse),]
    cat("Optimized elo for mse at a value of ",
        gridsearch$mse[which.min(gridsearch$mse)],'\n')
  }

  final_parameters = list()
  for(col in seq(ncol(gridsearch))){
    if(!(colnames(gridsearch)[col] %in%
                c('mse','accuracy','calibration','auc'))){
      final_parameters[colnames(gridsearch)[col]] = as.numeric(optimized[1,col])
    }
  }

  print(final_parameters)

  final_formula = formula(do.call("substitute",list(formula, final_parameters)))

  run = elo.run(formula = final_formula, data = data, ...)

  return(run)
}
