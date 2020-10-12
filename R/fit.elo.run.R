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
#' A column named "k" must be included. If you wish to optimize other parameters
#' while holding k constant, simply set k to the same value for all rows of the
#' grid search.
#'
#' @examples
#' data(tournament)
#' params = expand.grid(k = seq(1,250,by=5),
#'                      hfa = seq(0,100,by=5))
#' fit.elo.run(score(points.Home, points.Visitor) ~ adjust(team.Home, hfa) + team.Visitor,
#'             data = tournament,
#'             optimfun = 'mse',
#'             gridsearch = params)
#' @name fit.elo.run
NULL
#> NULL

#' @rdname fit.elo.run
#' @export

fit.elo.run <- function(formula, data, optimfun = 'mse', gridsearch, ...){

  if(!('k' %in% colnames(gridsearch))){
    stop("There is no column for k-values within the grid search dataframe.
          Please insert a column named \"k\" with values of k to iterate over and try again.")
  }

  gridsearch$mse = NA_real_
  gridsearch$accuracy = NA_real_
  gridsearch$calibration = NA_real_
  gridsearch$auc = NA_real_

  for(n in seq(nrow(gridsearch))){
    parameters = list()
    for(col in seq(ncol(gridsearch))){
      if(colnames(gridsearch)[col] == 'k'){
        k = gridsearch[n,col]
      } else if(!(colnames(gridsearch)[col] %in%
                  c('mse','accuracy','calibration','auc'))) {
        parameters[colnames(gridsearch)[col]] = as.numeric(gridsearch[n,col])
      }
    }

    adj_formula = formula(do.call("substitute",list(formula, parameters)))

    run = suppressWarnings(elo.run(formula = adj_formula, data = data, k = k, ...))

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
    if(colnames(gridsearch)[col] == 'k'){
      k = optimized$k[1]
    } else if(!(colnames(gridsearch)[col] %in%
                c('mse','accuracy','calibration','auc'))){
      final_parameters[colnames(gridsearch)[col]] = as.numeric(optimized[1,col])
    }
  }

  final_formula = formula(do.call("substitute",list(formula, final_parameters)))

  cat("Optimized K value: ", k,'\n')

  run = elo.run(formula = final_formula, data = data, k = k, ...)

  return(run)
}

cv.elo.run <- function(formula, data, optimfun = 'mse', gridsearch, nfolds = NULL,
                       groups = NULL, new.team.elo = 1500, ...){

  if(!('k' %in% colnames(gridsearch))){
    stop("There is no column for k-values within the grid search dataframe.
          Please insert a column named \"k\" with values of k to iterate over and try again.")
  }

  gridsearch$mse = 0
  gridsearch$accuracy = 0
  gridsearch$calibration = 0
  gridsearch$auc = 0

  for(n in seq(nrow(gridsearch))){
    parameters = list()
    for(col in seq(ncol(gridsearch))){
      if(colnames(gridsearch)[col] == 'k'){
        k = gridsearch[n,col]
      } else if(!(colnames(gridsearch)[col] %in%
                  c('mse','accuracy','calibration','auc'))) {
        parameters[colnames(gridsearch)[col]] = as.numeric(gridsearch[n,col])
      }
    }

    adj_formula = formula(do.call("substitute",list(formula, parameters)))

    if(!is.null(nfolds)){
      rows = nrows(data)
      fold_size <- ceiling(rows / nfolds)
      temp = split(data, rep(1:ceiling(rows/fold_size), each=fold_size, length.out=rows))
      nfolds.temp = nfolds
    } else if(!is.null(groups)){
      factors = factor(data[,groups], levels = unique(data[,groups]))
      temp = split(data, factors)
      nfolds.temp = length(unique(data[,groups]))
    } else {
      stop("No folds or groups provided. Please input a value for nfolds or groups and try again.")
    }
    denom = 0

    for(m in c(2:nfolds.temp)){
      train = do.call("rbind", temp[c(1:m-1)])
      test = temp[m][[1]]

      train.run = suppressWarnings(elo.run(formula = adj_formula, data = train, k = k, ...))
      test.run = suppressWarnings(elo.run(formula = adj_formula, data = test, k = k, ...))

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
      gridsearch$accuracy[n] = gridsearch$accuracy[n] + (sum(results) / length(results)) * nrow(train)

      x.won <- pred[test.run$elos[,4] == 1]
      x.lost <- pred[test.run$elos[,4] == 0]

      n.won <- 0 + length(x.won) # to coerce to double
      if(n.won == 0 || length(x.lost) == 0) stop("Unable to calculate AUC: need both 0 and 1 in the wins column.")
      gridsearch$auc[n] = gridsearch$auc[n] + ((sum(rank(c(x.won, x.lost))[1:n.won]) - n.won*(n.won + 1)/2)/(n.won * length(x.lost))) * nrow(train)

      pred_wins <- pred[which(score(pred,0.5) == 1)]
      correct <- score(pred,0.5) & test.run$elos[,4]
      gridsearch$calibration[n] = gridsearch$calibration[n] + (sum(pred_wins) / sum(correct)) * nrow(train)

      r <- test.run$elos[,4] - pred
      gridsearch$mse[n] = gridsearch$mse[n] + mean(r^2) * nrow(train)

      denom = denom + nrow(train)
    }
    gridsearch$mse[n] = gridsearch$mse[n] / denom
    gridsearch$accuracy[n] = gridsearch$accuracy[n] / denom
    gridsearch$calibration[n] = gridsearch$calibration[n] / denom
    gridsearch$auc[n] = gridsearch$auc[n] / denom
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
    if(colnames(gridsearch)[col] == 'k'){
      k = optimized$k[1]
    } else if(!(colnames(gridsearch)[col] %in%
                c('mse','accuracy','calibration','auc'))){
      final_parameters[colnames(gridsearch)[col]] = as.numeric(optimized[1,col])
    }
  }

  final_formula = formula(do.call("substitute",list(formula, final_parameters)))

  cat("Optimized K value: ", k,'\n')

  run = elo.run(formula = final_formula, data = data, k = k, ...)

  return(run)
}
