#' Train models with a gridsearch
#'
#' Takes in the arguments of the given elo function along with an optimization
#' function and a grid of parameters to find the best combination of the
#' parameters.
#' @inheritParams elo.calc
#' @param optimfun An optimization function to train the model on. See details.
#' @param gridsearch A dataframe of values to be used for optimization. See details.
#' @param ... Other arguments (not used at this time).
#' @return An elo object optimized by the given optimization function
#' @examples
#' data(tournament)
#' params = expand.grid(k = seq(1,250,by=5),
#'                      hfa = seq(0,100,by=5))
#' fit.elo.run(score(points.Home, points.Visitor) ~ adjust(team.Home, hfa) + team.Visitor,
#'             data = tournament,
#'             optimfun = 'mse',
#'             gridsearch = params)
#'
#' params = list(hfa = seq(0,100,by=5))
#' fit.elo.glm(score(points.Home, points.Visitor) ~ adjust(team.Home, hfa) + team.Visitor,
#'             data = tournament,
#'             optimfun = 'mse',
#'             gridsearch = params)

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

fit.elo.markovchain <- function(formula, data, optimfun = 'mse', gridsearch, ...){

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

    mc = suppressWarnings(elo.markovchain(formula = adj_formula, data = data, k = k, ...))

    gridsearch$mse[n] = mse(mc)
    gridsearch$accuracy[n] = accuracy(mc)
    gridsearch$calibration[n] = calibration(mc)
    gridsearch$auc[n] = calibration(mc)
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

  mc = elo.markovchain(formula = final_formula, data = data, k = k, ...)

  return(mc)
}

fit.elo.glm <- function(formula, data,  optimfun = 'mse', gridsearch, ...){

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

    glm = suppressWarnings(elo.glm(formula = adj_formula, data = data, ...))

    gridsearch$mse[n] = mse(glm)
    gridsearch$accuracy[n] = accuracy(glm)
    gridsearch$calibration[n] = calibration(glm)
    gridsearch$auc[n] = calibration(glm)
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

  glm = elo.glm(formula = final_formula, data = data)

  return(glm)
}

fit.elo.winpct <- function(formula, data,  optimfun = 'mse', gridsearch, ...){

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

    wpct = suppressWarnings(elo.winpct(formula = adj_formula, data = data, ...))

    gridsearch$mse[n] = mse(wpct)
    gridsearch$accuracy[n] = accuracy(wpct)
    gridsearch$calibration[n] = calibration(wpct)
    gridsearch$auc[n] = calibration(wpct)
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

  wpct = elo.winpct(formula = final_formula, data = data)

  return(wpct)
}

fit.elo.colley <- function(formula, data,  optimfun = 'mse', gridsearch, ...){

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

    col = suppressWarnings(elo.colley(formula = adj_formula, data = data, ...))

    gridsearch$mse[n] = mse(col)
    gridsearch$accuracy[n] = accuracy(col)
    gridsearch$calibration[n] = calibration(col)
    gridsearch$auc[n] = calibration(col)
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

  col = elo.colley(formula = final_formula, data = data)

  return(col)
}
