#' Get tree predictions for all predictor combinations in random forest
#'
#' @param m Random forest model. Must be of class \code{ranger} or \code{RandomForest}.
#' @param vars Character string of targeted predictor variable names from which to derive marginal predictions. Default is all predictors in \code{m} (not recommended).
#' @param data Dataset used to fit model
#' @param ext_vars Character string indicating the external variables to take into consideration when weighting
#' @param num.trees Number of trees from which to extract predictions. Default is 500.
#' @param n.breaks Number of breaks with which to split continuous predictors. Default is 10.
#' @param verbose Logical. Should information be printed?
#' @param breaks A named list with values representing custom points for which to get predictions for continuous predictors. If a number of length = 1 is used, that will be the number of evenly-spaced points used (similar to \code{n.breaks})
#'
#' @author Jason Grafmiller
#'
#' @details Add details here
#'
#' @return A \code{list} of class \code{treePredictions}.
#' \describe{
#'  \item{\code{predictions}}{A \code{data.table} of the marginal predictions from the model}
#'  \item{\code{model}}{The name of the model}
#'  \item{\code{data}}{The dataset the model was trained on}
#'  \item{\code{variable_names}}{The names of the independent variables in the model}
#'  \item{\code{predicted.outcome}}{The value representing the positive predicted outcome}
#'  \item{\code{n.breaks}}{The number of breakpoints used for binning continuous predictors}
#'  \item{\code{num.trees}}{The number of trees included in \code{predictions}}
#' }
#'
#' @references Add references here
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
tree_predictions <- function(m, data, num.trees = 500L, n.breaks = 10L,
                                 verbose = TRUE, breaks = NULL){
  require(data.table) # use data.table because the results can be very large

  if(class(m) == "ranger"){
    full_vars <- m$forest$independent.variable.names
  } else if (class(m) == "RandomForest"){
    full_vars <- names(m@data@get("input"))
    predicted.outcome <- m@responses@levels[[1]]
  }

  # create list of values for the full combination grid.
  var_list <- lapply(full_vars, function(v){
    if(is.numeric(data[, v])){ # column is continuous
      if(is.null(breaks)){
        # no custom breakpoints listed at all
        vals <- unique(cut2(data[, v], n.breaks))
      } else {
        if(v %in% names(breaks)){ # custom breakpoints use for this variable
          if(length(breaks[[v]]) == 1){
            vals <- unique(cut2(data[, v], breaks[[v]]))
          } else vals <- breaks[[v]]
        } else vals <- unique(cut2(data[, v], n.breaks))
      }
    } else { # column is factor/character
      vals <- unique(data[, v])
    }
    return(vals)
  })

  # create the expanded grid
  new_data <- do.call(expand.grid, var_list)
  names(new_data) <- full_vars

  if(verbose) message("Generating predictions for ", nrow(new_data),
                      " combinations from ", length(full_vars), " predictor variables.")

  if(class(m) == "ranger"){
    preds <- predict(m, data = new_data, predict.all = TRUE, type = "response")

    preds_df <- preds$predictions[, m$forest$class.values[1], ] |>
      as.data.frame()

    names(preds_df) <- paste("tree", 1:dim(preds$predictions)[3], sep = ".")

    # Get the outcomes of the model
    outcomes <- m$forest$levels

    # Allow for using custom number of trees in the predictions. Setting this
    # lower keeps the data.frames to a more manageable size. Advice is to fit
    # RFs with at least 1000 trees, but we set the default number of trees to
    # get predictions from at 500.
    # Alternatively, we can just get the num.trees directly from the model:
    # if(is.null(num.trees)) num.trees <- m$num.trees

    # If the number of trees is less than the values specified, just use the
    # number generated for the model.
    if(m$num.trees < num.trees) num_trees <- m$num.trees

    # Randomly sample num.trees from the forest
    preds_df <- preds_df[, sample(1:ncol(preds_df), num.trees)]

    # Allow for n > 2 level outcomes.
    if(length(outcomes) > 2){

      # Create a vector of column names that will be in the resulting data.table
      predicted.outcome <- outcomes

      # Create list of data.tables each with predicted probability for one outcome
      # level.

      outcome_list <- lapply(
        seq_along(outcomes),
        function(i) {
          label <- outcomes[i]
          preds_df <- preds$predictions[, i, ] |>
            as.data.frame()
          names(preds_df) <- paste("tree", 1:dim(preds$predictions)[3], sep = ".")

          # Reshape the dataframe to long format
          # Using data.table is MUCH faster than base R. These are likely to be very
          # big objects so speed counts :)
          outcome_dt <- cbind(new_data, preds_df) |>
            as.data.table() |>
            melt(id.vars = full_vars, measure.vars = names(preds_df),
                 variable.name = "tree", value.name = paste0(label, "_prob"))

          return(outcome_dt)
        })

      # Merge data.tables for the respective outcomes
      marginal_dt <- Reduce(
        function(...) {
          merge(..., by = NULL, all = TRUE, sort = FALSE)
        },
        outcome_list
      )
    } else { # if outcome is binary

      predicted.outcome <- m$forest$levels[m$forest$class.values[1]]
      label <- paste0(predicted.outcome, "_prob")
      preds_df <- preds$predictions[, m$forest$class.values[1], ] |>
        as.data.frame()

      names(preds_df) <- paste("tree", 1:dim(preds$predictions)[3], sep = ".")

      # Reshape the dataframe to long format
      # Using data.table is MUCH faster than base R. These are likely to be very
      # big objects so speed counts :)
      marginal_dt <- cbind(new_data, preds_df) |>
        as.data.table() |>
        melt(id.vars = full_vars, measure.vars = names(preds_df),
             variable.name = "tree", value.name = label)

    }} else if (class(m) == "RandomForest"){
      marginal_dt <- get_party_predictions(m, new_data, num.trees = num.trees)
      if(length(predicted.outcome) == 2) predicted.outcome <- predicted.outcome[2]
  }

  # Create list of relevant information and data
  marginal_preds <- list(
    predictions = marginal_dt,
    model = deparse(substitute(m)),
    data = data,
    variable_names = full_vars,
    predicted.outcome = predicted.outcome,
    n.breaks = n.breaks,
    num.trees = num.trees
  )

  class(marginal_preds) <- "treePredictions"

  return(marginal_preds)
}
