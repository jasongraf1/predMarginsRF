#' Get marginal predictions from a random forest
#'
#' @param m Random forest model. Must be of class \code{ranger}.
#' @param vars Character string of targeted predictor variable names from which to derive marginal predictions. Default is all predictors in \code{m} (not recommended).
#' @param data Dataset used to fit model
#' @param ext_vars Character string indicating the external variables to take into consideration when weighting
#' @param num.trees Number of trees from which to extract predictions. Default is 500.
#' @param n.breaks Number of breaks with which to split continuous predictors. Default is 10.
#' @param verbose Logical. Should information be printed?
#'
#' @author Jason Grafmiller
#'
#' @details Add details here
#'
#' @return A \code{list} of class \code{marginalPreds}.
#' \describe{
#'  \item{\code{predictions}}{A \code{data.table} of the marginal predictions from the model}
#'  \item{\code{model}}{The name of the model}
#'  \item{\code{data}}{The dataset the model was trained on}
#'  \item{\code{variable_names}}{The names of the independent variables in the model}
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
marginal_predictions <- function(m, data, num.trees = 500, n.breaks = 10,
                                 verbose = TRUE){
  require(data.table) # use data.table because the results can be very large

  full_vars <- m$forest$independent.variable.names

  # create list of values for the full combination grid.
  var_list <- lapply(full_vars, function(v){
    if(is.numeric(data[, v])){ # column is continuous
      if(length(n.breaks) == 1) {
        vals <- unique(cut2(data[, v], n.breaks))
      } else vals <- n.breaks # allow user to set specific breakpoint values
    } else { # column is factor/character
      vals <- unique(data[, v])
    }
    return(vals)
  })

  # create the expanded grid
  new_data <- do.call(expand.grid, var_list)
  names(new_data) <- full_vars

  if(verbose) print(paste("Generating predictions for", nrow(new_data), "combinations."))

  preds <- predict(m, data = new_data, predict.all = TRUE, type = "response")

  preds_df <- preds$predictions[, m$forest$class.values[1], ] |>
    as.data.frame()

  names(preds_df) <- paste("tree", 1:dim(preds$predictions)[3], sep = ".")

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

  # Reshape the dataframe to long format
  # Using data.table is MUCH faster than base R. These are likely to be very
  # big objects so speed counts :)
  marginal_dt <- cbind(new_data, preds_df) |>
    as.data.table() |>
    melt(id.vars = full_vars, measure.vars = names(preds_df),
         variable.name = "tree", value.name = "pred_prob")

  # Create version of the data based on the bins
  # peripheral_vars <- full_vars[!(full_vars %in% vars)]
  # num_vars <- peripheral_vars[sapply(data[, peripheral_vars], is.numeric)]
  #
  # if(length(num_vars) > 0) {
  #   # Convert numeric columns to factors for merging
  #   data_dt[ , (num_vars) := lapply(.SD, as.factor), .SDcols = num_vars]
  #   binned_d <- data
  #   binned_d[num_vars] <- lapply(data[num_vars], function(x) cut2(x, n.breaks))
  # } else {
  #   binned_d <- data
  # }
  #
  # # Create dataframe of weights
  # # TO DO: adjust for isolated weights
  # if(wt == "joint"){
  #   count_dt <- ftable(binned_d[, peripheral_vars]) |>
  #     as.data.frame() |>
  #     as.data.table()
  # } else if (wt == "iso"){
  #   stop("I'm not sure what to do here...")
  # }
  # prop_f <- function(x) x/nrow(data)
  # count_dt[, wt := prop_f(Freq)]
  # names(count_dt)[1:length(peripheral_vars)] <- peripheral_vars
  #
  # # merge data_dt with the the weighting data.table
  # mar_dt <- merge(data_dt, count_dt, by = peripheral_vars, all.x = TRUE)
  # # get the averages
  # mar_avg_df <- mar_dt[, .(mean_prob = weighted.mean(pred_prob, wt), SD = sd(pred_prob),
  #                          lower = quantile(pred_prob, interval[1]),
  #                          upper = quantile(pred_prob, interval[2])),
  #                   by = vars] |>
  #   as.data.frame() # convert back to data.frame
  # # convert back to numeric factors
  # # num_target_vars <- vars[sapply(data[, vars], is.numeric)]
  # # if(length(num_vars) > 0) mar_avg_df[num_vars] <- lapply(mar_avg_df[num_vars], function(x) as.numeric(as.character(x)))
  marginal_preds <- list(
    predictions = marginal_dt,
    model = deparse(substitute(m)),
    data = data,
    variable_names = full_vars,
    n.breaks = n.breaks,
    num.trees = num.trees
  )

  class(marginal_preds) <- "marginalPreds"

  return(marginal_preds)
}
