#' Get predictions for trees from a \code{party} forest
#'
#' @param m A random forest of class \code{RandomForest} fit with the \code{party} package
#' @param newdata A dataframe containing the new data to be predicted by the model
#' @param num.trees Number of trees from which to extract predictions. Default is 500.
#'
#' @author Jason Grafmiller
#'
#' @details A hacky function for obtaining the predictions from individual trees in a \code{party} conditional random forest. This function is normally not called by users, but used by the \code{marginal_predictions()} function behind the scenes.
#'
#' @return A \code{data.table} object of the predictions for each tree in the forest
#' @export
#'
#' @examples
#' \dontrun{}
get_party_predictions <- function(m, newdata, num.trees = 500L){

  # get the list of which columns are factors. This is needed for pulling out
  # the information of each tree
  fctrs <- lapply(m@data@get("input"), function(x) if(is.factor(x)) levels(x))

  # for getting the terminal nodes for the new data, we need to make sure
  # the column classes in the new data match those in the training data exactly
  newdata[names(newdata)] <- lapply(names(newdata), function(x) {
    match.fun(paste0("as.", class(m@data@get("input")[[x]])))(newdata[[x]])
  })

  # the list of terminal nodes into which each observation falls for each tree
  where_list <- m@get_where(newdata = newdata)
  trees_i <- sample(seq_along(where_list), num.trees)
  # where_list <- where_list[trees_i]

  tree_prediction_list <- lapply(
    trees_i,
    function(i){
      # get an individual tree from RF
      cur_tree <- party:::prettytree(m@ensemble[[i]],
                                     inames = names(m@data@get("input")),
                                     ilevels = fctrs)

      # Just use the data from the tree to find the terminal nodes, pull out their
      # predictions, and merge that with data from the tree model. We can get the
      # information about which terminal node in the tree a given observation falls
      # into, and then use that to find the prediction for that observation.
      cur_dt <- data.table(
        name = names(unlist(cur_tree)),
        value = unlist(cur_tree)
      )

      cur_dt$type <- sapply(cur_dt$name, function(x) data.table::last(unlist(strsplit(x, "\\."))))
      cur_dt$name <- gsub("\\.\\w+$", "", cur_dt$name)

      terminals <- cur_dt[type == "terminal" & value == TRUE]

      terminal_preds_dt <- merge(terminals, cur_dt, all.x = TRUE, by = "name")
      # remove NAs and odd cases where "psplitX" valuse appear in type.y
      terminal_preds_dt <- terminal_preds_dt[type.y != "NA"][!grepl("psplit", type.y)]

      names(terminal_preds_dt) <- c("name", "is.terminal", "type.x", "value", "type.y")

      terminal_preds_wide <- dcast(terminal_preds_dt, name ~ type.y)[, !c("terminal")]

      pred_cols <- names(terminal_preds_wide)[grepl("prediction", names(terminal_preds_wide))]
      terminal_preds_wide[ , (pred_cols) := lapply(.SD, as.numeric), .SDcols = pred_cols]

      # the responses from the random forest
      response <- m@responses
      levs <- levels(response@variables[[1]])

      if(is.null(levels(response@variables[[1]]))) {
        # outcome is continuous
        label <- names(response@variables)
        names(terminal_preds_wide)[grepl("prediction", names(terminal_preds_wide))] <- paste0(label, "_pred")
      } else if(length(levs) == 2){
        # outcome is binary
        terminal_preds_wide <- terminal_preds_wide[, !c("prediction1")]
        names(terminal_preds_wide)[grepl("prediction", names(terminal_preds_wide))] <- paste0(levs[2], "_pred")
      } else {
        names(terminal_preds_wide)[grepl("prediction", names(terminal_preds_wide))] <- paste0(levs, "_pred")
      }

      # the list of terminal nodes into which each observation falls into for each tree
      cur_where_dt <- data.table(nodeID = as.character(where_list[[i]]))

      # merge the dataframe of data with the
      pred_dt <- merge(cur_where_dt, terminal_preds_wide[, !c("name")], all.x = TRUE, by = "nodeID")
      pred_dt$tree <- paste0("tree.", i)
      pred_dt <- unique(pred_dt)

      newdata_dt <- as.data.table(newdata)
      newdata_dt$nodeID <- as.character(where_list[[i]])

      new_pred_dt <- merge(newdata_dt, pred_dt, all.x = TRUE, by = "nodeID")
      # leave out nodeID column
      return(new_pred_dt[, !c("nodeID")])
    })

  marginal_dt <- data.table::rbindlist(tree_prediction_list)

  return(marginal_dt)
}
