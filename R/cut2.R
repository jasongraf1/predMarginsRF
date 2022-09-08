#' Title
#'
#' @param x A vector
#' @param breaks a single number (greater than or equal to 2) giving the number of intervals into which x is to be cut.
#'
#' @return a numeric vector
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- runif(100)
#' cut2(x, 10)
#' sort(unique(cut2(x, 10)))
#' }
cut2 <- function(x, breaks) {
  # if(!is.numeric(x)) warning("Converting vector to numeric")
  if (length(unique(x)) <= breaks + 1){
    # If the number of breaks is longer than the number of distinct values,
    # we leave the actual values
    vals <- x
  } else {
    x <- as.numeric(as.character(x)) # convert to numeric if not
    r <- range(x)
    b <- seq(r[1], r[2], length=2*breaks+1)
    brk <- b[0:breaks*2+1]
    mid <- b[1:breaks*2]
    brk[1] <- brk[1]-0.01
    k <- cut(x, breaks=brk, labels=FALSE)
    vals <- mid[k]
  }
  return(vals)
}
