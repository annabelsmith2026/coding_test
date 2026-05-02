#' Calculate Median
#'
#' @param x Numeric vector.
#' @return Median of `x`.
#' @export
calc_median <- function(x) {
  if (length(x) == 0) stop("Input vector is empty.")
  if (!is.numeric(x)) stop("Input must be numeric.")
  median(x, na.rm = TRUE)
}
