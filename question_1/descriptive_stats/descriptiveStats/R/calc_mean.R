#' Calculate Arithmetic Mean
#'
#' Calculates the mean of a numeric vector, ignoring NA values.
#'
#' @param x A numeric vector.
#' @return A single numeric value, the mean of `x`.
#' @examples
#' calc_mean(c(1, 2, 3, 4, 5))
#' # Returns 3
#' @export
calc_mean <- function(x) {
  if (length(x) == 0) stop("Input vector is empty.")
  if (!is.numeric(x)) stop("Input must be numeric.")
  mean(x, na.rm = TRUE)
}
