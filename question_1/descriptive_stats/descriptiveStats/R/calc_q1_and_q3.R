#' Calculate First Quartile (Q1)
#'
#' @param x Numeric vector.
#' @return The first quartile (Q1).
#' @export
calc_q1 <- function(x) {
  if (length(x) == 0) stop("Input vector is empty.")
  if (!is.numeric(x)) stop("Input must be numeric.")
  as.numeric(quantile(x, probs = 0.25, na.rm = TRUE))
}

#' Calculate Third Quartile (Q3)
#'
#' @param x Numeric vector.
#' @return The third quartile (Q3).
#' @export
calc_q3 <- function(x) {
  if (length(x) == 0) stop("Input vector is empty.")
  if (!is.numeric(x)) stop("Input must be numeric.")
  as.numeric(quantile(x, probs = 0.75, na.rm = TRUE))
}
