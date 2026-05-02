#' Calculate Interquartile Range (IQR)
#'
#' @param x Numeric vector.
#' @return Interquartile range (Q3 - Q1).
#' @export
calc_iqr <- function(x) {
  calc_q3(x) - calc_q1(x)
}
