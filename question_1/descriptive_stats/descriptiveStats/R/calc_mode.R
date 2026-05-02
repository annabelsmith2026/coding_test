#' Calculate Mode
#'
#' @param x Numeric vector.
#' @return Mode(s) of `x`. Returns NA if there is no mode.
#' @export
calc_mode <- function(x) {
  if (length(x) == 0) stop("Input vector is empty.")
  if (!is.numeric(x)) stop("Input must be numeric.")
  tab <- table(x)
  max_freq <- max(tab)
  modes <- as.numeric(names(tab)[tab == max_freq])
  if (max_freq == 1) return(NA_real_) # no mode
  if (length(modes) > 1) warning("Multiple modes detected.")
  modes
}
