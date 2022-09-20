

#' Monotonicity Check
#'
#' @description
#' Method for checking if signal is increasing or decreasing monotonously
#'
#' @details
#' A monotonic signal is a function that keeps increasing or decreasing as its domain variable proceeds.#'
#' @param signal Signal values (Numeric | vector)
#' @return Boolean
#' @author Shubhra Prakash, \email{shubhraprakash279@@gmail.com}
#' @references \url{https://pypi.org/project/PyLMD/}
#' @keywords monotonic monotonous
#' @examples
#' x=1:100
#' is_monotonous(x)
#' @export is_monotonous

is_monotonous <- function(signal) {
  # Find if signal is decreasing monotonously
  is_monotonous_decrease <- function(signal) {
    y0 = signal[1]
    for (y1 in signal) {
      if (y1 > y0) {
        return(FALSE)
      }
      y0 = y1
    }
    return(TRUE)
  }
  # Find if signal is increasing monotonously
  is_monotonous_increase <- function(signal) {
    y0 = signal[1]
    for (y1 in signal) {
      if (y1 < y0) {
        return(FALSE)
      }
      y0 = y1
    }
    return(TRUE)
  }
  # Combined Descision
  if (length(signal) <= 0) {
    return(TRUE)
  } else {
    decision = is_monotonous_increase(signal) ||
      is_monotonous_decrease(signal)
    return(decision)

  }

}
