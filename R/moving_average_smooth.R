#' Weighted Moving Average
#'
#' @description
#' Weighted Moving Average Smoothing
#'
#' @details
#' Weighted Moving Average Smoothing is used to smooth en the mean and envelope signal
#' @param signal Signal values (Numeric | vector)
#' @param window filter weights for smoothing (Numeric | vector)
#' @param max_smooth_iteration Maximum number of iterations of moving average algorithm (Integer)
#' @return smooth signal
#' @author Shubhra Prakash, \email{shubhraprakash279@@gmail.com}
#' @references \url{https://pypi.org/project/PyLMD/}
#' @keywords Weighted Moving Average wma
#' @examples
#' x=0:100
#' y = (2 / 3 )* sin(x * 30) + (2 / 3) * sin(x * 17.5) + (4 / 5) *cos(x * 2)
#' plot(y,type="l")
#' wma=moving_average_smooth(y,5)
#' plot(wma,type="l")
#' @export moving_average_smooth

moving_average_smooth <-
  function(signal, window, max_smooth_iteration=12) {

    n = length(signal)
    # at least one nearby sample is needed for average
    if (window < 3) {
      window = 3
    }
    # adjust length of sliding window to an odd number for symmetry
    if ((window %% 2) == 0) {
      window = window + 1
    }
    half = floor(window / 2)
    weight = c(c(1:(half + 1)) , c(half:1))
    stopifnot(length(weight) == window)

    is_smooth <- function(signal) {
      for (x in 2:n) {
        if ((signal[x] == signal[x - 1])) {
          return(FALSE)
        }
      }
      if (signal[1] == signal[length(signal)]) {
        return(FALSE)
      }
      return(TRUE)
    }
    smoothed = signal

    for (ii in 1:max_smooth_iteration) {
      head = list()
      tail = list()
      w_num = half

      for (i in 1:half) {
        head_temp = c()
        tail_temp = c()
        for (j in (i - (half - w_num)):(i + half)) {
          head_temp = c(head_temp, smoothed[j])
          tail_temp = c(tail_temp, smoothed[length(smoothed) +
                                              1 - j])
        }
        head[[i]] = head_temp
        tail[[i]] = rev(tail_temp)
        w_num = w_num - 1
      }
      # smoothed = np.convolve(smoothed, weight, mode='same')
      smoothed = stats::convolve(smoothed, weight, type = "open")[(half +
                                                                     1):(length(smoothed) + half)]
      smoothed[(half + 1):(length(smoothed) - half)] = (smoothed[(half + 1):(length(smoothed) - half)]) /
        sum(weight)
      w_num = half
      for (i in 1:half) {
        smoothed[i] = sum(unlist(head[i]) * weight[(w_num+1):(length(weight))]) / sum(weight[(w_num+1):(length(weight))])
        smoothed[length(smoothed) - (i) + 1] = sum(unlist(tail[i]) * weight[1:(length(weight) - w_num)]) / sum(weight[1:(length(weight) - w_num)]) #### locha
        w_num = w_num - 1


      }
      smoothed=round(smoothed,7)

      if (is_smooth(smoothed)) {
        break
      }
    }


    return(smoothed)

  }



