

#' Extract Product Function
#'
#' @description
#' Method for extracting product functions
#' @param signal Signal values (Numeric | vector)
#' @param max_envelope_iteration Maximum number of iterations when separating local envelope signals (Integer)
#' @param envelope_epsilon Terminate processing when obtaining pure FM signal (Double)
#' @param convergence_epsilon Terminate processing when modulation signal converges (Double)
#' @return Product Function
#' @author Shubhra Prakash, \email{shubhraprakash279@@gmail.com}
#' @references \url{https://pypi.org/project/PyLMD/}
#' @keywords lmd
#' @examples
#' x=1:100
#' y = (2 / 3 )* sin(x * 30) + (2 / 3) * sin(x * 17.5) + (4 / 5) *cos(x * 2)
#' plot(y,type="l")
#' pf=extract_product_function(y)
#' @export extract_product_function


extract_product_function <- function(signal,max_envelope_iteration=200,envelope_epsilon=0.01,convergence_epsilon=0.01) {
  s = signal
  n = length(signal)
  envelopes = list()

  component <- function(s,envelopes) {
    c2 = s
    for (e in envelopes){  # Caculate PF，using PF_i(t) = a_i(t)* s_in()
      c2 = c2 * e
    }
    return(c2)
  }

  for (temp_var in 1:max_envelope_iteration ){
    extrema = find_extrema(s)
    if (length(extrema) <= 3) break

    mean_enve=local_mean_and_envelope(s, extrema)
    m0=mean_enve$mean
    m=mean_enve$ma
    a0=mean_enve$enve
    a=mean_enve$enve_sm

    for (i in 1:length(a)){ # robusting
      if(a[i]<=0) a[i] = 1 - 1e-4
    }

    #　subtracted from the original data. H is residue signal
    h = s - m
    #  amplitude demodulated by dividing a.
    t = h / a

    # Terminate processing when obtaining pure FM signal.
    err = sum(abs(1-a)) / n #robusting

    if (err <= envelope_epsilon) break

    # Terminate processing when modulation signal converges.
    err = sum(abs(s-t)) / n

    if (err <= convergence_epsilon) break

    envelopes[[temp_var]]=a
    s = t

  }

  return (component(s,envelopes))
}
