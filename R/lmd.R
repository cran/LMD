

#' Local Mean Decomposition
#'
#' @description
#' Method for finding Product Functions (PFs)
#'
#' @details
#' LMD is a method of decomposing signal into Product Functions (PFs) based on algorithm presented in Jonathan S. Smith. The local mean decomposition and its application to EEG perception data. Journal of the Royal Society Interface, 2005, 2(5):443-454
#'
#' @param signal Signal values (Numeric | vector)
#' @param include_endpoints Whether to treat the endpoint of the signal as a pseudo-extreme point (Boolean)
#' @param max_smooth_iteration Maximum number of iterations of moving average algorithm (Integer)
#' @param max_envelope_iteration Maximum number of iterations when separating local envelope signals (Integer)
#' @param envelope_epsilon Terminate processing when obtaining pure FM signal (Double)
#' @param convergence_epsilon Terminate processing when modulation signal converges (Double)
#' @param max_num_pf The maximum number of PFs generated(Integer)
#'
#' @return list(pf,residue) | PFs:The decompose functions arranged from high frequency to low frequency | residue:residual component
#' @author Shubhra Prakash, \email{shubhraprakash279@@gmail.com}
#' @references \url{https://pypi.org/project/PyLMD/}
#' @keywords lmd Local mean decomposition Product functions Demodulation Fault diagnosis self-adaptive mechanism

#' @examples
#' x=1:100
#' y = (2 / 3 )* sin(x * 30) + (2 / 3) * sin(x * 17.5) + (4 / 5) *cos(x * 2)
#' plot(y,type="l")
#' lmd(y)
#' @export lmd


lmd <- function(signal,
                include_endpoints=TRUE,
                max_smooth_iteration=12,
                max_envelope_iteration=200,
                envelope_epsilon=0.01,
                convergence_epsilon=0.01,
                max_num_pf=8) {

  # browser()

  pf = list()
  count = 1
  # until the residual function is close to a monotone function
  residue = signal
  while ((length(pf) < max_num_pf) &&
         (!is_monotonous(residue)) &&
         (length(find_extrema(residue,include_endpoints=include_endpoints)) >= 3)) {
    component = extract_product_function(residue,max_envelope_iteration=max_envelope_iteration,envelope_epsilon=envelope_epsilon,convergence_epsilon=convergence_epsilon)
    residue = residue - component
    pf[[count]] = component
    count = count + 1

  }

  return(list("pf"=pf,"residue"=residue))

}





