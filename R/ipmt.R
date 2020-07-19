#' Interest portion of annuity payment
#' 
#' Calculation of annuity interest.
#' 
#' @inheritParams pmt
#' @param per period of interest
#'
#' @return interest on annuity
#' @export
#' 
#' @source builds heavily on \url{https://gist.github.com/raadk/dcd503815bbb271484ff}
#'
#' @examples
#' ipmt(.1, 1, 1, -1000) # a single period at 10\% interest >> interest = 1/10 of principal
#' 
#' ipmt(.1, 1:5, 5, -1000) # see five payments of five; the amount of interest goes down
#' 
ipmt <- function(irate, per, nper, pv, fv = 0, type = "immediate", silent = FALSE) {
   
   # is interest rate valid?
   if (!length(irate) == 1) {
      if (!silent) warning("A single interest rate is required.")
      return(NA)
   } # /if
   
   # is type recognized?
   if (!length(type) == 1 & type %in% c("due", "immediate")) {
      if (!silent) warning("Not a recognized type of annuity; check the `type` argument.")
      return(NA)
   } # /if
   
   # is per compatible with nper?
   if (any(max(per) > max(nper), per <= 0)) {
      if (!silent) warning("Period must be greater than zero and less than total periods; check `per` and `nper` arguments.")
      return(NA)
   }
   
   result <- -(((1+irate)^(per-1)) * (pv*irate + pmt(irate, nper, pv, fv=0, type=type)) - pmt(irate, nper, pv, fv=0, type=type))
   
   result
   
}