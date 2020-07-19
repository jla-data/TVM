#' Principal portion of annuity payment
#' 
#' Calculation of principal repayment portion of an anuity payment.
#' 
#' @inheritParams pmt
#' @param per period of interest
#'
#' @return principal repayment
#' @export
#' 
#' @source builds heavily on \url{https://gist.github.com/raadk/dcd503815bbb271484ff}
#'
#' @examples
#' ppmt(.1, 1, 1, -1000) # a single period >> entire principal is repaid immediately
#' 
#' ppmt(.1, 1:5, 5, -1000) # see five payments of five; the amount of principal goes up
#' 
ppmt <- function(irate, per, nper, pv, fv = 0, type = "immediate", silent = FALSE) {
   
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
   if (max(per) > max(nper)) {
      if (!silent) warning("Period must be within total periods; check `per` and `nper` arguments.")
      return(NA)
   }
   
   
   result <- pmt(irate, nper, pv, fv, type) - ipmt(irate, per, nper, pv, fv, type)
   
   result
   
}