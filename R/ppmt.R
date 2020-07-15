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
   
   
   result <- pmt(irate, nper, pv, fv, type) - ipmt(irate, per, nper, pv, fv, type)
   
   result
   
}