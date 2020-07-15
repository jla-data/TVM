#' Annuity payment
#' 
#' Calculation of annuity payment.
#'
#' @param irate Interest rate per period (see nper)
#' @param nper Number of interest periods
#' @param pv present value
#' @param fv future value (default is zero)
#' @param type type of the annuity: due or immediate?
#' @param silent Should warning messages be suppressed? Default is no.
#'
#' @return annuity payment
#' @export
#' 
#' @source builds heavily on \url{https://gist.github.com/raadk/dcd503815bbb271484ff}
#'
#' @examples
#' 
pmt <- function(irate, nper, pv, fv = 0, type = "immediate", silent = FALSE) {
   
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
   
   
   
   ntype <- ifelse(type == "immediate", 0, 1)
   
   if (irate == 0) {
      
      pmt <- -1 * (fv + pv) / nper
      return(pmt)
   } 
   
   result <- irate * (fv + pv * (1 + irate) ^ nper) / ((1 + irate * ntype) * (1 - (1 + irate) ^ nper))
   
   result
}