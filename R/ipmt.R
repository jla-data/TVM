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
   
   
   result <- -(((1+irate)^(per-1)) * (pv*irate + pmt(irate, nper, pv, fv=0, type=type)) - pmt(irate, nper, pv, fv=0, type=type))
   
   result
   
}