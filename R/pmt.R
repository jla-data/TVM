#' Annuity payment
#' 
#' Calculation of annuity payment. 
#' 
#' The function expects a single interest rate; number of periods, present and 
#' future values can be vectors, and in such case a vector would be returned.
#' 
#'
#' @param irate Interest rate per period (see nper) as fraction (i.e. 5\% is .05 or 1/20)
#' @param nper Number of interest periods as integer (no fractions)
#' @param pv Present value
#' @param fv Future value (default is zero)
#' @param type type of the annuity: due or immediate?
#' @param silent Should warning messages be suppressed? Default is no.
#'
#' @return annuity payment
#' @export
#' 
#' @source builds heavily on \url{https://gist.github.com/raadk/dcd503815bbb271484ff}
#'
#' @examples
#' pmt(.1, 5, -1000) # a loan of 1000 units to be repaid in 5 periods at 10\% per period
#' 
#' 
pmt <- function(irate, nper, pv, fv = 0, type = "immediate", silent = FALSE) {
   
   # are vectorized inputs compatible? all lengths must be equal (or 1)
   inputs <- c(length(irate),
               length(nper),
               length(pv),
               length(fv))
   
   if(length(unique(inputs[inputs>1])) > 1) {
      if (!silent) warning("Incompatible length of inputs; all inputs must be of equal lenght (or scalar).")
      return(NA)
   } # /if
   
   if (!is.double(irate)) {
      if (!silent) warning("Unexpected interest rate; check value of `irate` argument.")
      return(NA)
   }
   
   if (!is.numeric(nper)) {
      if (!silent) warning("Unexpected number of periods; check value of `nper` argument.")
      return(NA)
   }
   
   if (any(nper %%1 != 0)) {
      if (!silent) warning("The number of periods should be integer (no fractions); check value of `nper` argument.")
      return(NA)
   }
   
   if (!is.numeric(pv)) {
      if (!silent) warning("Unexpected present value; check value of `pv` argument.")
      return(NA)
   }
   
   if (!is.numeric(fv)) {
      if (!silent) warning("Unexpected future value; check value of `fv` argument.")
      return(NA)
   }
   
   
   # is type recognized?
   if (!length(type) == 1 || !type %in% c("due", "immediate")) {
      if (!silent) warning("Not a recognized type of annuity; check the `type` argument.")
      return(NA)
   } # /if
   
   
   # does irate need recycling for the ifelse to work as expected?
   if(length(irate) < max(inputs)) {
      irate <- rep(irate, max(inputs))
   }

   
   ntype <- ifelse(type == "immediate", 0, 1)
   
   result <- ifelse(irate == 0,
                    -1 * (fv + pv) / nper,
                    irate * (fv + pv * (1 + irate) ^ nper) /  ((1 + irate * ntype) * (1 - (1 + irate) ^ nper)))
   
   result
}