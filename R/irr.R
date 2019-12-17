#' Internal Rate of Return
#'
#' Rate, at which all cashflows will discount to exactly zero.
#'
#' @param cashflows Cash flows, as a vector
#' @param dates Value dates for the cashflows vector
#' @param convention Interest Rate convention; default is ACT/365
#' @param silent should warning messages be suppressed? Default is no.
#'
#' @importFrom stats uniroot
#'
#' @return IRR - as a fraction, per annum.
#' @export
#'
#' @examples
#' irr(
#'   dates = as.Date(c("2019-01-01", "2020-01-01")),
#'   cashflows = c(-1000, 1100)
#' )
irr <- function(cashflows, dates, convention = "ACT/365", silent = FALSE) {

  # is there at least one positive and one negative CF?
  if (all(cashflows < 0) | all(cashflows > 0)) {
    if (!silent) warning("IRR undefined; at least one positive & one negative cash flow are required.")
    return(NA)
  } # /if

  # do the cash flows & dates vectors match?
  if (length(cashflows) != length(dates)) {
    if (!silent) warning("IRR undefined; cash flows and value dates do not match.")
    return(NA)
  } # /if

  # let the uniroot begin!
  result <- uniroot(
    f = npv, interval = c(0, 1), tol = 1e-9,
    cashflows = cashflows,
    dates = dates,
    convention = convention
  )

  result$root
} # / function
