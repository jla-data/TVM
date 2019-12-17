#' Net Present Value
#'
#' Discounted value of future cash flows; the date of the first cash flow is considered as origin.
#'
#' @param interest Interest rate per annum
#' @param cashflows Cash flows, as a vector
#' @param dates Value dates for the cashflows vector
#' @param convention Interest rate convention; default is ACT/365
#' @param silent should warning messages be suppressed? Default is no.
#'
#' @return Net present value of future cash flows
#' @export
#'
#' @examples
#' npv(
#'   interest = 10 / 100,
#'   dates = as.Date(c("2019-01-01", "2020-01-01")),
#'   cashflows = c(0, 1100)
#' )
npv <- function(interest, cashflows, dates, convention = "ACT/365", silent = FALSE) {

  # do the cash flows & dates vectors match?
  if (length(cashflows) != length(dates)) {
    if (!silent) warning("NPV undefined; cash flows and value dates do not match.")
    return(NA)
  } # /if

  t <- as.numeric((dates - dates[1]) / 365) # konvence ACT / 366

  sum(cashflows / (1 + interest)^t)
}
