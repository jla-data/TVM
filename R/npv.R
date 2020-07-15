#' NPV – Net Present Value
#'
#' Discounted value of future cash flows; the date of the first cash flow is
#' considered as origin.
#'
#'
#' @param irate Interest rate per annum. Either a scalar value, or vector.
#' @param cashflows Cash flows, as a vector.
#' @param dates Value dates for the cashflows vector.
#' @param method Compound or simple interest? Default is compound.
#' @param convention Interest rate convention; default is "ACT/365".
#' @param silent Should warning messages be suppressed? Default is no.
#'
#' @return Net present value of future cash flows
#'
#' @seealso dcf
#'
#' @export
#'
#' @examples
#' one_thousand <- npv(
#'   irate = 10 / 100, # i.e. 10% p.a.
#'   dates = as.Date(c("2019-01-01", "2020-01-01")), # i.e. one year
#'   cashflows = c(0, 1100)
#' )
#'
#' cat(one_thousand)
#' 

npv <- function(irate, cashflows, dates, method = "compound", convention = "ACT/365", silent = FALSE) {

  # check input validity ----

  # is method recognized?
  if (!length(method) == 1 & method %in% c("compound", "simple")) {
    warning("Not a recognized calculation method; check the `method` argument.")
    return(NA)
  } # /if

  # is silent NULL?
  if (is.null(silent)) {
    warning("NULL is not a recognized argument.")
    return(NA)
  } # /if

  # is any other argument null?
  if (any(sapply(list(irate, cashflows, dates, convention), is.null))) {
    if (!silent) warning("NULL is not a recognized argument.")
    return(NA)
  } # /if

  # do the cash flows & dates vectors match?
  if (length(cashflows) != length(dates)) {
    if (!silent) warning("NPV undefined; cash flows and value dates length does not match.")
    return(NA)
  } # /if

  # is the dates vector of date type?
  if (!inherits(dates, "Date")) {
    if (!silent) warning("Impossible to determine value dates; check the `dates` argument.")
    return(NA)
  } # /if

  # is interest numeric?
  if (!is.numeric(irate)) {
    if (!silent) warning("Impossible to determine interest rate; check the `irate` argument.")
    return(NA)
  } # /if

  # is cashflows numeric?
  if (!is.numeric(cashflows)) {
    if (!silent) warning("Cash flows impossible to determine; check the `cashflows` argument.")
    return(NA)
  } # /if

  # can interest be recycled?
  if ((length(cashflows) %% length(irate)) != 0) {
    if (!silent) warning("Impossible to determine discount rate; check the `irate` argument.")
    return(NA)
  } # /if

  # is interest vector or scalar?
  if (length(irate) > 1) {
    if (!silent) message("Multiple interest rates detected; will be applied as vector (recycled if necessary).")
  } # /if

  # is convention scalar?
  if (length(convention) > 1) {
    if (!silent) warning("A single interest rate convention is required.")
    return(NA)
  } # /if

  # is convention valid?
  if (!length(convention) == 1 & convention %in% c("ACT/360", "ACT/365", "30/360", "30E/360")) {
    if (!silent) warning("A valid interest rate convention is required; consider ACT/365 if uncertain")
    return(NA)
  } # /if

  # inputs are OK, real work starts here :) ------------------------------

  t <- dcf(date_to = dates, date_from = dates[1], convention = convention)

  if (method == "compound") {
    sum(cashflows / (1 + irate)^t)
  } else { # not compound = simple (two valid options)
    sum(cashflows / (1 + irate * t))
  } # /if method
}
