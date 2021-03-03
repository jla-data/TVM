#' IRR – Internal Rate of Return
#'
#' Rate at which all cashflows will be discounted to exactly zero.
#'
#' @inheritParams npv
#'
#' @return IRR as a fraction, per annum basis; recognized interval is -50\% – +150\% p.a.
#'
#' @seealso npv, dcf
#'
#' @export
#'
#' @importFrom stats uniroot
#'
#' @examples
#' irr(
#'   dates = as.Date(c("2019-01-01", "2020-01-01")),
#'   cashflows = c(-1000, 1100)
#' )
#'

irr <- function(cashflows, dates, method = "compound", convention = "ACT/365", silent = FALSE) {

  # check input validity ----

  # is there at least one positive and one negative CF?
  if (all(cashflows < 0) | all(cashflows > 0)) {
    if (!silent) warning("IRR undefined; at least one positive & one negative cash flow are required.")
    return(NA)
  } # /if

  # is method recognized?
  if (!length(method) == 1 & method %in% c("compound", "simple")) {
    warning("Not a recognized calculation method; check the `method` argument.")
    return(NA)
  } # /if

  # do the cash flows & dates vectors match?
  if (length(cashflows) != length(dates)) {
    if (!silent) warning("IRR undefined; cash flows and value dates do not match.")
    return(NA)
  } # /if

  # is the dates vector of date type?
  if (!inherits(dates, "Date")) {
    if (!silent) warning("Impossible to determine value dates; check the `dates` argument.")
    return(NA)
  } # /if

  # is cashflows numeric?
  if (!is.numeric(cashflows)) {
    if (!silent) warning("Cash flows impossible to determine; check the `cashflows` argument.")
    return(NA)
  } # /if

  # is convention valid?
  if (!length(convention) == 1 & convention %in% c("ACT/360", "ACT/365", "30/360")) {
    if (!silent) warning("A valid interest rate convention is required; consider ACT/365 if unsure.")
    return(NA)
  } # /if

  # let the uniroot begin! ----

  result <- uniroot(
    f = npv, interval = c(-.5, 1.5), tol = 1e-10,
    cashflows = cashflows,
    dates = dates,
    convention = convention,
    method = method
  )

  result$root
} # / function
