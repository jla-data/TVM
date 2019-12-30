#' DCF – Day Count Fraction
#'
#' @param date_from Start date (or a vector of dates)
#' @param date_to End date (or a vector of dates)
#'
#' @inheritParams npv
#'
#' @return Day Count Fraction - difference in days divided by days per year
#'
#' @export
#'
#' @examples
#' dcf(as.Date("2019-01-01"), as.Date("2019-01-31"), convention = "30/360")
dcf <- function(date_from = Sys.Date(), date_to = Sys.Date(), convention = "ACT/365", silent = FALSE) {
  if (convention == "ACT/365") {
    t <- as.numeric((date_to - date_from) / 365)
  } # /if act/365

  if (convention == "ACT/360") {
    t <- as.numeric((date_to - date_from) / 360)
  } # /if act/360

  if (convention == "30/360") { # ISDA 4.16 (f)

    D1 <- ifelse(format(date_from, "%d") == "31", 30, as.numeric(format(date_from, "%d")))
    D2 <- ifelse(format(date_to, "%d") == "31" & format(date_from, "%d") %in% c("30", "31"), 30, as.numeric(format(date_to, "%d")))
    M1 <- as.numeric(format(date_from, "%m"))
    M2 <- as.numeric(format(date_to, "%m"))
    Y1 <- as.numeric(format(date_from, "%Y"))
    Y2 <- as.numeric(format(date_to, "%Y"))

    t <- D2 - D1 # diff in days
    t <- t + 30 * (M2 - M1) # diff in months
    t <- t + 360 * (Y2 - Y1) # diff in years
    t <- t / 360
  } # /if 30/360

  if (convention == "30E/360") { # ISDA 4.16 (g)

    D1 <- ifelse(format(date_from, "%d") == "31", 30, as.numeric(format(date_from, "%d")))
    D2 <- ifelse(format(date_to, "%d") == "31", 30, as.numeric(format(date_to, "%d")))
    M1 <- as.numeric(format(date_from, "%m"))
    M2 <- as.numeric(format(date_to, "%m"))
    Y1 <- as.numeric(format(date_from, "%Y"))
    Y2 <- as.numeric(format(date_to, "%Y"))

    t <- D2 - D1 # diff in days
    t <- t + 30 * (M2 - M1) # diff in months
    t <- t + 360 * (Y2 - Y1) # diff in years
    t <- t / 360
  } # /if 30E/360

  t # return a value
}
