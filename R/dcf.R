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
#' dcf(as.Date(2019-01-01"), as.Date("2019-01-31"), convention = "30/360")

dcf <- function(date_from = Sys.Date(), date_to = Sys.Date(), convention = "ACT/365", silent = FALSE) {
   
   if (convention == "ACT/365") {
      t <- as.numeric((date_to - date_from) / 365)
   }
   
   if (convention == "ACT/360") {
      t <- as.numeric((date_to - date_from) / 360)
   }
   
   if (convention == "30/360") {
      
      date_from <- ifelse(format(date_from, "%d")=="31", date_from -1, date_from)
      date_to <- ifelse(format(date_to, "%d")=="31", date_to -1, date_to)
      
      class(date_from) <- "Date" # ifelse sucks!
      class(date_to) <- "Date"
      
      t <- as.numeric(format(date_to, "%d")) - as.numeric(format(date_from, "%d")) # diff in days
      t <- t + 30 * (as.numeric(format(date_to, "%m")) - as.numeric(format(date_from, "%m"))) # diff in months
      t <- t + 360 * (as.numeric(format(date_to, "%Y")) - as.numeric(format(date_from, "%Y"))) # diff in years
      t <- t / 360
   }
   
   t # return a value
   
}