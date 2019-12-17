
test_that("plain vanilla ten percent p.a.", {
  expect_equal(
    npv(
      interest = 10 / 100, # i.e. 10% PA
      dates = as.Date(c("2019-01-01", "2020-01-01")),
      cashflows = c(0, 1100)
    ),
    1000
  )
})

test_that("zero interest rate: PV = FV", {
  expect_equal(
    npv(
      interest = 0 / 100, # i.e. 0% PA
      dates = as.Date(c("2019-01-01", "2020-01-01")),
      cashflows = c(0, 1000)
    ),
    1000
  )
})

test_that("single cash flow = no discounting", {
  expect_equal(
    npv(
      interest = 35 / 100, # i.e. 35% PA
      dates = as.Date(c("2019-01-01")),
      cashflows = c(1000)
    ),
    1000
  )
})

test_that("vector length issues", {
  expect_warning(npv(
    interest = 10 / 100, # i.e. 10% PA
    dates = as.Date(c("2019-01-01", "2020-01-01")),
    cashflows = c(0, 1100, 500)
  ))
  expect_warning(npv(
    interest = 10 / 100, # i.e. 10% PA
    dates = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01")),
    cashflows = c(0, 1100)
  ))
  expect_equal(
    is.na(npv(
      interest = 10 / 100, # i.e. 10% PA
      dates = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01")),
      cashflows = c(1000),
      silent = T
    )),
    TRUE
  )
})
