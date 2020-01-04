context("formal IRR")

test_that("discounting works", {
  expect_equal(
    irr(
      cashflows = c(-1000, 1100), 
      dates = as.Date(c("2019-01-01", "2020-01-01")) # 1 year
    ),
    10 / 100 # i.e. 10% p.a.
  )
})

test_that("vector length mismatch is handled", {

  # long cashflows, short dates
  expect_warning(irr(
    cashflows = c(1000, 2000, 3000),
    dates = as.Date(c("2019-01-01", "2020-01-01"))
  ))

  # long dates, short cash flows
  expect_warning(irr(
    cashflows = c(1000, 2000, 3000),
    dates = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01"))
  ))
})

test_that("type mismatch on input is handled", {
  
  # dates is character
  expect_warning(irr(
    dates = c("2019-01-01", "2020-01-01"), # 1 year
    cashflows = c(-1000, 1100)
  ))
  
  # cashflows is character
  expect_warning(irr(
    dates = as.Date(c("2019-01-01", "2020-01-01")), # 1 year
    cashflows = c("-1000", "1100")
  ))
  
})
