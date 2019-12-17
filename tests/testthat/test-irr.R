test_that("plain vanilla ten percent p.a.", {
  expect_equal(
    irr(
      cashflows = c(-1000, 1100),
      dates = as.Date(c("2019-01-01", "2020-01-01"))
    ),
    10 / 100
  ) # i.e. 10%
})

test_that("vector length issues", {
  expect_warning(irr(
    cashflows = c(-1000, 1100, 500),
    dates = as.Date(c("2019-01-01", "2020-01-01"))
  ))
  expect_warning(irr(
    cashflows = c(-1000, 1100),
    dates = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01"))
  ))
})
