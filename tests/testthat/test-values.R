# dates from https://www.isda.org/2008/12/22/30-360-day-count-conventions/

test_that("easy date", {
  expect_equal(
    dcf(date_from = as.Date("2007-01-15"), date_to = as.Date("2007-01-30"), convention = "ACT/360"),
    15 / 360
  )
  expect_equal(
    dcf(date_from = as.Date("2007-01-15"), date_to = as.Date("2007-01-30"), convention = "ACT/365"),
    15 / 365
  )
  expect_equal(
    dcf(date_from = as.Date("2007-01-15"), date_to = as.Date("2007-01-30"), convention = "30/360"),
    15 / 360
  )
})

test_that("ACT less than 30", {
  expect_equal(
    dcf(date_from = as.Date("2007-02-28"), date_to = as.Date("2007-03-05"), convention = "ACT/360"),
    5 / 360
  )
  expect_equal(
    dcf(date_from = as.Date("2007-02-28"), date_to = as.Date("2007-03-05"), convention = "ACT/365"),
    5 / 365
  )
  expect_equal(
    dcf(date_from = as.Date("2007-02-28"), date_to = as.Date("2007-03-05"), convention = "30/360"),
    7 / 360
  )
})

test_that("ACT more than 30", {
  expect_equal(
    dcf(date_from = as.Date("2007-09-30"), date_to = as.Date("2008-03-31"), convention = "ACT/360"),
    183 / 360
  )
  expect_equal(
    dcf(date_from = as.Date("2007-09-30"), date_to = as.Date("2008-03-31"), convention = "ACT/365"),
    183 / 365
  )
  expect_equal(
    dcf(date_from = as.Date("2007-09-30"), date_to = as.Date("2008-03-31"), convention = "30/360"),
    180 / 360
  )
})

test_that("leap year", {
  expect_equal(
    dcf(date_from = as.Date("2007-09-30"), date_to = as.Date("2008-09-30"), convention = "ACT/360"),
    366 / 360
  )
  expect_equal(
    dcf(date_from = as.Date("2007-09-30"), date_to = as.Date("2008-09-30"), convention = "ACT/365"),
    366 / 365
  )
  expect_equal(
    dcf(date_from = as.Date("2007-09-30"), date_to = as.Date("2008-09-30"), convention = "30/360"),
    360 / 360
  )
})



test_that("flavors of NPV", {
  expect_equal(
    npv(
      interest = 10 / 100, # i.e. 10% p.a.
      dates = as.Date(c("2019-01-01", "2020-01-01")), # 1 year
      method = "compound",
      cashflows = c(0, 1100)
    ),
    1000
  )
  
  expect_equal(
    npv(
      interest = 10 / 100, # i.e. 10% p.a.
      dates = as.Date(c("2019-01-01", "2020-01-01")), # 1 year
      method = "simple",
      cashflows = c(0, 1100)
    ),
    1000
  )
})

test_that("flavors of IRR", {
  expect_equal(
    irr(
      cashflows = c(-1000, 1100),
      dates = as.Date(c("2019-01-01", "2020-01-01")) # 1 year
    ),
    10 / 100 # i.e. 10% p.a.
  )
})
