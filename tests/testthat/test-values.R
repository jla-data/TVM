context("values DCF")

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
  expect_equal(
    dcf(date_from = as.Date("2007-01-15"), date_to = as.Date("2007-01-30"), convention = "30E/360"),
    15 / 360
  )
})

test_that("period ending on 31th", {
  expect_equal(
    dcf(date_from = as.Date("2009-02-28"), date_to = as.Date("2009-08-31"), convention = "ACT/360"),
    184/ 360
  )
  expect_equal(
    dcf(date_from = as.Date("2009-02-28"), date_to = as.Date("2009-08-31"), convention = "ACT/365"),
    184 / 365
  )
  expect_equal(
    dcf(date_from = as.Date("2009-02-28"), date_to = as.Date("2009-08-31"), convention = "30/360"),
    183 / 360
  )
  expect_equal(
    dcf(date_from = as.Date("2009-02-28"), date_to = as.Date("2009-08-31"), convention = "30E/360"),
    182 / 360
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
  expect_equal(
    dcf(date_from = as.Date("2007-02-28"), date_to = as.Date("2007-03-05"), convention = "30E/360"),
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
  expect_equal(
    dcf(date_from = as.Date("2007-09-30"), date_to = as.Date("2008-03-31"), convention = "30E/360"),
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
  expect_equal(
    dcf(date_from = as.Date("2007-09-30"), date_to = as.Date("2008-09-30"), convention = "30E/360"),
    360 / 360
  )
})

test_that("february to february, leap year", {
  expect_equal(
    dcf(date_from = as.Date("2008-02-29"), date_to = as.Date("2009-02-28"), convention = "ACT/360"),
    365 / 360
  )
  expect_equal(
    dcf(date_from = as.Date("2008-02-29"), date_to = as.Date("2009-02-28"), convention = "ACT/365"),
    365 / 365
  )
  expect_equal(
    dcf(date_from = as.Date("2008-02-29"), date_to = as.Date("2009-02-28"), convention = "30/360"),
    359 / 360
  )
  expect_equal(
    dcf(date_from = as.Date("2008-02-29"), date_to = as.Date("2009-02-28"), convention = "30E/360"),
    359 / 360
  )
})

context("values NPV")

test_that("1 period simple = compound", {
  expect_equal(
    npv(
      irate = 10 / 100, # i.e. 10% p.a.
      dates = as.Date(c("2019-01-01", "2020-01-01")), # 1 year
      method = "compound",
      cashflows = c(0, 1100)
    ),
    npv(
      irate = 10 / 100, # i.e. 10% p.a.
      dates = as.Date(c("2019-01-01", "2020-01-01")), # 1 year
      method = "simple",
      cashflows = c(0, 1100)
    )
  )
})

context("values IRR")

test_that("1 period simple = compound", {
  expect_equal(
    irr(
      cashflows = c(-1000, 1100),
      dates = as.Date(c("2019-01-01", "2020-01-01")), # 1 year
      method = "simple"
    ),
  
    irr(
      cashflows = c(-1000, 1100),
      dates = as.Date(c("2019-01-01", "2020-01-01")), # 1 year
      method = "compound"
    )
  )
})

test_that("same CF, more than 1 period: simple > compound", {
  expect_gt(
    irr(
      cashflows = c(-1000, 1100),
      dates = as.Date(c("2019-01-01", "2020-07-01")), # 1 + 1/2 year
      method = "simple"
    ),
    
    irr(
      cashflows = c(-1000, 1100),
      dates = as.Date(c("2019-01-01", "2020-07-01")), # 1 +  1/2 year
      method = "compound"
    )
  )
})

test_that("same CF, less than 1 period: simple < compound", {
  expect_lt(
    irr(
      cashflows = c(-1000, 1100),
      dates = as.Date(c("2019-01-01", "2019-07-01")), # 1/2 year
      method = "simple"
    ),
    
    irr(
      cashflows = c(-1000, 1100),
      dates = as.Date(c("2019-01-01", "2019-07-01")), # 1/2 year
      method = "compound"
    )
  )
})
