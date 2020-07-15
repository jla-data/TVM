context("formal NPV")

test_that("discounting works", {
  expect_equal(
    npv(
      irate = 10 / 100, # i.e. 10% p.a.
      dates = as.Date(c("2019-01-01", "2020-01-01")), # 1 year
      cashflows = c(0, 1100)
    ),
    1000
  )
})

test_that("zero interest rate: PV = FV", {
  expect_equal(
    npv(
      irate = 0 / 100, # i.e. 0% p.a.
      dates = as.Date(c("2019-01-01", "2029-01-01")), # ten years
      cashflows = c(0, 1000)
    ),
    1000
  )
})

test_that("single cash flow = no discounting", {

  # what is, is
  expect_equal(
    npv(
      irate = 35 / 100, # i.e. 35% p.a.
      dates = as.Date(c("2019-01-01")),
      cashflows = c(1000)
    ),
    1000
  )
})

test_that("vector length mismatch is handled", {

  # long cashflows, short dates, silence default
  expect_warning(npv(
    irate = 10 / 100, # i.e. 10% p.a.
    dates = as.Date(c("2019-01-01", "2020-01-01")),
    cashflows = c(0, 1100, 500)
  ))

  # long dates, short cash flows, silence default
  expect_warning(npv(
    irate = 10 / 100, # i.e. 10% p.a.
    dates = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01")),
    cashflows = c(0, 1100)
  ))

  # mismatch, but silent is TRUE - still undefined, but quietly
  expect_equal(
    is.na(npv(
      irate = 10 / 100, # i.e. 10% p.a.
      dates = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01")),
      cashflows = c(1000),
      silent = T
    )),
    TRUE
  )
  
  # long convention
  expect_warning(npv(
    irate = 10 / 100, # i.e. 10% p.a.
    dates = as.Date(c("2019-01-01", "2020-01-01")),
    cashflows = c(0, 1100),
    convention = c("ACT/360", "ACT/365")
  ))
  
})

test_that("NULL on input is handled", {
  
  # NULL covention
  expect_warning(npv(
    irate = 10 / 100, # i.e. 10% p.a.
    dates = as.Date(c("2019-01-01", "2020-01-01")),
    cashflows = c(0, 1100),
    convention = NULL
  ))
  
  # NULL convention, but silent is TRUE - still failing, but quietly
  expect_equal(
    is.na(npv(
      irate = 10 / 100, # i.e. 10% p.a.
      dates = as.Date(c("2019-01-01", "2020-01-01")),
      cashflows = c(0, 1100),
      convention = NULL,
      silent = T
    )),
    TRUE
  )
  
  # NULL cashflow
  expect_warning(npv(
    irate = 10 / 100, # i.e. 10% p.a.
    dates = as.Date(c("2019-01-01", "2020-01-01")),
    cashflows = NULL
  ))
  
  # NULL cashflow, but silent is TRUE - still failing, but quietly
  expect_equal(
    is.na(npv(
      irate = 10 / 100, # i.e. 10% p.a.
      dates = as.Date(c("2019-01-01", "2020-01-01")),
      cashflows = NULL,
      silent = T
    )),
    TRUE
  )  
  
  # NULL interest
  expect_warning(npv(
    irate = 10 / 100, # i.e. 10% p.a.
    dates = NULL,
    cashflows = c(0, 1100)
  ))
  
  # NULL interest, but silent is TRUE - still failing, but quietly
  expect_equal(
    is.na(npv(
      irate = 10 / 100, # i.e. 10% p.a.
      dates = NULL,
      cashflows = c(0, 1100),
      silent = T
    )),
    TRUE
  )
  
  # NULL dates
  expect_warning(npv(
    irate = NULL,
    dates = as.Date(c("2019-01-01", "2020-01-01")),
    cashflows = c(0, 1100)
  ))
  
  # NULL dates, but silent is TRUE - still failing, but quietly
  expect_equal(
    is.na(npv(
      irate = NULL,
      dates = as.Date(c("2019-01-01", "2020-01-01")),
      cashflows = c(0, 1100),
      silent = T
    )),
    TRUE
  )
  
  # NULL silent
  expect_warning(npv(
    irate = 10 / 100, # i.e. 10% p.a.
    dates = as.Date(c("2019-01-01", "2020-01-01")),
    cashflows = c(0, 1100),
    silent = NULL
  ))
  
})

test_that("type mismatch on input is handled", {

  # interest is logical
  expect_warning(npv(
    irate = TRUE,
    dates = as.Date(c("2019-01-01", "2020-01-01")), # 1 year
    cashflows = c(0, 1100)
  ))

  # interest is character
  expect_warning(npv(
    irate = "karel",
    dates = as.Date(c("2019-01-01", "2020-01-01")), # 1 year
    cashflows = c(0, 1100)
  ))

  # dates is character
  expect_warning(npv(
    irate = 10 / 100,
    dates = c("2019-01-01", "2020-01-01"), # 1 year
    cashflows = c(0, 1100)
  ))

  # cashflows is character
  expect_warning(npv(
    irate = 10 / 100,
    dates = as.Date(c("2019-01-01", "2020-01-01")), # 1 year
    cashflows = c("0", "1100")
  ))
  
  # null interest
  expect_warning(npv(
    irate = NULL,
    dates = as.Date(c("2019-01-01", "2020-01-01")), # 1 year
    cashflows = c(0, 1100)
  ))
  
  # interest longer than cash flows
  expect_warning(npv(
    irate = c(10, 20, 30) / 100,
    dates = as.Date(c("2019-01-01", "2020-01-01")), # 1 year
    cashflows = c(0, 1100)
  ))
  
  # cash flows not a multiple of interest (cannot be recycled)
  expect_warning(npv(
    irate = c(10, 20) / 100,
    dates = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01")),
    cashflows = c(0, 100, 200)
  ))
  
  # interest longer than 1, and equal to cash flows
  expect_message(npv(
    irate = c(10, 20, 30) / 100,
    dates = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01")),
    cashflows = c(0, 100, 200)
  ))
  
})
