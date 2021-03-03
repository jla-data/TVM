test_that("a sum is a sum", {
   expect_equal(
      pmt(
         irate = 0,
         nper = 5,
         pv = -500
      ),
      100 # i.e. 5 * 100 = 500
   )
})

test_that("input type mismatch", {
   expect_warning(
      pmt(
         irate = "karel",
         nper = 5,
         pv = -500
      )
   )
   
   expect_warning(
      pmt(
         irate = 1/10,
         nper = "karel",
         pv = -500
      )
   )
   
   expect_warning(
      pmt(
         irate = 1/10,
         nper = 5,
         pv = "karel"
      )
   )
   
   expect_warning(
      pmt(
         irate = 1/10,
         nper = 5,
         pv = -500,
         fv = "karel"
      )
   )
   
   expect_warning(
      pmt(
         irate = 1/10,
         nper = 5,
         pv = -500,
         type = "karel"
      )
   )
   
   # non-integer number of periods
   expect_warning(
      pmt(
         irate = 1/10,
         nper = pi,
         pv = -500,
         type = "karel"
      )
   )
})


test_that("input lenght mismatch", {
   
   # 2 nper, 3 pv
   expect_warning(
      pmt(
         irate = 1/10,
         nper = c(5, 10),
         pv = c(-500, -1000, -1500)
      )
   )
   
   
   # same thing, but silently
   expect_silent(
      pmt(
         irate = 1/10,
         nper = c(5, 10),
         pv = c(-500, -1000, -1500),
         silent = TRUE
      )
   )
   
   
   # silent, but still wrong
   expect_equal(
      pmt(
         irate = 1/10,
         nper = c(5, 10),
         pv = c(-500, -1000, -1500),
         silent = TRUE
      ),
      NA
   )
   
})



test_that("mulitple input = multiple output", {
   
   # legit vector - 1 vector input irate
   expect_vector(
      pmt(
         irate = c(0, 1/10),
         nper = 5,
         pv = -500
      ),
      ptype = double(),
      size = 2
   )
   
   
   
   # legit vector - 1 vector input pv
   expect_vector(
      pmt(
         irate = 0,
         nper = 5,
         pv = c(-500, -1000)
      ),
      ptype = double(),
      size = 2
   )

   # legit vector - 1 vector input nper
   expect_vector(
      pmt(
         irate = 0,
         nper = c(5, 10),
         pv = -500
      ),
      ptype = double(),
      size = 2
   )
   
   # legit vector - 1 vector input fv
   expect_vector(
      pmt(
         irate = 0,
         nper = 5,
         pv = -500,
         fv = c(0, -100)
      ),
      ptype = double(),
      size = 2
   )
   

})
