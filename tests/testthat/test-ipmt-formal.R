
test_that("interest works", {
   expect_equal(
      ipmt(
         irate = 1/10,
         per = 1,
         nper = 1,
         pv = -500
      ),
      50 # i.e. 10% of 500 = 50
   )
})


test_that("input lenght mismatch", {
   
   # 2 nper, 3 pv
   expect_warning(
      ipmt(
         irate = 1/10,
         nper = 1,
         per = 2,
         pv = -500
      )
   )
   
   
   # same thing, but silently
   expect_silent(
      ipmt(
         irate = 1/10,
         nper = 1,
         per = 2,
         pv = -500,
         silent = TRUE
      )
   )
   
   
   # silent, but still wrong
   expect_equal(
      ipmt(
         irate = 1/10,
         nper = 1,
         per = 2,
         pv = -500,
         silent = TRUE
      ),
      NA
   )
   
})

test_that("nper & per mismatch", {
   
   # pi > 2
   expect_warning(
      ipmt(
         irate = 1/10,
         nper = 2,
         per = pi,
         pv = -500
      )
   )
   
   
   # same thing, but silently
   expect_silent(
      ipmt(
         irate = 1/10,
         nper = 2,
         per = pi,
         pv = -500,
         silent = TRUE
      )
   )
   
   
   # silent, but still wrong
   expect_equal(
      ipmt(
         irate = 1/10,
         nper = 2,
         per = pi,
         pv = -500,
         silent = TRUE
      ),
      NA
   )
   
})
