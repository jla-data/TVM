test_that("interest works", {
   expect_equal(
      ppmt(
         irate = 1/10,
         per = 1,
         nper = 1,
         pv = -500
      ),
      500 # single period = entire principal
   )
})


test_that("input lenght mismatch", {
   
   # 2 nper, 3 pv
   expect_warning(
      ppmt(
         irate = 1/10,
         nper = 1,
         per = 2,
         pv = -500
      )
   )
   
   
   # same thing, but silently
   expect_silent(
      ppmt(
         irate = 1/10,
         nper = 1,
         per = 2,
         pv = -500,
         silent = TRUE
      )
   )
   
   
   # silent, but still wrong
   expect_equal(
      ppmt(
         irate = 1/10,
         nper = 1,
         per = 2,
         pv = -500,
         silent = TRUE
      ),
      NA
   )
   
})

