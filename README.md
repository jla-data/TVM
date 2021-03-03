# TVM - Time Value of Money

<!-- badges: start --> 
[![R-CMD-check](https://github.com/jlacko/TVM/workflows/R-CMD-check/badge.svg)](https://github.com/jlacko/TVM/actions)
 [![Codecov test coverage](https://codecov.io/gh/jlacko/TVM/branch/master/graph/badge.svg)](https://codecov.io/gh/jlacko/TVM?branch=master) [![CRAN](http://www.r-pkg.org/badges/version/TVM)](https://cran.r-project.org/package=TVM)
<!-- badges: end -->

Fifty shades of cash flow discounting.

Currently implemented functions (loosely inspired by MS Excel, but vectorized):

-   **dcf** – Day Count Fraction
-   **irr** – Internal Rate of Return
-   **npv** – Net Present Value
-   **pmt** – Annuity Payment
-   **ipmt** – interest part of an Annuity Paymnet
-   **ppmt** – principal part of an Annuity Payment

With hopefully more to come, but not very actively developed at present.

Implemented with minimal dependencies, mostly in base R.
