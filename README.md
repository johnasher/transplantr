transplantr
================

The *transplantr* package provides a set of vectorised functions for
audit and clinical research in solid organ transplantation. These are
particularly intended to work well with multiple datapoints in large
series of data, where manual calculations would be particularly tedious.

The functions provided fall into three groups:

  - Donor and recipient risk indices
  - HLA mismatch level calculators
  - Estimated GFR calculators
  - Biochemical unit converters

Although the package was built with unit tests, inaccuracies cannot be
completely excluded. it is not a medical device and should not be used
for making clinical decisions.

# Installation

transplantr can be installed from CRAN:

``` r
# install transplantr
install.packages("transplantr")

# load transplantr once installed
library(transplantr)
```

## Development version

The development version can be installed from GitHub, if you want all
the latest features, together with all the latest bugs and errors.
Installing from CRAN is the best option for most users as the submitted
packages have to pass some very pedantic automated tests before they can
be hosted on CRAN. If you do want the *caveat emptor*, you have been
warned version, this is how:

``` r
# install from GitHub
devtools::install_packages("johnasher/transplantr")
```

# Tips on using *transplantr*

As vectorised functions, the functions can be applied across a whole
dataset fairly rapidly. I find that the easiest way to do this is using
a “pipe” of functions from the *dplyr* package. *dplyr* can be installed
on its own or, as I would recommend, by installing the whole *tidyverse*
family of packages - a family which includes the legendary *ggplot2*
graphing package.

``` r
# install whole tidyverse
install.packages("tidyverse")

# install just dplyr
install.packages("dplyr")
```

Although recommended, *dplyr* is not necessary for most *transplantr*
functions to work. *dplyr* is needed for the EPTS and KDPI functions,
and additionally *stringr* is needed for the `hla_mm_level_str()`
function and also for the `chi2dob()` function, one unlikely to be
needed by anyone working outside Scotland\!

## Biochemical units

By default, all the functions work with the units most commonly used in
the UK, which for creatinine and bilirubin is µmol/l, but each function
using either of these can be used with mg/dl instead by changing an
optional `units` parameter to `"US"` or by calling a wrapper function
suffixed with `_US()`; e.g. when calculating eGFR, the `ckd_epi_US()`
function calls `ckd_epi()` using creatinine in mg/dl.

Albumin is generally reported in g/l in the UK, but more commonly as
g/dl in the US. The few functions using albumin default to g/l but
change to g/dl if the `units` parameter is set to `"US"` or the `_US()`
wrapper function is called.

Which is the best option to use? Calling the wrapper function uses fewer
keystrokes so is quicker to type, but as it is a function calling
another function, there is a slight increase in computational overhead.

## Using *transplantr* functions with *dplyr*

Let’s say you want to calculate MELD scores for a series of liver
transplant candidates. OK, you probably actually want MELD-Na, but let’s
go with MELD as it has fewer variables\! The data is in a dataframe or
tibble called “oltx.assessments” and the relevant variables are
Patient.INR, Patient.Bilirubin, Patient.Creatinine and Patient.Dialysed.
To add a new Patient.MELD variable to the dataframe, you would use a
*dplyr* pipe with the `mutate()` verb:

``` r
oltx.assessments <- oltx.assessments %>%
  mutate(Patient.MELD = meld(INR = Patient.INR, bili = Patient.Bilirubin,
        creat = Patient.Creatinine, dialysis = Patient.Dialysed, units = "SI"))
```

The `units = "SI"` can be left out provided that creatinine and
bilirubin are both in µmol/l. To switch to mg/dl, use `units = "US"` or
call `meld_US()` instead.

## Using *transplantr* functions with base R

Although I think *dplyr* makes life much easier when organising data, I
concede that some people prefer to use base R functions instead. Using a
vectorised function with multiple vector inputs is not easy in base R
but can be done with the `mapply()`, or more easily with the
`pmap_dbl()` from the *purrr* package.

``` r
# attach oltx.assessments to save a lot of typing!
attach(oltx.assessments)

# method using pmap_dbl()
oltx.assessments$Patient.MELD = pmap_dbl(list(Patient.INR, Patient.Bilirubin,
                                              Patient.Creatinine, Patient.Dialysed),
                                          meld, units = "SI")

# alternative method using mapply()
oltx.assessments$Patient.MELD = mapply(FUN = meld, Patient.INR, Patient.Bilirubin,
                                        Patient.Creatinine, Patient.Dialysed,
                                        MoreArgs = list(units = "SI"),
                                        SIMPLIFY = TRUE)

# detach oltx.assessments to avoid namespace errors
detach(oltx.assessments)
```

The advantage of using a *dplyr* pipe, apart from easier code, is speed.
Benchmarking on a basic Linux laptop showed that the median time to
perform vectorised calculation of 100,000 MELD scores was 115
milliseconds, compared with 6007 milliseconds using \``pmap_dbl()` and
6484 with `mapply()`.

## Using the functions with a single case

Although vectorised functions for multiple calculations are one of the
best features of R, you might just want to collect data on a single
case. This is very straightforward:

``` r
# using µmol/l
meld(INR = 2.1, bili = 34, creat = 201, dialysis = 0)

# using mg/dl
meld(INR =  2.1, bili = 2.0, creat = 2.3, dialysis = 0, units = "US")

# using mg/dl with wrapper function
meld_US(INR =  2.1, bili = 2.0, creat = 2.3, dialysis = 0)
```

# More information

For more information, function documentation and usage vignettes, visit
[transplantr.txtools.net](https://transplantr.txtools.net/).
