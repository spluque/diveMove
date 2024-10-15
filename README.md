# diveMove

[![R build and check](https://github.com/spluque/diveMove/actions/workflows/build_check.yml/badge.svg)](https://github.com/spluque/diveMove/actions/workflows/build_check.yml)
[![CRAN_Release_Badge](https://www.r-pkg.org/badges/version-last-release/diveMove)](https://cran.r-project.org/package=diveMove)
[![CRAN_Downloads](https://cranlogs.r-pkg.org/badges/diveMove)](https://cran.r-project.org/package=diveMove)

diveMove is a GNU R package with tools to represent, visualize, filter,
analyse, and summarize time-depth recorder (TDR) data. It also provides
miscellaneous functions for handling location data.

Read ?diveMove for a quick overview of the major functionality.  A vignette
is also available by doing vignette("diveMove").

Dive analysis usually involves handling of large amounts of data, as new
instruments allow for frequent sampling of variables over long periods of
time.  The aim of this package is to make this process more efficient for
summarizing and extracting information gathered by time-depth recorders
(TDRs, hereafter).  The principal motivation for developing diveMove was to
provide more flexibility during the various stages of analysis than is
available in popular commercial software.  This is achieved by making the
results from intermediate calculations easily accessible, allowing the user
to make his/her own summaries beyond the default choices the package
provides.

## Installation

Get the released version from CRAN:

```R
install.packages("diveMove")
```

Or the development version from GitHub:

```R
# install.packages("devtools")
devtools::install_github("spluque/diveMove")
```

Python users can access the package's functionality via
[scikit-diveMove](https://github.com/spluque/scikit-diveMove).
