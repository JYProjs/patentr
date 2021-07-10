# patentr: Access USPTO Bulk Data in Tidy Rectangular Format

[![Travis-CI Build Status](https://api.travis-ci.com/JYProjs/patentr.svg?branch=main)](https://travis-ci.com/github/JYProjs/patentr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/JYProjs/patentr?svg=true)](https://ci.appveyor.com/project/JYProjs/patentr)
[![Coverage Status](https://img.shields.io/codecov/c/github/JYProjs/patentr/master.svg)](https://codecov.io/github/JYProjs/patentr?branch=master)

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![CRAN version](http://www.r-pkg.org/badges/version/patentr)](https://CRAN.R-project.org/package=patentr)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/patentr)](https://CRAN.R-project.org/package=patentr)

## Overview

The patentr R package allows easy access to USPTO (United States Patent and Trademark Office) bulk patent data in tidy, rectangular format.
By downloading, converting, and storing patent data directly from the USPTO website, patentr minimizes the work needed to acquire usable data, allowing users to focus on analyzing the data.

## Installation

To install patentr, run the following R code:

```r
# install from CRAN
install.packages("patentr")

# install development version from GitHub
devtools::install_github("JYProjs/patentr")
```

## Sample code

Bulk patent data can be downloaded using the year and week (within each year)
as follows:

```r
# load patentr
library("patentr")

# download patents from the first week of 1976
get_bulk_patent_data(year = 1976,
                     week = 1,
                     output_file = "patent_output1.csv")

# download patents from:
#   1. week 1 of 1976 (TXT format in USPTO)
#   2. week 48 of 2002 (XML format 1 in USPTO)
#   3. week 19 of 2006 (XML format 2 in USPTO)
# N.B. it will take a few minutes to run the next line
get_bulk_patent_data(year = c(1976, 2002, 2006),
                     week = c(1, 48, 19),
                     output_file = "patent_output2.csv")
```

### Data collected for each patent

* patent title
* application date
* issue date
* inventor name(s)
* assignee name(s)
* ICL classification
* unique identifier (WKU)
* referenced patent numbers
* claims

## Contribute

To contribute to patentr, you can create issues for any bugs/suggestions on the [issues page](https://github.com/JYProjs/patentr/issues).
You can also fork the patentr repository and create pull requests to add features you think will be useful for users.

## Citation

Wadhwa RR, Yu J, Beltz H, Desai MY, Érdi P, Scott JG. patentr: Access USPTO Bulk Data in Tidy Rectangular Format. 2021; R package version 0.1.0. URL https://github.com/JYProjs/patentr.
