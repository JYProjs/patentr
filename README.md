# patentr: Access USPTO Bulk Data in Tidy Rectangular Format

[![Travis-CI Build Status](https://api.travis-ci.com/JYProjs/patentr.svg?branch=main)](https://travis-ci.com/github/JYProjs/patentr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/JYProjs/patentr?branch=master&svg=true)](https://ci.appveyor.com/project/JYProjs/patentr)
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

Bulk patent data in TXT format (1976-2001) can be downloaded using the year and week (within each year) as follows:

```r
# download patents from the first week of 1976 and get data frame
patent_data <- get_bulk_patent_data(year = 1976, week = 1)

# download patents from the last 5 weeks of 1980
# and store in a CSV file named "patent-data.csv"
get_bulk_patent_data(year = rep(1980, 5), week = 48:52,
                     output_file = "patent-data.csv")
```

## Functionality

### Data collected for each patent

* patent title
* application date
* patent issue date
* text in patent abstract
* inventor name(s)
* assignee name(s)
* ICL classification
* unique identifier (AKA patent number)
* referenced patent numbers

## Contribute

To contribute to patentr, you can create issues for any bugs/suggestions on the [issues page](https://github.com/JYProjs/patentr/issues).
You can also fork the patentr repository and create pull requests to add features you think will be useful for users.

## Citation

Wadhwa RR, Yu J, Erdi P. patentr: Access USPTO Bulk Data in Tidy Rectangular Format. 2021; R package version 0.1.0. URL https://github.com/JYProjs/patentr.
