[![Appveyor build Status](https://ci.appveyor.com/api/projects/status/d4yfx75uiaaamich/branch/master?svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-heatsine/branch/master)
[![Travis build Status](https://travis-ci.com/KWB-R/kwb.heatsine.svg?branch=master)](https://travis-ci.com/KWB-R/kwb.heatsine)
[![codecov](https://codecov.io/github/KWB-R/kwb.heatsine/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.heatsine)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.heatsine)]()
[![DOI](https://zenodo.org/badge/274332678.svg)](https://zenodo.org/badge/latestdoi/274332678)

Requires daily temperature times series in a
surface water body and one groundwater observation well (in case of an
production well this data needs to be cleaned in order to reduce
temperature fluctuations due to the operation scheme!).

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.heatsine' from GitHub
remotes::install_github("KWB-R/kwb.heatsine")
```
