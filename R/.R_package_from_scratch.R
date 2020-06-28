### How to build an R package from scratch

usethis::create_package(".")

fs::file_delete(path = "DESCRIPTION")

author <- list(
  name = "Michael Rustler",
  orcid = "0000-0003-0647-7726",
  url = "https://mrustl.de"
)

pkg <- list(
  name = "kwb.heatsine",
  title = "R Package for Calculating Hydraulic Travel Times based on Sinus Temperature Fitting",
  desc  = paste(
    "Requires daily temperature times series in a surface water body and one groundwater",
    "observation well (in case of an production well this data needs to be cleaned",
    "in order to reduce temperature fluctuations due to the operation scheme!)."
  )
)

kwb.pkgbuild::use_pkg(
  author,
  pkg,
  version = "0.0.0.9000",
  stage = "experimental"
)

usethis::use_vignette("Tutorial")

### R functions

kwb.pkgbuild::use_autopkgdown("kwb.heatsine")

kwb.pkgbuild::create_empty_branch_ghpages("kwb.heatsine")
