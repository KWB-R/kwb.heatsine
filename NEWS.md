# [kwb.heatsine 0.1.1](https://github.com/KWB-R/kwb.heatsine/releases/tag/v0.1.1) <small>2020-06-29</small>

Added the following features as requested by @chsprenger

* [Tutorial](https://github.com/KWB-R/kwb.heatsine/blob/123a43c1df4b79141897711180259a91250d4025/vignettes/tutorial.Rmd) vignette: do not use monitoring label in title 

* Modified `plot_prediction_interactive()`

    + Use `ggplot2::geom_point()` instead of `ggplot2::geom_vline()` for plotting 
    special (min, max, turning-points)
    
    + Unable to add `prediction interval` in legend due to missing functionality 
    in function used for interactive plotting `plotly::ggplotly()` (see issue described here: https://github.com/ropensci/plotly/issues/1164)


# [kwb.heatsine 0.1.0](https://github.com/KWB-R/kwb.heatsine/releases/tag/v0.1.0) <small>2020-06-29</small>

* First beta-release for internal review by @chsprenger

# kwb.heatsine 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.

* see https://style.tidyverse.org/news.html for writing a good `NEWS.md`


