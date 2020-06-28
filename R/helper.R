# add_phi ----------------------------------------------------------------------
add_phi <- function(df, period_length, dbg = FALSE)
{
  kwb.utils::setColumns(
    df,
    phi = 2 * pi * kwb.utils::selectColumns(df, "day_number") / period_length,
    dbg
  )
}

# extdata_file -----------------------------------------------------------------

#' Get Path to File in This Package
#'
#' @param \dots parts of path passed to \code{\link{system.file}}
#' @export
extdata_file <- function(...) {
  system.file("extdata", ..., package = "kwb.heatsine")
}

# simulate ---------------------------------------------------------------------
simulate <- function(model, day_number, period_length, ...)
{
  predict(
    model,
    newdata = add_phi(tibble::tibble(day_number = day_number), period_length),
    ...
  )
}
