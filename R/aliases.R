# Type Conversions ----
#' @export
as.chr <- as.character
#' @export
as.lgl <- as.logical
#' @export
as.fct <- function(x) {as.factor(x)}  # Use base instead of forcats for arranged levels
#' @export
as.df <- as.data.frame
#' @export
as.dt <- data.table::as.data.table

# Pythonic ----
#' @export
len <- length

# Frequently used ----
#' @export
s <- function(...) {stopifnot(interactive()); str(...)}

#' @export
sn <- function(...) {purrr::set_names(...)}

#' @export
`%ni%` <- Negate(`%in%`)
