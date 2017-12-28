# Partials for my favorite parameterization when applying functions

# Tolerate NAs ----
#' @export
max_ <- function (...) max(na.rm = TRUE, ...)

#' @export
min_ <- function(...) min(na.rm = TRUE, ...)


#' @export
sum_ <- function (...) sum(na.rm = TRUE, ...)

#' @export
table_ <- function (...) table(useNA = "ifany", ...)


# Control output ----
#' @export
str_ <- function (...) str(max.level = 1, ...)
#' @export
lib <- function (...) library(warn.conflicts = FALSE, quietly = TRUE, ...)


# Misc Prefered Parameterization ----
#' @export
beep_ <- function (...) beepr::beep(11, ...)

