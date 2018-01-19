# Partials for my favorite parameterization when applying functions

# Tolerate NAs ----
#' @export
max_ <- function (...) max(na.rm = TRUE, ...)
#' @export
min_ <- function(...) min(na.rm = TRUE, ...)
#' @export
sum_ <- function (...) sum(na.rm = TRUE, ...)
#' @export
mean_ <- function(...) mean(na.rm=TRUE, ...)
#' @export
table_ <- function (...) table(useNA = "ifany", ...)


# Control output ----

#' Wrapper for \code{\link[utils]{str}}
#'
#' @param object objects to inspect
#' @param ... args passed to str
#'
#' @return side effect result of `str()` call
#'
#' @export
#' @examples
#' s(mtcars)
#' s(mtcars, list.len=Inf)
str_ <- s <- function (object, list.len=10, max.level=2, strict.width="cut", ...)
{
    str(object, list.len=list.len,
        max.level=max.level, strict.width=strict.width, ...)
}

#' @export
lib <- function (...) library(warn.conflicts = FALSE, quietly = TRUE, ...)


# Misc Prefered Parameterization ----
#' @export
beep_ <- function (...) beepr::beep(11, ...)

